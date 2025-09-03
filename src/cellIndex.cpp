// src/cellIndex.cpp
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// ---- helpers ----

// pack three nonnegative ints (<= 2,097,151) into 64 bits (21 bits each)
inline uint64_t pack_key(int ix, int iy, int iz) {
  const uint64_t mask = ((uint64_t)1 << 21) - 1;
  return (((uint64_t)ix & mask) << 42) |
    (((uint64_t)iy & mask) << 21) |
    ((uint64_t)iz & mask);
}

// wrap a value to [0, L)
inline double wrap(double v, double L) {
  double w = std::fmod(v, L);
  if (w < 0) w += L;
  if (w >= L) w = std::nextafter(L, 0.0);
  return w;
}

// minimal-image separation into [-L/2, L/2)
inline double min_image(double d, double L) {
  return d - L * std::round(d / L);
}

// Map [c-half, c+half] on [0,L) to voxel ranges [lo..hi] (inclusive) with wrap
static inline void interval_to_index_ranges(double c, double half, double L, double h,
                                            int Nx, std::vector<std::pair<int,int>>& ranges) {
  ranges.clear();
  if (2*half >= L) { ranges.emplace_back(0, Nx-1); return; } // covers full axis
  double cm = wrap(c, L);
  double low = cm - half, high = cm + half;

  auto ix = [&](double x)->int{
    int k = (int)std::floor(x / h);
    if (k < 0) k = 0;
    if (k >= Nx) k = Nx - 1;
    return k;
  };

  if (low >= 0 && high < L) {
    int a = ix(low), b = ix(high);
    if (a > b) std::swap(a,b);
    ranges.emplace_back(a,b);
  } else if (low < 0) {
    ranges.emplace_back(0, ix(high));
    ranges.emplace_back(ix(low + L), Nx-1);
  } else { // high >= L
    ranges.emplace_back(0, ix(high - L));
    ranges.emplace_back(ix(low), Nx-1);
  }
}

// ---- exported function ----
// [[Rcpp::export]]
IntegerMatrix point_cell_matches_cpp(NumericMatrix points,  // N x 3
                                     NumericMatrix cells,   // M x 4: x,y,z,size (size>0 sphere radius, size<0 cube width)
                                     double L,              // box side; if finite & >0 => periodic
                                     double cell_size = NA_REAL) {
  const R_xlen_t N = points.nrow();
  const R_xlen_t M = cells.nrow();
  if (points.ncol() != 3) stop("points must be N x 3");
  if (cells.ncol()  != 4) stop("cells must be M x 4 (x,y,z,size)");

  // collect valid point indices
  std::vector<int> valid; valid.reserve(N);
  for (R_xlen_t i=0; i<N; ++i) {
    if (!NumericVector::is_na(points(i,0)) &&
        !NumericVector::is_na(points(i,1)) &&
        !NumericVector::is_na(points(i,2))) valid.push_back((int)i);
  }
  if (valid.empty()) {
    IntegerMatrix out(0,2);
    colnames(out) = CharacterVector::create("point_idx","cell_idx");
    return out;
  }

  // choose default cell_size = median effective width (cube: width; sphere: 2*radius)
  if (NumericVector::is_na(cell_size)) {
    std::vector<double> eff; eff.reserve(M);
    for (R_xlen_t j=0; j<M; ++j) {
      double s = cells(j,3);
      if (!NumericVector::is_na(s) && s != 0.0) {
        double e = (s > 0.0) ? (2.0 * s) : (-s);
        if (e > 0 && R_finite(e)) eff.push_back(e);
      }
    }
    if (eff.empty()) stop("All cell sizes are NA/zero; pass a positive cell_size.");
    std::nth_element(eff.begin(), eff.begin() + eff.size()/2, eff.end());
    cell_size = eff[eff.size()/2];
    if (!(cell_size > 0 && R_finite(cell_size))) stop("Invalid cell_size.");
  }

  const bool periodic = R_finite(L) && (L > 0);

  // ---- build voxel index: map voxel -> list of point indices ----
  std::unordered_map<uint64_t, std::vector<int>> vox2pts;
  int Nx=1, Ny=1, Nz=1;
  double ox=0, oy=0, oz=0; // non-periodic origin
  int shx=0, shy=0, shz=0; // non-periodic shifts

  if (periodic) {
    Nx = std::max(1, (int)std::floor(L / cell_size));
    Ny = Nx; Nz = Nx;
    if (Nx >= (1<<21)) stop("Too many voxels per axis; increase cell_size.");
    vox2pts.reserve(valid.size()/4 + 1);

    for (int k=0; k<(int)valid.size(); ++k) {
      int i = valid[k];
      double px = wrap(points(i,0), L);
      double py = wrap(points(i,1), L);
      double pz = wrap(points(i,2), L);
      int ix = (int)std::floor(px / cell_size); if (ix >= Nx) ix = Nx-1;
      int iy = (int)std::floor(py / cell_size); if (iy >= Ny) iy = Ny-1;
      int iz = (int)std::floor(pz / cell_size); if (iz >= Nz) iz = Nz-1;
      vox2pts[pack_key(ix,iy,iz)].push_back(i);
    }
  } else {
    // anchor at mins, then shift to nonnegative indices
    double minx=R_PosInf, miny=R_PosInf, minz=R_PosInf;
    for (int i : valid) {
      double px=points(i,0), py=points(i,1), pz=points(i,2);
      if (px<minx) minx=px; if (py<miny) miny=py; if (pz<minz) minz=pz;
    }
    ox=minx; oy=miny; oz=minz;

    int min_ix=INT_MAX, min_iy=INT_MAX, min_iz=INT_MAX;
    std::vector<int> ixbuf(valid.size()), iybuf(valid.size()), izbuf(valid.size());
    for (size_t k=0; k<valid.size(); ++k) {
      int i=valid[k];
      int ix=(int)std::floor((points(i,0)-ox)/cell_size);
      int iy=(int)std::floor((points(i,1)-oy)/cell_size);
      int iz=(int)std::floor((points(i,2)-oz)/cell_size);
      ixbuf[k]=ix; iybuf[k]=iy; izbuf[k]=iz;
      if (ix<min_ix) min_ix=ix; if (iy<min_iy) min_iy=iy; if (iz<min_iz) min_iz=iz;
    }
    shx=-min_ix; shy=-min_iy; shz=-min_iz;
    vox2pts.reserve(valid.size()/4 + 1);
    for (size_t k=0; k<valid.size(); ++k) {
      vox2pts[pack_key(ixbuf[k]+shx, iybuf[k]+shy, izbuf[k]+shz)].push_back(valid[k]);
    }
  }

  // ---- sweep cells and test exact membership ----
  std::vector<int> out_i; out_i.reserve((size_t)std::min((double)valid.size()*2.0, 1e7));
  std::vector<int> out_j; out_j.reserve(out_i.capacity());

  // temp ranges for periodic
  std::vector<std::pair<int,int>> xr, yr, zr;

  for (R_xlen_t j=0; j<M; ++j) {
    double cx = cells(j,0), cy = cells(j,1), cz = cells(j,2);
    double s  = cells(j,3);
    if (NumericVector::is_na(cx) || NumericVector::is_na(cy) || NumericVector::is_na(cz) ||
        NumericVector::is_na(s)  || s == 0.0) continue;

    const bool sphere = (s > 0.0);
    const double half = sphere ? s : 0.5 * (-s);   // search half-extent for voxelization
    if (!(half > 0 && R_finite(half))) continue;

    if (periodic) {
      interval_to_index_ranges(cx, half, L, cell_size, Nx, xr);
      interval_to_index_ranges(cy, half, L, cell_size, Ny, yr);
      interval_to_index_ranges(cz, half, L, cell_size, Nz, zr);

      for (auto rx : xr) for (int ix=rx.first; ix<=rx.second; ++ix)
        for (auto ry : yr) for (int iy=ry.first; iy<=ry.second; ++iy)
          for (auto rz : zr) for (int iz=rz.first; iz<=rz.second; ++iz) {
            auto it = vox2pts.find(pack_key(ix,iy,iz));
            if (it == vox2pts.end()) continue;
            const auto& pv = it->second;
            if (sphere) {
              const double r2 = s*s;
              for (int idx : pv) {
                double dx = min_image(points(idx,0) - cx, L);
                double dy = min_image(points(idx,1) - cy, L);
                double dz = min_image(points(idx,2) - cz, L);
                if (dx*dx + dy*dy + dz*dz <= r2) {
                  out_i.push_back(idx+1); out_j.push_back((int)j+1);
                }
              }
            } else { // cube
              for (int idx : pv) {
                double dx = min_image(points(idx,0) - cx, L);
                double dy = min_image(points(idx,1) - cy, L);
                double dz = min_image(points(idx,2) - cz, L);
                if (std::fabs(dx) <= half && std::fabs(dy) <= half && std::fabs(dz) <= half) {
                  out_i.push_back(idx+1); out_j.push_back((int)j+1);
                }
              }
            }
          }
    } else {
      auto idx_from = [&](double v, double o) { return (int)std::floor((v - o) / cell_size); };
      int ix_min = idx_from(cx - half, ox), ix_max = idx_from(cx + half, ox);
      int iy_min = idx_from(cy - half, oy), iy_max = idx_from(cy + half, oy);
      int iz_min = idx_from(cz - half, oz), iz_max = idx_from(cz + half, oz);

      for (int ix=ix_min; ix<=ix_max; ++ix)
        for (int iy=iy_min; iy<=iy_max; ++iy)
          for (int iz=iz_min; iz<=iz_max; ++iz) {
            auto it = vox2pts.find(pack_key(ix+shx,iy+shy,iz+shz));
            if (it == vox2pts.end()) continue;
            const auto& pv = it->second;
            if (sphere) {
              const double r2 = s*s;
              for (int idx : pv) {
                double dx = points(idx,0) - cx;
                double dy = points(idx,1) - cy;
                double dz = points(idx,2) - cz;
                if (dx*dx + dy*dy + dz*dz <= r2) {
                  out_i.push_back(idx+1); out_j.push_back((int)j+1);
                }
              }
            } else { // cube
              for (int idx : pv) {
                double dx = points(idx,0) - cx;
                double dy = points(idx,1) - cy;
                double dz = points(idx,2) - cz;
                if (std::fabs(dx) <= half && std::fabs(dy) <= half && std::fabs(dz) <= half) {
                  out_i.push_back(idx+1); out_j.push_back((int)j+1);
                }
              }
            }
          }
    }
  }

  IntegerMatrix out((int)out_i.size(), 2);
  for (int k=0; k<(int)out_i.size(); ++k) { out(k,0)=out_i[k]; out(k,1)=out_j[k]; }
  colnames(out) = CharacterVector::create("point_idx","cell_idx");
  return out;
}
