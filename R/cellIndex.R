#' @useDynLib swift, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

#' Map 3D points to cubic or spherical cells
#'
#' Efficiently computes all matches between 3D \code{points} and a set of 3D \code{cells},
#' where each cell is either an axis-aligned cube or a sphere. A periodic cubic simulation
#' box of side length \code{boxsize} can be enabled; distances then use the minimal-image convention.
#'
#' Internally, points are hashed into an efficient 3D voxel grid to avoid scanning all pairs,
#' and the algorithm uses a fast C++ implementation.
#'
#' @param points N-by-3 matrix containing the 3D Cartesian coordinates (x,y,z) of N points.
#'   Rows containing any \code{NA} are ignored.
#' @param cells M-by-4 matrix specifying M cubic or spherical cells. The first three columns contain the Cartesian (x,y,z);
#' the forth column contains a \code{size} value: if positive, this value is interpreted as the radius of a sphere,
#' if negative, its absolute value is interpreted as the side length of a cube. Cells with \code{size=0} or \code{NA} are skipped.
#' @param boxsize Numeric scalar giving the side length \eqn{L} of the periodic cube. If \code{NA} or \eqn{\le 0},
#'   the domain is treated as non-periodic (no wrapping).
#' @param voxel_size Optional numeric scalar: voxel edge length for the internal spatial hash. If
#'   \code{NA}, defaults to the median \emph{effective width} across \code{cells}, where
#'   effective width is \code{width} for cubes and \code{2*radius} for spheres. This only affects
#'   performance (granularity of the hash), not correctness.
#'
#' @return An integer matrix with two columns \code{point_idx} and \code{cell_idx}. Each row \eqn{(i,j)}
#'   means \code{points[i, ]} lies inside \code{cells[j, ]}. Indices are 1-based.
#'
#' @details
#' \strong{Membership tests}:
#' \itemize{
#'   \item Sphere: \eqn{||\Delta||^2 \le r^2}.
#'   \item Cube: \eqn{|dx| \le w/2, |dy| \le w/2, |dz| \le w/2}.
#' }
#' With periodic boundaries, deltas use the minimal-image convention:
#' \eqn{\Delta \leftarrow \Delta - L\,\mathrm{round}(\Delta/L)}.
#'
#' Boundary checks are inclusive (\eqn{\le}).
#'
#' @section Performance notes:
#' In the periodic case the voxel grid has \code{floor(boxsize / voxel_size)} cells per axis
#' (guarded to \eqn{\le 2{,}097{,}151}). Choose \code{voxel_size} to balance voxel count vs.
#' candidates per voxel. The default heuristic (median effective width) is a robust start.
#'
#' @examples
#' # Minimal PBC sanity check: a point near the top edge matching a wrapped sphere
#' L = 1
#' pts = rbind(c(0.3, 0.9, 0.9))
#' cls = rbind(c(0.1, 0.2, 0.3, 0.55))  # sphere radius 0.55 (> sqrt(0.29))
#' cellIndex(pts, cls, boxsize = L)
#'
#' # Larger toy example to illustrate speed
#' boxsize = 7
#' npoints = 1e6
#' ngrid = 100 # number of cubic cells a side in a Cartesian grid
#' points = array(runif(npoints*3,0,boxsize),c(npoints,3))
#' cellwidth = boxsize/ngrid
#' x = (seq(ngrid)-0.5)/ngrid*boxsize
#' cells = cbind(expand.grid(x,x,x),-cellwidth)
#' matches = cellIndex(points, cells, boxsize)
#' cat(sprintf("Number of points = %d\n", nrow(points)))
#' cat(sprintf("Number of cells = %d\n", nrow(cells)))
#' cat(sprintf("Number of matches = %d\n", nrow(matches)))
#' cat(sprintf("Average number of points per cell = %.2f\n", nrow(matches)/nrow(cells)))
#'
#' @export
cellIndex = function(points, cells, boxsize=NA_real_, voxel_size=NA_real_) {
  if (!is.matrix(points)) points = as.matrix(points)
  if (!is.matrix(cells))  cells  = as.matrix(cells)
  stopifnot(ncol(points) == 3L, ncol(cells) == 4L)
  storage.mode(points) = "double"
  storage.mode(cells)  = "double"
  point_cell_matches_cpp(points, cells, boxsize, voxel_size)
}
