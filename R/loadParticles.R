#' Load and regroup particles from snapshot files
#'
#' @importFrom hdf5r H5File
#' @importFrom utils tail
#' @importFrom bigmemory big.matrix
#' @importFrom cooltools tick tock progress
#' @importFrom data.table data.table shift .N :=
#'
#' @description This function loads properties of particles of a SWIFT simulation snapshot, accounting for multiple subvolumes if present. The particles are filtered and regrouped according to the \code{method} argument.
#'
#' @param method One of the following character strings
#' \itemize{
#'   \item \code{"halos"}: Loads all the particles associated with the subhalos stored in the data.frame `swift$halos`, normally created earlier via \link{loadHaloList}. Subhalo associations are red from membership files, whose path must be set in `swift$.paths$membership`.
#'   \item \code{"cells"}: Loads all the particles specified in the data.frame `swift$cells`, which must contain columns named `x`, `y`, `z`, specifying the cell centres, as well as columns named `width` and/or `radius` specifying, either the side-length of cubic cells or the radius of spherical ones.
#' }
#' @param species Integer vector of particle types to include. Recognized values are: `0` (gas), `1` (dark matter), `2` (background), `3` sink, `4` (stars), `5` (black holes), and `6` (neutrinos). Defaults to all four.
#' @param properties Character vector of particle properties to load. Defaults to `c("Masses", "Coordinates", "Velocities")`. These properties have to exist in the snapshot files, except for the property `Rank_bound`, which is assumed to exist in the membership file.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @details Particle data is read from multiple subvolume files.\cr
#'
#' Particle data are written into the list `swift$particles`, which contains a separate structure `PartType#` for each particle species.
#' Individual particle properties of a given species are written into a big matrix `swift$particles$PartType#$data`,
#' created via the \code{bigmemory} package. These matrices (one per species) are stored on the disk in the directory
#' `swift$.paths$tmp/"particledata"` with filenames specified in `swift$particles$PartType#$filename`. Data are
#' automatically loaded into the RAM when needed in a fast and memory-lean manner.\cr
#'
#' Each particle occupies one row in `swift$particles$PartType#$data`, and successive groups of particles correspond to successive rows in `swift$halos` or `swift$cells`.\cr
#'
#' For each species `#`, two new columns are added to `swift$halos`: `npart.#` is the number of bound particles of that species in each subhalo, and `ipart.#` gives the 1-based index of the first corresponding particle
#' in the matrix `swift$particles$PartType#$data`.
#' These pointers allow efficient access to all particles belonging to a given subhalo or cell. In the case of subhalos, if \link{sortHaloList} is called
#' before loading the particles, satellite subhalos immediately follow their centrals.
#' In this case, the particle data of each halo, including its substructure, appear contiguously in
#' the particle matrices.\cr
#'
#' All required file paths and filename patterns must be provided via the list `swift$.paths`, using the fields `snapshot`, `membership`, and `tmp`. These paths can be set using the function \link{setPath}.
#'
#' @return None. The loaded particle data is stored in the `swift$particles` list. Pointers to the particle data are added as new columns to `swift$halos` or `swift$cells` (see details).
#'
#' @seealso \link{setPath}, \link{sortHaloList}
#'
#' @export

loadParticles = function(method, species=NULL, properties=c('Masses','Coordinates','Velocities'), verbose=TRUE) {

  if (verbose) cooltools::tick('Load particles')

  # initialise method
  if (method=='halos') {
    if (is.null(swift$halos)) stop('swift$halos table is not available')
    if (nrow(swift$halos)==0) stop('swift$halos is an empty table')
    required.paths = c('snapshot','tmp','membership')
    bindSwift(halos)
  } else if (method=='cells') {
    if (is.null(swift$cells)) stop('swift$cells table is not available')
    if (nrow(swift$cells)==0) stop('swift$cells is an empty table')
    if (is.null(swift$simulation) || is.null(swift$simulation$BoxSize)) {
      stop('swift$simulation$BoxSize must exist when calling makeGridCells. Consider loading metadata via initialiseSwift().')
    } else {
      boxsize = swift$simulation$BoxSize[1]
    }
    required.paths = c('snapshot','tmp')
    bindSwift(cells)
    if (all(c('x','y','z')%in%colnames(cells))) {
      cls = cells[,c('x','y','z'),drop=FALSE]
    } else {
      stop('cells do not contain all of the columnnames "x", "y", "z"')
    }
    if ('radius'%in%colnames(cells)) {
      cls = cbind(cls,cells[,'radius',drop=FALSE])
    } else if ('width'%in%colnames(cells)) {
      cls = cbind(cls,-cells[,'width',drop=FALSE]) # negative sign to keep track of this being a cubic cell
    } else {
      stop('cells do not contain columns "radius" or "width"')
    }
  } else {
    stop('method unknown')
  }

  # check paths
  for (field in required.paths) {
    if (is.null(swift$.paths[[field]])) {
      stop(sprintf('no path provided via swift$.paths$%s, please set via setPath()',field))
    }
  }

  # determine particle species to be loaded
  if (is.null(species)) {
    species = swift$simulation$PartTypes
  } else {
    if (!all(species %in% swift$simulation$PartTypes)) stop(sprintf('only species %s are recognised for this simulation',paste(swift$simulation$PartTypes,collapse=',')))
  }
  if (length(species)==0) stop('species must have at least one element')

  # particle species names, which are used in some SOAP groups instead of numerical species indices
  PartTypeName = function(species) {
    if (species==0) {
      return('GasParticles')
    } else if (species==1) {
      return('DarkMatterParticles')
    } else if (species==2) {
      return('BackgroundParticles')
    } else if (species==3) {
      return('SinkParticles')
    } else if (species==4) {
      return('StarParticles')
    } else if (species==5) {
      return('BlackHoleParticles')
    } else if (species==6) {
      return('NeutrinoParticles')
    } else {
      stop('species not recognised')
    }
  }

  # check snapshot filenames and count subvolumes
  fn.snapshot.list = Sys.glob(sub('%d','*',swift$.paths$snapshot))
  if (length(fn.snapshot.list)==0) stop(sprintf('cannot find file %s',swift$.paths$snapshot))
  nsubvolumes = length(fn.snapshot.list)

  # check membership filenames
  if (method=='halos') {
    fn.membership.list = Sys.glob(sub('%d','*',swift$.paths$membership))
    if (length(fn.membership.list)==0) stop(sprintf('cannot find file %s',swift$.paths$membership))
    if (nsubvolumes!=length(fn.membership.list)) stop('number of snapshot files differs from number of membership files')
  }

  # add particle pointers to halos/cells
  if (method=='halos') {

    for (s in species) {
      # number of particles per halo
      oldnames = colnames(halos)
      loadHaloProperties(sprintf('BoundSubhalo/NumberOf%s',PartTypeName(s)),verbose=FALSE) # number of particles per halo
      colnames(halos) = c(oldnames, sprintf("npart.%d",s))
      # first particle index in particle table
      halos[[sprintf('ipart.%d',s)]] = 1+cumsum(as.double(data.table::shift(halos[[sprintf('npart.%d',s)]],fill=0)))
    }

  } else if (method=='cells') {

    for (s in species) {
      cells[[sprintf('npart.%d',s)]] = 0 # numbers need to be determined later from the particle data
      cells[[sprintf('ipart.%d',s)]] = NA
    }

    for (isub in seq(nsubvolumes)) {
      file.snapshot = hdf5r::H5File$new(fn.snapshot.list[isub], mode = "r")
      for (s in species) {
        pts = t(file.snapshot[[sprintf('PartType%d/Coordinates',s)]]$read())
        if (length(pts)>0) {
          match = cellIndex(pts,cls,boxsize)
          counts = tabulate(match[,2],nbins=nrow(cells))
          cells[[sprintf('npart.%d',s)]] = cells[[sprintf('npart.%d',s)]]+counts
        }
      }
      file.snapshot$close_all()
    }

    for (s in species) {
      cells[[sprintf('ipart.%d',s)]] = 1+cumsum(as.double(data.table::shift(cells[[sprintf('npart.%d',s)]],fill=0)))
    }

  }

  # make temporary index to write particles
  current.pointer = matrix(0,nrow=nrow(get(method)),ncol=length(species))
  colnames(current.pointer) = paste0('PartType',species)
  for (s in species) {
    field = sprintf('PartType%d',s)
    current.pointer[,field] = get(method)[[sprintf('ipart.%d',s)]]
  }

  # make directory for particle data
  directory = paste0(swift$.paths$tmp,'particledata')
  dir.create(directory, showWarnings = FALSE)

  # make active binding to relevant particle container
  bindSwift(particles,paste0("particles$",method))

  # initialize particle container (deletes existing particles in this container)
  particles = list()

  # initialize list holding the particle data
  str = c()
  for (field in c('simulation',method)) {
    if (!is.null(swift[[field]])) str = c(str,digest::digest(swift[[field]], algo='md5'))
  }
  str = digest::digest(str, algo='md5')
  for (s in species) {
    field = sprintf('PartType%d',s)
    particles[[field]] = list(filename = paste0(str,'_',s),
                              npart = sum(get(method)[[sprintf('npart.%d',s)]]))
  }

  # determine columns for each particle species
  file.snapshot = hdf5r::H5File$new(fn.snapshot.list[1], mode = "r")
  hdf5structure = file.snapshot$ls(recursive = TRUE)
  file.snapshot$close_all()
  for (s in species) {

    field = sprintf('PartType%d',s)

    nproperties = 0 # total number of properties
    ncol = 0 # total number of columns
    ncolprop = c() # number of columns for each property
    prop = c()
    colnames = c()

    for (property in properties) {

      group = paste(field,property,sep='/')
      icol = which(hdf5structure$name==group)

      if (length(icol)==1 || property=='Rank_bound') {

        if (property=='Rank_bound') {
          if (method!='halos') stop('Rank_bound property on available for method "halos"')
          dims = 0
        } else {
          dim_str = hdf5structure$dataset.dims[icol]
          dims = as.numeric(strsplit(dim_str, " x ")[[1]])
        }

        if (is.null(dims) || any(is.na(dims)) || length(dims)>2) {

          stop(sprintf("Format of object %s not supported",group))

        } else {

          if (length(dims)==1) {

            # handle properties
            nproperties = nproperties+1
            prop = c(prop,property)

            # handle columns
            ncol = ncol+1
            ncolprop = c(ncolprop,1)
            colnames = c(colnames,property)

          } else {

            # handle properties
            nproperties = nproperties+1
            prop = c(prop,property)

            # handle columns
            ncol = ncol+dims[1]
            ncolprop = c(ncolprop,dims[1])
            for (d in seq(dims[1])) {
              colnames = c(colnames,sprintf('%s.%d',property,d))
            }

          }
        }

      } else if (length(icol)>1) {

        stop(sprintf("Dataset %s not unique",group))

      }

    }

    # save column properties
    particles[[field]]$properties = prop
    particles[[field]]$ncolprop = ncolprop
    particles[[field]]$colnames = colnames
  }

  # allocate large data array
  options(bigmemory.allow.dimnames=TRUE)
  for (s in species) {
    field = sprintf('PartType%d',s)
    if (particles[[field]]$npart>0) {
      for (ext in c('.bin','.txt')) {
        full = paste0(directory,'/',particles[[field]]$filename,ext)
        if (file.exists(full)) file.remove(full)
      }
      particles[[field]]$data = bigmemory::big.matrix(nrow=particles[[field]]$npart,
                                                      ncol=length(particles[[field]]$colnames),
                                                      type="double",
                                                      backingpath=directory,
                                                      backingfile=paste0(particles[[field]]$filename,".bin"),
                                                      descriptorfile=paste0(particles[[field]]$filename,".txt"))
      colnames(particles[[field]]$data) = particles[[field]]$colnames
    }
  }

  # read particles and write into list
  npart.tot = 0
  nprogress = 0
  for (s in species) {
    field = sprintf('PartType%d',s)
    npart.tot = npart.tot+particles[[field]]$npart
    nprogress = nprogress+particles[[field]]$npart*length(particles[[field]]$colnames)
  }
  iprogress = 0
  npart.check = 0

  for (isub in seq(nsubvolumes)) {

    # initialise HDF5 files
    file.snapshot = hdf5r::H5File$new(fn.snapshot.list[isub], mode = "r")
    if (method=='halos') file.membership = hdf5r::H5File$new(fn.membership.list[isub], mode = "r")

    for (s in species) {

      field = sprintf('PartType%d',s)

      if (!is.null(particles[[field]]$data)) {

        if (method=='halos') {
          # read membership and only keep particles that belong to a selected subhalo
          igroup = match(file.membership[[paste(field,'GroupNr_bound',sep='/')]]$read(), halos$HaloCatalogueIndex)
          sel = which(!is.na(igroup))
          match = cbind(seq_along(sel),igroup[sel])
          fselected = length(sel)/length(igroup) # fraction of selected particles
          rm(igroup)
        } else if (method=='cells') {
          pts = t(file.snapshot[[paste0(field,'/Coordinates')]]$read())
          if (length(pts)>0) {
            match = cellIndex(pts,cls,boxsize)
            sel = sort(unique(match[,1]))
            match[,1] = match(match[,1],sel)
            fselected = length(sel)/nrow(pts) # fraction of selected particles
          } else {
            sel = c()
          }
          rm(pts)
        }

        if (length(sel)>0) {

          npart.check = npart.check+nrow(match)

          # determine index of each particle in the particle array
          ptr = current.pointer[,field]
          dt = data.table(j = match[, 2L])               # take the 2nd column once
          dt[, index := ptr[j] + seq_len(.N) - 1L, by = j]   # take-and-increment per group
          index = dt$index                                  # same length as nrow(match)
          current.pointer[,field] = ptr+tabulate(dt$j, nbins=length(ptr))  # advance pointers
          rm(dt)

          # read properties and write into big matrix
          icol = 0
          for (iprop in seq_along(particles[[field]]$properties)) {

            property = particles[[field]]$properties[iprop]
            group = paste(field,property,sep='/')

            if (particles[[field]]$ncolprop[iprop]==1) {
              icol = icol+1
              if (property=='Rank_bound') {
                if (fselected<.spareReadThreshold) {
                  x = file.membership[[group]][sel]
                } else {
                  x = file.membership[[group]]$read()[sel]
                }
              } else {
                if (fselected<.spareReadThreshold) {
                  x = file.snapshot[[group]][sel]
                } else {
                  x = file.snapshot[[group]]$read()[sel]
                }
              }
              iprogress = iprogress+nrow(match)
              cooltools::progress(sprintf('%.2f%%',iprogress/nprogress*100))
              particles[[field]]$data[index,icol] = .simplify(x)[match[,1]]
            } else {
              # NOTE: In this case, the code does not use fselected to distinguish between
              # a partial reading mode (x = file.snapshot[[group]][,sel,drop=FALSE])
              # and the full reading mode, as the former can get stuck for large vectors.
              x = file.snapshot[[group]]$read()[,sel,drop=FALSE]
              for (d in seq_len(particles[[field]]$ncolprop[iprop])) {
                icol = icol+1
                iprogress = iprogress+nrow(match)
                cooltools::progress(sprintf('%.2f%%',iprogress/nprogress*100))
                particles[[field]]$data[index,icol] = x[d,match[,1]]
              }
            }
          }

          if (icol!=length(particles[[field]]$colnames)) stop('column number mismatch')

        }
      }
    }

    file.snapshot$close_all()
    if (method=='halos') file.membership$close_all()

  }

  if (iprogress!=nprogress) stop('Progress error')
  if (npart.check!=npart.tot) stop('Inconsistent particle count.')

  if (is.null(swift$backup$stage)) {
    particles$addedAfterStage = 0
  } else {
    particles$addedAfterStage = swift$backup$stage
  }

  if (verbose) cooltools::tock(sprintf('# particles = %d',npart.tot))

  invisible(NULL)

}
