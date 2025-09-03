#' Extract and store particles associated with halos
#'
#' @importFrom hdf5r H5File
#' @importFrom utils tail
#' @importFrom bigmemory big.matrix
#' @importFrom cooltools tick tock progress
#' @importFrom data.table data.table shift .N :=
#'
#' @description This function loads properties of particles associated with bound subhalos in a SWIFT simulation, which have previously been selected and stored in the data.frame `swift$halos`.
#'
#' @param species Integer vector of particle types to include. Recognized values are: `0` (gas), `1` (dark matter), `2` (background), `3` sink, `4` (stars), `5` (black holes), and `6` (neutrinos). Defaults to all four.
#' @param properties Character vector of particle properties to load. Defaults to `c("Masses", "Coordinates", "Velocities")`. These properties have to exist in the snapshot files, except for the property `Rank_bound`, which is assumed to exist in the membership file.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @details Particle data is read from multiple subvolume files. For each particle species, only the particles bound to selected subhalos stored in `swift$halos` are loaded, where subhalo associations are red from membership files.\cr
#'
#' Particle data are written into the list `swift$particles`, which contains a separate structure `PartType#` for each particle species.
#' Individual particle properties of a given species are written into a big matrix `swift$particles$PartType#$data`,
#' created via the \code{bigmemory} package. These matrices (one per species) are stored on the disk in the directory
#' `[swift$.paths$tmp/particledata]` with filenames specified in `swift$particles$PartType#$filename`. Data are
#' automatically loaded into the RAM when needed in a fast and memory-lean manner.\cr
#'
#' Each particle occupies one row in `swift$particles$PartType#$data`, halo-by-halo,
#' with halos ordered as in `swift$halos`.\cr
#'
#' For each species `#`, two new columns are added to `swift$halos`: `npart.#` is the number of bound particles of that species in each subhalo, and `ipart.#` gives the 1-based index of the first corresponding particle
#' in the matrix `swift$particles$PartType#$data`.
#' These pointers allow efficient access to all particles belonging to a given subhalo. If \link{sortHaloList} is called
#' before loading the particles, satellite subhalos immediately follow their centrals.
#' In this case, the particle data of each halo, including its substructure, appear contiguously in
#' the particle matrices.\cr
#'
#' All required file paths and filename patterns must be provided via the list `swift$.paths`, using the fields `snapshot`, `membership`, and `tmp`. These paths can be set using the function \link{setPath}.
#'
#' @return None. The loaded particle data is stored in the `swift$particles` list. Pointers to the particle data are added as new columns to `swift$halos` (see details).
#'
#' @seealso \link{setPath}, \link{sortHaloList}
#'
#' @export

loadParticlesInHalos = function(species=NULL, properties=c('Masses','Coordinates','Velocities'), verbose=TRUE) {

  if (verbose) cooltools::tick('Load particles in haloes')

  if (is.null(swift$halos)) stop('swift$halos table is not available')
  if (nrow(swift$halos)==0) stop('swift$halos is an empty table without selected halos')

  if (is.null(species)) {
    species = swift$simulation$PartTypes
  } else {
    if (!all(species %in% swift$simulation$PartTypes)) stop(sprintf('only species %s are recognised for this simulation',paste(swift$simulation$PartTypes,collapse=',')))
  }
  if (length(species)==0) stop('species must have at least one element')

  bindSwift(halos)
  bindSwift(particles)

  # particle species names
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

  # check paths
  for (field in c('snapshot','membership','tmp')) {
    if (is.null(swift$.paths[[field]])) {
      stop(sprintf('no path provided via swift$.paths$%s, please set via setPath()',field))
    }
  }

  # check filenames and count subvolumes
  fn.snapshot.list = Sys.glob(sub('%d','*',swift$.paths$snapshot))
  fn.membership.list = Sys.glob(sub('%d','*',swift$.paths$membership))
  if (length(fn.snapshot.list)==0) stop(sprintf('cannot find file %s',swift$.paths$snapshot))
  if (length(fn.membership.list)==0) stop(sprintf('cannot find file %s',swift$.paths$membership))
  if (length(fn.snapshot.list)!=length(fn.membership.list)) stop('number of snapshot files differs from number of membership files')
  nsubvolumes = length(fn.snapshot.list)

  # make directory for particle data
  directory = paste0(swift$.paths$tmp,'particledata')
  dir.create(directory, showWarnings = FALSE)

  # delete existing particles
  clearSwift('particles')

  # add extra halo properties for each species
  for (s in species) {
    # number of particles per halo
    oldnames = colnames(halos)
    loadHaloProperties(sprintf('BoundSubhalo/NumberOf%s',PartTypeName(s)),verbose=FALSE) # number of particles per halo
    colnames(halos) = c(oldnames, sprintf("npart.%d",s))
    # first particle index in particle table
    halos[[sprintf('ipart.%d',s)]] = 1+cumsum(as.double(data.table::shift(halos[[sprintf('npart.%d',s)]],fill=0)))
  }

  # make temporary index to write particles
  current.pointer = matrix(0,nrow=nrow(halos),ncol=length(species))
  colnames(current.pointer) = paste0('PartType',species)
  for (s in species) {
    field = sprintf('PartType%d',s)
    current.pointer[,field] = halos[[sprintf('ipart.%d',s)]]
  }

  # initialize list holding/linking the particle data
  str = c()
  for (field in c('simulation','halos')) {
    if (!is.null(swift[[field]])) str = c(str,digest::digest(swift[[field]], algo='md5'))
  }
  str = digest::digest(str, algo='md5')
  particles = list()
  for (s in species) {
    field = sprintf('PartType%d',s)
    particles[[field]] = list(filename = paste0(str,'_',s),
                             npart = sum(halos[[sprintf('npart.%d',s)]]))
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
    file.membership = hdf5r::H5File$new(fn.membership.list[isub], mode = "r")
    file.snapshot = hdf5r::H5File$new(fn.snapshot.list[isub], mode = "r")

    for (s in species) {

      field = sprintf('PartType%d',s)

      if (!is.null(particles[[field]]$data)) {

        # read membership and only keep particles that belong to a selected subhalo
        isubhalo = match(file.membership[[paste(field,'GroupNr_bound',sep='/')]]$read(), halos$HaloCatalogueIndex)
        sel = which(!is.na(isubhalo))

        if (length(sel)>0) {

          npart.check = npart.check+length(sel)

          # evaluate fraction of items to read from the dataset
          fselected = length(sel)/length(isubhalo) # fraction of selected particles

          # determine index of each particle in the particle array
          isubhalo = isubhalo[sel]
          dt = data.table(idx = isubhalo, id = seq_along(isubhalo))
          nrep = dt[, x := seq_len(.N) - 1, by = idx]$x # vector of same length as isubhalo, giving the cummulative count a value of isubhalo has appeared already
          index = current.pointer[isubhalo,field]+nrep
          rm(isubhalo)

          # increase the values of current.pointer[,field] for 'index' computation at next iteration
          occurrence_dt = dt[, .N, by = idx]
          i = occurrence_dt$idx  # unique values in isubhalo
          N = occurrence_dt$N    # number of occurrences of each value of isubhalo
          current.pointer[i,field] = current.pointer[i,field]+N
          rm(dt, occurrence_dt)

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
              iprogress = iprogress+length(sel)
              cooltools::progress(sprintf('%.2f%%',iprogress/nprogress*100))
              particles[[field]]$data[index,icol] = .simplify(x)
            } else {
              # NOTE: In this case, the code does not use fselected to distinguish between
              # a partial reading mode (x = file.snapshot[[group]][,sel,drop=FALSE])
              # and the full reading mode, as the former can get stuck for large vectors.
              x = file.snapshot[[group]]$read()[,sel,drop=FALSE]
              for (d in seq_len(particles[[field]]$ncolprop[iprop])) {
                icol = icol+1
                iprogress = iprogress+length(sel)
                cooltools::progress(sprintf('%.2f%%',iprogress/nprogress*100))
                particles[[field]]$data[index,icol] = x[d,]
              }
            }
          }

          if (icol!=length(particles[[field]]$colnames)) stop('column number mismatch')

        }
      }
    }

    file.snapshot$close_all()
    file.membership$close_all()

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
