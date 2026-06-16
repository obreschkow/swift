#' Fetch particles associated with a subhalo
#'
#' @importFrom cooltools tick tock
#'
#' @description
#' Retrieves the particles associated with a subhalo, and optionally its substructure, from the currently loaded data.
#'
#' @details
#' This function assumes that halo and particle data have already been loaded into memory
#' via the `swift$halos` and `swift$particles$halos` objects, respectively.
#'
#' @param index Integer halo index. By default, this index is interpreted as row-index in the halo table `swift$halos`.
#' @param isHaloCatalogueIndex Logical flag. If \code{TRUE}, the \code{index} value is taken to be the unique `HaloCatalogueIndex` available in the file `swift$.paths$halos` and locally stored in an identically named column of `swift$halos`.
#' @param unwrap Logical flag to control whether the objects are unwrapped using periodic boundary conditions. Requires size of simulation box to be specified in `swift$simulation$BoxSize`.
#' @param properties Vector of character strings specifying the properties to be extracted. If `NULL` all available properties are returned. For vector properties, provide the vector name (e.g. `Coordinates`) without component index (e.g. `Coordinates.1`).
#' @param substructure Logical flag for including substructure. If \code{TRUE} and if the requested index refers to a central subhalo, particles from its satellite subhalos are included.
#' @param species Integer vector of particle species to include. Recognized values are:
#' `0` (gas), `1` (dark matter), `4` (stars), and `5` (black holes). If `NULL`, all available species are used.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @return A list of particle matrices, one for each selected species (e.g., `PartType1`, `PartType4`, etc.).
#'
#' @export

fetchHalo = function(index, isHaloCatalogueIndex=FALSE, unwrap=TRUE, properties=NULL, substructure=FALSE, species=NULL, verbose=FALSE) {

  if (verbose) cooltools::tick('Fetch particles in halo')

  if (is.null(swift$halos)) stop('swift$halos table is not available')
  if (is.null(swift$particles)) stop('swift$particles data are not available')
  if (is.null(swift$particles$halos)) stop('swift$particles$halos data are not available')

  bindSwift(halos)
  bindSwift(particles,"particles$halos")

  # basic input checks
  if (length(index)>1) stop('index must be a single value')
  if (is.null(species)) {
    species = availableSpecies('halos')
  } else {
    if (!all(species%in%availableSpecies('halos'))) stop('not all species are available')
  }
  if (unwrap) {
    if (is.null(swift$simulation) || is.null(swift$simulation$BoxSize)) {
      stop('swift$simulation$BoxSize must be available if unwrap=TRUE. Consider loading metadata via initialiseSwift() or using setSwift().')
    }
    BoxSize = swift$simulation$BoxSize
    if (length(BoxSize)==1) BoxSize = rep(BoxSize,3)
    wrapped = rep(FALSE,3)
  }

  # handle index argument
  if (isHaloCatalogueIndex) {
    if (is.null(halos$HaloCatalogueIndex)) stop('halos$HaloCatalogueIndex needed, but does not exist')
    row = match(index, halos$HaloCatalogueIndex)
    if (is.na(row)) stop("Some HaloCatalogueIndex values not found in swift$halos.")
  } else {
    row = index
    if (row<1 | row>nrow(halos)) stop(sprintf('row index must be between 1 and the number of halos (%d)',nrow(halos)))
  }

  # handle substructure
  if (substructure & halos$HostHaloIndex[row]==0) {
    allrows = seq(row,row+halos$NumberOfSubhalos[row])
  } else {
    allrows = row
  }

  x = list()
  npart.tot = 0

  for (s in species) {

    field = sprintf('PartType%d',s)

    # determine particle indices
    ipart = halos[[sprintf('ipart.%d',s)]][row]
    npart = sum(halos[[sprintf('npart.%d',s)]][allrows])
    if (npart==0) {
      sel = integer(0)
    } else {
      sel = seq(ipart,ipart+npart-1)
    }
    npart.tot = npart.tot+npart

    # determine properties to be extracted
    if (is.null(properties)) {
      prop = particles[[field]]$properties
    } else {
      prop = intersect(properties,particles[[field]]$properties)
      if (length(prop) == 0) warning(sprintf("No valid properties found for species %d", s))
    }

    # extract properties
    x[[field]] = list()
    for (iprop in seq_along(prop)) {
      property = prop[iprop]
      ncol = particles[[field]]$ncolprop[which(particles[[field]]$properties==property)]
      if (ncol==1) {
        ip = which(particles[[field]]$colnames==property)
        if (length(ip)!=1) stop('particle property indexing error')
        x[[field]][[property]] = particles[[field]]$data[sel,ip]
      } else {
        mat = matrix(NA, nrow=length(sel), ncol=ncol)
        for (icol in seq_len(ncol)) {
          ip = which(particles[[field]]$colnames==sprintf('%s.%d',property,icol))
          if (length(ip)!=1) stop('particle property indexing error')
          mat[,icol] = particles[[field]]$data[sel,ip]
        }
        x[[field]][[property]] = mat
      }
    }

    # Unwrap coordinates
    if (unwrap && 'Coordinates'%in%prop && nrow(x[[field]]$Coordinates)>0) {
      for (d in seq(3)) {
        wrapped[d] = wrapped[d] | diff(range(x[[field]]$Coordinates[,d]))>BoxSize[d]/2
      }
    }

  }

  # Unwrap coordinates
  if (unwrap && any(wrapped)) {
    for (s in species) {
      field = sprintf('PartType%d',s)
      if (!is.null(x[[field]]$Coordinates) && nrow(x[[field]]$Coordinates)>0) {
        for (d in which(wrapped)) {
          L = BoxSize[d]
          x[[field]]$Coordinates[,d] = (x[[field]]$Coordinates[,d]+L/2)%%L+L/2
        }
      }
    }
  }

  # Add halo data of selected subhalos
  x$halos = halos[allrows,]

  # also wrap coordinates in halo data
  if (unwrap && any(wrapped)) {
    tc = transformationCols('position')
    for (d in which(wrapped)) {
      L = BoxSize[d]
      for (icol in tc[[d]]) {
        x$halos[[icol]] = (x$halos[[icol]]+L/2)%%L+L/2
      }
    }
  }

  # Add other post-processing information
  x$Information = list(substructure=substructure, unwrap=unwrap)

  if (verbose) cooltools::tock(sprintf('# particles = %d',npart.tot))

  return(x)

}
