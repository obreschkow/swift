#' Fetch particles associated with one or more cells
#'
#' @importFrom cooltools tick tock
#'
#' @description
#' Retrieves the particles associated with one or more cells from the currently loaded data.
#'
#' @details
#' This function assumes that cell and particle data are already available in
#' `swift$cells` and `swift$particles$cells` objects, respectively.
#'
#' @param index Integer vector of cell indices, interpreted as row-index in the cell table `swift$cells`.
#' @param unwrap Logical flag to control whether the objects are unwrapped using periodic boundary conditions. Requires size of simulation box to be specified in `swift$simulation$BoxSize`. If multiple cells are loaded via a vector index, they are all unwrapped collectively, not cell-by-cell.
#' @param properties Vector of character strings specifying the properties to be extracted. If `NULL` all available properties are returned. For vector properties, provide the vector name (e.g. `Coordinates`) without component index (e.g. `Coordinates.1`).
#' @param species Integer vector of particle species to include. Recognized values are:
#' `0` (gas), `1` (dark matter), `4` (stars), and `5` (black holes). If `NULL`, all available species are used.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @return A list of particle matrices, one for each selected species (e.g., `PartType1`, `PartType4`, etc.).
#'
#' @export

fetchCell = function(index, unwrap=TRUE, properties=NULL, species=NULL, verbose=FALSE) {

  if (verbose) cooltools::tick('Fetch particles in cell(s)')

  if (is.null(swift$cells)) stop('swift$cells table is not available')
  if (is.null(swift$particles)) stop('swift$particles data are not available')
  if (is.null(swift$particles$cells)) stop('swift$particles$cells data are not available')

  bindSwift(cells)
  bindSwift(particles,"particles$cells")

  # basic input checks
  if (is.null(species)) {
    species = availableSpecies('cells')
  } else {
    if (!all(species%in%availableSpecies('cells'))) stop('not all species are available')
  }
  if (unwrap) {
    if (is.null(swift$simulation) || is.null(swift$simulation$BoxSize)) {
      stop('swift$simulation$BoxSize must be available if unwrap=TRUE. Consider loading metadata via initialiseSwift() or using setSwift().')
    }
    BoxSize = swift$simulation$BoxSize
    if (length(BoxSize)==1) BoxSize = rep(BoxSize,3)
    wrapped = rep(FALSE,3)
  }

  x = list()
  npart.tot = 0

  for (s in species) {

    field = sprintf('PartType%d',s)

    # determine properties to be extracted
    if (is.null(properties)) {
      prop = particles[[field]]$properties
    } else {
      prop = intersect(properties,particles[[field]]$properties)
    }
    if (length(prop) == 0) warning(sprintf("No valid properties found for species %d", s))

    # determine number of particles
    npart = as.numeric(cells[[sprintf('npart.%d',s)]][index])
    npart.this = sum(npart)

    if (npart.this>0) {

      npart.tot = npart.tot+npart.this

      # determine all indices of the particles to fetch
      ipart = as.numeric(cells[[sprintf('ipart.%d',s)]][index])
      sel = unlist(
        lapply(seq_along(npart), function(i) {
          if (npart[i] > 0)
            ipart[i] + seq_len(npart[i]) - 1
        }),
        use.names = FALSE
      )

      # extract desired particle properties
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
      if (unwrap && 'Coordinates'%in%prop) {
        for (d in seq(3)) {
          wrapped[d] = wrapped[d] | diff(range(x[[field]]$Coordinates[,d]))>BoxSize[d]/2
        }
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

  if (verbose) cooltools::tock(sprintf('# particles = %d',npart.tot))

  return(x)

}
