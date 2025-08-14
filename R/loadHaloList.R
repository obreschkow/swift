#' Load basic halo list
#'
#' @importFrom cooltools tick tock
#' @importFrom hdf5r H5File
#'
#' @description Loads basic halo properties from an HDF5 file and constructs a `data.frame` with key columns required for further analysis.
#'
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details Creates a `data.frame` of halo properties accessible via `swift$halos`.
#' Each row represents a subhalo, which is either a central subhalo (excluding substructure)
#' or a satellite subhalo (associated with a central). The table includes the following columns:
#'
#' The full filename of the halo catalogue must be available in `swift$.paths$halos`, which can be set using the function \link{setPath}.
#'
#' \itemize{
#'   \item \code{HaloCatalogueIndex}: Unique subhalo index.
#'   \item \code{TotalMass}: Total mass of all particles in the subhalo, in the units of the halo file.
#'   \item \code{HostHaloIndex}: For satellite subhalos, this is the row-index of the associated central subhalo.
#'         For central subhalos, this is 0.
#'   \item \code{SubhaloRankByBoundMass}: For satellite, this is the rank of the subhalo among the satellites
#'         belonging to the same central, from most to least massive. For centrals, this is 0.
#' }
#'
#' @return None. The loaded halo list is stored in `swift$halos`.
#'
#' @export

loadHaloList = function(verbose=TRUE) {

  if (verbose) cooltools::tick('Load halo list from raw HDF5 file')

  bindSwift(halos)

  if (is.null(swift$.paths$halos)) stop('no filename provided as argument or via swift$.paths$halos, consider setting path using setPath()')

  # Load basic HDF5 data
  file = hdf5r::H5File$new(swift$.paths$halos, mode = "r")
  hdf5structure = file$ls(recursive = TRUE)
  groups = list(c('InputHalos','HaloCatalogueIndex'),
                c('SOAP','HostHaloIndex'),
                c('SOAP','SubhaloRankByBoundMass'),
                c('BoundSubhalo','TotalMass'))
  icol = which(hdf5structure$name==paste0(groups[[1]],collapse='/'))
  nhalos = as.numeric(hdf5structure$dataset.dims[icol])
  halos = as.data.frame(matrix(NA,nrow=nhalos,ncol=0))
  for (group in groups) {
    name = group[2]
    full = paste0(group,collapse='/')
    halos[[name]] = .simplify(file[[full]]$read())
  }
  file$close_all()

  # adjust HostHaloIndex to 1-based indexing
  halos$HostHaloIndex = halos$HostHaloIndex+1

  if (verbose) cooltools::tock(sprintf('# halos = %d',nrow(halos)))

  invisible(NULL)
}
