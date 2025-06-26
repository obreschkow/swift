#' Load basic halo list
#'
#' @importFrom bit64 as.integer64
#' @importFrom cooltools readhdf5 tick tock userattributes
#' @import data.table
#'
#' @description Loads basic halo properties from an HDF5 file and constructs a `data.table` with key columns required for further analysis.
#'
#' @param filename Full path of the HDF5 halo file. If \code{NULL}, the value stored in `swift$paths$halos` will be used, which can be set with \code{\link{setPath}}.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details Creates a `data.table` of halo properties accessible via `swift$halos`.
#' Each row represents a subhalo, which is either a central subhalo (excluding substructure)
#' or a satellite subhalo (associated with a central). The table includes the following columns:
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
#' For a full example workflow, see the vignette: `vignette("swift-workflow")`
#'
#' @return None. The loaded halo list is stored in `swift$halos`.
#'
#' @seealso \link{vignette("swift-workflow")}
#'
#' @export

loadHaloList = function(filename=NULL, verbose=TRUE) {

  if (verbose) cooltools::tick('Load halo list from raw HDF5 file')

  bindHalos() # makes 'halos' a pointer to swift$halos

  if (is.null(filename)) {
    if (is.null(swift$paths$halos)) {
      stop('no filename provided as argument or via swift$paths$halos, consider setting path using setPath()')
    } else {
      filename = swift$paths$halos
    }
  }

  # remove attributes and converts 64-bit integers to 32-bit integers if possible without loss
  simplify = function(x) {
    myattributes = names(cooltools::userattributes(x))
    for (a in myattributes) attr(x,a) = NULL
    int_min = bit64::as.integer64(-2^31)
    int_max = bit64::as.integer64(2^31 - 1)
    if (inherits(x,"integer64") && all(x >= int_min & x <= int_max)) {
      return(as.integer(x))
    } else {
      return(x)
    }
  }

  # Load basic HDF5 data
  subtree = list(InputHalos=list(HaloCatalogueIndex=NA),
                 SOAP=list(HostHaloIndex=NA, SubhaloRankByBoundMass=NA),
                 BoundSubhalo=list(TotalMass=NA))
  dat = cooltools::readhdf5(filename, subtree = subtree)
  halos = data.table::data.table(HaloCatalogueIndex=simplify(dat$InputHalos$HaloCatalogueIndex),
                                 TotalMass=simplify(dat$BoundSubhalo$TotalMass),
                                 HostHaloIndex=simplify(dat$SOAP$HostHaloIndex+1),
                                 SubhaloRankByBoundMass=simplify(dat$SOAP$SubhaloRankByBoundMass))

  if (verbose) cooltools::tock(sprintf('# halos = %d',nrow(halos)))

  invisible(NULL)
}
