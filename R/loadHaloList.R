#' Load basic halo list
#'
#' @importFrom bit64 as.integer64
#' @importFrom cooltools readhdf5 tick tock userattributes
#' @import data.table
#'
#' @description Loads basic halo properties from an HDF5 file and constructs a `data.table` with key columns required for further analysis.
#'
#' @param filename Full path of the HDF5 halo file.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details Creates a `data.table` of halo properties and assigns it to `swift$halos` via an active binding for convenient in-scope access using the variable `halos`.
#'
#' @return None. The loaded halo list is stored in `swift$halos`.
#'
#' @export

loadHaloList = function(filename, verbose=TRUE) {

  if (verbose) cooltools::tick('Load halo list from raw HDF5 file')

  bindHalos() # makes 'halos' a pointer to swift$halos

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
  halos = data.table::data.table(TotalMass=simplify(dat$BoundSubhalo$TotalMass),
                                 HostHaloIndex=simplify(dat$SOAP$HostHaloIndex+1),
                                 SubhaloRankByBoundMass=simplify(dat$SOAP$SubhaloRankByBoundMass),
                                 HaloCatalogueIndex=simplify(dat$InputHalos$HaloCatalogueIndex))

  if (verbose) cooltools::tock(sprintf('# halos = %d',dim(halos)[1]))

}
