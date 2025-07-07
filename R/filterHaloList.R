#' Filter halo list
#'
#' @importFrom cooltools tick tock
#'
#' @description Filters the halo list to a specified subset of `index` values, ensuring internal indices remain valid and consistent.
#'
#' @param index Integer vector of halo indices. By default, these indices are interpreted as row-indices in the halo data.table `swift$halos`.
#' @param isHaloCatalogueIndex Logical flag. If \code{TRUE}, the \code{index} values are taken to be the unique halo indices `HaloCatalogueIndex` available in the file `swift$paths$halos` and locally stored in an identically named column of `swift$halos`.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details The function updates `swift$halos` by keeping only the selected halos and any required central halos to avoid orphaned satellites. Internal indices (`HostHaloIndex`, `SubhaloRankByBoundMass`, `NumberOfSubhalos`) are recomputed accordingly.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

filterHaloList = function(index, isHaloCatalogueIndex=FALSE, verbose=TRUE) {

  if (verbose) cooltools::tick('Filter halos')

  bindSwift(halos)

  # checks
  if (is.null(halos)) stop('halos does not exist')

  # handle index
  if (isHaloCatalogueIndex) {
    if (is.null(halos$HaloCatalogueIndex)) stop('halos$HaloCatalogueIndex needed, but does not exist')
    index = match(index, halos$HaloCatalogueIndex)
    if (any(is.na(index))) stop("Some HaloCatalogueIndex values not found in swift$halos.")
  }
  if (anyDuplicated(index)) stop("Duplicate values found in index.")

  # ensure that all centrals of selected satellites are included, even if they weren't directly selected, to avoid generating orphans
  index.sat = index[halos$HostHaloIndex[index]>0]
  index = union(index,halos$HostHaloIndex[index.sat])

  # select halos
  halos = halos[index,]

  # adjust HostHaloIndex
  halos$HostHaloIndex = match(halos$HostHaloIndex,index)
  halos$HostHaloIndex[halos$SubhaloRankByBoundMass==0] = 0

  # recompute SubhaloRankByBoundMass if it exists
  if (!is.null(halos$SubhaloRankByBoundMass)) {
    halos[HostHaloIndex > 0, SubhaloRankByBoundMass := rank(SubhaloRankByBoundMass, ties.method = "first"), by = HostHaloIndex]
    halos[HostHaloIndex == 0, SubhaloRankByBoundMass := 0]
  }

  # recompute NumberOfSubhalos if it exists
  if (!is.null(halos$NumberOfSubhalos)) {
    satellites = which(halos$HostHaloIndex>0)
    halos$NumberOfSubhalos = 0
    tab = sort(table(halos$HostHaloIndex[satellites]), decreasing = TRUE)
    halos$NumberOfSubhalos[as.integer(names(tab))] = as.integer(tab)
  }

  if (verbose) cooltools::tock(sprintf('# halos = %d',nrow(halos)))

  invisible(NULL)
}
