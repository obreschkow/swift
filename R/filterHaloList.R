#' Filter halo list
#'
#' @importFrom cooltools tick tock
#' @importFrom stats ave
#'
#' @description Filters the halo list to a specified subset of `index` values, ensuring internal indices remain valid and consistent.
#'
#' @param index Integer vector of halo indices or logical vector. Integer indices are interpreted as row-indices in the halo data.frame `swift$halos`, except of `isHaloCatalogueIndex` is set to \code{TRUE}. Logical vectors must have a length matched to the number of halos in `swift$halos`, and are directly indicated which halos to select.
#' @param isHaloCatalogueIndex Logical flag. If \code{TRUE}, the \code{index} values are taken to be the unique halo indices `HaloCatalogueIndex` available in the file `swift$.paths$halos` and locally stored in an identically named column of `swift$halos`.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details The function updates `swift$halos` by keeping only the selected halos and any required central halos to avoid orphaned satellites. Internal indices (`HostHaloIndex`, `SubhaloRankByBoundMass`, `NumberOfSubhalos`) are recomputed accordingly.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

filterHaloList = function(index, isHaloCatalogueIndex=FALSE, verbose=TRUE) {

  if (verbose) cooltools::tick('Filter halos')

  if (!is.null(swift$particles$halos)) stop('swift$particles is not empty, consider calling clearSwift("particles") before filtering the halo list')

  bindSwift(halos)

  # checks
  if (is.null(halos)) stop('halos does not exist')

  # handle index
  if (isHaloCatalogueIndex) {
    if (is.logical(index)) stop('if isHaloCatalogueIndex=TRUE, index cannot be a logical vector')
    if (is.null(halos$HaloCatalogueIndex)) stop('halos$HaloCatalogueIndex needed, but does not exist')
    index = match(index, halos$HaloCatalogueIndex)
    if (any(is.na(index))) stop("Some HaloCatalogueIndex values not found in swift$halos.")
  }
  if (is.logical(index)) index=which(index)
  if (anyDuplicated(index)) stop("Duplicate values found in index.")

  # ensure that all centrals of selected satellites are included, even if they weren't directly selected, to avoid generating orphans
  index.sat = index[halos$HostHaloIndex[index]>0]
  index = union(index,halos$HostHaloIndex[index.sat])

  # sort index in increasing order to preserve previous ordering
  index = sort(index)

  # select halos
  halos = halos[index,]

  # adjust HostHaloIndex
  halos$HostHaloIndex = match(halos$HostHaloIndex,index)
  halos$HostHaloIndex[halos$SubhaloRankByBoundMass==0] = 0

  # recompute SubhaloRankByBoundMass if it exists
  if (!is.null(halos$SubhaloRankByBoundMass)) {
    #halos[HostHaloIndex > 0, SubhaloRankByBoundMass := rank(SubhaloRankByBoundMass, ties.method = "first"), by = HostHaloIndex]
    #halos[HostHaloIndex == 0, SubhaloRankByBoundMass := 0]
    satellites = halos$HostHaloIndex > 0
    halos$SubhaloRankByBoundMass[satellites] = ave(
      halos$SubhaloRankByBoundMass[satellites],
      halos$HostHaloIndex[satellites],
      FUN = function(x) rank(x, ties.method = "first")
    )
    halos$SubhaloRankByBoundMass[halos$HostHaloIndex == 0] = 0
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
