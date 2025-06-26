#' Filter halo list
#'
#' @importFrom cooltools tick tock
#'
#' @description Filters the halo list to a specified subset of `HaloCatalogueIndex` values, ensuring internal indices remain valid and consistent.
#'
#' @param HaloCatalogueIndex A vector of halo catalogue indices specifying the halos to retain.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details The function updates `swift$halos` by keeping only the selected halos and any required central halos to avoid orphaned satellites. Internal indices (`HostHaloIndex`, `SubhaloRankByBoundMass`, `NumberOfSubhalos`) are recomputed accordingly. The `halos` object is locally bound via an active binding to reference `swift$halos` directly.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

filterHaloList = function(HaloCatalogueIndex, verbose=TRUE) {

  if (verbose) cooltools::tick('Filter halos')

  bindHalos() # makes 'halos' a pointer to swift$halos

  # checks
  if (is.null(halos)) stop('halos does not exist')
  if (is.null(halos$HaloCatalogueIndex)) stop('halos$HaloCatalogueIndex needed, but does not exist')

  # determine indices of selected galaxies
  selection = match(HaloCatalogueIndex, halos$HaloCatalogueIndex)
  if (any(is.na(selection))) stop("Some HaloCatalogueIndex values not found in swift$halos.")

  # ensure that all centrals of selected satellites are included, even if they wheren't directly selected, to avoid generating orphans
  selection = union(selection,halos$HostHaloIndex[selection])

  # select halos
  halos = halos[selection,]

  # adjust HostHaloIndex
  halos$HostHaloIndex = match(halos$HostHaloIndex,selection)
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
