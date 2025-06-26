#' Check halo list
#'
#' @importFrom cooltools tick tock
#'
#' @description Verifies the self-consistency of the halo list by checking internal indices and structural properties.
#'
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details This function inspects the current halo table (`swift$halos`) and throws descriptive errors if inconsistencies are found in the following fields: `NumberOfSubhalos`, `HaloCatalogueIndex`, `HostHaloIndex`, `SubhaloRankByBoundMass`, and `TotalMass`. It assumes that centrals are sorted by decreasing mass and that no orphan satellites are present. No changes are made to the halo table.
#'
#' @return None. The function stops with an error if any inconsistency is detected.
#'
#' @export

checkHaloList = function(verbose=TRUE) {

  if (verbose) cooltools::tick('Check halo list for self-consistency')

  bindHalos() # makes 'halos' a pointer to swift$halos

  # find centrals and satellites for following calculations
  centrals = which(halos$HostHaloIndex==0)
  satellites = which(halos$HostHaloIndex>0) # excludes ophans, i.e. satellites whose central was not selected
  if (any(is.na(halos$HostHaloIndex))) stop('halo list should not contain orphans (satellites without a central)')

  # NumberOfSubhalos
  if (sum(halos$NumberOfSubhalos)!=length(satellites)) stop('halo list NumberOfSubhalos error')

  # HaloCatalogueIndex
  if (length(unique(halos$HaloCatalogueIndex))!=dim(halos)[1]) stop('halo list HaloCatalogueIndex error')

  # HostHaloIndex
  if (any(halos$HostHaloIndex[halos$HostHaloIndex[satellites]]!=0)) stop('halo list HostHaloIndex error')

  # SubhaloRankByBoundMass
  if (any(halos$SubhaloRankByBoundMass[centrals]!=0)) stop('halo list SubhaloRankByBoundMass error 1')
  if (any(halos$SubhaloRankByBoundMass[satellites]==0)) stop('halo list SubhaloRankByBoundMass error 2')
  if (any(halos$SubhaloRankByBoundMass[centrals+halos$NumberOfSubhalos[centrals]]!=halos$NumberOfSubhalos[centrals])) stop('halo list SubhaloRankByBoundMass error 3')

  # TotalMass
  if (any(diff(halos$TotalMass[centrals])>0)) stop('halo list error: centrals not sorted by decreasing mass')

  if (verbose) cooltools::tock('Passed.')

  invisible(NULL)

}
