#' Sort halo list
#'
#' @importFrom cooltools tick tock
#'
#' @description Sorts the halo list from most to least massive, ensuring that each central halo is followed by its associated satellite halos.
#'
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details The input halo table (`swift$halos`) is sorted so that more massive centrals appear earlier, followed by their satellites ordered by subhalo rank. The function also updates `HostHaloIndex` and computes `NumberOfSubhalos` for each central. The variable `halos` is locally bound via an active binding to refer to `swift$halos` directly.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

sortHaloList = function(verbose=TRUE) {

  if (verbose) cooltools::tick('Sort halos from most massive to least massive, with satellites following their centrals')

  bindHalos() # makes 'halos' a pointer to swift$halos

  # checks
  if (is.null(halos)) stop('halos does not exist')
  if (is.null(halos$HostHaloIndex)) stop('halos$HostHaloIndex needed, but does not exist')
  if (is.null(halos$SubhaloRankByBoundMass)) stop('halos$SubhaloRankByBoundMass needed, but does not exist')
  if (is.null(halos$TotalMass)) stop('halos$TotalMass needed, but does not exist')

  # sort halos
  centrals = which(halos$HostHaloIndex==0)
  satellites = which(halos$HostHaloIndex>0)
  m = integer(dim(halos)[1])
  m[centrals] = rank(halos$TotalMass[centrals],ties.method='first')
  m[satellites] = m[halos$HostHaloIndex[satellites]]
  i = order(-m, halos$SubhaloRankByBoundMass)
  halos = halos[i,]

  # adjust HostHaloIndicex
  halos$HostHaloIndex = match(halos$HostHaloIndex,i)
  halos$HostHaloIndex[halos$SubhaloRankByBoundMass==0] = 0

  # compute NumberOfSubhalos
  # NB: it makes sense to add this property to the sorted list, because each central is now followed by
  # NumberOfSubhalos satellites in this sorted list
  satellites = which(halos$HostHaloIndex>0)
  halos$NumberOfSubhalos = 0
  tab = sort(table(halos$HostHaloIndex[satellites]), decreasing = TRUE)
  halos$NumberOfSubhalos[as.integer(names(tab))] = as.integer(tab)

  if (verbose) cooltools::tock()

}
