#' Sort halo list
#'
#' @importFrom cooltools tick tock
#'
#' @description Sorts the subhalo list from most to least massive, ensuring that each central subhalo is directly followed by its associated satellite subhalos.
#'
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details The input halo table (`swift$halos`) is sorted so that more massive centrals appear earlier, followed by their satellites ordered by subhalo rank (also from most to least massive). The function also updates `HostHaloIndex` and computes a new property `NumberOfSubhalos` for each central.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @seealso \link{checkHaloList}
#'
#' @export

sortHaloList = function(verbose=TRUE) {

  if (verbose) cooltools::tick('Sort halos by decreasing mass, with satellites following their centrals')

  bindSwift(halos)

  # checks
  if (is.null(halos)) stop('halos does not exist')
  if (is.null(halos$HostHaloIndex)) stop('halos$HostHaloIndex needed, but does not exist')
  if (is.null(halos$SubhaloRankByBoundMass)) stop('halos$SubhaloRankByBoundMass needed, but does not exist')
  if (is.null(halos$TotalMass)) stop('halos$TotalMass needed, but does not exist')
  if (!is.null(swift$particles$halos)) stop('swift$particles is not empty, consider calling clearSwift("particles") before sorting the halo list')

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

  invisible(NULL)
}
