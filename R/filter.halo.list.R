# select a subset of halos and correctly recompute the key indices

filter.halo.list = function(HaloCatalogueIndex, force.remake=FALSE) {
  
  tick('Filter halos')
  
  # this function allows the code to use "halos" instead of "swift$halos" without making a copy
  makeActiveBinding("halos", function(v) {
    if (missing(v)) swift$halos else swift$halos <<- v
  }, env = environment())
  
  # checks
  if (is.null(halos)) stop('halos does not exist')
  if (is.null(halos$HaloCatalogueIndex)) stop('halos$HaloCatalogueIndex needed, but does not exist')
  
  # determine indices of selected galaxies
  selection = match(HaloCatalogueIndex, halos$HaloCatalogueIndex)
  
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
  
  tock(sprintf('# halos = %d',dim(halos)[1]))
  
}