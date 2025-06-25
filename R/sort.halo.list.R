sort.halo.list = function() {
  
  tick('Sort halos from most massive to least massive, with satellites following their centrals')
  
  # this function allows the code to use "halos" instead of "swift$halos" without making a copy
  makeActiveBinding("halos", function(v) {
    if (missing(v)) swift$halos else swift$halos <<- v
  }, env = environment())
  
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
  
  tock()
  
}