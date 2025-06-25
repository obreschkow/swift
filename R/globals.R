# This tells R CMD check to ignore these variables in this context because they are used as column names, and local environment variables, not global objects.

utils::globalVariables(c(
  "halos",
  "HostHaloIndex",
  "SubhaloRankByBoundMass",
  "HaloCatalogueIndex",
  "NumberOfSubhalos",
  "TotalMass"
))
