# routine loads bare halo structure:

library(cooltools)
library(bit64)
library(data.table)

load.halo.list = function(filename) {
  
  tick('Load halo list from raw HDF5 file')
  
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
  
  # this function allows the code to use "halos" instead of "swift$halos" without making a copy
  makeActiveBinding("halos", function(v) {
    if (missing(v)) swift$halos else swift$halos <<- v
  }, env = environment())
  
  # Load basic HDF5 data
  subtree = list(InputHalos=list(HaloCatalogueIndex=NA),
                 SOAP=list(HostHaloIndex=NA, SubhaloRankByBoundMass=NA),
                 BoundSubhalo=list(TotalMass=NA))
  dat = cooltools::readhdf5(filename, subtree = subtree)
  halos = data.table::data.table(TotalMass=simplify(dat$BoundSubhalo$TotalMass),
                                 HostHaloIndex=simplify(dat$SOAP$HostHaloIndex+1),
                                 SubhaloRankByBoundMass=simplify(dat$SOAP$SubhaloRankByBoundMass),
                                 HaloCatalogueIndex=simplify(dat$InputHalos$HaloCatalogueIndex))
  
  tock(sprintf('# halos = %d',dim(halos)[1]))

}