# Define protected names that should be read-only.
# preparticles is intentionally NOT protected: loadPreParticles() assigns
# swift$preparticles directly, unlike particles which uses bindSwift().
.protected_names <- c(".paths", "simulation",  "halos", "cells", "particles", "backup")

# The following parameter is the fraction of items to read from a HDF5 group, below which
# a spare reading, x=file[[group]][sel], is adopted rather than a full reading with
# subsequent selection, x=file[[group]]$read()[sel]; used when readling particle
# data and halo properties. The best choice of this value,
# in terms of minimal read-time, depends on the system, size of data, and even details
# of the selection vector `sel`. It can be determined empirically. A value of 0.1
# is the approximate optimum determined
# for a typical colibre snapshot file on an M1 MacBook Pro.
.spareReadThreshold = 0.1

# This tells R CMD check to ignore these variables in this context because they are used as column names, and local environment variables, not global objects.
utils::globalVariables(c(
  .protected_names,
  "HostHaloIndex",
  "SubhaloRankByBoundMass",
  "HaloCatalogueIndex",
  "NumberOfSubhalos",
  "TotalMass",
  "idx",
  "Rank_bound",
  "j", "index", ".N"
))

# remove attributes and converts 64-bit integers to double if possible without loss
.simplify = function(x) {
  if (inherits(x,"integer64")) {
    if (all(x >= -2^53 & x <= 2^53)) {
      return(as.double(x))
    } else {
      return(x)
    }
  } else {
    return(as.double(x))
  }
}

# produces a quasi-unique identifier of an environment
.hashEnvironment = function(env, exclude=NULL) {
  contents <- .swiftSerializable(as.list(env, all.names = FALSE))
  digest::digest(serialize(contents[sort(setdiff(names(contents),exclude))], NULL), algo = "md5")
}

# Produce a pointer-free copy of the saved state. big.matrix external pointers
# cannot survive an R session restart; the descriptor files are authoritative.
# The same representation is used for hashing and disk serialization.
.swiftSerializable <- function(contents) {
  for (root in c('particles', 'preparticles')) {
    if (is.null(contents[[root]])) next
    for (container in names(contents[[root]])) {
      block <- contents[[root]][[container]]
      if (!is.list(block)) next
      for (field in grep('^PartType[0-9]+$', names(block), value=TRUE)) {
        if (is.list(block[[field]])) block[[field]]$data <- NULL
      }
      contents[[root]][[container]] <- block
    }
  }
  contents
}
