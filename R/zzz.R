.onLoad <- function(libname, pkgname) {
  makeReadOnlyBinding <- function(name) {
    makeActiveBinding(name, function(v) {
      if (missing(v)) {
        .internal_storage[[name]]
      } else {
        stop(sprintf("Direct modification of 'swift$%s' is not allowed.", name))
      }
    }, env = swift)
  }

  # Register read-only bindings for known names
  for (name in c("paths", "halos", "particles")) {
    makeReadOnlyBinding(name)
  }
}
