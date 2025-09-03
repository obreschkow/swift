.onLoad <- function(libname, pkgname) {
  makeReadOnlyBinding <- function(name) {
    makeActiveBinding(name, function(v) {
      if (missing(v)) {
        .internal_storage[[name]]
      } else {
        stop(sprintf("Direct modification of 'swift$%s' is not allowed. Use setSwift() instead.", name), call. = FALSE)
      }
    }, env = swift)
  }

  # Register read-only bindings for known names
  for (name in .protected_names) {
    makeReadOnlyBinding(name)
  }
}
