# call bindSwift(paths)
# to make paths a substitute for swift$paths, etc.

bindSwift <- function(name) {
  n <- as.character(substitute(name))
  makeActiveBinding(n, function(v) {
    if (missing(v)) {
      .internal_storage[[n]]
    } else {
      .internal_storage[[n]] <- v
    }
  }, env = parent.frame())
}
