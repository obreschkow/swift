# call bindSwift(.paths)
# to make paths a substitute for swift$.paths, etc.
# or call bindSwift(particles,"particles$halos")
# to make particles a substitute for swift$particles$halos, etc.

bindSwift <- function(ptr, namestr=NULL) {

  lhs <- as.character(substitute(ptr))
  rhs <- ifelse(is.null(namestr),lhs,namestr)
  if (!is.character(rhs)) stop('namestr must be a character string like "aaa$bbb"')
  rhs <- strsplit(rhs, "\\$")[[1L]]

  if (length(rhs)==1) {

    makeActiveBinding(
      lhs,
      function(v) {
        if (missing(v)) {
          .internal_storage[[rhs]]
        } else {
          .internal_storage[[rhs]] <- v
        }
      },
      env = parent.frame()
    )

  } else if (length(rhs)==2) {

    makeActiveBinding(
      lhs,
      function(v) {
        if (missing(v)) {
          .internal_storage[[rhs[1]]][[rhs[2]]]
        } else {
          .internal_storage[[rhs[1]]][[rhs[2]]] <- v
        }
      },
      env = parent.frame()
    )

  } else {
    stop('sub-sublists currently not supported by bindSwift')
  }
}
