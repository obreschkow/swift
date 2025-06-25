#' Bind `swift$halos` as local variable `halos`
#'
#' @param env Environment in which to create the binding. Defaults to caller's environment.
#' @keywords internal
#' @export
bindHalos <- function(env = parent.frame()) {
  makeActiveBinding("halos", function(v) {
    if (missing(v)) swift$halos else assign("halos", v, envir = swift)
  }, env = env)
}

#' Bind `swift$particles` as local variable `particles`
#'
#' @param env Environment in which to create the binding. Defaults to caller's environment.
#' @keywords internal
#' @export
bindParticles <- function(env = parent.frame()) {
  makeActiveBinding("particles", function(v) {
    if (missing(v)) swift$particles else assign("particles", v, envir = swift)
  }, env = env)
}
