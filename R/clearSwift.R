#' Clear variables from the `swift` environment
#'
#' @description Empties or removes variables stored in `swift` and its internal storage.
#' Default variables (active bindings like `simulation`, `halos`, `particles`) are retained but set to \code{NULL}.
#'
#' @param names Character vector of object names to clear or remove. If \code{NULL}, all objects
#'        in `swift` are targeted. Default variables are cleared (set to \code{NULL}) rather than removed.
#' @param all.names a logical value. If TRUE, all object names are removed. If FALSE, names which begin with a '.', such as `.paths`, are omitted.
#'
#' @return None. Modifies or removes entries from the `swift` environment and its internal storage.
#'
#' @export

clearSwift <- function(names = NULL, all.names = FALSE) {

  if (is.null(names)) names <- ls(swift, all.names=all.names)

  protected <- get(".protected_names", envir = asNamespace("swift"), inherits = FALSE)
  internal <- get(".internal_storage", envir = asNamespace("swift"), inherits = FALSE)

  for (name in names) {
    if (name%in%protected) {
      internal[[name]] <- NULL
    } else {
      rm(list=name, envir=swift)
    }
  }

}
