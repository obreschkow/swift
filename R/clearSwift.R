#' Clear variables from the `swift` environment
#'
#' @description Empties or removes variables stored in `swift` and its internal storage.
#' Default variables (active bindings like `halos`, `particles`, `paths`) are retained but set to \code{NULL}.
#'
#' @param names Character vector of object names to clear or remove. If \code{NULL}, all objects
#'        in `swift` are targeted. Default variables are cleared (set to \code{NULL}) rather than removed.
#'
#' @return None. Modifies or removes entries from the `swift` environment and its internal storage.
#'
#' @export
clearSwift <- function(names = NULL) {
  # Access internal storage
  internal <- get(".internal_storage", envir = asNamespace("swift"), inherits = FALSE)

  # Detect default bindings: all active bindings in swift
  all_names <- ls(envir = swift, all.names = TRUE)
  default_names <- all_names[sapply(all_names, function(n) {
    bindingIsActive(n, env = swift)
  })]

  # Determine what to clear/remove
  if (is.null(names)) {
    names <- all_names
  } else {
    names <- intersect(as.character(names), all_names)
  }

  for (name in names) {
    if (name %in% default_names) {
      # Set internal value to NULL, preserving binding
      internal[[name]] <- NULL
    } else {
      # Fully remove from both environments
      if (exists(name, envir = swift, inherits = FALSE)) {
        rm(list = name, envir = swift)
      }
      if (exists(name, envir = internal, inherits = FALSE)) {
        rm(list = name, envir = internal)
      }
    }
  }

  invisible(NULL)
}
