#' Set a named value in a swift field
#'
#' @importFrom utils modifyList
#'
#' @description Stores a value in the global `swift` object under a specified name.
#' This function is used primarily to write values into the protected (read-only)
#' fields of swift, such as `swift$.paths`, `swift$halos`, `swift$particles`, `swfit$simulation`.
#'
#' @param name A character string specifying the name/key to be written. Sub-lists can be specified using the `$` symbol.
#' @param value The value to store under `swift[[name]]`. Can be a path, object, or structure depending on context.
#'
#' @return None. The value is stored in `swift[[name]]`.
#'
#' @seealso \link{setPath}
#'
#' @examples
#' setSwift("newField", diag(3))
#' setSwift("simulation$BoxSize", 100)
#'
#' @export
#'
setSwift <- function(name, value) {

  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("Argument 'name' must be a non-empty character string of length 1.")
  }

  parts <- strsplit(name, "\\$")[[1]]

  if (any(parts == "")) {
    stop("Invalid 'name': empty components in hierarchical path.")
  }

  # make sure global storage exists and is an environment
  if (is.null(.internal_storage)) {
    .internal_storage <<- new.env(parent = emptyenv())
  }

  env <- .internal_storage

  # walk / create nested environments
  for (i in seq_len(length(parts) - 1)) {

    if (!exists(parts[i], envir = env, inherits = FALSE) ||
        !is.environment(get(parts[i], envir = env))) {

      assign(parts[i], new.env(parent = emptyenv()), envir = env)
    }

    env <- get(parts[i], envir = env)
  }

  # final assignment
  assign(parts[length(parts)], value, envir = env)

  invisible(NULL)
}
