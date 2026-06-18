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

  # start from root
  if (is.null(.internal_storage)) .internal_storage <<- list()

  ref <- .internal_storage

  # walk / build tree
  for (i in seq_len(length(parts) - 1)) {
    if (is.null(ref[[parts[i]]])) {
      ref[[parts[i]]] <- list()
    }
    ref <- ref[[parts[i]]]
  }

  # assign final value
  ref[[parts[length(parts)]]] <- value

  # IMPORTANT: write back into global storage
  assign(".internal_storage", .internal_storage, envir = .GlobalEnv)

  invisible(NULL)
}
