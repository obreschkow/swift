#' Set a named value in a swift field
#'
#' @importFrom utils modifyList
#'
#' @description Stores a value in the global `swift` object under a specified name.
#' This function is used primarily to write values into the protected (read-only)
#' fields of swift, such as `swift$paths`, `swift$halos`, `swift$particles`, `swfit$simulation`.
#'
#' @param name A character string specifying the name/key to be written. Sub-lists can be specified using the `$` symbol.
#' @param value The value to store under `swift[[name]]`. Can be a path, object, or structure depending on context.
#'
#' @return None. The value is stored in `swift[[name]]`.
#'
#' @seealso \link{setPath}
#'
#' @examples
#' setSwift("simulation$BoxSize", 100)
#'
#' @export
#'
setSwift <- function(name, value) {
  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("Argument 'name' must be a non-empty character string of length 1.")
  }

  parts <- strsplit(name, "\\$")[[1]]
  n <- length(parts)

  if (any(parts == "")) {
    stop("Invalid 'name': empty components in hierarchical path.")
  }

  if (n == 1) {
    .internal_storage[[parts[1]]] <- value
  } else {
    # Create intermediate structure if it doesn't exist
    x <- .internal_storage[[parts[1]]]
    if (is.null(x)) x <- list()

    ref <- x
    for (i in seq_len(n - 2)) {
      if (is.null(ref[[parts[i + 1]]])) ref[[parts[i + 1]]] <- list()
      ref <- ref[[parts[i + 1]]]
    }

    # Set the final value
    ref[[parts[n]]] <- value

    # Reconstruct the nested structure back up
    for (i in rev(seq_len(n - 1))) {
      value <- list(value)
      names(value) <- parts[i + 1]
    }

    .internal_storage[[parts[1]]] <- utils::modifyList(x, value)
  }

  invisible(NULL)
}
