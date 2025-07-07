#' Assign transformation metadata to halo properties
#'
#' @description Assigns a transformation attribute to one or more columns in `swift$halos`,
#' based on an exact or partial name match. This is needed to ensure that halo properties are
#' correctly transformed, e.g. when a halo is unwrapped, centred, or rotated.
#'
#' @param pattern A character string used to match column names in `swift$halos`.
#' @param transformation A character string, "position" or "velocity", describing the
#' transformation-type of the variable.
#' @param exact Logical flag. If \code{TRUE}, matches column names exactly;
#' if \code{FALSE}, uses case-insensitive pattern matching.
#'
#' @return None. Attributes are assigned in-place to columns of `swift$halos`.
#'
#' @export

assignHaloTransformation = function(pattern, transformation, exact = FALSE) {

  if (!is.character(pattern) || length(pattern) != 1)
    stop("'pattern' must be a single character string")
  if (!is.character(transformation) || length(transformation) != 1)
    stop("'transformation' must be a single character string")
  if (!transformation %in% c("position", "velocity"))
    stop("'transformation' must be 'position' or 'velocity'")

  bindSwift(halos)

  # Assign specified transformation to matched columns
  names <- colnames(halos)
  if (exact) {
    index <- which(names == pattern)
  } else {
    index <- which(grepl(pattern, names, ignore.case = TRUE))
  }

  for (i in index) attr(halos[[i]], "transformation") <- transformation

  invisible(NULL)
}
