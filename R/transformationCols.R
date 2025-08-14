#' Identify halo columns associated with a specific transformation
#'
#' @description Returns the column indices in `swift$halos` corresponding to a given
#' transformation type, such as `"position"` or `"velocity"`. These columns are assumed to
#' represent 3D vectors, with components indicated by a trailing `.1`, `.2`, or `.3` in the
#' column names.
#'
#' @param transformation A character string specifying the transformation type. Must be
#' either `"position"` or `"velocity"`.
#'
#' @return A list of three integer vectors corresponding to the column indices for the x, y, and z
#' components of the selected transformation. If column names are not consistent with this
#' 3D convention, an error is thrown.
#'
#' @export

transformationCols = function(transformation) {

  if (!is.character(transformation) || length(transformation) != 1)
    stop("'transformation' must be a single character string")
  if (!transformation %in% c('position', 'velocity'))
    stop("'transformation' must be 'position' or 'velocity'")

  bindSwift(halos)

  names <- names(unlist(lapply(halos, attr, which = "transformation")) == transformation)
  icol <- match(names,names(halos))
  d <- as.integer(substr(names, nchar(names), nchar(names)))

  if (!all(d %in% 1:3) | any(d/seq(3)!=1))
    stop("Column names not consistent with vector component suffixes .1, .2, .3")

  return(list(icol[d == 1], icol[d == 2], icol[d == 3]))
}
