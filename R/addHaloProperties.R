#' Add new halo properties
#'
#' @importFrom cooltools tick tock progress userattributes
#'
#' @description Adds new halo properties to the halo table stored in `swift$halos`.
#'
#' @param x A vector, matrix, data.frame, or data.table to append as new columns to the data.table `swift$halos`.
#' @param names A character string or vector of strings specifying the name(s) of the new column(s). If not given, the column names of \code{x} are used.
#' @param HaloCatalogueIndex Optional vector specifying the rows in the halo table where the new properties should be written. If \code{NULL}, all rows are updated, and the order is the same as in \code{x}.
#' @param verbose Logical flag to control whether progress and timing information should be printed in the console.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

addHaloProperties <- function(x, names=NULL, HaloCatalogueIndex = NULL, verbose = FALSE) {

  if (verbose) cooltools::tick('Add halo properties')

  bindHalos()  # binds 'halos' to .internal_storage$halos

  if (is.null(names) & is.null(names(x))) stop('names must be provided, either implicitly via column names of x or directly in the vector names')

  # convert x
  if (!is.data.table(x)) x = data.table(x)

  # Extract, validate and normalise column names
  if (!is.null(names)) {
    if (!is.character(names)) stop("`names` must be a character vector.")
    names = as.character(names)
    if (ncol(x)==length(names)) {
      names(x) = names
    } else {
      stop("Length of `names` must match the number of columns in `x`.")
    }
  }

  # Append x to halos
  if (is.null(HaloCatalogueIndex)) {
    halos = cbind(halos,x)
  } else {
    if (is.null(halos$HaloCatalogueIndex)) stop("swift$halos must contain a HaloCatalogueIndex column.")
    sel = match(HaloCatalogueIndex, halos$HaloCatalogueIndex)
    if (any(is.na(sel))) stop("Some HaloCatalogueIndex values not found in swift$halos.")
    halos = cbind(halos[sel],x)
  }

  if (verbose) cooltools::tock()

  invisible(NULL)
}
