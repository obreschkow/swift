#' Add new halo properties
#'
#' @importFrom cooltools tick tock
#'
#' @description Adds new halo properties to the halo table stored in `swift$halos`.
#'
#' @param x A vector, matrix, data.frame, or data.table to append as new column(s) to the data.table `swift$halos`.
#' @param names A character string or vector of strings specifying the name(s) of the new column(s). If not given, the column names of \code{x} are used.
#' @param index Optional integer vector of halo indices. These indices are interpreted as row-indices in the halo table `swift$halos` or as the unique `HaloCatalogueIndex` values, depending on the argument \code{isHaloCatalogueIndex}.
#' @param isHaloCatalogueIndex Logical flag. If \code{TRUE}, the \code{index} values are taken to be the unique `HaloCatalogueIndex` available in the file `swift$.paths$halos` and locally stored in an identically named column of `swift$halos`.
#' @param verbose Logical flag to control whether progress and timing information should be printed in the console.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

addHaloProperties <- function(x, names=NULL, index=NULL, isHaloCatalogueIndex=FALSE, verbose=TRUE) {

  if (verbose) cooltools::tick('Add halo properties')

  bindSwift(halos)

  if (is.null(names) & is.null(colnames(x))) stop('names must be provided, either implicitly via column names of x or directly in the vector names')

  x = data.frame(x)
  if (nrow(x)>nrow(halos)) stop('x must not have more rows that swift$halos')

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

  # check if names already exist
  if (any(names(x)%in%names(halos))) stop('cannot overwrite existing columns with the same name. Consider calling removeHaloProperties first.')

  # Append x to halos
  if (is.null(index)) {

    if (nrow(halos)!=nrow(x)) stop('x does not have the same number of rows as swift$halos, consider specifying rows with the index argument')
    halos = cbind(halos,x)

  } else {

    if (nrow(x)!=length(index)) stop('length of index must match the number of rows in the new data x')

    # handle index argument
    if (isHaloCatalogueIndex) {
      if (is.null(halos$HaloCatalogueIndex)) stop('halos$HaloCatalogueIndex needed, but does not exist')
      row = match(index, halos$HaloCatalogueIndex)
      if (is.na(row)) stop("Some HaloCatalogueIndex values not found in swift$halos.")
    } else {
      row = index
      if (row<1 | row>nrow(halos)) stop(sprintf('row index must between 1 and the number of halos (%d)',nrow(halos)))
    }

    # resort and also take care of the situation where x has fewer rows than swift$halos
    xnew = as.data.frame(matrix(NA,nrow=nrow(halos),ncol=ncol(x)))
    colnames(xnew) = colnames(x)
    xnew[row,] = x

    # append new columns
    halos = cbind(halos,xnew)
  }

  if (verbose) cooltools::tock()

  invisible(NULL)
}
