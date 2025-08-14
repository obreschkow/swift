#' Remove columns from the halo table
#'
#' @importFrom cooltools tick tock
#'
#' @description Removes one or more named columns from the `swift$halos` table.
#'
#' @param names Character vector of column names to remove.
#' @param verbose Logical flag to control whether progress and timing information should be printed in the console.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export
#'
removeHaloProperties <- function(names, verbose = TRUE) {

  if (verbose) cooltools::tick('Remove halo properties')

  if (missing(names)) stop("Please provide one or more column names to remove.")

  bindSwift(halos)

  # identify columns to remove
  names <- as.character(names)
  icol <- match(names,colnames(halos))
  if (any(is.na(icol))) stop('unrecognised names, not matching column names in swift$halos')

  # remove columns
  halos = halos[,-icol,drop=FALSE]

  if (verbose) cooltools::tock()

  invisible(NULL)
}
