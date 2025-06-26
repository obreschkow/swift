#' Remove columns from the halo table
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
removeHaloProperties <- function(names, verbose = FALSE) {

  if (verbose) cooltools::tick('Remove halo properties')

  if (missing(names)) stop("Please provide one or more column names to remove.")

  bindHalos()

  names <- as.character(names)
  existing <- names[names %in% names(halos)]

  if (length(existing) == 0) {
    warning("None of the specified columns were found in `swift$halos`.")
    return(invisible(NULL))
  }

  halos[, (existing) := NULL]

  if (verbose) cooltools::tock()

  invisible(NULL)
}
