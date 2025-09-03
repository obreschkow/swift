#' Generate a 3D Grid of Cubic Cells Partitioning the Simulation Box
#'
#' @importFrom cooltools tick tock
#'
#' @description
#' Creates a cubic grid of `nside^3` cells that partition the entire simulation box.
#' Each cell is represented by its midpoint coordinates and a constant cell width.
#'
#' @param nside Integer number of cells along each box dimension.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @return None. The cells properties stored as data frame in `swift$cells`, containing the columns:
#'   \itemize{
#'     \item \code{x}, \code{y}, \code{z}: Midpoint coordinates of each cell.
#'     \item \code{width}: The side length of each cubic cell.
#'   }
#'
#' @export
makeGridCells <- function(nside = 8, verbose=TRUE) {

  if (verbose) cooltools::tick('Generate 3D Grid of Cubic Cells')

  # create an active binding to .internal_storage$cells
  bindSwift(cells)

  # generate cells
  if (is.null(swift$simulation) || is.null(swift$simulation$BoxSize)) {
    stop('swift$simulation$BoxSize must exist when calling makeGridCells. Consider loading metadata via initialiseSwift().')
  }
  L <- swift$simulation$BoxSize[1]
  midpoints <- cooltools::midseq(0, L, nside)
  cells <- cbind(
    expand.grid(x = midpoints, y = midpoints, z = midpoints),
    width = L / nside
  )

  if (verbose) cooltools::tock(sprintf('# cells = %d',nrow(cells)))

  invisible(NULL)

}
