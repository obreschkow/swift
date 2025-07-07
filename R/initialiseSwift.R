#' Initialise SWIFT simulation metadata
#'
#' @description Loads simulation metadata from a SWIFT snapshot file, stores it in `swift$simulation`, and determines which particle species are present.
#'
#' @param verbose Logical flag to print process and timing information in console.
#'
#' @details The filename pattern of snapshots must be provided via the list `swift$paths$snapshot`, which can be set using the function \link{setPath}.
#'
#' @return None. Modifies the global `swift$simulation` object.
#'
#' @export

initialiseSwift = function(verbose=TRUE) {

  if (verbose) cooltools::tick('Initialise SWIFT data analysis')

  if (is.null(swift$paths)) stop('swift$paths not yet set')

  fn.snapshot.list = Sys.glob(sub('%d','*',swift$paths$snapshot))
  if (length(fn.snapshot.list)==0) stop(sprintf('cannot find file %s',swift$paths$snapshot))
  fn = fn.snapshot.list[1]

  hdr = cooltools::readhdf5(fn, subtree=list(Header='*'), group.attr.as.data=TRUE)
  if (length(hdr)==0) stop(sprintf('cannot find Header group in file %s',fn))

  bindSwift(simulation)
  simulation = hdr[[1]]

  # determine species in this simulation
  n = 0
  names = c('NumPart_Total','NumPart_ThisFile','TotalNumberOfParticles')
  for (name in names) {
    if (!is.null(simulation[[name]])) {
      n = n+simulation[[name]]
    }
  }
  if (length(n)==1 && n==0) stop('unable to determine particle types in simulation')
  simulation$PartTypes = (seq_along(n)-1)[n>0]

  if (verbose) cooltools::tock()

  invisible(NULL)

}
