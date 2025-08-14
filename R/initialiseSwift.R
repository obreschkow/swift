#' Initialise SWIFT simulation metadata
#'
#' @importFrom cooltools tick tock
#'
#' @description Loads simulation metadata from a SWIFT snapshot file and stores it in `swift$simulation`. Metadata properties containing the pattern `thisfile` are removed, assuming that they are specific to a subvolume.
#' Available particle species present are determined automatically and stored in the integer vector `swift$simulation$PartTypes`.
#'
#' @param verbose Logical flag to print process and timing information in console.
#'
#' @details The filename pattern of snapshots must be provided via the list `swift$.paths$snapshot`, which can be set using the function \link{setPath}.
#'
#' @return None. Modifies the global `swift$simulation` object.
#'
#' @export

initialiseSwift = function(verbose=TRUE) {

  if (verbose) cooltools::tick('Initialise SWIFT data analysis')

  if (is.null(swift$.paths)) stop('swift$.paths not yet set')
  if (is.null(swift$.paths$tmp)) stop('swift$.paths$tmp not yet set. call setPath(...)')

  if (!file.exists(swift$.paths$tmp)) {
    dir.create(swift$.paths$tmp, recursive = TRUE)
  }

  fn.snapshot.list = Sys.glob(sub('%d','*',swift$.paths$snapshot))
  if (length(fn.snapshot.list)==0) stop(sprintf('cannot find file %s',swift$.paths$snapshot))
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

  # remove fields that are specific to this subvolume
  fields = names(swift$simulation)[grepl('thisfile', names(swift$simulation), ignore.case = TRUE)]
  for (field in fields) {
    simulation[[field]] = NULL
  }

  if (verbose) cooltools::tock()

  invisible(NULL)

}
