#' Save the `swift` environment to disk
#'
#' @importFrom digest digest
#' @importFrom cooltools tick tock
#'
#' @description
#' Saves the global `swift` environment (including all its objects and internal state)
#' to a file on disk using base R's `save()` function. This allows reloading the entire
#' simulation context in a later session.
#'
#' @param filename Optional filename (including path) where the data should be saved.
#' If not provided, the file is written to the directory `swift$paths$tmp` using an automatically
#' generated filename that uniquely encodes `swift$paths` via \code{\link{swiftFileName}}.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @return None. Writes a `.img` file to disk containing the full `swift` environment.
#'
#' @seealso \link{loadSwiftData}, \link{swiftFileName}
#'
#' @export

saveSwiftData = function(filename=NULL, verbose=TRUE) {

  cooltools::tick('Save post-processed data as R-image')

  if (is.null(filename)) filename = swiftFileName()

  save(list = ls(envir = swift, all.names = TRUE), envir = swift, file = filename, compress = FALSE)

  hash = .hashEnvironment(swift)
  writeLines(hash, paste0(filename,'.hash'))

  cooltools::tock()

  invisible(NULL)

}
