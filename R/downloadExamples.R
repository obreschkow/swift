#' Download and Install the Example Data
#'
#' @importFrom cooltools tick tock
#'
#' @description This helper downloads the SWIFT example dataset (about 260 MB),
#' which is too large to be included in the CRAN package bundle.
#' The data are hosted externally on GitHub, using Git Large File Storage
#' (see https://git-lfs.com).
#'
#' By default, the data are downloaded into a user-specified directory,
#' and the function returns the path to the downloaded files. This is the base
#' directory that need to be provided when setting the various paths
#' using \link{setPath}.
#'
#' @param destdir Character string. Destination directory where the data
#'   should be stored. Defaults to the current working directory. If not given, the
#'   path in `swift$.paths$examples` is used.
#' @param overwrite Logical. If \code{TRUE}, any existing file of the same
#'   name will be overwritten.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @return Paths to the downloaded files, invisibly.
#'
#' @export
downloadExamples <- function(destdir = NULL, overwrite = FALSE, verbose = TRUE) {

  # URL of the hosted dataset
  url <- "https://github.com/obreschkow/swiftdata/releases/download/v1.0/swiftdata.tar.gz"

  if (verbose) cooltools::tick('Fetch example datasets from github')

  if (is.null(destdir)) {
    if (is.null(swift$.paths$examples)) {
      stop('No destination provided via destdir or swift$.paths$examples.')
    } else {
      destdir = swift$.paths$examples
    }
  }

  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }

  destfile <- file.path(destdir, basename(url))
  destfile <- gsub('//','/',destfile)

  if (!overwrite && file.exists(destfile)) {
    if (verbose) cooltools::tock('Already downloaded.')
    return(invisible(destfile))
  }

  utils::download.file(url, destfile, mode = "wb", quiet = TRUE)

  # auto-extract tar.gz
  if (grepl("\\.tar\\.gz$", destfile)) {
    utils::untar(destfile, exdir = destdir)
  }

  if (verbose) cooltools::tock(sprintf('Downloaded to:\n=> %s',destfile))
  invisible(destfile)
}
