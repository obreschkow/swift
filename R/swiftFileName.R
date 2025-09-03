#' Generate default filename for storing swift data
#'
#' @importFrom digest digest
#'
#' @description
#' Constructs a quasi-unique and reproducible filename for storing the global `swift` environment
#' using a md5-hash of the file properties (basename, size, timestamp) of the HDF5-files given in `swift$.paths`.
#' The filename is placed in the directory specified by `swift$.paths$tmp`.
#'
#' This function is typically used by \code{\link{saveSwiftData}} and \code{\link{loadSwiftData}}
#' when no filename is explicitly provided.
#'
#' @return A character string with the full path, but no file extension.
#'
#' @seealso \link{saveSwiftData}, \link{loadSwiftData}
#'
#' @export

swiftFileName <- function() {

  if (is.null(swift$.paths)) stop("No filename provided and swift$.paths is NULL")
  if (is.null(swift$.paths$tmp)) stop("No filename provided and swift$.paths$tmp is NULL")

  # make automatic filename based on the names, sizes, and timestamps of the HDF5-files under swift$.paths
  paths_vec = Sys.glob(sub('%d','*',swift$.paths))
  extensions <- c("hdf5", "h5", "hdf")
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$", collapse = "")
  matches <- grepl(pattern, paths_vec, ignore.case = TRUE)
  paths_filtered <- paths_vec[matches]
  if (length(paths_filtered)==0) stop("Check if all paths in swift$.paths exist")
  info <- file.info(paths_filtered) # Get file information
  sizes <- info$size
  timestamps <- as.POSIXct(info$mtime, tz = "UTC")
  fn = digest::digest(list(basename(paths_filtered),sizes,timestamps), algo="md5")
  filename <- paste0(swift$.paths$tmp, fn)

  return(filename)

}
