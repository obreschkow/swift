#' Delete disk image of a `swift` environment
#'
#' @importFrom bigmemory dir.name file.name
#'
#' @description
#' Deletes the files containing a global `swift` environment previously on the disk,
#' including associated particle data in subdirectories.
#'
#' @param filename Optional path+filename to the saved `.img` file. If not provided, a default filename
#' that uniquely encodes `swift$paths` is used, as in \code{\link{saveSwiftData}}.
#'
#' @return None.
#'
#' @seealso \link{saveSwiftData}, \link{loadSwiftData}, \link{swiftFileName}
#'
#' @export

deleteSwiftData <- function(filename = NULL) {

  if (is.null(filename)) filename <- swiftFileName()
  if (!file.exists(filename)) stop(sprintf('file %s not found',filename))

  # Load all saved variables into a temporary environment
  tmp_env <- new.env()
  load(filename, envir = tmp_env)
  if (!is.null(tmp_env$particles)) {
    all_names = names(tmp_env$particles)
    if (length(all_names)>0) {
      valid = grepl("^PartType\\d+$", all_names)
      if (any(valid)) {
        directory = paste0(tmp_env$paths$tmp,'particledata/')
        species_names = all_names[valid]
        for (field in species_names) {
          for (extension in c('.txt','.bin')) {
            fn = paste0(directory,tmp_env$particles[[field]]$filename,extension)
            if (file.exists(fn)) file.remove(fn)
          }
        }
      }
    }
  }

  # remove image file
  file.remove(filename)

  # remove hash file
  fn.hash <- paste0(filename, '.hash')
  if (file.exists(fn.hash)) file.remove(fn.hash)

  invisible(NULL)
}
