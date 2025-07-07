#' Load the `swift` environment from disk
#'
#' @importFrom cooltools tick tock
#' @importFrom bigmemory attach.big.matrix
#'
#' @description
#' Loads the global `swift` environment from a previously saved `.img` file on disk,
#' restoring all objects and internal state as they were at the time of saving.
#' This complements the function \code{\link{saveSwiftData}}. If the data on the image file
#' is identical to that currently in the `swift` environment, it is not reloaded.
#'
#' @param filename Optional path+filename to the saved `.img` file. If not provided, a default filename
#' that uniquely encodes `swift$paths` is used, as in \code{\link{saveSwiftData}}.
#' @param verbose Logical flag to enable timing and progress messages.
#'
#' @return Logical value: `TRUE` if the file was found and successfully loaded, `FALSE` otherwise.
#'
#' @seealso \link{saveSwiftData}, \link{swiftFileName}
#'
#' @export

loadSwiftData <- function(filename = NULL, verbose = TRUE) {

  cooltools::tick('Access post-processed data')

  if (is.null(filename)) filename <- swiftFileName()
  if (!file.exists(filename)) {
    tock('Unavailable.')
    return(FALSE)
  }

  fn.hash <- paste0(filename, '.hash')
  if (file.exists(fn.hash)) {
    hash = readLines(fn.hash) # loads `hash`
    if (!exists('hash')) stop('error with hash_file')
  } else {
    hash <- 0
  }

  hash_current <- .hashEnvironment(swift)

  if (hash_current == hash) {

    cooltools::tock('RAM already up-to-date.')

  } else {

    # Clear existing data
    clearSwift()

    # Load all saved variables into a temporary environment
    tmp_env <- new.env()
    load(filename, envir = tmp_env)

    # Copy data into internal storage and (re)bind into swift
    for (name in ls(tmp_env, all.names = TRUE)) {
      if (name %in% .protected_names) {
        .internal_storage[[name]] <- tmp_env[[name]]
      } else {
        swift[[name]] <- tmp_env[[name]]
      }
    }

    # Reattach bigmemory data
    if (!is.null(swift$particles)) {
      all_names = names(swift$particles)
      if (length(all_names)>0) {
        valid = grepl("^PartType\\d+$", all_names)
        if (any(valid)) {
          options(bigmemory.allow.dimnames=TRUE)
          directory = paste0(swift$paths$tmp,'particledata/')
          species_names = all_names[valid]
          for (field in species_names) {
            fn = paste0(directory,swift$particles[[field]]$filename,".txt")
            if (!file.exists(fn)) stop('cannot find particle data files')
            .internal_storage$particles[[field]]$data = bigmemory::attach.big.matrix(fn)
            colnames(.internal_storage$particles[[field]]$data) = swift$particles[[field]]$colnames
          }
        }
      }
    }
    directory = paste0(swift$paths$tmp,'particledata')

    cooltools::tock('Loaded from R-image.')

  }

  return(TRUE)
}
