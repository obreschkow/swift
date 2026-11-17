#' Measure RAM and Disk Usage of the Swift Environment
#'
#' Recursively estimates the memory occupied by R objects in the global
#' `swift` environment and the disk space used by its file-backed
#' `bigmemory` matrices. Each backing file is counted only once.
#'
#' @return A named numeric vector with two elements:
#'   \describe{
#'     \item{RAM}{Estimated R-managed memory usage in bytes.}
#'     \item{Disk}{Total size of referenced backing files in bytes.}
#'   }
#'
#' @details
#' RAM usage excludes memory-mapped pages and external allocations.
#' Disk usage includes only existing backing files referenced by `swift`.
#'
#' @examples
#' \dontrun{
#' swiftMemory()
#' }
#'
#' @export

swiftMemory = function() {

  # Recursively measure RAM and identify disk-backed matrices
  files = character()
  seen = list()

  measure = function(x) {

    # Disk-backed bigmemory matrix
    if (inherits(x, "big.matrix")) {
      d = bigmemory::describe(x)@description
      if (!is.null(d$dirname) && !is.null(d$filename)) {
        files <<- c(files, file.path(d$dirname, d$filename))
      }
      return(as.numeric(utils::object.size(x)))
    }

    # Environments
    if (is.environment(x)) {
      if (any(vapply(seen, identical, logical(1), x))) return(0)
      seen[[length(seen) + 1L]] <<- x
      return(sum(vapply(as.list.environment(x, all.names = TRUE),
                        measure, numeric(1))))
    }

    # Lists and data frames
    if (is.list(x)) {
      return(sum(vapply(x, measure, numeric(1))))
    }

    # Ordinary R objects
    as.numeric(utils::object.size(x))
  }

  ram = measure(swift)

  # Count each backing file only once
  files = unique(files)
  files = files[file.exists(files)]
  disk = sum(file.info(files)$size, na.rm = TRUE)

  c(RAM = ram, Disk = disk)
}
