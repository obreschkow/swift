#' Set a named file path
#'
#' @description Stores a file path in `swift$paths` under a specified name.
#' These paths are used by other functions that accept `filename = NULL`.
#'
#' @param name A character string specifying the name of the path to set. Examples include:
#'             \itemize{
#'               \item \code{"halos"}: path to the HDF5 halo file, including `.hdf5` extension
#'               \item \code{"particles"}: path to the HDF5 particle file, including `.hdf5` extension, but without subvolume numbers (`.0`, `.1`, ...).
#'               \item \code{"tmp"}: path to the directory containing temporary R-img files for fast access
#'             }
#' @param path A character string specifying the full file or directory path.
#'
#' @return None. The named path is stored in `swift$paths[[name]]`.
#'
#' @export
#'
setPath = function(name, path) {

  bindPaths()
  paths[[name]] = path

  invisible(NULL)

}
