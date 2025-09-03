#' Set a named file path
#'
#' @description Stores a file path in `swift$.paths` under a specified name.
#' These paths are used by other functions that accept `filename = NULL`.
#'
#' @param name A character string specifying the attribute name of the path to set. For instance,:
#'             \itemize{
#'               \item \code{"halos"}: path to the HDF5 file containing the subhalo properties. This must be the full path+filename, including file extension (.hdf/.hdf5/.h5).
#'               \item \code{"particles"}: path to the HDF5 particle file, typically corresponding to a simulation snapshot. This must be the full path+filename, including file extension (.hdf/.hdf5/.h5) and subvolume placeholder `%d` if available.
#'               \item \code{"membership"}: path to the HDF5 membership file, linking the particles to subhalos. This must be the full path+filename, including file extension (.hdf/.hdf5/.h5) and subvolume placeholder `%d` if available.
#'               \item \code{"tmp"}: path to the directory containing post-processed data for fast access.
#'               \item \code{"examples"}: path to the directory of the sample data that can be downloaded using \link{downloadExamples}.
#'             }
#' @param path A character string specifying the full file or directory path.
#'
#' @return None. The named path is stored in `swift$.paths[[name]]`.
#'
#' @examples
#' setPath("particles", "/data/snapshot_199.%d.hdf5")
#'
#' @export
#'
setPath = function(name, path) {

  bindSwift(.paths)

  .paths[[name]] = gsub('//','/',path)

  invisible(NULL)

}
