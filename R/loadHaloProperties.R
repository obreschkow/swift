#' Load extra halo properties
#'
#' @importFrom hdf5r H5File
#' @importFrom cooltools tick tock progress
#'
#' @description Reads additional halo properties from a HDF5 halo catalogue and appends them to the existing halo table `swift$halos`.
#'
#' @param properties A vector of character strings specifying the datasets to be loaded, in the usual HDF5 structure `group/dataset`, `group/group/dataset`, etc.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details This function updates `swift$halos` by appending new columns specified in `properties`. If the requested properties already exist as columns in `swift$halos`, they are ignored. The new column names are identical to the HDF5 path names in `properties`, except that `/` are replaced by `.`.
#'
#' The full filename of the halo catalogue must be available in `swift$.paths$halos`, which can be set using the function \link{setPath}.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

loadHaloProperties = function(properties, verbose=TRUE) {

  if (verbose) cooltools::tick('Load extra halo properties')

  bindSwift(halos)

  if (is.null(swift$.paths$halos)) stop('no filename provided as argument or via swift$.paths$halos, consider setting path using setPath()')

  # open HDF5 file
  file = hdf5r::H5File$new(swift$.paths$halos, mode = "r")
  hdf5structure = file$ls(recursive = TRUE)

  # determine and order indices of rows to be extracted
  masterHaloCatalogueIndex = file[['InputHalos/HaloCatalogueIndex']]$read()
  sel = match(halos$HaloCatalogueIndex, masterHaloCatalogueIndex)
  if (any(is.na(sel))) stop('Unknown HaloCatalogueIndex matching error.')
  fselected = length(sel)/length(masterHaloCatalogueIndex)
  rm(masterHaloCatalogueIndex)
  ord = order(sel)
  selord = sel[ord]
  ordord = order(ord)

  for (i in seq_along(properties)) {

    if (verbose) cooltools::progress(sprintf('%d/%d',i,length(properties)))

    # extract branch
    property = properties[i]
    name = gsub("/", "\\.", property)

    # Determine dimension
    row = which(hdf5structure$name == property)
    if (length(row) != 1) stop(sprintf("Dataset %s not found or not unique",property))
    dim_str = hdf5structure$dataset.dims[row]
    dims = as.numeric(strsplit(dim_str, " x ")[[1]])
    if (is.null(dims) || any(is.na(dims)) || length(dims)>2) stop(sprintf("Format of object %s not supported",property))

    # Read data
    if (length(dims)==1) {
      # Vector
      if (is.null(halos[[name]])) {
        if (fselected<.spareReadThreshold) {
          x = file[[property]][selord][ordord]
        } else {
          x = file[[property]]$read()[sel]
        }
        halos[[name]] = .simplify(x)
      }
    } else if (length(dims)==2) {
      # Matrix
      if (is.null(halos[[paste0(name,'.1')]])) {
        if (fselected<.spareReadThreshold) {
          x = file[[property]][,selord,drop=FALSE][,ordord,drop=FALSE]
        } else {
          x = file[[property]]$read()[,sel,drop=FALSE]
        }
        for (d in seq(dims[1])) {
          halos[[sprintf('%s.%d',name,d)]] = x[d,]
        }
      }
    } else {
      stop(sprintf("Format of object %s not supported",property))
    }

  }

  file$close_all()

  if (verbose) cooltools::tock()

  invisible(NULL)
}
