#' Load extra halo properties
#'
#' @importFrom rhdf5 h5ls h5read
#' @importFrom bit64 as.integer64
#' @importFrom cooltools tick tock progress userattributes
#' @importFrom stats setNames
#'
#' @description Reads additional halo properties from a SOAP HDF5 file and appends them to the existing halo table.
#'
#' @param subtree A structured list specifying the datasets to read from the HDF5 file.
#' @param filename Full path of the HDF5 halo file. If \code{NULL}, the value stored in `swift$paths$halos` will be used, which can be set with \code{\link{setPath}}.
#' @param verbose Logical flag to control whether progress and timing information should be printed in console.
#'
#' @details This function updates `swift$halos` by appending new columns based on datasets specified in the `subtree`. The variable `halos` is bound via an active binding to refer directly to `swift$halos` within the function scope.
#'
#' @return None. Modifies `swift$halos` in place.
#'
#' @export

loadHaloProperties = function(subtree, filename=NULL, verbose=TRUE) {

  if (verbose) cooltools::tick('Load extra halo properties')

  bindHalos() # makes 'halos' a pointer to swift$halos

  if (is.null(filename)) {
    if (is.null(swift$paths$halos)) {
      stop('no filename provided as argument or via swift$paths$halos, consider setting path using setPath()')
    } else {
      filename = swift$paths$halos
    }
  }

  flatten_branches <- function(x, prefix = NULL) {
    results <- list()
    for (name in names(x)) {
      full_name <- c(prefix, name)
      value <- x[[name]]
      if (is.list(value) && !is.null(names(value))) {
        results <- c(results, flatten_branches(value, full_name))
      } else {
        nested <- value
        for (n in rev(full_name)) {
          nested <- stats::setNames(list(nested), n)
        }
        results[[length(results) + 1]] <- nested
      }
    }
    results
  }

  get_branch_names <- function(x, prefix = NULL) {
    result <- character()
    for (name in names(x)) {
      full_name <- c(prefix, name)
      if (is.list(x[[name]]) && !is.null(names(x[[name]]))) {
        result <- c(result, get_branch_names(x[[name]], full_name))
      } else {
        result <- c(result, paste(full_name, collapse = "."))
      }
    }
    result
  }

  # remove attributes and converts 64-bit integers to 32-bit integers if possible without loss
  simplify = function(x) {
    myattributes = names(cooltools::userattributes(x))
    for (a in myattributes) attr(x,a) = NULL
    int_min = bit64::as.integer64(-2^31)
    int_max = bit64::as.integer64(2^31 - 1)
    if (inherits(x,"integer64") && all(x >= int_min & x <= int_max)) {
      return(as.integer(x))
    } else {
      return(x)
    }
  }

  i = simplify(readhdf5(filename, subtree=list(InputHalos=list(HaloCatalogueIndex=NA)))[[1]][[1]])
  sel = match(halos$HaloCatalogueIndex, i)
  nhalos = length(i)

  branches = flatten_branches(subtree)
  nbranches = length(branches)

  info = h5ls(filename)
  info$full_path = file.path(info$group, info$name)

  for (i in seq(nbranches)) {

    progress(sprintf('%d/%d',i,nbranches))

    # extract branch
    branch = branches[[i]]

    # make new column name
    name = get_branch_names(branch)
    path = paste0("/", gsub("\\.", "/", name))

    # Determine dimensionality
    row = which(info$full_path == path)
    if (length(row) != 1) stop("Dataset not found or not unique")
    dim_str = info$dim[row]
    dims = as.numeric(strsplit(dim_str, " x ")[[1]])

    # Read accordingly
    if (is.null(dims) || length(dims)==1) {
      # 1D vector (no 'dim' attribute): use simple index
      halos[[name]] = simplify(as.vector(h5read(filename, path, index = list(sel))))
    } else if (length(dims)==2 && dims[1]==3) {
      # Matrix: read all rows, selected columns
      x = simplify(h5read(filename, path, index = list(NULL, sel)))
      for (d in seq(3)) {
        halos[[sprintf('%s.%s',name,c('x','y','z')[d])]] = x[d,]
      }
    } else {
      stop("Unsupported data dimensionality.")
    }

  }

  if (verbose) cooltools::tock()

  invisible(NULL)
}
