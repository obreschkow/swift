#' Extract vectors from tables with vector components
#'
#' @description Reunites vector-components in table columns into a simple matrix
#'
#' @param x object containing table-like data, such as a data.frames, data.table, matrix, big.matrix, etc.
#' @param name A character string used as a pattern to match column names \code{"[name].[number]"}.
#' @param ignore.case Logical flag indicating whether the match should ignore case. Defaults to \code{TRUE}.
#' @param keep.colnames Logical flag indicating whether to return the result with column names.
#'
#' @return m-by-n matrix, where m is the number of rows in x and n is the number of columns containing the pattern \code{name}.
#'
#' @examples
#' # simple matrix
#' m = matrix(seq(15),nrow=5,ncol=3)
#' colnames(m) = c('a','bb.1','bb.2')
#' test = list(m=m)
#'
#' # data.frame
#' test$df = as.data.frame(m)
#'
#' # data.table
#' test$dt = data.table::as.data.table(m)
#'
#' # big.matrix
#' test$bm = bigmemory::as.big.matrix(m)
#'
#' # evaluate all
#' for (keep.colnames in c(FALSE,TRUE)) {
#'   for (x in test) {
#'     print(getVector(x[4:5,],'bb',keep.colnames=keep.colnames))
#'   }
#' }
#'
#' @export

getVector = function(x, name, ignore.case=TRUE, keep.colnames=FALSE) {

  if (!is.character(name) || length(name) != 1)
    stop("'name' must be a single character string")

  # extract column names
  names = colnames(x)
  if (is.null(names)) names = names(x)
  if (is.null(names)) stop('colnames of x cannot be found')

  # escape regex metacharacters in name
  name_esc = gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", name)

  # find matching columns of the form [name].[number]
  pattern = paste0("^", name_esc, "\\.[0-9]+$")
  matches = which(grepl(pattern, names, ignore.case = ignore.case))

  if (length(matches)==0)
    stop('no column names match the pattern "[name].[number]"')

  # convert to matrix
  if (is.list(x)) {
    x = matrix(unlist(x, use.names=FALSE), ncol=length(x))[, matches, drop=FALSE]
    if (keep.colnames) colnames(x) = names[matches]
  } else {
    if (!is.matrix(x)) x = matrix(x, nrow=1)
    x = x[, matches, drop=FALSE]
    if (!keep.colnames) colnames(x) = NULL
  }

  return(x)
}
