# hidden internal environment
.internal_storage <- new.env(parent = emptyenv())

#' The `swift` environment
#'
#' @description
#' An environment that stores loaded simulation data, such as `halos`, `particles`, and file `paths`.
#' The entries in this environment are exposed to the user as read-only bindings.
#' Internally, values are stored in a hidden environment and modified via package functions.
#'
#' @docType data
#' @name swift
#' @export
swift <- new.env()
