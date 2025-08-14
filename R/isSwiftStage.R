#' Check if current swift backup stage matches a given value
#'
#' Compares the current value of swift$backup$stage against a user-provided integer.
#'
#' @param stage Non-negative integer to compare against the current `swift$backup$stage`.
#'
#' @return `TRUE` if the current stage matches `stage`, otherwise `FALSE`.
#' @export

isSwiftStage = function(stage) {

  if (!(is.numeric(stage) && length(stage)==1 && stage==round(stage) && stage>=0)) stop('if provided, stage must be a non-negative integer')

  if (is.null(swift$backup$stage)) {
    current_stage = 0
  } else {
    current_stage = swift$backup$stage
  }

  return(current_stage==stage)

}
