
#' Is Character Transformational
#'
#' @template statesParam
#'
#' @return A logical specifying whether each character is transformational
#' @export
#'
IsTransformational <- function (states) {
  gsub("^'(.*)'$", "\\1", states)[1] == ""
}

#' @describeIn IsTransformational The inverse of IsTransformational
IsNeomorphic <- function (states) !IsTransformational(states)

