#' Check validity across DataSHIELD server results
#'
#' Evaluates whether all elements in a list of logical validity indicators are
#' \code{TRUE}. Typically used to determine whether all server-side analyses
#' passed validity checks.
#'
#' @param x A list or vector of logical values, or values coercible to logical.
#'
#' @return A single logical value:
#' \describe{
#'   \item{TRUE}{All elements are \code{TRUE}.}
#'   \item{FALSE}{At least one element is \code{FALSE} or missing.}
#' }
#'
#' @examples
#' poolIsValidDS(list(TRUE, TRUE, TRUE))
#' poolIsValidDS(list(TRUE, FALSE, TRUE))
#'
#' @export
poolIsValidDS <- function(x = NULL) {
  if (is.null(x) || length(x) == 0L) return(FALSE)
  all(unlist(x), na.rm = FALSE)
}
