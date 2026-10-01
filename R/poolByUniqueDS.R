#' Pool DataSHIELD missing-value counts across servers
#'
#' Returns the unique values of e.g. \code{dsBase::classDS} across multiple servers.
#'
#' @param server_results Non-empty list of numeric vectors (length 1),
#'   each giving the number of missing values on a server.
#'
#' @return Numeric scalar: total number of missing values.
#'
#' @examples
#' res <- list(server1 = "numeric", server2 = "numeric", server3 = "categorical")
#' poolByUniqueDS(res)
#'
#' @export
poolByUniqueDS <- function(server_results){
  unique(unlist(server_results), na.rm=na.rm)
}
