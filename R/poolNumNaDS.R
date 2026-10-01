#' Pool DataSHIELD missing-value counts across servers
#'
#' Sums the output of e.g. \code{dsBase::numNaDS} across multiple servers.
#'
#' @param server_results Non-empty list of numeric vectors (length 1),
#'   each giving the number of missing values on a server.
#' @param na.rm Logical; should missing values be removed before summation? Default is FALSE.
#'
#' @return Numeric scalar: total number of missing values.
#'
#' @examples
#' res <- list(server1 = 0L, server2 = 2L, server3 = 1L)
#' poolBySumDS(res)
#'
#' @export
poolBySumDS <- function(server_results, na.rm = FALSE){
  sum(unlist(server_results), na.rm=na.rm)
}
