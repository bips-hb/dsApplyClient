#' Pool DataSHIELD quantiles + mean results across servers
#'
#' Pools the output of \code{dsBase::quantileMeanDS} computed on multiple DataSHIELD servers
#' using a weighted average with weights = (Length - NumNa).
#'
#' @param quants Non-empty list of per-server named numeric vectors. Each element should be the
#'   server-side result of \code{dsBase::quantileMeanDS}
#' @param extras List with elements \code{Length} and \code{NumNa}, each a list
#'   aligned with \code{quants} (same server order/names), giving total length and
#'   number of NAs per server.
#'
#' @return Named numeric vector of pooled quantiles/mean (same names as input vectors).
#'
#' @examples
#' quants <- list(
#'   `server1` = c(`5%`=15.2, `10%`=16.2, `25%`=17.9, `50%`=20.2,
#'                         `75%`=23.3, `90%`=26.6, `95%`=29.0, Mean=20.81706),
#'   `server2` = c(`5%`=15.5, `10%`=16.5, `25%`=18.5, `50%`=20.6,
#'                         `75%`=24.8, `90%`=27.82, `95%`=29.9, Mean=21.65472),
#'   `server3` = c(`5%`=15.06, `10%`=16.0, `25%`=17.7, `50%`=19.8,
#'                         `75%`=23.4, `90%`=27.28, `95%`=29.48, Mean=20.9933)
#' )
#' extras <- list(
#'   Length = list(`server1`=100, `server2`=120, `server3`=80),
#'   NumNaDS  = list(`server1`=  5, `server2`= 10, `server3`= 0)
#' )
#' poolQuantileMeanDS(quants, extras)
#'
#' @export
poolQuantileMeanDS <- function(quants, extras) {
  if (!is.list(quants) || length(quants) == 0L) {
    stop("quants must be a non-empty list", call. = FALSE)
  }
  template <- setNames(rep(NA_real_, length(quants[[1]])), names(quants[[1]]))

  lengths <- extras[["Length"]]
  numNAs <- extras[["NumNaDS"]]

  weights <- mapply(function(len,nNa) len-nNa, lengths, numNAs, SIMPLIFY=TRUE)
  keep <- is.finite(weights) & weights > 0

  if (!any(keep)) return(template)

  Q <- do.call(rbind, quants[keep])
  pooled <- colSums(Q * weights[keep]) / sum(weights[keep])
  names(pooled) <- colnames(Q)
  pooled
}
