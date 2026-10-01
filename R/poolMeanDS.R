#' Pool DataSHIELD mean results across servers
#'
#' Pools the output of \code{dsBase::meanDS} computed on multiple DataSHIELD servers.
#' Server-level means are combined using a weighted mean with weights \code{Nvalid}.
#' Counts (\code{Nmissing}, \code{Nvalid}, \code{Ntotal}) are summed across included servers.
#'
#' Server results are included only if:
#' \itemize{
#'   \item \code{ValidityMessage} begins with \code{"VALID"}, and
#'   \item \code{Nvalid > 0}.
#' }
#'
#' @param server_results A non-empty list. Each element should be the
#'   server-side result of \code{dsBase::meanDS} and must contain the fields
#'   \code{EstimatedMean}, \code{Nmissing}, \code{Nvalid}, \code{Ntotal}, and \code{ValidityMessage}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{EstimatedMean}{Weighted mean of \code{EstimatedMean} using \code{Nvalid} as weights.}
#'   \item{Nmissing}{Total missing count across included servers.}
#'   \item{Nvalid}{Total valid (non-missing) count across included servers.}
#'   \item{Ntotal}{Total count across included servers.}
#'   \item{ValidityMessage}{Summary validity message for the pooled analysis.}
#' }
#'
#' @examples
#' # Example using mock server outputs (same shape as dsBase::meanDS)
#' res <- list(
#'   serverA = list(
#'     EstimatedMean = 10,
#'     Nmissing = 1L,
#'     Nvalid = 9L,
#'     Ntotal = 10L,
#'     ValidityMessage = "VALID ANALYSIS"
#'   ),
#'   serverB = list(
#'     EstimatedMean = 20,
#'     Nmissing = 0L,
#'     Nvalid = 10L,
#'     Ntotal = 10L,
#'     ValidityMessage = "VALID ANALYSIS"
#'   ),
#'   serverC = list(
#'     EstimatedMean = NA_real_,
#'     Nmissing = NA_integer_,
#'     Nvalid = 0L,
#'     Ntotal = 0L,
#'     ValidityMessage = "INVALID: too few observations"
#'   )
#' )
#'
#' poolMeanDS(res)
#'
#' @export
#' @importFrom data.table rbindlist
poolMeanDS <- function(server_results) {

  if (!is.list(server_results) || length(server_results) == 0L) {
    stop("server_results must be a non-empty list", call. = FALSE)
  }

  required <- c("EstimatedMean", "Nmissing", "Nvalid", "Ntotal", "ValidityMessage")

  # Keep only elements that look like valid meanDS outputs (malformed are treated as invalid)
  #ok_shape <- vapply(
  #  server_results,
  #  function(x) is.list(x) && all(required %in% names(x)),
  #  logical(1)
  #)
  # okay shape might be different now:
  ok_shape <- vapply(
    server_results,
    function(x) is.list(x) && length(x)>0 && (all(required %in% names(x)) || all(required %in% names(x[[1]])) ),
    logical(1)
  )

  Nstudies <- length(server_results)
  server_results_ok <- server_results[ok_shape]

  # If not everything is malformed...
  if (length(server_results_ok) > 0L) {
    DT <- data.table::rbindlist(server_results_ok, idcol = "server", fill = TRUE)

    # Drop invalid analyses
    DT_valid <- DT[grepl("^VALID", DT$ValidityMessage) & !is.na(DT$Nvalid) & DT$Nvalid > 0L]

    Nstudies_included <- nrow(DT_valid)
    Nstudies_invalid <- Nstudies - Nstudies_included

    PoolValidityMessage <- if (Nstudies_invalid > 0L) {
      paste0("INVALID STUDIES: ", Nstudies_invalid)
    } else {
      "VALID POOLED ANALYSIS"
    }
  }

  # If everything is malformed or no valid studies, return dummy
  if (length(server_results_ok) == 0L || Nstudies_included == 0L) {
    return(list(
      EstimatedMean = NA_real_,
      Nmissing = 0L,
      Nvalid = 0L,
      Ntotal = 0L,
      ValidityMessage = "NO VALID SERVER RESULTS"
    ))
  }

  # Return pooled result
  list(
    EstimatedMean = stats::weighted.mean(DT_valid$EstimatedMean, DT_valid$Nvalid, na.rm = TRUE),
    Nmissing = sum(DT_valid$Nmissing, na.rm = TRUE),
    Nvalid   = sum(DT_valid$Nvalid,   na.rm = TRUE),
    Ntotal   = sum(DT_valid$Ntotal,   na.rm = TRUE),
    ValidityMessage = PoolValidityMessage
  )
}
