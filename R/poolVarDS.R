#' Pool DataSHIELD variance results across servers
#'
#' Pools the output of \code{dsBase::varDS} computed on multiple DataSHIELD servers.
#' Server results are included only if \code{ValidityMessage} begins with \code{"VALID"}
#' and \code{Nvalid > 0}.
#'
#' @param server_results A non-empty list. Each element should be the server-side result
#'   of \code{dsBase::varDS} and must contain \code{Sum}, \code{SumOfSquares},
#'   \code{Nmissing}, \code{Nvalid}, \code{Ntotal}, and \code{ValidityMessage}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{Global.Variance}{Pooled variance computed from pooled \code{Sum} and \code{SumOfSquares}.}
#'   \item{Nmissing}{Total missing count across included servers.}
#'   \item{Nvalid}{Total valid (non-missing) count across included servers.}
#'   \item{Ntotal}{Total count across included servers.}
#'   \item{Nstudies}{Number of studies which are pooled.}
#'   \item{ValidityMessage}{Summary validity message for the pooled analysis.}
#' }
#'
#' @examples
#' res <- list(
#'   server1 = list(Sum=6224.3, SumOfSquares=134720.5, Nmissing=0L, Nvalid=299L, Ntotal=299L,
#'                  ValidityMessage="VALID ANALYSIS"),
#'   server2 = list(Sum=8943.4, SumOfSquares=202201.9, Nmissing=0L, Nvalid=413L, Ntotal=413L,
#'                  ValidityMessage="VALID ANALYSIS")
#' )
#' poolVarDS(res)
#'
#' @export
#' @importFrom data.table rbindlist
poolVarDS <- function(server_results){

  if (!is.list(server_results) || length(server_results) == 0L) {
    stop("server_results must be a non-empty list", call. = FALSE)
  }

  required <- c("Sum", "SumOfSquares", "Nmissing", "Nvalid", "Ntotal", "ValidityMessage")

  # Keep only elements that look like valid varDS outputs (malformed are treated as invalid)
  ok_shape <- vapply(
    server_results,
    function(x) is.list(x) && all(required %in% names(x)),
    logical(1)
  )

  Nstudies <- length(server_results)
  server_results_ok <- server_results[ok_shape]

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
      EstimatedVar = NA_real_,
      Nmissing = NA_integer_,
      Nvalid = 0L,
      Ntotal = 0L,
      ValidityMessage = "NO VALID SERVER RESULTS"
    ))
  }

  # Pool results
  GlobalNvalid <- sum(DT_valid$Nvalid, na.rm = TRUE)
  GlobalSum <- sum(DT_valid$Sum, na.rm = TRUE)
  GlobalSS  <- sum(DT_valid$SumOfSquares, na.rm = TRUE)

  GlobalVar <- (GlobalSS - (GlobalSum^2) / GlobalNvalid) / (GlobalNvalid - 1)

  # Return pooled result
  list(
    Global.Variance = GlobalVar,
    Nmissing = sum(DT_valid$Nmissing, na.rm = TRUE),
    Nvalid   = GlobalNvalid,
    Ntotal   = sum(DT_valid$Ntotal,   na.rm = TRUE),
    Nstudies = Nstudies_included,
    ValidityMessage = PoolValidityMessage
  )
}
