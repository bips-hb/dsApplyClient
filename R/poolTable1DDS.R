#' Pool one-dimensional DataSHIELD table results across servers
#'
#' Pools one-dimensional frequency tables returned by DataSHIELD servers. The
#' function combines per-server count tables, computes per-server percentages,
#' and computes pooled counts and percentages across studies.
#'
#' If one or more server-side tables are marked as invalid because invalid
#' counts are present, only total counts are pooled and non-total rows are set
#' to \code{NA} in the combined output.
#'
#' @param pool_res A non-empty list. Each element should be a server-side table
#'   result, where the first element is a count table and the second element is
#'   a validity message.
#' @param warningMessage Logical. If \code{TRUE}, print a warning message when
#'   one or more server-side tables are invalid. Default is \code{TRUE}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{split}{A list containing per-server \code{counts},
#'     \code{percentages}, and a \code{validity} message.}
#'   \item{combined}{A list containing pooled \code{counts},
#'     \code{percentages}, and a \code{validity} message.}
#' }
#'
#' @examples
#' pool_res <- list(
#'   server1 = list(
#'     matrix(c(10, 20, 30), nrow = 1,
#'            dimnames = list("count", c("A", "B", "Total"))),
#'     "valid table"
#'   ),
#'   server2 = list(
#'     matrix(c(5, 15, 20), nrow = 1,
#'            dimnames = list("count", c("A", "B", "Total"))),
#'     "valid table"
#'   )
#' )
#'
#' poolTable1DDS(pool_res)
#'
#' @export
poolTable1DDS <- function(pool_res=NULL, warningMessage=TRUE){

  if (!is.list(pool_res) || length(pool_res) == 0L) {
    stop("pool_res must be a non-empty list", call. = FALSE)
  }

  stdnames <- names(pool_res)
  if (is.null(stdnames)) {
    stdnames <- paste0("server", seq_along(pool_res))
  }

  # extract contingency (count) tables and validity information for each study
  countTables <- vector("list", length(pool_res))
  validityInfo <- vector("list", length(pool_res))

  for(i in seq_along(pool_res)){
    if (length(pool_res[[i]]) < 2L) {
      stop("Each element of pool_res must contain a count table and validity information", call. = FALSE)
    }
    countTables[[i]] <- t(pool_res[[i]][[1]])
    colnames(countTables[[i]]) <- "somename"
    validityInfo[[i]] <- pool_res[[i]][[2]]
  }

  names(countTables) <- stdnames
  names(validityInfo) <- stdnames

  # first check if any study is invalid - if yes then only add up the outer columns (i.e. the total)
  invalids <- which(unlist(validityInfo) == "invalid table - invalid counts present")
  has_invalids <- length(invalids) > 0L

  # generate the pooled counts table
  pooledCounts <- countTables[[1]]
  pooledCounts[,1] <- 0
  for(i in seq_along(pool_res)){
    pooledCounts <- pooledCounts + countTables[[i]]
  }
  if(has_invalids) pooledCounts[seq_len(nrow(pooledCounts) - 1L), 1] <- NA

  # percentage tables (one for each study)
  percentageTables <- vector("list", length(pool_res))
  for(i in seq_along(pool_res)){
    temp <- countTables[[i]]
    totalIdx <- nrow(temp) #totalIdx <- dim(countTables[[i]])[1]
    temp[, 1] <- round((temp[, 1] / temp[totalIdx, 1]) * 100, 2)
    percentageTables[[i]]  <- temp
  }
  names(percentageTables) <- stdnames

  # generate the pooled counts table
  if(has_invalids){
    pooledPercentages <- percentageTables[[1]]
    #pooledPercentages[1:(dim(pooledCounts)[1])-1, 1] <- NA
    pooledPercentages[seq_len(nrow(pooledPercentages) - 1L), 1] <- NA
  }else{
    pooledPercentages <- round((pooledCounts/pooledCounts[nrow(pooledCounts), 1])*100,2)
  }

  if(has_invalids){
    validityValue <- paste0("Invalid tables from '", paste(stdnames[invalids], collapse = ", "), "'!")

    if(warningMessage){
      message(paste0("WARNING: ", validityValue, "\n         Only total values are returned in the output table(s)."))
    }
  } else {
      validityValue <- "All tables are valid!"
  }

  out <- list(split=list(counts=countTables, percentages=percentageTables, validity=validityValue),
       combined=list(counts=pooledCounts, percentages=pooledPercentages, validity=validityValue))

  class(out) <- c("dsPooledTable1D", class(out))

  out
}

#' Print a pooled DataSHIELD 1D table
#'
#' Prints the combined (pooled) counts and percentages from an object of class
#' \code{dsPooledTable1D}. This method is intended to provide a concise summary
#' of the pooled result.
#'
#' @param x An object of class \code{dsPooledTable1D}, as returned by
#'   \code{poolTable1DDS}.
#' @param ... Further arguments passed to \code{print}.
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' \dontrun{
#' res <- poolTable1DDS(pool_res)
#' print(res)
#' }
#'
#' @export
print.dsPooledTable1D <- function(x, ...) {
  #print(x$combined)
  #invisible(x)

  cat("Pooled counts:\n")
  print(x$combined$counts, ...)
  cat("\nPooled percentages:\n")
  print(x$combined$percentages, ...)
  cat("\nValidity:", x$combined$validity, "\n")
  invisible(x)
}
