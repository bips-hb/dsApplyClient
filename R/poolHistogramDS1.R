#' Pool histogram ranges across DataSHIELD servers
#'
#' Pools per-server histogram ranges by computing the global minimum and maximum
#' across all supplied server-side range outputs. This is intended as the first
#' aggregation step before requesting server-side histograms with common breaks.
#'
#' @param ranges A non-empty list. Each element should be a numeric vector or
#'   two-column object containing the minimum and maximum values returned by a
#'   server-side histogram range function.
#'
#' @return A named numeric vector with elements:
#' \describe{
#'   \item{min}{Global minimum across all servers.}
#'   \item{max}{Global maximum across all servers.}
#' }
#'
#' @examples
#' res <- list(
#'   server1 = c(min = 1.2, max = 9.8),
#'   server2 = c(min = 0.5, max = 12.4)
#' )
#' poolHistogramDS1(res)
#'
#' @export
poolHistogramDS1 <- function(ranges){

  # list to matrix
  ranges_mat <- do.call(rbind, ranges)
  colnames(ranges_mat) <- c("min", "max")

  # get global range
  ranges_global <- c(min(ranges_mat[,"min"], na.rm=TRUE), max(ranges_mat[,"max"], na.rm=TRUE))
  names(ranges_global) <- c("min", "max")

  return(ranges_global)
}

#' Pool DataSHIELD histogram results across servers
#'
#' Pools server-side histogram outputs by summing bin counts across studies and
#' combining densities for histograms constructed using common break points.
#' Each server result is expected to contain a histogram object and invalid cell
#' information.
#'
#' @param outputs A non-empty list. Each element should be the server-side
#'   histogram result and must contain \code{histobject} and \code{invalidcells}.
#'   The \code{histobject} should contain standard histogram components such as
#'   \code{breaks}, \code{counts}, \code{density}, and \code{mids}. All histogram
#'   objects are assumed to use identical \code{breaks} and \code{mids}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{histObject}{Combined histogram object with pooled \code{counts},
#'     \code{density}, and \code{intensities}.}
#'   \item{invalidCells}{Summary of invalid cell counts across servers.}
#' }
#'
#' @examples
#' data1 <- rnorm(100)
#' data2 <- rnorm(100)
#' xlim <- range(c(data1,data2))
#' xlim[1] <- floor(xlim[1])
#' xlim[2] <- ceiling(xlim[2])
#' breaks <- seq( xlim[1], xlim[2], length.out=10)
#'
#' h1 <- hist(data1, breaks = breaks, plot = FALSE)
#' h2 <- hist(data2, breaks = breaks, plot = FALSE)
#'
#' invalidcells1 <- (h1$counts < 3) & (h1$counts > 0)
#' invalidcells2 <- (h2$counts < 3) & (h2$counts > 0)
#' h1$counts[invalidcells1] <- 0#NA
#' h2$counts[invalidcells2] <- 0#NA
#' h1$density[invalidcells1] <- 0#NA
#' h2$density[invalidcells2] <- 0#NA
#'
#' res <- list(
#'   server1 = list(histobject = h1, invalidcells = sum(invalidcells1)),
#'   server2 = list(histobject = h2, invalidcells = sum(invalidcells2))
#' )
#'
#' pooled <- poolHistogramDS2(res)
#' graphics::plot(pooled$histObject, freq = FALSE,
#'   xlab="my  data", main='Histogram of the pooled data')
#'
#' @export
poolHistogramDS2 <- function(outputs){

  # name of the studies to be used in the plots' titles
  stdnames <- names(outputs)

  # number of studies
  num.sources <- length(outputs)

  hist.objs <- vector("list", num.sources)
  invalidcells <- vector("list", num.sources)

  for(i in seq_along(outputs)){
    output <- outputs[[i]]
    if(is.null(output)){
      stop(paste0("Equidistant break points that span all the data points could not be found in study ",
                  ifelse(!is.null(stdnames) && stdnames[i] !="", stdnames[i],i)),
           call. = FALSE)
    }
    if (length(output)==1 && is.na(output)) {
      warning("a histogram is missing")
    } else {
      if (is.null(output$histobject)) {
        if(!is.null(output$error)){ # this would produce an error if output was NA!
          warning(output$error)
        } else {
          warning("a histogram is missing")
        }
        hist.objs[[i]] <- NA
        invalidcells[[i]] <- NA
      } else {
        hist.objs[[i]] <- output$histobject
        invalidcells[[i]] <- output$invalidcells
      }
    }
  }

  # combine the histogram objects
  # 'breaks' and 'mids' are the same for all studies
   # global.counts <- rep(0, length(hist.objs[[1]]$counts))
   # global.density <- rep(0, length(hist.objs[[1]]$density))
    global.counts <- Reduce(`+`, lapply(hist.objs, `[[`, "counts"))

   # for(i in seq_along(outputs)){
   #   global.counts <- global.counts + hist.objs[[i]]$counts
   #   global.density <- global.density + hist.objs[[i]]$density
   # }
    #global.density <- global.density/length(outputs)  ### NO: voids averaging densities. Since the pooled counts are known, the pooled density should be recomputed from pooled counts and bin widths.
    #global.intensities <- global.density

    # generate the combined histogram object to plot
    combined.histobject <- hist.objs[[1]]
    combined.histobject$counts <- global.counts

    bin_widths <- diff(combined.histobject$breaks)
    total_n <- sum(global.counts, na.rm = TRUE)

    #combined.histobject$density <- global.density
    combined.histobject$density <- global.counts / (total_n * bin_widths)
    combined.histobject$intensities <- combined.histobject$density

    combined.invalidCells <- unique(c(
      max(unlist(invalidcells)), #?, na.rm = TRUE
      sum(unlist(invalidcells)))) #?, na.rm = TRUE

    if (length(combined.invalidCells)==2)
      combined.invalidCells<- setNames(combined.invalidCells, c("min", "max"))

    out <- list(
      histObject = combined.histobject,
      invalidCells = combined.invalidCells#,
      #nStudies = length(outputs) # do we want this?
      )

    class(out) <- c("dsPooledHistogram", class(out))

    out
}


#' Plot pooled DataSHIELD histogram results
#'
#' Creates a histogram plot from an object returned by \code{poolHistogramDS2}.
#'
#' @param x An object of class \code{dsPooledHistogram}, as returned by
#'   \code{poolHistogramDS2}.
#' @param freq Logical. If \code{TRUE}, plot counts. If \code{FALSE}, plot
#'   densities. Default is \code{FALSE}.
#' @param xlab Character string for the x-axis label. Default is
#'   \code{"Value"}.
#' @param main Character string for the plot title. Default is
#'   \code{"Histogram of the pooled data"}.
#' @param ... Further arguments passed to \code{graphics::plot.histogram}.
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' data1 <- rnorm(100)
#' data2 <- rnorm(100)
#' xlim <- range(c(data1, data2))
#' breaks <- seq(floor(xlim[1]), ceiling(xlim[2]), length.out = 10)
#'
#' h1 <- hist(data1, breaks = breaks, plot = FALSE)
#' h2 <- hist(data2, breaks = breaks, plot = FALSE)
#'
#' res <- list(
#'   server1 = list(histobject = h1, invalidcells = 0L),
#'   server2 = list(histobject = h2, invalidcells = 0L)
#' )
#'
#' pooled <- poolHistogramDS2(res)
#' plot(pooled)
#'
#' @export
plot.dsPooledHistogram <- function(x,
                                   freq = FALSE,
                                   xlab = "Value",
                                   main = "Histogram of the pooled data",
                                   ...) {
  graphics::plot(
    x$histObject,
    freq = freq,
    xlab = xlab,
    main = main,
    ...
  )

  invisible(x)
}

#' Plot a list of DataSHIELD histogram results
#'
#' Converts a list of server-side histogram outputs into a stacked
#' \pkg{ggplot2} bar plot. The plot can display either bin counts or densities.
#'
#' @param hist_list A non-empty list. Each element should contain a
#'   \code{histobject} component with standard histogram elements:
#'   \code{breaks}, \code{counts}, \code{density}, and \code{mids}.
#' @param ycounts Logical. If \code{TRUE}, plot bin counts on the y-axis.
#'   If \code{FALSE}, plot densities. Default is \code{FALSE}.
#' @param xlabel Character string used as the x-axis label. Default is
#'   \code{"mids"}.
#' @param title Character string used as the legend title and, currently, the
#'   x-axis label. Default is \code{"title"}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' data1 <- rnorm(100)
#' data2 <- rnorm(100)
#' xlim <- range(c(data1,data2))
#' xlim[1] <- floor(xlim[1])
#' xlim[2] <- ceiling(xlim[2])
#' breaks <- seq( xlim[1], xlim[2], length.out=10)
#'
#' h1 <- hist(data1, breaks = breaks, plot = FALSE)
#' h2 <- hist(data2, breaks = breaks, plot = FALSE)
#'
#' invalidcells1 <- (h1$counts < 3) & (h1$counts > 0)
#' invalidcells2 <- (h2$counts < 3) & (h2$counts > 0)
#' h1$counts[invalidcells1] <- 0#NA
#' h2$counts[invalidcells2] <- 0#NA
#' h1$density[invalidcells1] <- 0#NA
#' h2$density[invalidcells2] <- 0#NA
#'
#' res <- list(
#'   server1 = list(histobject = h1, invalidcells = sum(invalidcells1)),
#'   server2 = list(histobject = h2, invalidcells = sum(invalidcells2))
#' )
#' class(res) <- c("dsPooledHistogram", class(res))
#'
#' histlist2plot(res)
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_discrete guide_legend xlab ylab
histlist2plot <- function(hist_list,
                          ycounts=FALSE, xlabel="mids", title="title") {
  # prepare data
  hist_obj <- lapply(hist_list, function(x) x$histobject) |>
    lapply(function(hist_obj) data.frame(left=head(hist_obj$breaks,-1), right=tail(hist_obj$breaks,-1), counts=hist_obj$counts, density=hist_obj$density, mids=hist_obj$mids))

  if (is.null(names(hist_obj))) names(hist_obj) <- LETTERS[seq_along(hist_obj)]

  the_data <- do.call(
    rbind,
    lapply(names(hist_obj), function(nm) {
      cbind(group = nm, hist_obj[[nm]])
    })
  )

  row.names(the_data) <- NULL

  labels4ticks <- prettyNum(signif(unique(the_data$mids),3))
 # the_data$mids <- as.factor(the_data$mids)

  # plot data

  type <- if (ycounts) counts else density
  pl <- ggplot2::ggplot(the_data,
                        ggplot2::aes(x = mids, y = if (ycounts) counts else density, fill=group
                        )) +
    ggplot2::xlab(title) +
    ggplot2::ylab("y") +
    ggplot2::geom_bar(stat = "identity", position="stack") +
    ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(title = title))

  pl
}

