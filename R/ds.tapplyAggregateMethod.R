#' Apply a DataSHIELD aggregate function by groups across servers
#'
#' Calls the server-side function \code{tapplyAggregateMethodDS} on each
#' DataSHIELD server. The function applies an aggregate server-side function
#' \code{FUN.name} to \code{X.name}, optionally stratified by one or more
#' grouping variables given in \code{INDEX.names}.
#'
#' This is a client-side wrapper around \code{DSI::datashield.aggregate}. It is
#' useful when the same aggregate function should be evaluated by groups on all
#' connected DataSHIELD servers.
#'
#' @param X.name Character string specifying the name of the server-side object
#'   to be summarized, for example \code{"pooled$weight_T0"}.
#' @param INDEX.names Character vector specifying one or more server-side
#'   grouping variables, for example \code{"pooled$sex"} or
#'   \code{c("pooled$sex", "pooled$country")}. If \code{NULL}, no grouping
#'   variable is used.
#' @param FUN.name Character string specifying the name of the server-side
#'   aggregate function to apply, for example \code{"boxPlotGGDS"}.
#' @param FUN.pars A list of additional parameters passed to the server-side
#'   aggregate function. Default is an empty list.
#' @param checks Logical. If \code{TRUE}, check that \code{X.name} and
#'   \code{INDEX.names} are defined on all servers and refer to atomic objects
#'   or factors. Default is \code{FALSE}.
#' @param simplify Logical passed to \code{tapplyAggregateMethodDS}. If
#'   \code{TRUE}, simplify grouped output where possible. Default is
#'   \code{FALSE}.
#' @param array2DF Logical passed to \code{tapplyAggregateMethodDS}. If
#'   \code{TRUE}, convert array-like output to a data frame where supported.
#'   Default is \code{FALSE}.
#' @param wrapErrors Logical passed to \code{tapplyAggregateMethodDS}. If
#'   \code{TRUE}, server-side errors are wrapped and returned rather than
#'   stopping immediately. Default is \code{TRUE}.
#' @param datasources A list of DataSHIELD connections. Each element must be of
#'   class \code{DSConnection}. Default is
#'   \code{datashield.connections_find()}.
#'
#' @return A list containing one result per DataSHIELD server, as returned by
#'   \code{DSI::datashield.aggregate}.
#'
#' @examples
#' if (require(DSLite) && require(dsBase)) {
#' data('CNSIM1')
#' data('CNSIM2')
#'
#' # build a DSLite server with the datasets inside
#' dslite.server1 <- DSLite::newDSLiteServer(tables=list(table1=CNSIM1))
#' dslite.server2 <- DSLite::newDSLiteServer(tables=list(table2=CNSIM2))
#'
#' #dslite.server1$aggregateMethod("boxPlotGGDS", dsBase::boxPlotGGDS)
#' #dslite.server2$aggregateMethod("boxPlotGGDS", dsBase::boxPlotGGDS)
#'
#' # build DS login information
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = 'server1', driver = 'DSLiteDriver', url = 'dslite.server1')
#' builder$append(server = 'server2', driver = 'DSLiteDriver', url = 'dslite.server2')
#' logindata <- builder$build()
#'
#' # do login and table assignment
#' cur_conn <- DSI::datashield.login(logindata)
#' datashield.assign.table(cur_conn, 'pooled', table = list(server1='table1', server2='table2'))
#'
#' # unstratified
#' ds.tapplyAggregateMethod(
#'   X.name = "pooled$LAB_TRIG",
#'   FUN.name = "meanDS",
#'   checks = FALSE,
#'   array2DF = T,
#'   datasources = cur_conn
#' )
#'
#' # stratified
#' ds.tapplyAggregateMethod(
#'   X.name = "pooled$LAB_TRIG",
#'   INDEX.names = "pooled$GENDER",
#'   FUN.name = c("meanDS","varDS"),
#'   checks = FALSE,
#'   array2DF = T,
#'   datasources = cur_conn
#' )
#'
#' res <- ds.tapplyAggregateMethod(
#'   X.name = "pooled$LAB_TRIG",
#'   INDEX.names = "pooled$GENDER",
#'   FUN.name = "meanDS",
#'   checks = FALSE,
#'   datasources = cur_conn
#' )
#' #poolMeanDS(res) gives wrong results but should return an error, because the structure is not as expected
#' poolDS(res)
#'
#' lapply( res, function(x) {lapply(x, array2DF)}) # same output as ds.tapplyAggregateMethod with array2DF = TRUE
#'
#' res <- ds.tapplyAggregateMethod(
#'   X.name = "pooled$LAB_TRIG",
#'   INDEX.names = "pooled$GENDER",
#'   FUN.name = "boxPlotGGDS",
#'   checks = FALSE,
#'   datasources = cur_conn
#' )  # ERROR?
#' }
#'
#' @export
#' @importFrom DSI datashield.aggregate
#' @importFrom methods is
ds.tapplyAggregateMethod <- function(X.name,#=NULL,
                                     INDEX.names=NULL,
                                     FUN.name,#=NULL,
                                     FUN.pars=list(),
                                     checks=FALSE,
                                     simplify=FALSE, array2DF=FALSE, # keep original structure of results
                                     wrapErrors=TRUE,
                                     datasources=datashield.connections_find()){
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("'datasources' is expected to be a list of DSConnection-class", call.=FALSE)
  }

  # ---- resolve X.name ----
  if(is.null(X.name)){
    stop("Please provide the name of the variable to be summarized!")
  }

  # ---- resolve FUN.name ----
  if(is.null(FUN.name)){
    stop("Please provide the name of a valid aggregate function")
  }

  # ---- optional checks - the process stops and reports as soon as one check fails ----
  if(checks){
    # check if the input objects are defined in all the studies
    dsBaseClient:::isDefined(datasources, X.name)
    sapply(INDEX.names, function(x) dsBaseClient:::isDefined(datasources, x))

    # check if the input objects are of the same class in all studies
    type.X <- dsBaseClient:::checkClass(datasources, X.name)
    type.INDICES <- lapply(INDEX.names, function(x) dsBaseClient:::checkClass(datasources, x))

    # the input objects must be atomic or a factor
    basic_types <- c("logical", "integer", "numeric", "complex", "character", "raw", "factor")
    if (length(intersect(type.X, basic_types)) < 1 ||
        any(sapply(type.INDICES, function(x) length(intersect(x, basic_types)) <1 ) ) )
      stop("All input object names must refer to atomic objects or factors.", call.=FALSE)
  }

  # make INDEX.names transmittable
  INDEX.names.transmit <- paste(INDEX.names, collapse=",")
  # Opal does not allow "" but NULL
  if (INDEX.names.transmit=="") INDEX.names.transmit <- NULL

  # make FUN.name transmittable
  FUN.name.transmit <- paste(FUN.name, collapse=",")
  if (FUN.name.transmit=="") FUN.name.transmit <- NULL

  # make FUN.pars transmittable
  FUN.pars.transmit <-  sub("=+$", "", base64enc::base64encode(charToRaw(jsonlite::toJSON(FUN.pars))))

  # ---- call the server side function ----
  calltext <- call("tapplyAggregateMethodDS", X.name, INDEX.names.transmit, FUN.name.transmit, FUN.pars.transmit, simplify, array2DF, wrapErrors)
  output <- DSI::datashield.aggregate(datasources, calltext)
  return(output)

}

