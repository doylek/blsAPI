## blsAPI.R
#
#' @title Request Data from the U.S. Bureau Of Labor Statistics API
#' @description Allows users to request data for one or multiple series through the U.S. Bureau of Labor Statistics API.  Users provide parameters as specified in <\url{https://www.bls.gov/developers/api_signature.htm}> and the function returns a JSON string or data frame.
#' @details See <\url{https://www.bls.gov/developers/}> and <\url{https://www.bls.gov/developers/api_signature.htm}> for more details on the payload.
#' @param payload a string or a list containing data to be sent to the API.
#' @param api.version an integer for which api version you want to use (i.e. 1 for v1, 2 for v2)
#' @param return.data.frame a boolean if you want to the function to return JSON (default) or a data frame. If the data frame option is used, the series id will be added as a column.  This is helpful if multiple series are selected.
#' @keywords bls api economics
#' @export blsAPI
#' @import rjson RCurl magrittr plyr
#' @examples
#' ## These examples are taken from <https://www.bls.gov/developers/api_signature.htm>
#' library(rjson)
#' library(blsAPI)
#' 
#' ## API Version 1.0 R Script Sample Code
#' ## Single Series request
#' response <- blsAPI('LAUCN040010000000005')
#' json <- fromJSON(response)
#' \dontrun{
#' ## Multiple Series
#' payload <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'))
#' response <- blsAPI(payload)
#' json <- fromJSON(response)
#' 
#' ## One or More Series, Specifying Years
#' payload <- list(
#'   'seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'),
#'   'startyear'=2010,
#'   'endyear'=2012)
#' response <- blsAPI(payload)
#' json <- fromJSON(response)
#' 
#' ## API Version 2.0 R Script Sample Code
#' ## Single Series
#' response <- blsAPI('LAUCN040010000000005', 2)
#' json <- fromJSON(response)
#' ## Or request a data frame
#' df <- blsAPI('LAUCN040010000000005', 2, TRUE)
#' 
#' ## Multiple Series
#' payload <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'))
#' response <- blsAPI(payload, 2)
#' json <- fromJSON(response)
#' 
#' ## One or More Series with Optional Parameters
#' payload <- list(
#'   'seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'),
#'   'startyear'=2010,
#'   'endyear'=2012,
#'   'catalog'=FALSE,
#'   'calculations'=TRUE,
#'   'annualaverage'=TRUE,
#'   'registrationKey'='995f4e779f204473aa565256e8afe73e')
#' response <- blsAPI(payload, 2)
#' json <- fromJSON(response)
#' }

blsAPI <- function(payload=NA, api.version=1, return.data.frame=FALSE){
  # added faster BLS series processing developed by ScarboroughVenturesLLC
  addSeriesIDToDataFrame <- function (seriesDataFrame=NA, seriesId=NA) {
    seriesDataFrame$seriesId <- seriesId
    return (seriesDataFrame)
  }
  
  createDataFrameFromBLSSeriesObject <- function(BLSSeriesObject=NA) {
    sapply(BLSSeriesObject$data, c) %>% 
      t %>%
      data.frame %>% 
      addSeriesIDToDataFrame(BLSSeriesObject$seriesID) %>%
      return
  }
  
  getBLSSeriesList <- function (BLSRawResult=NA) {
    listOfSeries <- fromJSON(BLSRawResult)$Results$series
    lapply(listOfSeries, createDataFrameFromBLSSeriesObject)    %>% 
      ldply(data.frame) %>% 
      return
  }
  
  
  h = basicTextGatherer()
  h$reset()
  if(class(payload)=='logical'){
    ## Payload not defined
    message('blsAPI: No payload specified.')
  }
  else{
    ## Payload specified so make the request
    api.url <- paste0('https://api.bls.gov/publicAPI/v',api.version,'/timeseries/data/')
    if(is.list(payload)){
      ## Multiple Series or One or More Series, Specifying Years request
      payload <- toJSON(payload)
      m <- regexpr('\\"seriesid\\":\\"[a-zA-Z0-9]*\\",', payload)
      str <- regmatches(payload, m)
      if(length(str)>0){
        ## wrap single series in []
        replace <- sub(',', '],', sub(':', ':[',str))
        payload <- sub(str, replace, payload)
      }
      curlPerform(url=api.url, httpheader=c('Content-Type' = "application/json;"), postfields=payload, verbose = FALSE, writefunction = h$update)
    }
    else{
      ## Single Series request
      curlPerform(url=paste0(api.url,payload), verbose = FALSE, writefunction = h$update)
    }
    ## Return the results of the API call
    if(return.data.frame){
      return(getBLSSeriesList(h$value()))
    }
    else {
      ## Return the JSON results
      return(h$value()) 
    }
  }
}