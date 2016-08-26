

library(stringr) ; library(dplyr)




splitReader <- function(pathToFile, rowSkip, maxRows){
  require(dplyr) ; require(stringr) ; require(readr)
  data <- read.delim(
    pathToFile, header = FALSE, sep = '\t', quote = '',
    col.names = colNames,  skip = rowSkip, nrows = maxRows,
    stringsAsFactors  = FALSE) %>%
  dplyr::rename(observationID = SAMPLING.EVENT.IDENTIFIER,
                observer = OBSERVER.ID, 
                lat = LATITUDE,  lon = LONGITUDE,
                localityType = LOCALITY.TYPE,
                protocol = PROTOCOL.TYPE,
                date = OBSERVATION.DATE, time =  TIME.OBSERVATIONS.STARTED,
                sp = SCIENTIFIC.NAME,
                count = OBSERVATION.COUNT,
                durMinutes = DURATION.MINUTES,
                effortDist = EFFORT.DISTANCE.KM,
                nObservers = NUMBER.OBSERVERS) %>%
    dplyr::select(observationID, observer, lat, lon,
                  localityType, protocol,
                  date, time, sp, count,
                  durMinutes, effortDist, nObservers) %>%
    filter(
      localityType %in% c('H','P'), # points selected from list (H) or on map (P)
      (str_detect(protocol, 'Traveling')|str_detect(protocol, 'Rusty'))
    ) %>%
    select(-c(localityType)) %>%
    tbl_df
  return(data)
}


readEBird <- function(pathToFile, rowsPerRead){
  colNames <- read.delim(
    pathToFile, skip = 0, nrows = 1,
    header = TRUE) %>% names 
  # Get the total number of rows in the file:
  tRows <- R.utils::countLines(pathToFile)[1]
  # Get the number of records per set:
  nSets <- plyr::round_any(tRows/rowsPerRead, 1, f = ceiling)
  outList <- vector('list', length = nSets)
  for(i in 1:nSets){
    if(i == 1){
      rowsToSkip <- 1
    } else {
      rowsToSkip <- (i-1) * rowsPerRead
    }
    outList[[i]] <- splitReader(pathToFile, rowsToSkip, rowsPerRead)
  }
  bind_rows(outList) %>%
    distinct
}

pathToFile <- 'C:/Users/Brian/Desktop/ebd_rusbla_relMay-2016/ebd_rusbla_relMay-2016.txt'

rubl1 <- readEBird(pathToFile, 100000)
