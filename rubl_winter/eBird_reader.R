library(stringr) ; library(dplyr) ; library(lubridate)

#---------------------------------------------------------------------------------------------------*
# ---- FUNCTION TO READ EBIRD DATA ----
#---------------------------------------------------------------------------------------------------*


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

#---------------------------------------------------------------------------------------------------*
# ---- GET RUSTY OBSERVATIONS ----
#---------------------------------------------------------------------------------------------------*

pathToFile <- 'C:/Users/Brian/Desktop/ebd_rusbla_relMay-2016/ebd_rusbla_relMay-2016.txt'

# Get vector of column names:

colNames <- read.delim(
  pathToFile, skip = 0, nrows = 1,
  header = TRUE) %>% names 

# Get the total number of rows in the file:

tRows <- R.utils::countLines(pathToFile)[1]

# Get the number of records per set:

rowsPerRead <- 1E7

nSets <- plyr::round_any(tRows/rowsPerRead, 1, f = ceiling)

# Read in tRows at a time 

outList <- vector('list', length = nSets)

for(i in 1:nSets){
  if(i == 1){
    rowsToSkip <- 1
  } else {
    rowsToSkip <- (i-1) * rowsPerRead
  }
  outList[[i]] <- splitReader(pathToFile, rowsToSkip, rowsPerRead)
}

# List to data frame:

rubl1 <- bind_rows(outList) %>%
  distinct


#---------------------------------------------------------------------------------------------------*
# ---- GET SAMPLING EVENTS ----
#---------------------------------------------------------------------------------------------------*


pathToFile <- 'C:/Users/Brian/Desktop/rubl_summer_2016/ebd_US_200601_201605_relMay-2016/ebd_US_200601_201605_relMay-2016.txt'

# Get vector of column names:

colNames <- read.delim(
  pathToFile, skip = 0, nrows = 1,
  header = TRUE) %>% names 

# Get the total number of rows in the file:

tRows <- R.utils::countLines(pathToFile)[1]

# Get the number of iterations:

rowsPerRead <- 1E7

nSets <- plyr::round_any(tRows/rowsPerRead, 1, f = ceiling)


# Make progress bar:

outList <- vector('list', length = nSets)

for(i in 1:nSets){
  if(i == 1){
    rowsToSkip <- 1
  } else {
    rowsToSkip <- (i-1) * rowsPerRead
  }
  outList[[i]] <- splitReader(pathToFile, rowsToSkip, rowsPerRead) %>%
    select(-c(sp, count)) %>%
    distinct %>%
    mutate(date = as.Date(date)) %>%
    filter(month(date) %in% 1:5) %>%
    distinct
}

# List to data frame:

samplingEvents <- bind_rows(outList) %>%
  distinct

saveRDS(samplingEvents, 'samplingEvents.RDS')


  splitReader(pathToFile, 1, 1E7) %>%
  select(-c(sp, count)) %>%
    distinct %>%
  mutate(date = as.Date(date)) %>%
  filter(month(date) %in% 1:5) %>%
    distinct

