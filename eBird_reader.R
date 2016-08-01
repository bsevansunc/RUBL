pathInput <- 'C:/Users/Brian/Downloads'
pathOutput <- 'C:/Users/Brian/Desktop/rubl_summer_2016/'

pathToFile <- 'C:/Users/Brian/Downloads/eBird1/ebd_US_200901_200904_relMay-2016.txt'
pathToOutput <- 'C:/Users/Brian/Desktop/rubl_summer_2016/winter2009.csv'

readWriteEbird <- function(pathToFile, pathToOutput){
  require(dplyr)
  columnsToRead <- c('OBSERVER ID', 'PROTOCOL TYPE',
                     'LATITUDE','LONGITUDE','OBSERVATION DATE', 'SCIENTIFIC NAME',
                     'OBSERVATION COUNT')
  data <- readr::read_tsv(pathToFile) %>%
    dplyr::rename(observer = `OBSERVER ID`, protocol = `PROTOCOL TYPE`,
           lat = LATITUDE,  lon = LONGITUDE,
           date = `OBSERVATION DATE`, sp = `SCIENTIFIC NAME`,
           count = `OBSERVATION COUNT`) %>%
    dplyr::select(observer, protocol, lat, lon, date, sp, count)
  write.csv(data, pathToOutput, row.names = FALSE)
}

splitReader <- function(pathToFile, rowSkip, maxRows){
  colnames <- readr::read_tsv(pathToFile, n_max = 1) %>% names
  data <- readr::read_tsv(pathToFile, col_names = colnames,
                          skip = rowSkip, n_max = maxRows) %>%
    dplyr::rename(observer = `OBSERVER ID`, protocol = `PROTOCOL TYPE`,
                  lat = LATITUDE,  lon = LONGITUDE,
                  date = `OBSERVATION DATE`, sp = `SCIENTIFIC NAME`,
                  count = `OBSERVATION COUNT`) %>%
    dplyr::select(observer, protocol, lat, lon, date, sp, count)
  return(data)
}

readWriteEbird <- function(pathToFile, pathToOutput){
  data <- bind_rows(
    splitReader(pathToFile, 1, 1000000),
    splitReader(pathToFile, 1000000, 1000000),
    splitReader(pathToFile, 2000000, 1000000),
    splitReader(pathToFile, 3000000, 1000000),
    splitReader(pathToFile, 4000000, 1000000),
    splitReader(pathToFile, 5000000, 1000000),
    splitReader(pathToFile, 6000000, 1000000),
    splitReader(pathToFile, 7000000, 1000000),
    splitReader(pathToFile, 8000000, 1000000),
    splitReader(pathToFile, 9000000, 1000000),
    splitReader(pathToFile, 10000000, 1000000),
    splitReader(pathToFile, 11000000, -1)
  ) %>%
    distinct
  write.csv(data, pathToOutput, row.names = FALSE)
}

# 2009 eBird. Load successful

readWriteEbird(
  'C:/Users/Brian/Downloads/eBird1/ebd_US_200901_200904_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2009.csv'
)


# 2010 eBird. Load successful

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201001_201004_relMay-2016/ebd_US_201001_201004_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2010.csv'
)

# 2011 eBird. NOTE: ERROR ON READ, LINE 3308791

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201101_201106_relMay-2016/ebd_US_201101_201106_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2011.csv'
)

# 2012 eBird. NOTE: ERROR ON READ, LINE 9442906

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201201_201206_relMay-2016/ebd_US_201201_201206_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2012.csv'
)

# 2013 eBird. NOTE: ERROR ON READ, LINE 14072990

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201301_201306_relMay-2016/ebd_US_201301_201306_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2013.csv'
)

# 2014 eBird

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201401_201406_relMay-2016/ebd_US_201401_201406_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2014.csv'
)

# 2015 eBird

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201501_201506_relMay-2016/ebd_US_201501_201506_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2015.csv'
)

# 2016 eBird

readWriteEbird(
  'C:/Users/Brian/Downloads/ebd_US_201603_201606_relMay-2016/ebd_US_201603_201606_relMay-2016.txt', 
  'C:/Users/Brian/Desktop/rubl_summer_2016/winter2016.csv'
)
