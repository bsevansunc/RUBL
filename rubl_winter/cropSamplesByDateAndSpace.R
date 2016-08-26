library(dplyr)
library(tidyr)
library(lubridate)

options(stringsAsFactors = FALSE)

# Set read directories

readDirectory <- 'C:/Users/Brian/Desktop/rubl_summer_2016/'

# Get spring and winter samples:

samplingWinter <- read.csv(
  paste0(readDirectory, 'samplingRecordsWinter.csv')
  ) %>%
  tbl_df %>%
  mutate(date = as.Date(date)) %>%
  filter(year(date) < 2012,
         month(date) < 4) %>%
  select(-lists)

rublWinter <- read.csv('C:/Users/Brian/Desktop/rubl_summer_2016/rublRecordsWinter.csv') %>%
  tbl_df %>%
  mutate(date = as.Date(date)) %>%
  filter(year(date) < 2012,
         month(date) < 4)

samplingSpring <- read.csv('C:/Users/Brian/Desktop/rubl_summer_2016/samplingRecordsSpring.csv') %>%
  tbl_df %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(year(date) > 2013,
         month(date) > 2 & month(date) < 6) %>%
  dplyr::select(-lists)

rublSpring <- read.csv('C:/Users/Brian/Desktop/rubl_summer_2016/rublRecordsSpring.csv') %>%
  tbl_df %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::filter(year(date) > 2013,
         month(date) > 2  & month(date) < 6)

# Remove long tail in the distrubtion:

quantileLatWinter <- c(29.17833,43.38250)
quantileLonWinter <- c(-97.54667,-70.95833)

samplingWinterCrop <- samplingWinter %>%
  dplyr::filter(
    lat >= quantileLatWinter[1],
    lat <= quantileLatWinter[2],
    lon >= quantileLonWinter[1],
    lon <= quantileLonWinter[2]
  )

rublWinterCrop <- rublWinter %>%
  dplyr::filter(
    lat >= quantileLatWinter[1],
    lat <= quantileLatWinter[2],
    lon >= quantileLonWinter[1],
    lon <= quantileLonWinter[2]
  )

# Subset spring sampling to the 99th percentile of rubl observations (lat lon):

quantileLatSpring <- c(24.0625, 49.9375)
quantileLonSpring <- c(-100.0208, -66.47917)

samplingSpringCrop <- samplingSpring %>%
  dplyr::filter(
    lat >= quantileLatSpring[1],
    lat <= quantileLatSpring[2],
    lon >= quantileLonSpring[1],
    lon <= quantileLonSpring[2]
  )

rublSpringCrop <- rublSpring %>%
  dplyr::filter(
    lat >= quantileLatSpring[1],
    lat <= quantileLatSpring[2],
    lon >= quantileLonSpring[1],
    lon <= quantileLonSpring[2]
  )

write.csv(samplingWinterCrop, 'samplingWinterCrop.csv', row.names = FALSE)

write.csv(samplingSpringCrop, 'samplingSpringCrop.csv', row.names = FALSE)

write.csv(rublWinterCrop, 'rublWinterCrop.csv', row.names = FALSE)

write.csv(rublSpringCrop, 'rublSpringCrop.csv', row.names = FALSE)
