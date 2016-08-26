
#===================================================================================================*
# ---- SET-UP ----
#===================================================================================================*

library(dplyr)

#---------------------------------------------------------------------------------------------------*
# ---- FUNCTIONS ----
#---------------------------------------------------------------------------------------------------*

# Condense sampling files:

samplingRecordMaker <- function(data){
  require(dplyr)
  data %>%
    tbl_df %>%
    select(observer, lat, lon, date) %>%
    distinct %>%
    group_by(lat, lon, date) %>%
    summarize(lists = n())
}

# Get RUBL files

rublRecordMaker <- function(data){
  data %>%
    tbl_df %>%
    select(sp, protocol, lat, lon, date, count) %>%
    filter(sp == 'Euphagus carolinus') %>%
    mutate(protocol = ifelse(stringr::str_detect(protocol, 'Blitz'),
                             'blitz', 'ebird'),
           sp = 'rubl')
}

#===================================================================================================*
# ---- WINTER ----
#===================================================================================================*

# Read files

fileDirectory <- 'C:/Users/Brian/Desktop/rubl_summer_2016/winter'
  
eBirdFiles <- list.files(fileDirectory, pattern = '*.csv')

eBird <- lapply(
  paste(fileDirectory, eBirdFiles,sep ='/'),
  data.table::fread)


samplingRecords <- lapply(eBird, samplingRecordMaker) %>%
  bind_rows %>%
  tbl_df %>%
  filter(!is.na(lat))

write.csv(samplingRecords,
          'C:/Users/Brian/Desktop/rubl_summer_2016/samplingRecordsWinter.csv',
          row.names = FALSE)

rublRecords <- lapply(eBird, rublRecordMaker) %>%
  bind_rows %>%
  tbl_df

# rublRecords <- do.call('rbind', rublRecordList)

write.csv(rublRecords,
          'C:/Users/Brian/Desktop/rubl_summer_2016/rublRecordsWinter.csv',
          row.names = FALSE)

#===================================================================================================*
# ---- SPRING ----
#===================================================================================================*

# Read files

fileDirectory <- 'C:/Users/Brian/Desktop/rubl_summer_2016/spring'

eBirdFiles <- list.files(fileDirectory, pattern = '*.csv')

eBird <- lapply(
  paste(fileDirectory, eBirdFiles,sep ='/'),
  data.table::fread)

samplingRecords <- lapply(eBird, samplingRecordMaker) %>%
  bind_rows %>%
  tbl_df %>%
  filter(!is.na(lat))

write.csv(samplingRecords,
          'C:/Users/Brian/Desktop/rubl_summer_2016/samplingRecordsSpring.csv',
          row.names = FALSE)

rublRecords <- lapply(eBird, rublRecordMaker) %>%
  bind_rows %>%
  tbl_df

write.csv(rublRecords,
          'C:/Users/Brian/Desktop/rubl_summer_2016/rublRecordsSpring.csv',
          row.names = FALSE)




