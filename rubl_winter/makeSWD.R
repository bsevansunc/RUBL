# Summarize point data by raster cell

library(sp)
library(raster)
library(dplyr)
library(stringr)

options(stringsAsFactors = FALSE)

#---------------------------------------------------------------------------------------------------*
# ---- FUNCTIONS ----
#---------------------------------------------------------------------------------------------------*

loadEnv <- function(rasterDirectory){
  require(raster)
  # Find the raster data (searches for and ID's all files that end in ".asc":
  
  raster_data <- list.files(rasterDirectory,
                            pattern='\\.asc$', full=T)    
  
  # Create a raster stack of raster layers:
  
  env.stack <- stack(raster_data)
  
  # Add raster stack values to memory:
  
  values(env.stack) <- getValues(env.stack)
  
  # Add projection information to the raster stack:
  
  projection(env.stack) <- CRS('+proj=longlat +datum=WGS84')
  
  names(env.stack) <- c('dev_hi','dev_li','flood','forh', 'form', 'grass',
                       'pasture','ppt','rowcrop', 'shrub','tmin', 'upfor',
                       'weth', 'wetw', 'woodland')
  return(env.stack)
}

# Load tmin raster for a given date:

downloadTminRaster <- function(year, month, day){
  yearMonth <- paste0(year, '0', month, '00') %>% as.numeric
  date <- yearMonth + day
  if(year != 2016){
    ftpAddress <- paste('ftp://prism.oregonstate.edu/daily/tmin',
                        year,
                        'PRISM_tmin_stable_4kmD1',
                        sep = '/')
  } else {
    ftpAddress <- paste('ftp://prism.oregonstate.edu/daily/tmin',
                        year,
                        'PRISM_tmin_provisional_4kmD1',
                        sep = '/')
  }
  tmin <- tempfile()
  ftp <- paste(ftpAddress, date, 'bil.zip', sep = '_')
  download.file(ftp, tmin)
  # Unzip:
  tmin <- unzip(tmin)
  # Convert to raster format:
  tmin <- raster(tmin[1])
  # Provide projection information:
  crs(tmin) <- "+proj=longlat +datum=WGS84"
  return(crop(tmin, rStack[[1]]))
}

downloadPPTRaster <- function(year, month, day){
  yearMonth <- paste0(year, '0', month, '00') %>% as.numeric
  date <- yearMonth + day
  if(year != 2016){
    ftpAddress <- paste('ftp://prism.oregonstate.edu/daily/ppt',
                        year,
                        'PRISM_ppt_stable_4kmD2',
                        sep = '/')
  } else {
    ftpAddress <- paste('ftp://prism.oregonstate.edu/daily/ppt',
                        year,
                        'PRISM_ppt_provisional_4kmD2',
                        sep = '/')
  }
  ppt <- tempfile()
  ftp <- paste(ftpAddress, date, 'bil.zip', sep = '_')
  download.file(ftp, ppt)
  # Unzip:
  ppt <- unzip(ppt)
  # Convert to raster format:
  ppt <- raster(ppt[1])
  # Provide projection information:
  crs(ppt) <- "+proj=longlat +datum=WGS84"
  return(crop(ppt, rStack[[1]]))
}

# Load tmin raster for a given date:

downloadTminRasterMonthly <- function(year){
  month = c('01', '02')
  outList <- vector('list', length = 2)
  for(i in 1:2){
    ftp <- paste0('ftp://prism.oregonstate.edu/monthly/tmin/',
                  year, '/',
                  'PRISM_tmin_stable_4kmM2_',
                  paste0(year, month[i]),
                  '_bil.zip')
    tmin <- tempfile()
    download.file(ftp, tmin)
    # Unzip:
    tmin <- unzip(tmin)
    # Convert to raster format:
    tmin <- raster(tmin[1])
    # Provide projection information:
    crs(tmin) <- "+proj=longlat +datum=WGS84"
    outList[[i]] <- crop(tmin, rStack[[1]])
  }
  writeRaster(overlay(stack(outList), fun = mean), 
              paste0('tmin', year),
              overwrite = TRUE)
  }


downloadPPTRasterMonthly <- function(year){
  month = c('01', '02')
  outList <- vector('list', length = 2)
  for(i in 1:2){
    ftp <- paste0('ftp://prism.oregonstate.edu/monthly/ppt/',
                  year, '/',
                  'PRISM_ppt_stable_4kmM3_',
                  paste0(year, month[i]),
                  '_bil.zip')
    ppt <- tempfile()
    download.file(ftp, ppt)
    # Unzip:
    ppt <- unzip(ppt)
    # Convert to raster format:
    ppt <- raster(ppt[1])
    # Provide projection information:
    crs(ppt) <- "+proj=longlat +datum=WGS84"
    outList[[i]] <- crop(ppt, rStack[[1]])
  }
  writeRaster(overlay(stack(outList), fun = mean), 
              paste0('ppt', year),
              overwrite = TRUE)
}

years <- 2006:2016

for(i in 1:length(years)){
  downloadTminRasterMonthly(years[i])
  downloadPPTRasterMonthly(years[i])
}


#---------------------------------------------------------------------------------------------------*
# ---- GET CELL DATA AND SUMMARIZE SAMPLING EFFORT TO DATE AND CELL ----
#---------------------------------------------------------------------------------------------------*
# pathToRasterData <- 'C:/Users/Brian/Dropbox/rubl_12_15/'  # Helm
# pathToRasterData <- '/Users/bsevans/Dropbox/rubl_12_15/'   # MacBook Air
pathToRasterData <- 'C:/Users/Default.Default-THINK/Dropbox/rubl_12_15/' # Thinkpad
# Get raster data:

rStack <- loadEnv(paste0(pathToRasterData, 'lc_asc'))

# Raster data to be used for addresses:

r <- rStack[[1]]

projInfo = projection(r)

# Get rusty blackbird data:

# note: 
# wd, helm =  'C:/Users/Brian/Desktop/gits/RUBL/rubl_winter/'
# wd, thinkPad = 'C:/Users/Default.Default-THINK/Desktop/gits/RUBL/rubl_winter'

rustyLists <- read.csv('rublEbird.csv') %>%
  tbl_df %>%
  # Subset to dates associated with the winter blitz:
  mutate(date = as.Date(date)) %>%
  mutate(month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month == 1|
           (month == 2 & day <15),
         lubridate::year(date) < 2015) %>%
  dplyr::select(-c(month, day)) %>%
  # Remove counts recorded as 'X':
  filter(count != 'X') %>%
  mutate(count = as.numeric(count))
  
# Make a vector of rusty lists where count is recorded as 'X':

rustyXobservations <- read.csv('rublEbird.csv') %>%
  filter(count == 'X') %>%
  .$observationID
  
# Get eBird list data:

# pathToEbirdListData <- 'C:/Users/Brian/Dropbox/eBirdListData.csv'   # Helm
# pathToEbirdListData <- 'C:/Users/Default.Default-THINK/Dropbox/eBirdListData.csv' # ThinkPad
# pathToEbirdListData <- '/Users/bsevans/Dropbox/eBirdListData.csv'   # MacBook Air

eBirdLists <- read.csv(pathToEbirdListData) %>%
  tbl_df %>%
  # Filter to the study extent:
  filter(lon > extent(r)[1] & lon < extent(r)[2],
         lat > extent(r)[3] & lat < extent(r)[4]) %>%
  # Subset lists to only dates associated with the rusty observations:
  filter(date %in%
           (rustyLists$date %>% 
              unique %>% 
              as.character)) %>%
  # Remove observationIDs where rusty count was reported as X:
  filter(!observationID %in% rustyXobservations) %>%
  # Add count data:
  left_join(rustyLists %>%
              dplyr::select(observationID, count),
            by = 'observationID') %>%
  # Change na counts (no observation match) to 0:
  mutate(count = ifelse(is.na(count), 0, count))

# Add cell addresses

eBirdLists$cellAddress <- cellFromXY(
  r,
  eBirdLists %>%
    dplyr::select(lon, lat) %>%
    data.frame %>%
    SpatialPoints(proj4string = CRS(projInfo)) 
)

# Get sampling summaries (by date and cell address):

# eBirdSampling <- eBirdLists %>%
#   group_by(cellAddress, date) %>%
#   summarize(
#     lon = mean(lon),
#     lat = mean(lat),
#     nLists = n(),
#     durMinutes = sum(durMinutes),
#     effortDist = sum(effortDist)) %>%
#   filter(!is.na(cellAddress)) %>%
#   ungroup %>%
#   mutate(date = as.Date(date))

# Extract raster land cover data by cell ID:

envByCell <- data.frame(
  cellAddress = (eBirdLists$cellAddress %>% unique),
  raster::extract(
  x = rStack,
  y = (eBirdLists$cellAddress %>% unique),
  df = TRUE)
) %>%
  tbl_df %>%
  dplyr::select(-c(ID, tmin, ppt))

# Join sampling data to environment data and add year field:

eBirdSamplingEnv <- left_join(
  eBirdLists,
  envByCell,
  by = 'cellAddress'
) %>%
  mutate(date = as.Date(date),
         year = lubridate::year(date))

# Add precipitation and temperature data:

years <- 2006:2014

samplingByYearList <- vector('list', length = length(years))

for(i in 1:length(years)){
  samplingSubset <- eBirdSamplingEnv %>%
    filter(year == years[i])
  pptR <- raster(paste0(pathToRasterData, 'climateRasters/ppt',years[i]))
  tminR <- raster(paste0(pathToRasterData, 'climateRasters/tmin',years[i]))
  samplingSubset$ppt <- extract(pptR, cbind(samplingSubset$lon, samplingSubset$lat))
  samplingSubset$tmin <- extract(tminR, cbind(samplingSubset$lon, samplingSubset$lat))
  samplingByYearList[[i]] <- samplingSubset %>%
    filter(!is.na(tmin), !is.na(ppt)) %>%
    dplyr::select(-year)
}

swd <- do.call('rbind', samplingByYearList) %>%
  tbl_df %>%
  dplyr::select(-c(observationID, observer, lat, lon, nObservers, cellAddress)) %>%
  mutate(protocol = ifelse(
    stringr::str_detect(protocol, 'Blitz'),
    'blitz', 'eb'
  )) %>%
  filter(!is.na(dev_hi), !is.na(effortDist))

# write.csv(swd, 'swdWinter.csv', row.names = FALSE)
# 
# swd <- read.csv('swdWinter.csv')
# 
# swdCombinedFun <- function(flockSize){
#   swdCombinedSamples <- do.call('rbind', samplingByYearList) %>%
#     tbl_df %>%
#     mutate(
#       protocol = ifelse(str_detect(protocol, 'Blitz'), 'blitz', 'eb'),
#       year = lubridate::year(date),
#       pa = ifelse(count >= flockSize, 1, 0)
#     ) %>%
#     filter(!is.na(dev_hi), !is.na(effortDist)) %>%
#     dplyr::select(-c(observationID, observer, lat, lon, date, time, nObservers)) %>%
#     group_by(cellAddress, year, protocol) %>%
#     summarize(
#       tLists = n(),
#       pLists = sum(pa),
#       durMinutes = sum(durMinutes),
#       effortDist = sum(effortDist)
#     ) %>%
#     left_join(
#       do.call('rbind', samplingByYearList) %>%
#         mutate(year = lubridate::year(date)) %>%
#         dplyr::select(cellAddress, year, dev_hi:tmin) %>%
#         distinct,
#       by = c('cellAddress', 'year')
#     ) %>%
#     mutate(pa = ifelse(pLists > 0, 1, 0)) %>%
#     dplyr::select(cellAddress, year, protocol, pLists, tLists, pa, dev_hi:tmin)
#   return(swdCombinedSamples)
# }


swdCombinedFun <- function(flockSizeMin, flockSizeMax){
  swdCombinedSamples <- do.call('rbind', samplingByYearList) %>%
    tbl_df %>%
    mutate(
      protocol = ifelse(str_detect(protocol, 'Blitz'), 'blitz', 'eb'),
      year = lubridate::year(date),
      pa = ifelse(count >= flockSizeMin & count <= flockSizeMax, 1, 0)
    ) %>%
    filter(!is.na(dev_hi), !is.na(effortDist),
           !(pa == 0 & count > 0)) %>%
    dplyr::select(-c(observationID, observer, lat, lon, date, time, nObservers)) %>%
    group_by(cellAddress, year, protocol) %>%
    summarize(
      tLists = n(),
      pLists = sum(pa),
      durMinutes = sum(durMinutes),
      effortDist = sum(effortDist)
    ) %>%
    left_join(
      do.call('rbind', samplingByYearList) %>%
        mutate(year = lubridate::year(date)) %>%
        dplyr::select(cellAddress, year, dev_hi:tmin) %>%
        distinct,
      by = c('cellAddress', 'year')
    ) %>%
    mutate(pa = ifelse(pLists > 0, 1, 0)) %>%
    dplyr::select(cellAddress, year, protocol, pLists, tLists, pa, dev_hi:tmin)
  return(swdCombinedSamples)
}


#   
# swdCombinedSamples %>%
#   group_by(cellAddress) %>%
#   filter(year %in% 2009:2011) %>%
#   summarize(years = length(unique(year)),
#             pLists = length(pLists > 0)) %>%
#   filter(years == 3) %>%
#   filter(pLists > 0) %>% View

  
# 
# summary(swd)
# table(swd$protocol)

# Get winter blitz time window samples:
# 
# winterSampling <- eBirdSamplingEnv %>%
#   mutate(month = lubridate::month(date),
#          day = lubridate::day(date)) %>%
#   filter(month == 1|
#            (month == 2 & day <15),
#          lubridate::year(date) < 2015) %>%
#   dplyr::select(-c(month, day))

# Get tmin and ppt for a given date:
# 
# dates <- winterSampling$date %>% unique
# 
# outList <- vector('list', length = length(dates))
# 
# for(i in 1:length(dates)){
#   # Date specification:
#   date <- as.Date(dates[i])
#   year <- year(date)
#   month <- month(date)
#   day <- day(date)
#   # Get rasters for date:
#   tmin <- downloadTminRaster(year, month, day)
#   ppt <- downloadPPTRaster(year, month, day)
# 
#   # Subset sampling data to the selected date:
#   winterSamplingSubset <- winterSampling %>%
#     filter(date == dates[i])
#   # Extract raster data
#   winterSamplingSubset$tmin <- extract(
#     tmin,
#     data.frame(winterSamplingSubset$lon,
#                winterSamplingSubset$lat)
#   )
#   winterSamplingSubset$ppt <- extract(
#     ppt,
#     data.frame(winterSamplingSubset$lon,
#                winterSamplingSubset$lat)
#   )
#   # Return as list
#   outList[[i]] <- winterSamplingSubset
# }
# 
# saveRDS(outList, 'eBirdSamplingList.RDS')
# 
# winterEbirdByCell <- do.call('rbind', outList) %>%
#   tbl_df 
# 
# write.csv(winterEbirdByCell, 'eBirdSampling_WithDailyTempsPPT.csv', row.names = FALSE)

#---------------------------------------------------------------------------------------------------*
# ---- ATTACH RUBL SAMPLES FOR A GIVEN CELL AND DATE ----
#---------------------------------------------------------------------------------------------------*

getRustyPaFrame <- function(minFlockSize,maxFlockSize, years, protocolChoice = 'all'){
  paFrame <- swdCombinedFun(minFlockSize, maxFlockSize) %>%
    filter(year %in% years) %>%
    ungroup
  if(protocolChoice == 'eb') {
    paFrame <- paFrame %>%
      filter(!(protocol == 'blitz' & count == 1))
  }
  if(protocolChoice == 'blitz'){
    paFrame <- paFrame %>%
      filter(!(protocol == 'eb' & count == 1))
  }
  return(paFrame)
}

# getRustyPaFrame <- function(minFlockSize, years, protocolChoice = 'all'){
#   paFrame <- swd %>%
#     filter(count >= minFlockSize | count == 0) %>%
#     filter(lubridate::year(date) %in% years) %>%
#     mutate(count = ifelse(count >= minFlockSize, 1, 0))
#   if(protocolChoice == 'eb') {
#     paFrame <- paFrame %>%
#       filter(!(protocol == 'blitz' & count == 1))
#   }
#   if(protocolChoice == 'blitz'){
#     paFrame <- paFrame %>%
#       filter(!(protocol == 'eb' & count == 1))
#   }
#   return(paFrame)
# }
# 
# getRustyPaFrame <- function(minFlockSize, protocolChoice){
#   # Filtering by flock size
#   
#   rustyListsSubset <- rustyLists %>%
#     filter(count >= minFlockSize)
#   
# #   if(protocolChoice == 'eBird'){
# #     rustyListsSubset <- rustyListsSubset %>%
# #       filter(str_detect(protocol, 'eBird'))
# #   }
#   
#   rustyListsSubset$cellAddress <- cellFromXY(
#     r,
#     rustyListsSubset %>%
#       dplyr::select(lon, lat) %>%
#       data.frame %>%
#       SpatialPoints(proj4string = CRS(projInfo)) 
#   )
#   
#   rustyPa <- winterEbirdByCell %>%
#     left_join(
#       rustyListsSubset %>%
#         group_by(cellAddress, date) %>%
#         summarize(
#           nListsR = n(),
#           durMinutesR = sum(durMinutes),
#           effortDistR = sum(effortDist)
#         ) %>%
#         filter(!is.na(cellAddress)) %>%
#         ungroup %>%
#         mutate(date = as.Date(date)),
#       by = c('cellAddress', 'date')
#     ) %>% 
#     mutate(
#       nListsR = ifelse(is.na(nListsR), 0, nListsR),
#       durMinutesR = ifelse(is.na(durMinutesR), 0, durMinutesR),
#       effortDistR =  ifelse(is.na(effortDistR), 0, effortDistR),
#       pa = ifelse(nListsR == 0, 0, 1)
#     )
#   return(rustyPa)
# }

# dTest <- getRustyPaFrame(100,2009:2011)

mod <- glm(pa~scale(tmin)*scale(ppt) + scale(flood) +
             scale(forh) + scale(form) + scale(dev_hi) + scale(dev_li) +
             scale(grass) + scale(rowcrop) + scale(pasture) + scale(shrub) +
             scale(weth) + scale(woodland) + scale(wetw) +
             I(scale(tmin)^2) + I(scale(ppt)^2), family = binomial,
           data = getRustyPaFrame(100,2009))
summary(mod)
pscl::pR2(mod)

test <- lme4::glmer(pa ~ scale(tmin)*scale(ppt) + 
                      I(scale(tmin)^2) + I(scale(ppt)^2) + 
                      scale(flood) + scale(forh) + scale(form) +
                      scale(dev_hi) + scale(dev_li) + 
                      scale(grass) + scale(rowcrop) +
                      scale(pasture) + scale(shrub) +
                      scale(weth) + scale(woodland) + scale(wetw) +
                     (1|year),
                   data = getRustyPaFrame(100,2009:2011) %>%
                     mutate(year = factor(year)),
                   family = binomial,
                   control = lme4::glmerControl(optimizer = "bobyqa"),
                   nAGQ = 10)


