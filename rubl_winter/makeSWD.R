# Summarize point data by raster cell

library(sp)
library(raster)
library(dplyr)

options(stringsAsFactors = FALSE)

#---------------------------------------------------------------------------------------------------*
# ---- FUNCTIONS ----
#---------------------------------------------------------------------------------------------------*


loadEnv = function(rasterDirectory){
  require(raster)
  # Find the raster data (searches for and ID's all files that end in ".asc":
  
  raster_data <- list.files(rasterDirectory,
                            pattern='\\.asc$', full=T)    
  
  # Create a raster stack of raster layers:
  
  env.stack = stack(raster_data)
  
  # Add raster stack values to memory:
  
  values(env.stack) = getValues(env.stack)
  
  # Add projection information to the raster stack:
  
  newproj = CRS('+proj=longlat +datum=WGS84')
  
  projection(env.stack) = newproj
  names(env.stack) = c('dev_hi','dev_li','flood','forh', 'form', 'grass',
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

#---------------------------------------------------------------------------------------------------*
# ---- GET CELL DATA AND SUMMARIZE SAMPLING EFFORT TO DATE AND CELL ----
#---------------------------------------------------------------------------------------------------*

# Get raster data:

rStack <- loadEnv('C:/Users/Brian/Dropbox/rubl_12_15/lc_asc')

# Raster data to be used for addresses:

r <- rStack[[1]]

projInfo = projection(r)

# Get rusty blackbird data:

rustyLists <- read.csv('C:/Users/Brian/Desktop/gits/RUBL/rubl_winter/rublEbird.csv') %>%
  tbl_df %>%
  mutate(date = as.Date(date)) %>%
  filter(lubridate::month(date) %in% 1:5,
         lubridate::year(date) %in% 2006:2016) %>%
  filter(count != 'X') %>%
  mutate(count = as.numeric(count))

rustyXobservations <- rustyLists %>%
  filter(count == 'X') %>%
  .$observationID
  
# Get eBird list data:

eBirdLists <- read.csv('C:/Users/Brian/Dropbox/eBirdListData.csv') %>%
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
  filter(!observationID %in% rustyXobservations)

# Add cell addresses

eBirdLists$cellAddress <- cellFromXY(
  r,
  eBirdLists %>%
    dplyr::select(lon, lat) %>%
    data.frame %>%
    SpatialPoints(proj4string = CRS(projInfo)) 
)

# Get sampling summaries (by date and cell address):

eBirdSampling <- eBirdLists %>%
  group_by(cellAddress, date) %>%
  summarize(
    lon = mean(lon),
    lat = mean(lat),
    nLists = n(),
    durMinutes = sum(durMinutes),
    effortDist = sum(effortDist)) %>%
  filter(!is.na(cellAddress)) %>%
  ungroup %>%
  mutate(date = as.Date(date))

# Extract raster land cover data by cell ID:

envByCell <- data.frame(
  cellAddress = (eBirdSampling$cellAddress %>% unique),
  raster::extract(
  x = rStack,
  y = (eBirdSampling$cellAddress %>% unique),
  df = TRUE)
) %>%
  tbl_df %>%
  dplyr::select(-c(ID, tmin, ppt))

# Join sampling data to environment data:

eBirdSamplingEnv <- left_join(
  eBirdSampling,
  envByCell,
  by = 'cellAddress'
)

# Get winter blitz time window samples:

winterSampling <- eBirdSamplingEnv %>%
  mutate(month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  filter(month == 1|
           (month == 2 & day <15),
         lubridate::year(date) < 2015) %>%
  dplyr::select(-c(month, day))

# Get tmin and ppt for a given date:

dates <- winterSampling$date %>% unique

outList <- vector('list', length = length(dates))

for(i in 1:length(dates)){
  # Date specification:
  date <- as.Date(dates[i])
  year <- year(date)
  month <- month(date)
  day <- day(date)
  # Get rasters for date:
  tmin <- downloadTminRaster(year, month, day)
  ppt <- downloadPPTRaster(year, month, day)

  # Subset sampling data to the selected date:
  winterSamplingSubset <- winterSampling %>%
    filter(date == dates[i])
  # Extract raster data
  winterSamplingSubset$tmin <- extract(
    tmin,
    data.frame(winterSamplingSubset$lon,
               winterSamplingSubset$lat)
  )
  winterSamplingSubset$ppt <- extract(
    ppt,
    data.frame(winterSamplingSubset$lon,
               winterSamplingSubset$lat)
  )
  # Return as list
  outList[[i]] <- winterSamplingSubset
}

saveRDS(outList, 'eBirdSamplingList.RDS')

winterEbirdByCell <- do.call('rbind', outList) %>%
  tbl_df 

#---------------------------------------------------------------------------------------------------*
# ---- ATTACH RUBL SAMPLES FOR A GIVEN CELL AND DATE ----
#---------------------------------------------------------------------------------------------------*

getRustyPaFrame <- function(minFlockSize, protocolChoice){
  # Filtering by flock size
  
  rustyListsSubset <- rustyLists %>%
    filter(count >= minFlockSize)
  
#   if(protocolChoice == 'eBird'){
#     rustyListsSubset <- rustyListsSubset %>%
#       filter(str_detect(protocol, 'eBird'))
#   }
  
  rustyListsSubset$cellAddress <- cellFromXY(
    r,
    rustyListsSubset %>%
      dplyr::select(lon, lat) %>%
      data.frame %>%
      SpatialPoints(proj4string = CRS(projInfo)) 
  )
  
  rustyPa <- winterEbirdByCell %>%
    left_join(
      rustyListsSubset %>%
        group_by(cellAddress, date) %>%
        summarize(
          nListsR = n(),
          durMinutesR = sum(durMinutes),
          effortDistR = sum(effortDist)
        ) %>%
        filter(!is.na(cellAddress)) %>%
        ungroup %>%
        mutate(date = as.Date(date)),
      by = c('cellAddress', 'date')
    ) %>% 
    mutate(
      nListsR = ifelse(is.na(nListsR), 0, nListsR),
      durMinutesR = ifelse(is.na(durMinutesR), 0, durMinutesR),
      effortDistR =  ifelse(is.na(effortDistR), 0, effortDistR),
      pa = ifelse(nListsR == 0, 0, 1)
    )
  return(rustyPa)
}

mod <- glm(pa~tmin * I(flood+wetw+weth) + I(tmin^2) + ppt + dev_hi, family = binomial, data = getRustyPaFrame(50,'e'))
summary(mod)
pscl::pR2(mod)  
