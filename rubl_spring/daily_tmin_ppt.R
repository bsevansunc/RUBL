# Load libraries:

library(raster)
library(dismo)
library(gstat)
library(maps)


#----------------------------------------------------------------------------
# Acquiring and formatting data
#============================================================================

# Get a shapefile of the United States:

us = data('state')

#-----------------------------------------------------------------------
# Download a raster file from PRISM website:
# Note: These are minimum temperature data for 4/27/2014
# Steps in preparing the file for using in R:
#	a. Create an empty temp file for storage
#	b. Provide the website from which the data are retrieved
#	c. Download the file into the temp slot
#	d. PRISM data are provided as zip files, these must be unzipped
#	d. Convert to raster format
#	e. Specify projection information

# Create temporary storage location:

tmin = tempfile()

# Specify website:

ftp = 'ftp://prism.oregonstate.edu/daily/tmin/2014/PRISM_tmin_stable_4kmD1_20140301_bil.zip'

# Download file:

download.file(ftp,tmin)

# Unzip:

tmin = unzip(tmin)

# Convert to raster format:

tmin = raster(tmin[1])

# Provide projection information:

crs(tmin) = "+proj=longlat +datum=WGS84"

rList <- vector('list', length = 31)

downloadTminToRaster <- function(year, month, day){
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
   outAddress <- 'C:/Users/Brian/Desktop/rubl_summer_2016/minTempRasters/tmin_'
   dateCharacter <- paste(year, month, day, sep = '-') %>% as.Date %>% as.character
   writeRaster(tmin, paste0(outAddress, dateCharacter), overwrite = TRUE)
}

years <- 2014:2016
months <- 3:5

for(i in 1:length(years)){
  for(j in 1:length(months)){
    if(months[j] == 4){
      days <- 1:30
    } else {
      days <- 1:31
    }
    for(k in 1:length(days)){
      downloadTminToRaster(years[i], months[j], days[k])
    }
  }
}


downloadPPTToRaster <- function(year, month, day){
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
  outAddress <- 'C:/Users/Brian/Desktop/rubl_summer_2016/pptRasters/ppt_'
  dateCharacter <- paste(year, month, day, sep = '-') %>% as.Date %>% as.character
  writeRaster(ppt, paste0(outAddress, dateCharacter), overwrite = TRUE)
}


years <- 2014:2016
months <- 3:5

for(i in 1:length(years)){
  for(j in 1:length(months)){
    if(months[j] == 4){
      days <- 1:30
    } else {
      days <- 1:31
    }
    for(k in 1:length(days)){
      downloadPPTToRaster(years[i], months[j], days[k])
    }
  }
}

