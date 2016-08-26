#===================================================================================================*
# ---- SET-UP ----
#===================================================================================================*

# Set file paths

pathToFiles <- 'C:/Users/Brian/Dropbox/rubl_12_15/'

pathToSource <- 'C:/Users/Brian/Desktop/gits/RUBL/'

pathToFiles <- 'C:/Users/Default.Default-THINK/Dropbox/rubl_12_15/'

pathToFiles <- 'D:/rubl_summer_2016/'


# pathToOutput <- ''

# Load libraries:

library(dplyr) ; library(tidyr); library(stringr)
library(sp) ; library(raster) ; library(dismo) 

# Load environment data:

source(str_c(pathToSource, 'load_env.R'))

env.stack <- loadEnv(str_c(pathToFiles, 'lc_asc'))

# Remove tmin and precipitation:

# env.stack <- env.stack[[c(1:7, 9:10, 12:15)]]

# Find the SWD data (searches for and ID's all files that end in ".csv":

swdData <- list.files(
  str_c(pathToSource,'rubl_spring/spring'),
  pattern='\\.csv$', full=T
)

# Add data to memory:

swdList <- lapply(swdData, read.csv)

rubl <- swdList[[1]] %>% dplyr::arrange(date)
bg <- swdList[[2]] %>%
  dplyr::mutate(sp = 'bg',
                count = 0,
                protocol = 'ebird') %>%
  dplyr::select(sp, protocol,lat, lon, date, count)


# Project pts and extract lcData:

extractLcToPts <- function(ptFile){
  spPts <- SpatialPoints(ptFile[,c('lon','lat')],
                         proj4string = CRS(raster::projection(env.stack)))
  ptsEnv <- raster::extract(env.stack, spPts) %>%
    as.data.frame(na.rm = TRUE)
  envOut <- cbind(dplyr::select(ptFile,sp:count), ptsEnv) %>%
    filter(!is.na(dev_hi))
}

swd <- bind_rows(extractLcToPts(rubl) %>%
                   mutate(count = as.numeric(count)),
                 extractLcToPts(bg))

#---------------------------------------------------------------------------------------------------*
# ---- PRECIPITATION AND MINIMUM TEMPERATURE ----
#---------------------------------------------------------------------------------------------------*

# Add tmin and ppt data for a given date to the table:

dates <- swd %>%
  select(date) %>%
  distinct %>%
  arrange(date) %>%
  .$date

# Function to get a stack of ppt and tmin rasters associated with a given date:

rasterPrepTminPpt <- function(date){
  # Get ppt and tmin rasters for given date:
  
  rasterDirTmin <- 'C:/Users/Brian/Desktop/rubl_summer_2016/minTempRasters/tmin_'
  rasterDirPPt <- 'C:/Users/Brian/Desktop/rubl_summer_2016/pptRasters/ppt_'
  
  tminR <- raster(paste0(rasterDirTmin, date))
  pptR <- raster(paste0(rasterDirPPt, date))
  
  # Create a raster stack of raster layers:
  
  tminPptStack = stack(tminR, pptR)
  
  # Add raster stack values to memory:
  
  values(tminPptStack) = getValues(tminPptStack)
  
  # Add projection information to the raster stack:
  
  newproj = CRS('+proj=longlat +datum=WGS84')
  
  projection(tminPptStack) = newproj
  names(tminPptStack) = c('tmin','ppt')
  return(tminPptStack)
}

# Average a stack of ppt and tmin rasters associated with a given week (jumps ahead!):

swdRUBL <- readRDS('swdRUBL.RDS')

library(lubridate)

swdDateWeekList <- vector('list', length = 3)

for(i in 1:3){
  swdDateWeekList[[i]] <- swdRUBL[[i]] %>%
    mutate(lat = round(lat, 2),
           lon = round(lon, 2),
           week = lubridate::week(as.Date(date)) %>%
             plyr::round_any(2, f = ceiling),
           year = lubridate::year(as.Date(date))) %>%
    group_by(lat, lon, week, year) %>%
    mutate(count = max(count),
           tmin = mean(tmin),
           ppt = mean(ppt)
    ) %>%
    ungroup %>%
    dplyr::select(date, week) %>%
    distinct
}

weeks <- seq(10, 18, 2)
swdDateWeek <- bind_rows(swdDateWeekList) %>%
  filter(week %in% weeks)

rasterWeekList <- vector('list', length = length(weeks))

for(i in 1:length(weeks)){
  dates <- swdDateWeek %>%
    filter(week == weeks[i]) %>%
    .$date
  tminDateRasterList <- vector('list', length = length(dates))
  pptDateRasterList <- vector('list', length = length(dates))
  for(j in 1:length(dates)){
    rasterListByDate <- rasterPrepTminPpt(dates[j])
    tminDateRasterList[[j]] <- rasterListByDate[[1]]
    pptDateRasterList[[j]] <- rasterListByDate[[2]]
  }
  tempPpt <- vector('list', length = 2)
  names(tempPpt) <- c('tmin', 'ppt')
  tempPpt[[1]] <- mean(stack(tminDateRasterList)) %>%
    crop(env.stack[[1]])
  tempPpt[[2]] <- mean(stack(pptDateRasterList)) %>%
    crop(env.stack[[1]])
  rasterWeekList[[i]] <- tempPpt
}
# 
# for(i in 1:5) {plot(rasterWeekList[[i]][[2]], col=rev( rainbow( 99, start=0,end=1 ) ), breaks=seq(min(minValue(rasterWeekList[[1]][[2]])),max(maxValue(rasterWeekList[[5]][[2]])),length.out=100) )}

# Function extract tmin, ppt to points and add the columns to the swd file

extractTminPptToPts <- function(dateValue, ptFile){
  # Prepare point file:
  pts <- ptFile %>%
    filter(date == dateValue) %>%
    as.data.frame
  spPts <- SpatialPointsDataFrame(coords = pts[,c('lon','lat')],
                                  data = pts,
                                  proj4string = CRS(raster::projection(env.stack)))
  # Get raster file:
  rasterStack <- rasterPrepTminPpt(dateValue)
  # Extract To pts
  ptsEnv <- raster::extract(rasterStack, spPts)
  ptsEnv <- cbind(spPts@data, ptsEnv) %>%
    filter(!is.na(tmin))
  return(ptsEnv)
}

# Make swds

dateList <- vector('list', length = length(dates))

for(i in 1:length(dateList)){
  dateList[[i]] <- extractTminPptToPts(dates[i], swd)
}

swdComplete <- bind_rows(dateList)

swdRUBL <- list(
  all = swdComplete %>%
    dplyr::filter(sp == 'rubl') %>%
    mutate(count = ifelse(is.na(count), 1, count),
           sp =  ifelse(count > 99, 'large',ifelse(
             count > 19 & count <100, 'medium', 'small')
           )),
  bz = swdComplete %>%
    dplyr::filter(sp == 'rubl', protocol == 'blitz') %>%
    mutate(count = ifelse(is.na(count), 1, count),
           sp =  ifelse(count > 99, 'large',ifelse(
             count > 19 & count <100, 'medium', 'small')
           )),
  eb = swdComplete %>%
    dplyr::filter(sp == 'rubl', protocol == 'ebird') %>%
    mutate(count = ifelse(is.na(count), 1, count),
           sp =  ifelse(count > 99, 'large',ifelse(
             count > 19 & count <100, 'medium', 'small')
           ))
)

saveRDS(swdRUBL, 'swdRUBL.RDS')

swdBG <- filter(swdComplete, sp == 'bg') %>%
  mutate(sp = 0)

saveRDS(swdBG, 'swdBG.RDS')
########################################################

setwd('C:/Users/Brian/Desktop/gits/RUBL/rubl_spring')

swdRUBL <- readRDS('swdRUBL.RDS')

names(swdRUBL)
library(lubridate)

for(i in 1:3){
  swdRUBL[[i]] <- swdRUBL[[i]] %>%
    mutate(lat = round(lat, 2),
           lon = round(lon, 2),
           week = lubridate::week(as.Date(date)) %>%
             plyr::round_any(2, f = ceiling),
           year = lubridate::year(as.Date(date))) %>%
    group_by(lat, lon, week, year) %>%
    mutate(count = max(count),
           tmin = mean(tmin),
           ppt = mean(ppt)
    ) %>%
    ungroup %>%
    dplyr::select(-date) %>%
    dplyr::select(sp:count, week, year, dev_hi:ppt) %>%
    distinct
}

swdBG <- readRDS('swdBG.RDS') %>%
  mutate(lat = round(lat, 2),
         lon = round(lon, 2),
         week = lubridate::week(as.Date(date)) %>%
           plyr::round_any(2, f = ceiling),
         year = lubridate::year(as.Date(date))) %>%
  group_by(lat, lon, week, year) %>%
  mutate(tmin = mean(tmin),
         ppt = mean(ppt)
  ) %>%
  ungroup %>%
  dplyr::select(-date) %>%
  dplyr::select(sp:count, week, year, dev_hi:ppt) %>%
  distinct

########################################################

# Function to prepare swd files:

prepSWDweek <- function(inData){
  swdIn <- inData
  weeks <- seq(10, 18, 2)
  swdWkList <- vector('list',length = length(weeks))
  names(swdWkList) <- paste0('w',weeks)
  for(i in 1:length(weeks)){
    weekValue <- weeks[i]
    flockSizes <- c('small', 'medium', 'large')
    swdFSList <- vector('list', length = 3)
    names(swdFSList) <- flockSizes
    for(j in 1:length(flockSizes)){
      swdPres <- swdIn %>%
        dplyr::filter(sp == flockSizes[j],
                      week == weekValue) %>%
        dplyr::mutate(sp = 1,
                      ll = paste(lat, lon, week, year, sep = ','))
      swdAbs <- swdBG %>%
        dplyr::filter(week == weeks[i]) %>%
        mutate(ll = paste(lat, lon, week, year, sep = ',')) %>%
        dplyr::filter(!ll %in% swdPres$ll)
      swdAbs$k <- kfold(swdAbs, k = 5)
      swdPres$k <- kfold(swdPres, k = 5)
      swdFSList[[j]] <- bind_rows(swdPres, swdAbs) %>%
        dplyr::select(-c(ll))
    }
    swdWkList[[i]] <- swdFSList
  }
  return(swdWkList)
}



swdWeek <- prepSWDweek(swdRUBL$all)

# 
# prepSWDyr <- function(inData){
#   swdIn <- inData
#   swdYrList <- vector('list',length = 3)
#   years <- 2014:2016
#   names(swdYrList) <- paste0('y',years)
#   for(i in 1:length(years)){
#     flockSizes <- c('small', 'medium', 'large')
#     swdFSList <- vector('list', length = 3)
#     names(swdFSList) <- flockSizes
#     for(j in 1:length(flockSizes)){
#       swdPres <- swdIn %>%
#         dplyr::filter(sp == flockSizes[j],
#                       year == years[i],
#                       week < 19) %>%
#         dplyr::mutate(sp = 1,
#                       ll = paste(lat, lon, week, year, sep = ','))
#       swdAbs <- swdBG %>%
#         dplyr::filter(year == years[w]) %>%
#         mutate(ll = paste(lat, lon, week, year, sep = ',')) %>%
#         filter(!ll %in% swdPres$ll) %>%
#         filter(week < 19)
#       swdAbs$k <- kfold(swdAbs, k = 5)
#       swdPres$k <- kfold(swdPres, k = 5)
#       swdFSList[[j]] <- bind_rows(swdPres, swdAbs) %>%
#         dplyr::select(-c(ll))
#     }
#     swdYrList[[i]] <- swdFSList
#   }
#   return(swdYrList)
# }
# 
# swdYr <- prepSWDyr(swdRUBL$all)


#       swd[[i]] <- vector('list',length = 3)
#       for(j in 1:3){
#         swdBG$k <- kfold(swdBG, k = 5)
#         swdFS <- swdRUBL[[i]] %>%
#           dplyr::filter(sp == flockSizes[j]) %>%
#           dplyr::mutate(sp = 1,
#                         ll = paste(lat, lon, week, year, sep = ','))
#         swdFS$k <- kfold(swdFS, k = 5)
#         swd[[i]][[j]] <- rbind(swdFS,
#                                swdBG %>%
#                                  mutate(ll = paste(lat, lon, week, year, sep = ',')) %>%
#                                  filter(!ll %in% swdFS$ll)
#         ) %>%
#           dplyr::select(-c(ll))
#       }
#       names(swd[[i]]) <- flockSizes
#     }
#   }
#   names(swd) <- c('all','bz','eb')
#   return(swd)
# }

# swd <- prepSWD(swdRUBL$all)

# Run model for a given value of K, observationClass, and flock size class:

maxentRun <- function(swd1, flockSizeClass, kFold, beta.multiplier){
  # Create input file of k fold::
  max.in <- swd1[[flockSizeClass]] %>%
    dplyr::filter(k != kFold) %>%
    dplyr::select(-c(k, lon, lat, protocol, count, week, year)) %>%
    data.frame
  # Set model arguments
  beta.r <- str_c('betamultiplier=', beta.multiplier)
  mod.args <- c('nothreshold', 'nohinge', 'noproduct',#'noquadratic',
                beta.r, 'addallsamplestobackground',
                'writebackgroundpredictions','writeplotdata',
                'noautofeature','nooutputgrids',
                'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
  maxentModel <- maxent(max.in[,-1], max.in[,1], args = mod.args)
  return(list(swd = max.in, maxentModel = maxentModel))
}

# 

# observationClass = 'all'
# flockSizeClass = 'large'
# kFold = 3
beta.multiplier = 1

# modelsWk <- vector('list', length = )
# 
# test14 <- maxentRun(swdWeek$w15, flockSizeClass, kFold, beta.multiplier)
# test14
# 
# test14 <- maxentRun(swdWeek$w9, flockSizeClass, kFold, beta.multiplier)
# test14
# 
# test15<- maxentRun(swd$y2015, flockSizeClass, kFold, beta.multiplier)
# test15
# 
# test16 <- maxentRun(swd$y2016, flockSizeClass, kFold, beta.multiplier)
# test16
# 
# mTest <- maxentEvaluate(observationClass, flockSizeClass, kFold, beta.multiplier)

maxentEvaluateWk <- function(swd1, week1, flockSizeClass, kFold, beta.multiplier){
  # Run model
  maxentModelOut <- maxentRun(swd1, flockSizeClass, kFold, beta.multiplier)
  maxentModel <- maxentModelOut$maxentModel
  # Create test point files for evaluation
  swdTest <-   swd1[[flockSizeClass]] %>%
    dplyr::filter(k == kFold)
  testPresence <- swdTest %>%
    dplyr::filter(sp == 1) %>%
    dplyr::select(-c(sp,lon, lat,k)) # select(lon, lat)%>% 
  #SpatialPoints(proj4string = CRS(projection(env.stack)))
  testAbsence <- swdTest %>%
    dplyr::filter(sp == 0) %>%
    dplyr::select(-c(sp,lon, lat,k)) # select(lon, lat)%>%%>%
  #SpatialPoints(proj4string = CRS(projection(env.stack)))
  modelEvaluation <- evaluate(testPresence, testAbsence, maxentModel)
  return(list(maxentModel = maxentModel, modelEvaluation = modelEvaluation,
              swd = maxentModelOut$swd, swdTest = swdTest))
}

# test = maxentEvaluateWk(swdWeek$w15, 'w15', flockSizeClass, kFold, beta.multiplier)



# Function to run across folds:

maxentAcrossFoldsWk <- function(swd1, week1, flockSizeClass, beta.multiplier){
  kFold <- 1:5
  outList <- vector('list', length = 5)
  aucList <- vector('list', length = 5)
  corList <- vector('list', length = 5)
  presenceList <- vector('list', length = 5)
  absenceList <- vector('list', length = 5)
  
  for(i in 1:5){
    outList[[i]]<- maxentEvaluateWk(swd1, week1, flockSizeClass, kFold[i],
                                    beta.multiplier)
    aucList[[i]] <- outList[[i]]$modelEvaluation@auc
    corList[[i]] <- outList[[i]]$modelEvaluation@cor
    presenceList[[i]] <- data.frame(pa = 1, 
                                    predicted = outList[[i]]$modelEvaluation@presence)
    absenceList[[i]] <- data.frame(pa = 0, 
                                   predicted = outList[[i]]$modelEvaluation@absence)
  }
  aucValues <- as.numeric(unlist(aucList))
  corValues <- as.numeric(unlist(corList))
  paFrame <- rbind(do.call('rbind', presenceList),
                   do.call('rbind', absenceList))
  return(list(modelOut = outList, aucValues = aucValues, corValues = corValues, paFrame = paFrame))
}

# test = maxentAcrossFoldsWk(swd1, week1, 'large', 1)


weeks <- seq(10,18, 2)
names(swdWkList) <- paste0('w',weeks)
modelsWkList <- vector('list',length = length(weeks))
beta.multiplier <- 1

for(i in 1:length(weeks)){
  flockSizes <- c('small', 'medium','large')
  modelsFSlist <- vector('list',length = length(weeks))
  names(modelsFSlist) <- flockSizes
  for(j in 1:length(flockSizes)){
    modelsWkList[[i]][[j]] <- maxentAcrossFoldsWk(swdWeek[[paste0('w', weeks[i])]],
                                                  weeks[i],
                                                  flockSizes[j],
                                                  beta.multiplier
    )
  }
}

# length(modelsWkList)

# modelsWkList[[2]][[3]]$modelOut  %>% names

# Across data (all), flock sizes:
# 
# observationClass = 'all'
# beta.multiplier = 0
# 
# allIndOut <- maxentAcrossFoldsWk(observationClass, 'ind', beta.multiplier)
# allSfOut <- maxentAcrossFoldsWk(observationClass, 'sf', beta.multiplier)
# allLfOut <- maxentAcrossFoldsWk(observationClass, 'lf', beta.multiplier)

# Functions to extract variable contribution and lambda data:

se <- function(x) sd(x)/sqrt(length(x))

readLambdaFile <- function(model){
  lambdaData <- model@lambdas[1:15]
  tf <- tempfile()
  writeLines(lambdaData, tf)
  read.csv(tf, fill = TRUE, header = F) %>%
    dplyr::select(variable = V1, lambda = V2)
}

makeModelList <- function(weekNumber, flockSizeNumber){
  outList <- vector('list', length = 5)
  for(k in 1:5){
    outList[[k]] <- modelsWkList[[weekNumber]][[flockSizeNumber]]$modelOut[[k]]$maxentModel
  }
  return(outList)
}

getPredictedArea <- function(week, flockSize, k){
  results <- modelsWkList[[week]][[flockSize]]$modelOut[[k]]$maxentModel@results
  results[65,]
}

meanSePredictedArea <- function(week, flockSize){
  v <- numeric()
  for(i in 1:5){
    v[i] <- getPredictedArea(week, flockSize, i)
  }
  data.frame(period = week, flock = flockSize, mean = mean(v), se = se(v)) %>%
    mutate(
      flock = ifelse(flock == 1, 'small',
                     ifelse(flock == 2, 'medium', 'large'))
    )
}

predArea1 <- function(week,flockSize){
  outMat <- matrix(nrow = 5, ncol = 3)
  for(i in 1:5){
    outMat[i,3] <- getPredictedArea(week, flockSize, i)
  }
  colnames(outMat) <- c('period', 'flock', 'area')
  outMat %>% data.frame %>%
    mutate(period = week,
           flock = flockSize,
           flock = ifelse(flock == 1, 'small',
                          ifelse(flock == 2, 'medium', 'large')))
}

predArea2 <-  function(flockSize){
  outList <- vector('list', length = 5)
  for(i in 1:5){
    outList[[i]] <- predArea1(i, flockSize)
  }
  bind_rows(outList)
}

predArea3 <- bind_rows(
  predArea2(1),
  predArea2(2),
  predArea2(3)
)

meanSePredictedFlockFrame <- function(flockSize){
  outList <- vector('list', length = 5)
  for(i in 1:5){
    outList[[i]] <- meanSePredictedArea(i, flockSize)
  }
  bind_rows(outList)
}

predictedArea <- bind_rows(
  meanSePredictedFlockFrame(1),
  meanSePredictedFlockFrame(2),
  meanSePredictedFlockFrame(3)
) %>%
  mutate(bigC = se * 1.96,
         lower = mean - bigC,
         upper = mean + bigC)



limits <- aes(ymax = mean + bigC, ymin=mean - bigC)

predictedArea$flock <- factor(predictedArea$flock, levels = c('small', 'medium','large'))


for(i in 1:5){
  ggplot(
    predictedArea %>%
      filter(period == i),
    aes(x = flock, y  = mean)) +
    geom_point(size = 5.5) +
    geom_errorbar(limits, width = 0, size = 1.5) +
    ylim(.1, .6) +
    plot_theme() + 
    ylab('Fractional predicted area') +
    xlab('Flock size class') +
    theme(legend.title = element_blank(),
          legend.key.height = unit(1,"line"),
          legend.key = element_rect(size=2, color = 'white'),
          axis.title = element_text(size = rel(2)),
          axis.text.x = element_text(size = rel(2)),
          # legend.margin = unit(0, 'line'),
          # legend.key.size = unit(2, 'lines'),
          legend.position = 'top')
  
  ggsave(paste0('outPlots/predictedArea', i, '.png'),
         width = 6.5, height = 4.5, units = 'in')
}
dev.off()




makelambdaFrame <- function(modelList){
  
  dfContributionMat <- matrix(nrow = 15, ncol = 5)
  lambdaMat <- matrix(nrow = 15, ncol = 5)
  
  for(i in 1:ncol(lambdaMat)){
    model <- modelList[[i]]
    lambdaMat[,i] <- readLambdaFile(model)[,2]
    dfContributionMat[,i] <- model@results[7:21]
  }
  
  lambdaSummary <- data.frame(
    variable = as.character(readLambdaFile(modelList[[1]])$variable), 
    meanLambda = apply(lambdaMat, 1, mean),
    seLambda = apply(lambdaMat, 1, se))
  
  variableContributionSummary <- data.frame(
    variable = as.character(readLambdaFile(modelList[[1]])$variable), 
    meanContribution = apply(dfContributionMat, 1, mean),
    seContribution = apply(dfContributionMat, 1, se))
  
  outFrame <- plyr::join(variableContributionSummary, lambdaSummary) %>%
    arrange(desc(meanContribution))
  
  return(outFrame)
}

lambdaFrameS1 <- makelambdaFrame(makeModelList(1, 1))
lambdaFrameS2 <- makelambdaFrame(makeModelList(2, 1))
lambdaFrameS3 <- makelambdaFrame(makeModelList(3, 1))
lambdaFrameS4 <- makelambdaFrame(makeModelList(4, 1))
lambdaFrameS5 <- makelambdaFrame(makeModelList(5, 1))


lambdaFrameM1 <- makelambdaFrame(makeModelList(1, 2))
lambdaFrameM2 <- makelambdaFrame(makeModelList(2, 2))
lambdaFrameM3 <- makelambdaFrame(makeModelList(3, 2))
lambdaFrameM4 <- makelambdaFrame(makeModelList(4, 2))
lambdaFrameM5 <- makelambdaFrame(makeModelList(5, 2))

lambdaFrameL1 <- makelambdaFrame(makeModelList(1, 3))
lambdaFrameL2 <- makelambdaFrame(makeModelList(2, 3))
lambdaFrameL3 <- makelambdaFrame(makeModelList(3, 3))
lambdaFrameL4 <- makelambdaFrame(makeModelList(4, 3))
lambdaFrameL5 <- makelambdaFrame(makeModelList(5, 3))


envList <- loadEnvList('D:/rubl_summer_2016/lc_asc')



prob.r.stack = function(model, week){
  newEnvList <- envList
  newEnvList[14:15] <-rasterWeekList[[week]]
  r0 <- brick(newEnvList) #%>% brick
  names(r0)[14:15] <- c('tmin','ppt')
  predictionList <- vector('list', length = 5)
  for (i in 1:5){
    predictionList[[i]] = predict(model[[i]],r0,
                                  args=c(paste('outputformat=',outformat = 'logistic', sep = '')), 
                                  progress='text')
  }
  return(mean(stack(predictionList)))
}

prob.r.stack(makeModelList(1,1), 1)


rS1 <- prob.r.stack(makeModelList(1, 1),1)
rS2 <- prob.r.stack(makeModelList(2, 1),2)
rS3 <- prob.r.stack(makeModelList(3, 1),3)
rS4 <- prob.r.stack(makeModelList(4, 1),4)
rS5 <- prob.r.stack(makeModelList(5, 1),5)


rM1 <- prob.r.stack(makeModelList(1, 2),1)
rM2 <- prob.r.stack(makeModelList(2, 2),2)
rM3 <- prob.r.stack(makeModelList(3, 2),3)
rM4 <- prob.r.stack(makeModelList(4, 2),4)
rM5 <- prob.r.stack(makeModelList(5, 2),5)

rL1 <- prob.r.stack(makeModelList(1, 3),1)
rL2 <- prob.r.stack(makeModelList(2, 3),2)
rL3 <- prob.r.stack(makeModelList(3, 3),3)
rL4 <- prob.r.stack(makeModelList(4, 3),4)
rL5 <- prob.r.stack(makeModelList(5, 3),5)


#-------------------------------------------------------------------------------*
# Niche equivalency:
#-------------------------------------------------------------------------------*

flockData <- swdRUBL[[1]] 

# Generate a random sample of species 1 by species 2
# Note: Species 1 is the species of interest, comparison to species 2

random.swd.pair = function(sp1, sp2){
  # Remove unevaluated flock
  flockData <- flockData %>%
    dplyr::filter(sp == sp1|sp == sp2)
  nSp1 <- nrow(dplyr::filter(flockData, sp == sp1))
  # Determine the sample size as the proportion of species 1:
  prob.s1 = nSp1/nrow(flockData)
  # Generate random value of 1 or 0 with the probability of obtaining
  # a 1 related to the # of s1 observations:
  flockData %>%
    mutate(s.rand = rbinom(nrow(.),1,prob.s1)) %>%
    dplyr::filter(s.rand == 1) %>%
    dplyr::mutate(sp = 1) %>%
    dplyr::select(-s.rand)
}

# Function to run maxent for a given dataset:

maxentRunRawPlot = function(inFlockData, beta.multiplier = 0){
  # Model input:
  swdFlock <- inFlockData
  max.in <- rbind(swdFlock, swdBG) %>%
    dplyr::select(-c(lat, lon))
  # Set model arguments
  beta.r <- str_c('betamultiplier=', beta.multiplier)
  mod.args <- c('nothreshold', 'nohinge', 'noproduct',#'noquadratic',
                beta.r, 'addallsamplestobackground',
                'writebackgroundpredictions','writeplotdata',
                'noautofeature','nooutputgrids',
                'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
  maxentModel <- maxent(max.in[,-1], max.in[,1], args = mod.args)
  return(predict(maxentModel, env.stack, args = c('outputformat=logistic')))
}

# Run models of empirical data:

lf <- maxentRunRawPlot(dplyr::filter(flockData, sp == 'lf') %>%
                         mutate(sp = 1))
sf <- maxentRunRawPlot(dplyr::filter(flockData, sp == 'sf') %>%
                         mutate(sp = 1))
ind <- maxentRunRawPlot(dplyr::filter(flockData, sp == 'ind') %>%
                          mutate(sp = 1))

# Run null models for flock size pairs
# Note: Each of the null surface lists are 382.5 Mb!
# 
# n.lf.sf <- list(length = 1000)
# n.sf.lf <- list(length = 1000)
# n.lf.ind <- list(length = 1000)
# n.ind.lf <- list(length = 1000)
# n.sf.ind <- list(length = 1000)
# n.ind.sf <- list(length = 1000)
# 
# for(i in 1:1000) n.sf.ind[[i]] <- maxentRunRawPlot(random.swd.pair('sf', 'ind'))
# for(i in 1:1000) n.ind.sf[[i]] <- maxentRunRawPlot(random.swd.pair('ind', 'sf'))
# for(i in 1:1000) n.lf.ind[[i]] <- maxentRunRawPlot(random.swd.pair('lf', 'ind'))
# for(i in 1:1000) n.ind.lf[[i]] <- maxentRunRawPlot(random.swd.pair('ind', 'lf'))
# for(i in 1:1000) n.lf.sf[[i]] <- maxentRunRawPlot(random.swd.pair('lf', 'sf'))
# for(i in 1:1000) n.sf.lf[[i]] <- maxentRunRawPlot(random.swd.pair('sf', 'lf'))


#-------------------------------------------------------------------------------
# Function to calculate modified Hellinger similarities for a given model run
#-------------------------------------------------------------------------------

I.dist = function(p.x, p.y){
  # Convert the rasters to SpatialGridDataFrame format:
  p.x = as(p.x, 'SpatialGridDataFrame')
  p.y = as(p.y, 'SpatialGridDataFrame')
  # Make a list of the probability surfaces:
  # p.list = list(p.x,p.y)
  # Calculate the modified-Hellinger similarity (Warren 2008, pg. 2870)
  # niche.overlap(p.list)[2,1]
  niche.overlap(list(p.y, p.x))
}

#-------------------------------------------------------------------------------
# Function to run niche equivalency analyses on two flock size classes
#-------------------------------------------------------------------------------
# This function generates a list of two elements, slot1 contains the modified-H
# distance (I) of the actual data and slot2 contains the modified-H of the
# null distribution.

run.nea <- function(sp1, sp2, iterations){ #, null.xy, null.yx){
  I.actual <- I.dist(
    maxentRunRawPlot(dplyr::filter(flockData, sp == sp1) %>%
                       mutate(sp = 1)),
    maxentRunRawPlot(dplyr::filter(flockData, sp == sp2) %>%
                       mutate(sp = 1))
  )[2]
  I.null <- rep(NA, iterations)
  for(i in 1:iterations){
    I.null[i] <- I.dist(
      maxentRunRawPlot(random.swd.pair(sp1, sp2)),
      maxentRunRawPlot(random.swd.pair(sp2, sp1))
    )[2]
  }
  nea.list <- list(I.actual, I.null)
  names(nea.list) <- c('I.actual','I.null')
  return(nea.list)
}

#-------------------------------------------------------------------------------
# Run niche equivalency analyses
#-------------------------------------------------------------------------------

# Large flock vs. small flock:

I.lf.sf <- run.nea('lf','sf',1000)

# Large flock vs. individual sightings (<20 individuals):

I.lf.ind <- run.nea('lf','ind',1000)

# Small flock vs. individual sightings (<20 individuals):

I.sf.ind <- run.nea('sf','ind',1000)

#-------------------------------------------------------------------------------
# Stats for niche equivalency analyses
#-------------------------------------------------------------------------------
# Statistic pairwise comparisons of null and actual modified-Hellinger 
# similarities for large vs. small flocks, large vs. individual sightings and 
# small flocks vs.individual sightings. 

out.stats = function(I.sp1.sp2){
  I.actual = I.sp1.sp2$I.actual
  I.null = I.sp1.sp2$I.null
  I.null.mean = mean(I.null)
  I.null.se = se(I.null)
  I.null.05 = quantile(I.null, probs = 0.05)
  I.null.10 = quantile(I.null, probs = 0.1)
  p = ecdf(I.null)(I.actual)
  l = list(I.actual, I.null,I.null.mean, I.null.se, I.null.05, 
           I.null.10, p)
  names(l) = c('I.actual','I.null', 'I.null.mean', 'I.null.se',
               'I.null.05','I.null.10','p') ; l
}

# LARGE FLOCK VS. SMALL FLOCK:

outStats_I.lf.sf <- out.stats(I.lf.sf)

# LARGE FLOCK VS. INDIVIDUALS:

outStats_I.lf.ind <- out.stats(I.lf.ind)

# SMALL FLOCK VS. INDIVIDUAL:

outStats_I.sf.ind <- out.stats(I.sf.ind)

#-------------------------------------------------------------------------------
# Plot the histograms
#-------------------------------------------------------------------------------
# Histograms compare the density distribution of the modified-Hellinger 
# similarities of the null models against the distance of the actual model for
# large vs. small flocks, large vs. individual sightings and small flocks vs. 
# individual sightings.

# Histogram function:

hist.mhd = function(I.sp1.sp2,main.label,out.name, leg){
  null.dist = out.stats(I.sp1.sp2)[[2]]
  emp.dist = out.stats(I.sp1.sp2)[[1]]
  plot.new()
  # setwd('C:/Users/Brian/Dropbox/rubl_12_15/scratch_out')
  # jpeg(out.name, 1200,1250, res = 300)
  hist(null.dist, breaks = seq(0,1, by = .005), freq = F,
       xlim = c(0.85,1), ylim = c(0,110),
       col = 'gray80', cex.lab = .9,
       main = '', ylab = '',xlab='',
       axes =F)
  axis(1, line = -0.4)
  axis(2, line = 0)
  title(line = 2.5, xlab = 'Modified-Hellinger similarity (I)', ylab = 'Density')
  title(main = main.label, line = .5, cex.main = 1)
  lines(c(emp.dist, emp.dist), c(0,100), lwd = 2, lty = 2)
  if (leg == T)
    legend(.855,90,'Null','gray80',1, bty = 'n',
           x.intersp = .95, cex = .9)
  if (leg == T)
    legend(.85,100,'Actual',lty = 2,lwd = 2, bty = 'n', 
           x.intersp = .4, cex = .9)
  # dev.off()
}

# Make plots:

library(ggplot2)

data.frame(I = I.lf.sf$I.null) %>%
  tbl_df %>%
  ggplot(aes(I)) +
  geom_density(fill = 'gray90') +
  scale_x_continuous(limits = c(.9, 1)) +
  ylab('Density')+
  xlab ('Modified-Hellinger similarity (I)') +
  theme_bw() +
  theme(axis.title.y = element_text(size = rel(1.5), vjust = .9),
        axis.title.x = element_text(size = rel(1.5), vjust = -0.4)) +
  # geom_vline(xintercept = I.lf.sf$I.actual, linewidth = 2.5, linetype = 'longdash')
  geom_segment(data = data.frame(I = I.lf.sf$I.actual),
               aes(x = I, y = 0, xend = I, yend = Inf))#,
# linewidth = 2.5, linetype = 'longdash')

data.frame(I = I.lf.ind$I.null) %>%
  tbl_df %>%
  ggplot(aes(I)) +
  geom_density(fill = 'gray90') +
  scale_x_continuous(limits = c(.9, 1)) +
  ylab('Density')+
  xlab ('Modified-Hellinger similarity (I)') +
  theme_bw() +
  # geom_vline(xintercept = I.lf.sf$I.actual, linewidth = 2.5, linetype = 'longdash')
  geom_segment(data = data.frame(I = I.lf.ind$I.actual),
               aes(x = I, y = 0, xend = I, yend = Inf))#,
#linewidth = 2.5, linetype = 'longdash')


data.frame(I = I.sf.ind$I.null) %>%
  tbl_df %>%
  ggplot(aes(I)) +
  geom_density(fill = 'gray90') +
  scale_x_continuous(limits = c(.9, 1)) +
  ylab('Density')+
  xlab ('Modified-Hellinger similarity (I)') +
  theme_bw() +
  # geom_vline(xintercept = I.lf.sf$I.actual, linewidth = 2.5, linetype = 'longdash')
  geom_segment(data = data.frame(I = I.sf.ind$I.actual),
               aes(x = I, y = 0, xend = I, yend = Inf))#,
#linewidth = 2.5, linetype = 'longdash')

hist.mhd(I.lf.sf, 'Large vs.medium flock sightings', 'mh_dist_lf_sf.jpg',T)

hist.mhd(I.lf.ind, 'Large flock vs. small flock sightings', 'mh_dist_lf_ind.jpg',F)

hist.mhd(I.sf.ind, 'Medium flock vs. small flock sightings', 'mh_dist_sf_ind.jpg',F)

#-------------------------------------------------------------------------------
# Predicted niche occupancy
#===============================================================================

#-------------------------------------------------------------------------------
# PNO functions:
#-------------------------------------------------------------------------------

# Calculate pno:

pno.df <- function(mod.x, mod.y, env.var){
  # Sum the raw probabilities about a given value of an environmental variable:
  pno.df <- data.frame(zonal(mod.x,env.stack[[env.var]],'sum', digits = 2))
  pno.df[,3] <- zonal(mod.y,env.stack[[env.var]],'sum', digits = 2)[,2]
  colnames(pno.df) <- c('env','pno.sp1','pno.sp2')
  pno.df
}


# Determine the modified-Hellinger similarity between two pnos:

pno.I <- function(mod.x, mod.y, env.var){
  df <- pno.df(mod.x, mod.y, env.var)
  # Calculate the modified-Hellinger similarity (I):
  niche.overlap(df)[2,1]
}


run.pno <- function(sp1, sp2, env.var, iterations){
  mod.x <- maxentRunRawPlot(dplyr::filter(flockData, sp == sp1) %>%
                              mutate(sp = 1))
  mod.y <- maxentRunRawPlot(dplyr::filter(flockData, sp == sp2) %>%
                              mutate(sp = 1))
  pno.actual <- pno.df(mod.x, mod.y, env.var)
  pno.I.actual <- pno.I(mod.x, mod.y, env.var)
  pno.I.null <- numeric()
  for (i in 1:iterations){
    null.xy <- maxentRunRawPlot(random.swd.pair(sp1, sp2))
    null.yx <- maxentRunRawPlot(random.swd.pair(sp2, sp1))
    pno.I.null[i] = pno.I(null.xy,null.yx, env.var)
  }
  pno.list <- list(pno.I.actual, pno.I.null, pno.actual) # pno.actual, 
  names(pno.list) = c('pno.I.actual','pno.I.null','pno.actual')
  return(pno.list)
}

# Run pno models, returns a list of the
# actual pno-I (one value) and a vector of 100 null pno-I's:

# run.pno = function(mod.x, mod.y, null.xy, null.yx, env.var){
#   pno.actual = pno.df(mod.x, mod.y, env.var)
#   pno.I.actual = pno.I(mod.x, mod.y, env.var)
#   pno.I.null = numeric()
#   for (i in 1:100){
#     pno.I.null[i] = pno.I(null.xy[[i]],null.yx[[i]], env.var)
#   }
#   pno.list = list(pno.I.actual, pno.I.null, pno.actual) # pno.actual, 
#   names(pno.list) = c('pno.I.actual','pno.I.null','pno.actual')
#   return(pno.list)
# }

#-------------------------------------------------------------------------------
# Run PNO
#-------------------------------------------------------------------------------
# THIS TAKES A VERY LONG TIME TO RUN!

pno.lf.sf <- vector('list', length = 15)
for(j in 1:15){
  pno.lf.sf[[j]] <- run.pno('lf', 'sf', j, 100)
}

pno.lf.ind <- vector('list', length = 15)
for(j in 1:15){
  pno.lf.ind[[j]] <- run.pno('lf', 'ind', j, 100)
}

pno.sf.ind <- vector('list', length = 15)
for(j in 1:15){
  pno.sf.ind[[j]] <- run.pno('sf', 'ind', j, 100)
}


# 
# pno.lf.sf = list()
# for(i in 1:15){
#   pno.lf.sf[[i]] = run.pno(lf, sf, n.lf.sf, n.sf.lf,i)
# }
# 
# pno.lf.ind = list()
# for(i in 1:15){
#   pno.lf.ind[[i]] = run.pno(lf, ind, n.lf.ind, n.ind.lf,i)
# }
# 
# pno.sf.ind = list()
# for(i in 1:15){
#   pno.sf.ind[[i]] = run.pno(sf, ind, n.sf.ind, n.ind.sf,i)
# }

names(pno.lf.sf) = names(env.stack)
names(pno.lf.ind) = names(env.stack)
names(pno.sf.ind) = names(env.stack)


saveRDS(pno.lf.sf, 'pno.lf.sf.rds')
saveRDS(pno.lf.ind, 'pno.lf.ind.rds')
saveRDS(pno.sf.ind, 'pno.sf.ind.rds')

pno.lf.sf <- readRDS('C:/Users/Brian/Desktop/gits/RUBL/output/modifiedHellingerDistance/pno.lf.sf.rds')
pno.lf.ind <- readRDS('C:/Users/Brian/Desktop/gits/RUBL/output/modifiedHellingerDistance/pno.lf.ind.rds')
pno.sf.ind <- readRDS('C:/Users/Brian/Desktop/gits/RUBL/output/modifiedHellingerDistance/pno.sf.ind.rds')

#-------------------------------------------------------------------------------
# Stat output of pno
#-------------------------------------------------------------------------------

pno.stats = function(pno.sp1.sp2.env){
  I.actual = pno.sp1.sp2.env$pno.I.actual
  I.null = pno.sp1.sp2.env$pno.I.null
  I.null.mean = mean(I.null)
  I.null.se = se(I.null)
  I.null.05 = quantile(I.null, probs = 0.05)
  I.null.10 = quantile(I.null, probs = 0.1)
  p = ecdf(I.null)(I.actual)
  l = list(I.actual, I.null,I.null.mean, I.null.se, I.null.05, 
           I.null.10, p)
  names(l) = c('I.actual','I.null', 'I.null.mean', 'I.null.se',
               'I.null.05','I.null.10','p') ; l
}

out.pno.stats = function(pno.sp1.sp2){
  pno.out = list()
  for (i in 1:15){
    pno.out[[i]] = pno.stats(pno.sp1.sp2[[i]])
  }
  names(pno.out) = names(env.stack)
  pno.out
}

out.pno.lf.sf = out.pno.stats(pno.lf.sf)
out.pno.lf.ind = out.pno.stats(pno.lf.ind)
out.pno.sf.ind = out.pno.stats(pno.sf.ind)

#-------------------------------------------------------------------------------
# Plotting PNO
#-------------------------------------------------------------------------------

pno.lf.ind[['dev_hi']][[1]][,3]

names(env.stack)[env]

plot.pno = function(env) {
  df = pno.lf.sf[[env]][[3]]
  ind = pno.lf.ind[[env]][[3]][,3]
  env = df[,1]
  lf = df[,2]
  sf = df[,3]
  plot(lf~env, type = 'l', xlim = c(0,1), lwd =2, bty ='l',
       main = names(env.stack[[env]]), ylab = 'PNO')
  lines(env, sf, lty = 2, lwd = 2)
  lines(env, ind, lty = 3, lwd = 2)
}

plot.pno(3)


plot.pno = function(env) {
  df = pno.lf.sf[[env]][[3]]
  ind = pno.lf.ind[[env]][[3]][,3]
  lc = df[,1]
  lf = df[,2]
  sf = df[,3]
  plot(lf~lc, type = 'l', xlim = c(0,1), lwd =2, bty ='l',
       main = names(env.stack)[env], ylab = 'PNO')
  lines(lc, sf, lty = 2, lwd = 2)
  lines(lc, ind, lty = 3, lwd = 2)
}

plot.pno(3)

#########################################################################################################
#########################################################################################################
#########################################################################################################
# Compare distributions

spFrameWithBG <- function(week){
  bind_rows(
    swdWeek[[week]][[1]] %>% filter(sp == 0) %>% mutate(sp = 'bg'),
    swdWeek[[week]][[1]] %>% filter(sp != 0) %>% mutate(sp = 'small'),
    swdWeek[[week]][[2]] %>% filter(sp != 0) %>% mutate(sp = 'medium'),
    swdWeek[[week]][[3]] %>% filter(sp != 0) %>% mutate(sp = 'large')
  ) %>%
    tbl_df %>%
    mutate(sp = factor(sp))
}
spFrameWithBG(1)


library(ggplot2)

# ptsENV <- swdBG %>%
#   dplyr::mutate(sp = 'bg', observationType = 'ebird') %>%
#   dplyr::select(sp, observationType, lon:woodland) %>%
#   dplyr::bind_rows(swdRUBL$eb %>%
#                      dplyr::mutate(observationType = 'ebird') %>%
#                      dplyr::select(sp, observationType, lon:woodland)) %>%
#   dplyr::bind_rows(swdRUBL$bz %>%
#                      dplyr::mutate(observationType = 'blitz') %>%
#                      dplyr::select(sp, observationType, lon:woodland)) %>%
#   dplyr::mutate(sp = factor(sp))
# 
# ptsENV$sp <- factor(sp, levels =  'bg','ind','sf', 'lf')


modeFun <- function(variable, rounding){
  d <- density(plyr::round_any(variable, rounding))
  dFrame <- data.frame(d$x, d$y)
  names(dFrame) <- c('x','y')
  dFrame %>% dplyr::filter(y == max(y)) %>%
    dplyr::select(x)
}

summarizeByFlockSize <- function(ptsENV, variable, rounding){
  outTable <- ptsENV %>%
    dplyr::select(sp, #observationType, 
                  variable = matches(variable)) %>%
    dplyr::group_by(sp) %>%
    dplyr::summarize(min = min(variable),
                     max = max(variable),
                     mean = mean(variable),
                     variance = var(variable),
                     IQR = IQR(variable))
  if(variable == 'ppt' | variable == 'tmin'){
    outTable <- ptsENV %>%
      dplyr::select(sp, #observationType, 
                    variable = matches(variable)) %>%
      dplyr::group_by(sp) %>%
      dplyr::summarize(min = min(variable),
                       max = max(variable),
                       mean = mean(variable),
                       variance = var(variable),
                       IQR = IQR(variable),
                       mode = as.numeric(modeFun(variable, 0.5)))
  }
  return(outTable)
}

summarizeByFlockSize(spFrameWithBG(1),'tmin', 0.5)

ksTest <- function(ptsENV, variable, sp1, sp2){
  v1 <- ptsENV %>% 
    dplyr::filter(sp == sp1) %>%
    collect %>%
    .[[variable]]
  v2 <- ptsENV %>% 
    dplyr::filter(sp == sp2) %>%
    collect %>%
    .[[variable]]
  ks.test(v1, v2)
}

ksFrame1 <- function(envVar, flockSize1, flockSize2){
  outMat <- matrix(nrow = 5, ncol = 6)
  colnames(outMat) <- c('env', 'fs1', 'fs2', 'period', 'D', 'p')
  for(i in 1:5){
    testOut <- ksTest(spFrameWithBG(i),envVar,flockSize1, flockSize2)
    outMat[i,4] <- i
    outMat[i,5] <- testOut$statistic
    outMat[i,6] <- testOut$p.value
  }
  outFrame <- outMat %>% data.frame 
  outFrame %>%
    mutate(env = envVar, fs1 = flockSize1, fs2 = flockSize2)
}

ksFrame1('tmin', 'small', 'large')

ksFrame1('flood', 'small', 'large')

ksFrame1('wetw', 'small', 'large')

ksFrame1('weth', 'small', 'large')

ksFrame1('forh', 'small', 'large')



ksTest(spFrameWithBG(1),'tmin','small','large')
ksTest(spFrameWithBG(1),'tmin','small','medium')
ksTest(spFrameWithBG(1),'tmin','medium','large')


ksTest(spFrameWithBG(2),'tmin','small','large')
ksTest(spFrameWithBG(2),'tmin','small','medium')
ksTest(spFrameWithBG(2),'tmin','medium','large')

ksTest(spFrameWithBG(3),'tmin','small','large')
ksTest(spFrameWithBG(3),'tmin','small','medium')
ksTest(spFrameWithBG(3),'tmin','medium','large')

ksTest(spFrameWithBG(4),'tmin','small','large')
ksTest(spFrameWithBG(4),'tmin','small','medium')
ksTest(spFrameWithBG(4),'tmin','medium','large')

ksTest(spFrameWithBG(5),'tmin','small','large')
ksTest(spFrameWithBG(5),'tmin','small','medium')
ksTest(spFrameWithBG(5),'tmin','medium','large')

ksTest(spFrameWithBG(1),'tmin','small','large')
ksTest(spFrameWithBG(1),'tmin','small','medium')
ksTest(spFrameWithBG(1),'tmin','medium','large')


ksTest(spFrameWithBG(2),'flood','small','large')
ksTest(spFrameWithBG(2),'flood','small','medium')
ksTest(spFrameWithBG(2),'flood','medium','large')

ksTest(spFrameWithBG(3),'flood','small','large')
ksTest(spFrameWithBG(3),'flood','small','medium')
ksTest(spFrameWithBG(3),'flood','medium','large')

ksTest(spFrameWithBG(4),'flood','small','large')
ksTest(spFrameWithBG(4),'flood','small','medium')
ksTest(spFrameWithBG(4),'flood','medium','large')

ksTest(spFrameWithBG(5),'flood','small','large')
ksTest(spFrameWithBG(5),'flood','small','medium')
ksTest(spFrameWithBG(5),'flood','medium','large')

ksTest(spFrameWithBG(2),'dev_hi','small','large')
ksTest(spFrameWithBG(2),'dev_hi','small','medium')
ksTest(spFrameWithBG(2),'dev_hi','medium','large')

ksTest(spFrameWithBG(3),'dev_hi','small','large')
ksTest(spFrameWithBG(3),'dev_hi','small','medium')
ksTest(spFrameWithBG(3),'dev_hi','medium','large')

ksTest(spFrameWithBG(4),'dev_hi','small','large')
ksTest(spFrameWithBG(4),'dev_hi','small','medium')
ksTest(spFrameWithBG(4),'dev_hi','medium','large')

ksTest(spFrameWithBG(5),'dev_hi','small','large')
ksTest(spFrameWithBG(5),'dev_hi','small','medium')
ksTest(spFrameWithBG(5),'dev_hi','medium','large')


ksTest('dev_hi','sf','ind')

ksTest('dev_li','sf','lf')

ksTest('flood','ind','bg')

ksTest('flood','lf','ind')

ksTest('flood','lf','sf')



ggplot(spFrameWithBG(5), 
       aes(x = flood, fill = sp)) +
  geom_histogram(aes(y=0.5*..density..),
                 position = 'identity', binwidth=0.1) +
  facet_wrap(~sp,nrow=3) +
  theme(aspect.ratio = 1) +
  theme_bw()

ggplot(spFrameWithBG(1) %>%
         filter(sp != 'bg'), 
       aes(sp, #factor(sp,levels = c('bg','ind','sf','lf')),
           tmin, fill = sp)) +
  geom_violin(adjust = 0.5) +
  xlab('Point data class') +
  #ylab('Environmental variable') +
  coord_flip() +
  theme_bw()

mWindow <- function(ptsEnv, envVar, windowSize){
  envVar1 <- ptsEnv %>%
    select_(envVar) %>%
    unlist
  groupedPts <- ptsEnv %>%
    mutate(env = plyr::round_any(envVar1, windowSize, floor)) %>%
    select(sp, env) %>%
    group_by(sp) %>%
    mutate(tSamples = n()) %>%
    group_by(sp, env) %>%
    summarize(
      tSamples = unique(tSamples),
      count = n()) %>%
    ungroup %>%
    mutate(prop = count/tSamples)
  gp1 <- bind_rows(
    left_join(
      groupedPts %>%
        ungroup %>%
        filter(sp == 'bg') %>%
        dplyr::select(-c(sp, tSamples, count)) %>%
        rename(bgProp = prop),
      groupedPts %>%
        mutate(sp = as.character(sp)) %>%
        filter(sp == 'small') %>%
        dplyr::select(-c(tSamples, count)),
      by = 'env') %>% 
      mutate(
        sp = ifelse(is.na(sp), 'small', sp),
        prop = ifelse(is.na(prop), 0 , prop)
      ) %>%
      mutate(pe = prop/bgProp),
    left_join(
      groupedPts %>%
        ungroup %>%
        filter(sp == 'bg') %>%
        dplyr::select(-c(sp, tSamples, count)) %>%
        rename(bgProp = prop),
      groupedPts %>%
        mutate(sp = as.character(sp)) %>%
        filter(sp == 'medium') %>%
        dplyr::select(-c(tSamples, count)),
      by = 'env') %>% 
      mutate(
        sp = ifelse(is.na(sp), 'medium', sp),
        prop = ifelse(is.na(prop), 0 , prop)
      ) %>%
      mutate(pe = prop/bgProp),
    left_join(
      groupedPts %>%
        ungroup %>%
        filter(sp == 'bg') %>%
        dplyr::select(-c(sp, tSamples, count)) %>%
        rename(bgProp = prop),
      groupedPts %>%
        mutate(sp = as.character(sp)) %>%
        filter(sp == 'large') %>%
        dplyr::select(-c(tSamples, count)),
      by = 'env') %>% 
      mutate(
        sp = ifelse(is.na(sp), 'large', sp),
        prop = ifelse(is.na(prop), 0 , prop)
      ) %>%
      mutate(pe = prop/bgProp)
  )
  return(gp1)
}

envVar = 'tmin'
ptsEnv = spFrameWithBG(1)
windowSize = .05


caTools::runmean(inData$prop, 5)

mWindow(spFrameWithBG(1),'tmin',.5)

inData <- mWindow(spFrameWithBG(1),'tmin',.5)
plot(
  caTools::runmean(inData$pe, 10) ~ env,
  type = 'l',
  data = inData)

ggplot(mWindow(spFrameWithBG(2),'tmin',.25) %>%
         filter(env > -20) %>%
         mutate(pe = caTools::runmean(pe, 10)),
       aes(x = env, y = pe, group = sp, color = sp)) + 
  geom_line(size = .6) + theme_bw() +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylab('Predicted:Expected') +
  xlab('Average minimum temperature') +
  theme(legend.title = element_blank(),
        legend.key.height = unit(1,"line"),
        legend.key = element_rect(size=2, color = 'white'))


ggsave('outPlots/tmin.png', 
       width = 6.5, height = 4.5, units = 'in')

for(i in 1:5){
  Period <- i
  ggplot(
    mWindow(spFrameWithBG(Period),'tmin',.5) %>%
      filter(env > -20) %>%
      mutate(period = Period) %>%
      filter(period == Period) %>%
      mutate(pe = caTools::runmean(pe, 10)),
    aes(x = env, y = pe, group = sp, color = sp)) + 
    xlim(-20, 20) + ylim(0, 7.5) +
    geom_line(size = .6) + theme_bw() +
    geom_hline(aes(yintercept = 1), linetype = 2) +
    ylab('Predicted:Expected') +
    xlab('Average minimum temperature') +
    theme(legend.title = element_blank(),
          legend.key.height = unit(1,"line"),
          legend.key = element_rect(size=2, color = 'white'))
  ggsave(paste0('outPlots/tmin', i, '.png'),
         width = 6.5, height = 4.5, units = 'in')
}

dev.off()

ggplot(
  mWindow(spFrameWithBG(2),'tmin',.5) %>%
    filter(env > -20) %>%
    mutate(period = 2) %>%
    filter(period == 2) %>%
    mutate(pe = caTools::runmean(pe, 10)),
  aes(x = env, y = pe, group = sp, color = sp)) + 
  xlim(-20, 20) + ylim(0, 2.5) +
  geom_line(size = .6) + theme_bw() +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylab('Predicted:Expected') +
  xlab('Average minimum temperature') +
  theme(legend.title = element_blank(),
        legend.key.height = unit(1,"line"),
        legend.key = element_rect(size=2, color = 'white'))
ggsave(paste0('outPlots/tmin', 'example', '.png'),
       width = 6.5, height = 4.5, units = 'in')


ggsave('outPlots/tmin.png', 
       width = 6.5, height = 4.5, units = 'in')

ggplot(mWindow(spFrameWithBG(2),'flood',.25) %>%
         # filter(env > -20) %>%
         mutate(pe = caTools::runmean(pe, 10)),
       aes(x = env, y = pe, group = sp, color = sp)) + 
  geom_line(size = .6) + theme_bw() +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  ylab('Predicted:Expected') +
  xlab('Average minimum temperature') +
  theme(legend.title = element_blank(),
        legend.key.height = unit(1,"line"),
        legend.key = element_rect(size=2, color = 'white'))


ggsave('outPlots/tmin.png', 
       width = 6.5, height = 4.5, units = 'in')



# xlim(-20, 20)


ggplot(mWindow(spFrameWithBG(2),'wetw',.1),
       aes(x = env, y = pe, group = sp, color = sp)) + 
  geom_line(width = 2)

qFilter <- function(df, envVar, qCut){
  qVec <- c(0 + qCut, 1 - qCut) 
  df <- df %>% filter(sp != 'bg') %>%
    rename_(env = envVar) %>%
    select(sp, env)
  varExtract <- df$env
  q <- quantile(varExtract, qVec)
  df %>%
    filter(env > q[1] & env < q[2])
}

summaryFrame <- spFrameWithBG(1) %>%
  qFilter('tmin') %>%
  group_by(sp) %>%
  summarize(envM = mean(env),
            bigC = se(env) * 1.96)

limits <- aes(ymax = envM + bigC, ymin = envM - bigC)

ggplot(spFrameWithBG(5) %>%
         qFilter('flood', qCut = .05), 
       aes(sp, #factor(sp,levels = c('bg','ind','sf','lf')),
           env, fill = sp)) +
  geom_violin(adjust = 0.5) +
  geom_point(data = spFrameWithBG(5) %>%
               qFilter('flood', qCut = .05) %>%
               group_by(sp) %>%
               summarize(envM = mean(env),
                         bigC = se(env) * 1.96),
             aes(y = envM, s = sp),
             size = 6) +
  xlab('Flock size') + ylab('Proportion of floodplain forest') +
  #ylab('Environmental variable') +
  coord_flip() +
  theme_bw()


ggsave('outPlots/violinFlood5.png', 
       width = 6.5, height = 4.5, units = 'in')
dev.off()

ggplot(spFrameWithBG(5) %>%
         qFilter('tmin', qCut = .05), 
       aes(sp, #factor(sp,levels = c('bg','ind','sf','lf')),
           env, fill = sp)) +
  geom_violin(adjust = 0.5) +
  geom_point(data = spFrameWithBG(5) %>%
               qFilter('tmin', qCut = .05) %>%
               group_by(sp) %>%
               summarize(envM = mean(env),
                         bigC = se(env) * 1.96),
             aes(y = envM, s = sp),
             size = 6) +
  xlab('Flock size') + ylab('Average minimum temperature') +
  #ylab('Environmental variable') +
  coord_flip() +
  theme_bw()

ggsave('outPlots/violinTmin5.png', 
       width = 5, height = 3.46, units = 'in')
dev.off()

ggplot(spFrameWithBG(5) %>%
         qFilter('wetw', qCut = .05), 
       aes(sp, #factor(sp,levels = c('bg','ind','sf','lf')),
           env, fill = sp)) +
  geom_violin(adjust = 0.5) +
  geom_point(data = spFrameWithBG(5) %>%
               qFilter('wetw', qCut = .05) %>%
               group_by(sp) %>%
               summarize(envM = mean(env),
                         bigC = se(env) * 1.96),
             aes(y = envM, s = sp),
             size = 6) +
  xlab('Flock size') + ylab('Proportion of woody wetlands') +
  #ylab('Environmental variable') +
  coord_flip() +
  theme_bw()

ggsave('outPlots/violinWetw5.png', 
       width = 5, height = 3.46, units = 'in')
dev.off()

ggplot(spFrameWithBG(5) %>%
         qFilter('weth', qCut = .05), 
       aes(sp, #factor(sp,levels = c('bg','ind','sf','lf')),
           env, fill = sp)) +
  geom_violin(adjust = 0.5) +
  geom_point(data = spFrameWithBG(5) %>%
               qFilter('weth', qCut = .05) %>%
               group_by(sp) %>%
               summarize(envM = mean(env),
                         bigC = se(env) * 1.96),
             aes(y = envM, s = sp),
             size = 6) +
  xlab('Flock size') + ylab('Proportion of emergent wetlands') +
  #ylab('Environmental variable') +
  coord_flip() +
  theme_bw()

ggsave('outPlots/violinWeth5.png', 
       width = 5, height = 3.46, units = 'in')
dev.off()

ggplot(spFrameWithBG(4) %>%
         qFilter('forh', qCut = .05), 
       aes(sp, #factor(sp,levels = c('bg','ind','sf','lf')),
           env, fill = sp)) +
  geom_violin(adjust = 0.5) +
  geom_point(data = spFrameWithBG(4) %>%
               qFilter('forh', qCut = .05) %>%
               group_by(sp) %>%
               summarize(envM = mean(env),
                         bigC = se(env) * 1.96),
             aes(y = envM, s = sp),
             size = 6) +
  xlab('Flock size') + ylab('Proportion of hardwood forest') +
  #ylab('Environmental variable') +
  coord_flip() +
  theme_bw()

ggsave('outPlots/violinforh4.png', 
       width = 5, height = 3.46, units = 'in')
dev.off()

