

getRustyPaFrame(100,2009:2011) %>%
  mutate(year = factor(year))

maxentRun <- function(flockSizeMin,flockSizeMax, years, beta.multiplier){
  observationData <- getRustyPaFrame(flockSizeMin,flockSizeMax, years)
  pa <- observationData$pa
  env <- observationData %>%
    dplyr::select_('dev_hi', 'dev_li','flood', 'forh','form','grass',
                     'pasture','rowcrop','shrub','upfor',
                     'weth','wetw', 'woodland',
                     'ppt', 'tmin') #%>%
    #mutate(tmin2 = tmin^2)
  # Set model arguments
  beta.r <- str_c('betamultiplier=', beta.multiplier)
  mod.args <- c('nothreshold', 'nohinge', 'noproduct',#'noquadratic',
                beta.r, 'addallsamplestobackground',
                'writebackgroundpredictions','writeplotdata',
                'noautofeature','nooutputgrids',
                'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
  maxentModel <- dismo::maxent(env, pa, args = mod.args)
  return(list(swd = observationData, maxentModel = maxentModel))
}


t1 <- maxentRun(100,Inf, 2009, 3)
t2 <- maxentRun(100,Inf, 2010, 3)
t3 <- maxentRun(100,Inf, 2011, 3)


makeEnvStack <- function(year){
  ppt <- raster(paste0(pathToRasterData, 'climateRasters/ppt',year))
  tmin <- raster(paste0(pathToRasterData, 'climateRasters/tmin',year))
  outStack <- stack(
    dropLayer(rStack, c('ppt', 'tmin')),
    ppt,
    tmin)
  names(outStack)[14:15] <- c('ppt', 'tmin')
  return(outStack)
}

plot(
  predict(
  maxentRun(100, Inf, 2010, 3)$maxentModel,
  makeEnvStack(2010),
  args = c('outputformat=logistic')
  )
)

inMat <- matrix(nrow = 105, ncol = 2)
inMat[,1] <- 1:105
for(i in 1:nrow(inMat)){
  if(i < 6) inMat[i, 1] <- 1
  if(i > 5) inMat[i, 1] <- i -5
  if(i <= 105) inMat[i,2] <- i + 5
  if(i == 105) inMat[i,2] <- Inf
}

outMat <- matrix(nrow = 105, ncol = 3)

for(i in 1:105){
  inMinMax <- inMat[i,]
  outMat[i,1] <- maxentRun(inMinMax[1],inMinMax[2], 2009, 0)$maxentModel@results['Equal.training.sensitivity.and.specificity.area',]
  outMat[i,2] <- maxentRun(inMinMax[1],inMinMax[2], 2010, 0)$maxentModel@results['Equal.training.sensitivity.and.specificity.area',]
  outMat[i,3] <- maxentRun(inMinMax[1],inMinMax[2], 2011, 0)$maxentModel@results['Equal.training.sensitivity.and.specificity.area',]
}

outFrame <- outMat %>%
  as.data.frame

outFrame$x <- rowMeans(outFrame)

outFrame$flockSize <- 1:105
outFrame$sd <- apply(outMat, 1, sd)

outFrame$ci <- (outFrame$sd/sqrt(3))*1.96
outFrame$lcl <- ifelse(
  outFrame$x - outFrame$ci < 0, 0,
  outFrame$x - outFrame$ci)

library(ggplot2)
ggplot(outFrame %>% filter(flockSize <= 100), aes(x = flockSize, y = x)) +
  geom_errorbar(aes(ymin=lcl, ymax=x+ci), width = 0) +#, position=pd)
  geom_point() +
  theme_bw() +
  xlab('Minimum flock size') +
  ylab('Proportional predicted area') +
  geom_line(size = 1)

