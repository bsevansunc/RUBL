# Load environment data:

source('C:/Users/Brian/Desktop/gits/RUBL/load_env.R')

env.stack

# Find the SWD data (searches for and ID's all files that end in ".csv":

swdData <- list.files('C:/Users/Brian/Dropbox/rubl_12_15/swd',
                       pattern='\\.csv$', full=T)

# Add data to memory:

swdList <- lapply(swdData, read.csv)

names(swd.list) <- c('all','bg','bz','eb')

# Assign names to each of the files:

pts.all = swd.list[[1]]
pts.bg = swd.list[[2]]
pts.bz = swd.list[[3]]
pts.eb = swd.list[[4]]

# For background points, add a 'bg' for the species column:
library(dplyr)

swdList[[2]] <- swdList[[2]] %>%
  dplyr::mutate(sp = 'bg') %>%
  dplyr::select(sp, lon:woodland)
  

# Project swd data as points:

spatialPointList <- list(length = length(swdList))

for(i in 1:length(swdList)){
  spatialPointList[[i]] <- SpatialPoints(swdList[[i]][,c('lon','lat')],
                proj4string = CRS(projection(env.stack)))
}

names(spatialPointList) <- c('all','bg','bz','eb')

# Extract environmental data to points:

ptEnvList <- list(length = length(swdList))

for(i in 1:length(swdList)){
   envExtract<- as.data.frame(extract(env.stack, 
                                          spatialPointList[[i]]),
                                          na.rm = TRUE)
   ptEnvList[[i]] <- cbind(dplyr::select(swdList[[i]], sp:lat),
                           envExtract)
}

names(ptEnvList) <- c('all','bg','bz','eb')

swdRUBL <- list(ptEnvList[[1]],ptEnvList[[3]],ptEnvList[[4]])
  names(swdRUBL) <- c('all','bz','eb')
  
swdBG <- ptEnvList[[2]]
swdBG$sp <-0

# Function to prepare swd files:

prepSWD <- function(inData){
  swdRUBL <-inData
  swd <- list(length = 3)
  flockSizes = as.character(unique(swdRUBL[[1]]$sp))
  
  for(i in 1:length(swdRUBL)){
    swd[[i]] <- list(length = 3)
    for(j in 1:3){
      swdBG$k <- kfold(swdBG, k = 5)
      swdFS <- swdRUBL[[i]] %>%
        dplyr::filter(sp == flockSizes[j]) %>%
        dplyr::mutate(sp = 1)
        swdFS$k <- kfold(swdFS, k = 5)
      swd[[i]][[j]] <- rbind(swdFS,swdBG)
    }
    names(swd[[i]]) <- flockSizes
  }
  names(swd) <- c('all','bz','eb')
  return(swd)
}

swd <- prepSWD(swdRUBL)

# Run model for a given value of K, observationClass, and flock size class:

maxentRun = function(observationClass, flockSizeClass, kFold, beta.multiplier){
  # Create input file of k fold::
  max.in <- swd[[observationClass]][[flockSizeClass]] %>%
    dplyr::filter(k != kFold) %>%
    dplyr::select(-c(k, lon, lat))
  max.in <- data.frame(max.in)
  # Set model arguments
  beta.r = paste('betamultiplier=',beta.multiplier,sep = '')
  mod.args = c('nothreshold', 'nohinge', 'noproduct',#'noquadratic',
               beta.r, 'addallsamplestobackground',
               'writebackgroundpredictions','writeplotdata',
               'noautofeature','nooutputgrids',
               'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
  maxentModel <- maxent(max.in[,-1], max.in[,1], args = mod.args)
  return(list(swd = max.in, maxentModel = maxentModel))
}

maxentEvaluate <- function(observationClass, flockSizeClass, kFold, beta.multiplier){
  # Run model
  maxentModelOut <- maxentRun(observationClass, flockSizeClass, kFold, beta.multiplier)
  maxentModel <- maxentModelOut$maxentModel
  # Create test point files for evaluation
  swdTest <-   swd[[observationClass]][[flockSizeClass]] %>%
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

observationClass = 'all'
flockSizeClass = 'lf'
kFold = 3
beta.multiplier = 2

test <- maxentRun(observationClass, flockSizeClass, kFold, beta.multiplier)
test

mTest <- maxentEvaluate(observationClass, flockSizeClass, kFold, beta.multiplier)

# Function to run across folds:

maxentAcrossFolds <- function(observationClass, flockSizeClass, beta.multiplier){
  kFold <- 1:5
  outList <- list(length = 5)
  aucList <- list(length = 5)
  corList <- list(length = 5)
  presenceList <- list(length = 5)
  absenceList <- list(length = 5)
    
  for(i in 1:5){
    outList[[i]]<- maxentEvaluate(observationClass, flockSizeClass, kFold[i],
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

# Across data (all), flock sizes:

observationClass = 'all'
beta.multiplier = 5

allIndOut <- maxentAcrossFolds(observationClass, 'ind', beta.multiplier)
allSfOut <- maxentAcrossFolds(observationClass, 'sf', beta.multiplier)
allLfOut <- maxentAcrossFolds(observationClass, 'lf', beta.multiplier)

# Functions to extract variable contribution and lambda data:

se <- function(x) sd(x)/sqrt(length(x))

readLambdaFile <- function(model, i){
  lambdaData <- model[[1]][[i]][[1]]@lambdas[1:15]
  tf <- tempfile()
  writeLines(lambdaData, tf)
  read.csv(tf, fill = TRUE, header = F) %>%
    dplyr::select(variable = V1, lambda = V2)
}

makeLambdaContributionFrame <- function(model){
  dfContributionMat <- matrix(nrow = 15, ncol = 5)
  lambdaMat <- matrix(nrow = 15, ncol = 5)

  for(i in 1:ncol(lambdaMat)){
    lambdaMat[,i] <- readLambdaFile(model, i)[,2]
    dfContributionMat[,i] <- model[[1]][[i]][[1]]@results[7:21]
  }

  lambdaSummary <- data.frame(
    variable = as.character(readLambdaFile(model,1)$variable), 
    meanLambda = apply(lambdaMat, 1, mean),
    seLambda = apply(lambdaMat, 1, se))

  variableContributionSummary <- data.frame(
    variable = as.character(readLambdaFile(model,1)$variable), 
    meanContribution = apply(dfContributionMat, 1, mean),
    seContribution = apply(dfContributionMat, 1, se))

  outFrame <- join(variableContributionSummary, lambdaSummary) %>%
    arrange(desc(meanContribution))
  
  return(outFrame)
}

lambdaContributionFrame_allIndOut <- makeLambdaContributionFrame(allIndOut)

lambdaContributionFrame_allSfOut <- makeLambdaContributionFrame(allSfOut)

lambdaContributionFrame_allLfOut <- makeLambdaContributionFrame(allLfOut)


df2 <- data.frame(variable = df1[,1], 
                  percentContribution = apply(df1[,-1], 1, mean),
                  standardError = apply(df1[,-1], 1, se)) %>%
                  arrange(desc(percentContribution))



prob.r.stack = function(model, outformat){
  r = stack()
  for (i in 1:5){
    r1 = predict(model[[1]][[i]][[1]],env.stack,
                 args=c(paste('outputformat=',outformat, sep = '')), 
                 progress='text')
    r = stack(r, r1)
  }
  r
}

allIndOutRlogistic <- prob.r.stack(allIndOut, 'logistic')
allSfOutRlogistic <- prob.r.stack(allSfOut, 'logistic')
allLfOutRlogistic <- prob.r.stack(allLfOut, 'logistic')

indLogisticMean <- mean(allIndOutRlogistic)
sfLogisticMean <- mean(allSfOutRlogistic)
lfLogisticMean <- mean(allLfOutRlogistic)

#-------------------------------------------------------------------------------*
# Niche equivalency:
#-------------------------------------------------------------------------------*

flockData <- swdRUBL[[1]]

# Generate a random sample of species 1 by species 2
# Note: Species 1 is the species of interest, comparison to species 2

random.swd.pair = function(sp1, sp2){
  # Remove unevaluated flock
  flockData <- swdRUBL[[1]] %>%
    dplyr::filter(sp == sp1|sp == sp2)
  nSp1 <- nrow(dplyr::filter(swdRUBL[[1]], sp == sp1))
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
  beta.r = paste('betamultiplier=',beta.multiplier,sep = '')
  mod.args = c('nothreshold', 'nohinge', 'noproduct',#'noquadratic',
               beta.r, 'addallsamplestobackground',
               'writebackgroundpredictions','writeplotdata',
               'noautofeature','nooutputgrids',
               'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
  maxentModel <- maxent(max.in[,-1], max.in[,1], args = mod.args)
  return(predict(maxentModel, env.stack, args = c('outputformat=raw')))
}

# Run models of empirical data:

lf = maxentRunRawPlot(dplyr::filter(flockData, sp == 'ind') %>%
               mutate(sp = 1))
sf = maxentRunRawPlot(dplyr::filter(flockData, sp == 'sf') %>%
               mutate(sp = 1))
ind = maxentRunRawPlot(dplyr::filter(flockData, sp == 'lf') %>%
                mutate(sp = 1))

# Run null models for flock size pairs
# Note: Each of the null surface lists are 382.5 Mb!

n.lf.sf <- list(length = 100)
n.sf.lf <- list(length = 100)
n.lf.ind <- list(length = 100)
n.ind.lf <- list(length = 100)
n.sf.ind <- list(length = 100)
n.ind.sf <- list(length = 100)

for(i in 1:100) n.sf.ind[[i]] <- maxentRunRawPlot(random.swd.pair('sf', 'ind'))
for(i in 1:100) n.ind.sf[[i]] <- maxentRunRawPlot(random.swd.pair('ind', 'sf'))
for(i in 1:100) n.lf.ind[[i]] <- maxentRunRawPlot(random.swd.pair('lf', 'ind'))
for(i in 1:100) n.ind.lf[[i]] <- maxentRunRawPlot(random.swd.pair('ind', 'lf'))
for(i in 1:100) n.lf.sf[[i]] <- maxentRunRawPlot(random.swd.pair('lf', 'sf'))
for(i in 1:100) n.sf.lf[[i]] <- maxentRunRawPlot(random.swd.pair('sf', 'lf'))


#-------------------------------------------------------------------------------
# Function to calculate modified Hellinger similarities for a given model run
#-------------------------------------------------------------------------------

I.dist = function(p.x, p.y){
  # Convert the rasters to SpatialGridDataFrame format:
  p.x = as(p.x, 'SpatialGridDataFrame')
  p.y = as(p.y, 'SpatialGridDataFrame')
  # Make a list of the probability surfaces:
  p.list = list(p.x,p.y)
  # Calculate the modified-Hellinger similarity (Warren 2008, pg. 2870)
  niche.overlap(p.list)[2,1]
}

#-------------------------------------------------------------------------------
# Function to run niche equivalency analyses on two flock size classes
#-------------------------------------------------------------------------------
# This function generates a list of two elements, slot1 contains the modified-H
# distance (I) of the actual data and slot2 contains the modified-H of the
# null distribution.

run.nea = function(sp1, sp2, null.xy, null.yx){
  I.actual = I.dist(sp1, sp2)
  I.null = numeric()
  for (i in 1:100){
    I.null[i] = I.dist(null.xy[[i]],null.yx[[i]])
  }
  nea.list = list(I.actual, I.null)
  names(nea.list) = c('I.actual','I.null')
  nea.list
}

#-------------------------------------------------------------------------------
# Run niche equivalency analyses
#-------------------------------------------------------------------------------

# Large flock vs. small flock:

I.lf.sf = run.nea(lf,sf,n.lf.sf,n.sf.lf)

# Large flock vs. individual sightings (<20 individuals):

I.lf.ind = run.nea(lf,ind,n.lf.ind,n.ind.lf)

# Small flock vs. individual sightings (<20 individuals):

I.sf.ind = run.nea(sf,ind,n.sf.ind,n.ind.sf)

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
  setwd('C:/Users/Brian/Dropbox/rubl_12_15/scratch_out')
  jpeg(out.name, 1200,1250, res = 300)
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
  dev.off()
}

# Make plots:

hist.mhd(I.lf.sf, 'Large vs.small flock sightings', 'mh_dist_lf_sf.jpg',T)

hist.mhd(I.lf.ind, 'Large flock vs. individual sightings', 'mh_dist_lf_ind.jpg',F)

hist.mhd(I.sf.ind, 'Small flock vs. individual sightings', 'mh_dist_sf_ind.jpg',F)

#-------------------------------------------------------------------------------
# Predicted niche occupancy
#===============================================================================

#-------------------------------------------------------------------------------
# PNO functions:
#-------------------------------------------------------------------------------

# Calculate pno:

pno.df = function(mod.x, mod.y, env.var){
  # Sum the raw probabilities about a given value of an environmental variable:
  pno.df = data.frame(zonal(mod.x,env.stack[[env.var]],'sum', digits = 2))
  pno.df[,3] = zonal(mod.y,env.stack[[env.var]],'sum', digits = 2)[,2]
  colnames(pno.df) = c('env','pno.sp1','pno.sp2')
  pno.df
}



# Determine the modified-Hellinger similarity between two pnos:

pno.I = function(mod.x, mod.y, env.var){
  df = pno.df(mod.x, mod.y, env.var)
  # Calculate the modified-Hellinger similarity (I):
  niche.overlap(df)[2,1]
}


# Run pno models, returns a list of the
# actual pno-I (one value) and a vector of 100 null pno-I's:

run.pno = function(mod.x, mod.y, null.xy, null.yx, env.var){
  pno.actual = pno.df(mod.x, mod.y, env.var)
  pno.I.actual = pno.I(mod.x, mod.y, env.var)
  pno.I.null = numeric()
  for (i in 1:100){
    pno.I.null[i] = pno.I(null.xy[[i]],null.yx[[i]], env.var)
  }
  pno.list = list(pno.I.actual, pno.I.null, pno.actual) # pno.actual, 
  names(pno.list) = c('pno.I.actual','pno.I.null','pno.actual')
  return(pno.list)
}

#-------------------------------------------------------------------------------
# Run PNO
#-------------------------------------------------------------------------------
# THIS TAKES A VERY LONG TIME TO RUN!

pno.lf.sf = list()
for(i in 1:15){
  pno.lf.sf[[i]] = run.pno(lf, sf, n.lf.sf, n.sf.lf,i)
}

pno.lf.ind = list()
for(i in 1:15){
  pno.lf.ind[[i]] = run.pno(lf, ind, n.lf.ind, n.ind.lf,i)
}

pno.sf.ind = list()
for(i in 1:15){
  pno.sf.ind[[i]] = run.pno(sf, ind, n.sf.ind, n.ind.sf,i)
}

names(pno.lf.sf) = names(env.stack)
names(pno.lf.ind) = names(env.stack)
names(pno.sf.ind) = names(env.stack)

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
  df = pno.lf.sf[[env]][[1]]
  ind = pno.lf.ind[[env]][[1]][,3]
  env = df[,1]
  lf = df[,2]
  sf = df[,3]
  plot(lf~env, type = 'l', xlim = c(0,1), lwd =2, bty ='l',
       main = names(env.stack[[env]]), ylab = 'PNO')
  lines(env, sf, lty = 2, lwd = 2)
  lines(env, ind, lty = 3, lwd = 2)
}

plot.pno(3)

#########################################################################################################
#########################################################################################################
#########################################################################################################
# Compare distributions
library(ggplot2)

ptsENV <- swdBG %>%
  dplyr::mutate(sp = 'bg', observationType = 'ebird') %>%
  dplyr::select(sp, observationType, lon:woodland) %>%
  dplyr::bind_rows(swdRUBL$eb %>%
                     dplyr::mutate(observationType = 'ebird') %>%
                     dplyr::select(sp, observationType, lon:woodland)) %>%
  dplyr::bind_rows(swdRUBL$bz %>%
                     dplyr::mutate(observationType = 'blitz') %>%
                     dplyr::select(sp, observationType, lon:woodland)) %>%
  dplyr::mutate(sp = factor(sp))

ptsENV$sp <- factor(sp, levels =  'bg','ind','sf', 'lf')


modeFun <- function(variable, rounding){
  d <- density(round_any(variable, rounding))
  dFrame <- data.frame(d$x, d$y)
  names(dFrame) <- c('x','y')
  dFrame %>% dplyr::filter(y == max(y)) %>%
    dplyr::select(x)
}

summarizeByFlockSize <- function(variable, rounding){
  outTable <- ptsENV %>%
    dplyr::select(sp, observationType, 
                  variable = matches(variable)) %>%
    dplyr::group_by(sp) %>%
    dplyr::summarize(min = min(variable),
                     max = max(variable),
                     mean = mean(variable),
                     variance = var(variable),
                     IQR = IQR(variable))
  if(variable == 'ppt' | variable == 'tmin'){
    outTable <- ptsENV %>%
      dplyr::select(sp, observationType, 
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


ksTest <- function(variable, sp1, sp2){
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

summarizeByFlockSize('tmin', 0.5)


ksTest('tmin','sf','lf')

ksTest('dev_hi','sf','ind')


ksTest('dev_li','sf','lf')

ksTest('flood','ind','bg')

ksTest('flood','lf','ind')

ksTest('flood','lf','sf')



ggplot(ptsENV, 
       aes(x = flood, fill = sp)) +
    geom_histogram(aes(y=0.5*..density..),
                   position = 'identity', binwidth=0.1) +
    facet_wrap(~sp,nrow=3) +
    theme(aspect.ratio = 1) +
    theme_bw()

ggplot(ptsENV, 
       aes(factor(sp,levels = c('bg','ind','sf','lf')),
           dev_hi, fill = sp)) +
  geom_violin(adjust = 0.5) +
  xlab('Point data class') +
  ylab('Environmental variable') +
  coord_flip() +
  theme_bw()
  
  
