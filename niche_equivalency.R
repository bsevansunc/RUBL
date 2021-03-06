################################################################################
# Niche equivalency and predicted niche occupancy 
################################################################################
# Author: Brian S Evans
# Updated: 5/6/2014
#
# This script is used to evaluate the question: Do small/large flocks occupy the
# same environmental niche as birds not associated with flocks? The
# script runs the niche equivalency test as described by Warren et al. 2008
# (see ?niche.equivalency.test in the package phyloclim for citation) and the
# predicted niche occupancy about a given environmental variable as described 
# by Evans et al. 2009 (see ?pno in the package phyloclim for citation)
#-------------------------------------------------------------------------------
# Set-up
#===============================================================================

source('C:/Users/Brian/Documents/GitHub/RUBL/maxent_prep.R')

# Gather eBird background points (same regardless of species group):
# Note: "-19" removes the k-fold column, which will not be used here.

ebird = swd.list$swd.lf.all[swd.list$swd.lf.all$sp == 0,-19]

#-------------------------------------------------------------------------------
# Random sample of points across flock size classes
#===============================================================================
# Generate a random sample of species 1 by species 2
# Note: Species 1 is the species of interest, comparison to species 2

random.swd.pair = function(sp1, sp2){
  # Get species data:
  s1 = swd.list[[sp1]]
  s1 = s1[s1$sp == 1,]
  s2 = swd.list[[sp2]]
  s2 = s2[s2$sp == 1,]
  # Combine into one file:
  pres = rbind(s1,s2)
  # Determine the sample size as the proportion of species 1:
  prob.s1 = length(s1[,1])/length(pres[,1])
  # Generate random value of 1 or 0 with the probability of obtaining
  # a 1 related to the # of s1 observations:
  pres$s.rand = rbinom(length(pres[,1]),1,prob.s1)
  # Extract species randomly assigned as "1" 
  pres[pres$s.rand == 1,]
}

#-------------------------------------------------------------------------------
# Function to run maxent models for niche equivalency and PNO analyses:
#===============================================================================

# Model running:

mod.run = function(sp){
  # Get presence data:
  pres = sp[,-c(19,20)]
  # Combine with background ("ebird") data:
  max.in = rbind(pres,ebird)
  env = max.in[,-c(1:3)]
  pa = max.in[,1]
  # Set model arguments
  mod.args = c('nothreshold', 'nohinge', 'betamultiplier=0',
               'addallsamplestobackground',
               'noautofeature','nooutputgrids',
               'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
    mod = maxent(env, pa, args = mod.args)
  # Create output map:
   predict(mod, env.stack, args = c('outputformat=raw'))
  }

# Setting null model runs:

run.nulls = function(species1, species2){
  sp.nulls = list()
  for(i in 1:100){
    sp.nulls[[i]] = random.swd.pair(species1, species2)
  }
  out.map = list()
  for(i in 1:100){
    out.map[[i]] = mod.run(sp.nulls[[i]])
  }
  out.map
  }

#-------------------------------------------------------------------------------
# Model running:
#-------------------------------------------------------------------------------
# This generates the actual and null prediction maps. WARNING!!! THIS TAKES
# SEVERAL HOURS TO RUN!!! Because of this, I recommend saving the environment
# once this is done running.

# Run models of empirical data:

lf = mod.run(swd.list[['swd.lf.all']])
sf = mod.run(swd.list[['swd.sf.all']])
ind = mod.run(swd.list[['swd.ind.all']])

# Run null models for flock size pairs
# Note: Each of the null surface lists are 382.5 Mb!

n.lf.sf = run.nulls('swd.lf.all','swd.sf.all')
n.sf.lf = run.nulls('swd.sf.all','swd.lf.all')

n.lf.ind = run.nulls('swd.lf.all','swd.ind.all')
n.ind.lf = run.nulls('swd.ind.all','swd.lf.all')

n.sf.ind = run.nulls('swd.sf.all','swd.ind.all')
n.ind.sf = run.nulls('swd.ind.all','swd.sf.all')

#-------------------------------------------------------------------------------
# Niche equivalency analysis
#===============================================================================
# The goal of this analysis is to determine the modified-Hellinger similarities
# between flock size classes, then the m-H distances of a null distribution
# generated by randomly permuting values of "species x" and "species y". M-H
# distances vary from 0, in which there is no overlap between niches, to 
# 1, in which two niche models are identical.

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

out.stats(I.lf.sf)

# LARGE FLOCK VS. INDIVIDUALS:

out.stats(I.lf.ind)

# SMALL FLOCK VS. INDIVIDUAL:

out.stats(I.sf.ind)

#-------------------------------------------------------------------------------
# Plot the histograms
#-------------------------------------------------------------------------------
# Histograms compare the density distribution of the modified-Hellinger 
# similarities of the null models against the distance of the actual model for
# large vs. small flocks, large vs. individual sightings and small flocks vs. 
# individual sightings.

# Histogram function:

hist.mhd = function(I.sp1.sp2,main.label,out.name, leg){
  null.dist = outstats(I.sp1.sp2)[[2]]
  emp.dist = outstats(I.sp1.sp2)[[1]]
  plot.new()
  setwd('C:/Users/Brian/Documents/GitHub/RUBL')
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

pno = function(mod.x, mod.y, env.var){
  df = pno.df(mod.x, mod.y, env.var)
  # Calculate the modified-Hellinger similarity (I):
  niche.overlap(pno.df)[2,1]
  }


# Run pno models, returns a list of the
# actual pno-I (one value) and a vector of 100 null pno-I's:

run.pno = function(mod.x, mod.y, null.xy, null.yx, env.var){
  pno.actual = pno(mod.x, mod.y, env.var)
  pno.I.actual = pno.I(mod.x, mod.y, env.var)
  pno.I.null = numeric()
  for (i in 1:100){
    pno.I.null[i] = pno.I(null.xy[[i]],null.yx[[i]], env.var)
  }
  pno.list = list(pno.actual, pno.I.actual, pno.I.null)
  names(pno.list) = c('pno.actual','pno.I.actual','pno.I.null')
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
  env.name = names(env.stack[[env]])
  df = pno.lf.sf[[env]][[1]]
  ind = pno.lf.ind[[env]][[1]][,3]
  env = df[,1]
  lf = df[,2]
  sf = df[,3]
  plot(lf~env, type = 'l', xlim = c(0,1), lwd =2, bty ='l',
       main = env.name, ylab = 'PNO')
  lines(env, sf, lty = 2, lwd = 2)
  lines(env, ind, lty = 3, lwd = 2)
}

plot.pno(3)

#################################################################################

