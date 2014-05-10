################################################################################
# Niche equivalency test
################################################################################
# Author: Brian S Evans
# Updated: 5/6/2014
#
# This script is used to evaluate the question: Do small/large flocks occupy the
# same environmental niche (habitat) as birds not associated with flocks? The
# script runs the niche equivalency test as described by Warren et al. 2008
# (see ?niche.equivalency.test in the package phyloclim for citation) and the
# predicted niche occupancy about a given environmental variable as described 
# by Evans et al. 2009 (see ?pno in the package phyloclim for citation)
# 
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
  # Select species randomly assigned as "1" and remove "s.rand" and k
  pres = pres[pres$s.rand == 1,]
  pres[,-c(19,20)]
}

#-------------------------------------------------------------------------------
# Niche equivalency analysis
#===============================================================================


# Function to run maxent models for niche equivalency analysis:
#-------------------------------------------------------------------------------

mod.run = function(sp){
  # Get presence data (the if statement determines whether you pull the real 
  # data or a permutation of the data):
  if (length(sp) == 1) 
    pres = swd.list[[sp]][,-c(19,20)]
  else
    pres = random.swd.pair(sp[1],sp[2])
  # Combine with background ("ebird") data:
  max.in = rbind(pres,ebird)
  env = max.in[,-c(1:3)]
  pa = max.in[,1]
  # Set model arguments
  mod.args = c('nothreshold', 'nohinge', 'noproduct','noquadratic',
               'addallsamplestobackground',
               'writebackgroundpredictions','writeplotdata',
               'noautofeature','nooutputgrids',
               'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data:
  mod = maxent(env, pa, args = mod.args)
  # Create a model evaluation object:
  eval = evaluate(pres[,-c(1:3)], a = ebird[,-c(1:3)],model = mod)
  # Predict model values:
  max.in$predictions = predict(mod, env, args = c('outputformat=raw'))
  max.in
  # Create output map:
  map.out = predict(mod, env.stack, args = c('outputformat=raw'))
  return(map.out)
}

#-------------------------------------------------------------------------------
# Function to calculate modified Hellinger distances for a given model run
#-------------------------------------------------------------------------------
# This can be used to calculate modified Hellinger distances of empirical or
# null distributions (based on randomization).

I.dist = function(run.x, run.y){
  # Load raw probabilities:
  p.x = run.x$map.out
  p.y = run.y$map.out
  # Calculate the Hellinger distance (Warren 2008, pg. 2870, EQ 2)
  h1 = (sqrt(p.x)-sqrt(p.y))^2
  h2 = cellStats(h1,'sum')
  h = sqrt(h2)
  # Calculate the modified Hellinger distance (pg. 2870, EQ 3)
  I = 1 -.5*h
  return(I)
}

null.I = function(species1, species2){
  mod1 = mod.run(c(species1, species2))
  mod2 = mod.run(c(species2, species1))
  I.dist(mod1, mod2)
}


#-------------------------------------------------------------------------------
# Function to run niche equivalency analyses on two flock size classes
#-------------------------------------------------------------------------------
# This function generates a list of two elements, slot1 contains the modified-H
# distance (I) of the empirical data and slot2 contains the modified-H of the
# null distribution.

run.nea = function(sp1, sp2){
  I.empirical = I.dist(mod.run(sp1), mod.run(sp2))
  I.null = numeric()
  for (i in 1:100){
    I.null[i] = null.I(sp1,sp2)
  }
  nea.list = list(I.empirical, I.null)
  names(nea.list) = c('I.empirical','I.null')
}


#-------------------------------------------------------------------------------
# Run niche equivalency analyses
#-------------------------------------------------------------------------------

# Large flock vs. small flock:

I.lf.sf = run.nea('swd.lf.all','swd.sf.all')

# Large flock vs. individual sightings (<20 individuals):

I.lf.ind = run.nea('swd.lf.all','swd.ind.all')

# Small flock vs. individual sightings (<20 individuals):

I.sf.ind = run.nea('swd.sf.all','swd.ind.all')

#-------------------------------------------------------------------------------
# Stats for niche equivalency analyses
#===============================================================================

###############################
# LARGE FLOCK VS. SMALL FLOCK:
###############################

# Test ins:

null.dist.lf.sf = I.lf.sf[[2]]
emp.dist.lf.sf = I.lf.sf[[1]]

# Summary stats:

emp.dist.lf.sf
mean(null.dist.lf.sf)
se(null.dist.lf.sf)

# Determine the percentile associated with the empirical modified-H:

quantile(null.dist.lf.sf, probs = 0.05)
quantile(null.dist.lf.sf, probs = 0.1)
ecdf(null.dist.lf.sf)(emp.dist.lf.sf)

###############################
# LARGE FLOCK VS. INDIVIDUALS:
###############################

# Test ins:

null.dist.lf.ind = I.lf.ind[[2]]
emp.dist.lf.ind = I.lf.ind[[1]]

# Summary stats:

emp.dist.lf.ind
mean(null.dist.lf.ind)
se(null.dist.lf.ind)

# Determine the percentile associated with the empirical modified-H:

quantile(null.dist.lf.ind, probs = 0.05)
quantile(null.dist.lf.ind, probs = 0.1)
ecdf(null.dist.lf.ind)(emp.dist.lf.ind)

###############################
# SMALL FLOCK VS. INDIVIDUAL:
###############################

# Test ins:
null.dist.sf.ind = I.sf.ind[[2]]
emp.dist.sf.ind = I.sf.ind[[1]]

# Summary stats:

emp.dist.sf.ind
mean(null.dist.sf.ind)
se(null.dist.sf.ind)

# Determine the percentile associated with the empirical modified-H:

quantile(null.dist.sf.ind, probs = 0.05)
quantile(null.dist.sf.ind, probs = 0.1)
ecdf(null.dist.sf.ind)(emp.dist.sf.ind)

#-------------------------------------------------------------------------------
# Plot the histograms
#===============================================================================

setwd('C:/Users/Brian/Documents/rubl_out')

# Modified-Hellinger distance, large and small flocks:

plot.new()
jpeg('mh_dist_lf_sf.jpg', 1200,1200,res = 300)

hist(null.dist.lf.sf, xlim = c(0,1), ylim = c(0,10), freq = F,
     col = 'gray80', cex.lab = 1.25,
     main = 'Large vs.small flock sightings',
     xlab = 'Modified-Hellinger distance (I)')
  lines(c(emp.dist.lf.sf, emp.dist.lf.sf), c(0,9.5), lwd = 2, lty = 2)
  legend(.02,10,'Null distance','gray80',1, bty = 'n',x.intersp = .95, cex = .7)
  legend(0,9,'Actual distance',lty = 2,bty = 'n', x.intersp = .4, cex = .7)

dev.off()

# Modified-Hellinger distance, large flocks and individual sightings:

plot.new()
jpeg('mh_dist_lf_ind.jpg',1200,1200,res = 300)

hist(null.dist.lf.ind, xlim = c(0,1), ylim = c(0,10),freq = F,
     col = 'gray80', cex.lab = 1.25,
     main = 'Large flock vs. individual sightings',
     xlab = 'Modified-Hellinger distance (I)')
  lines(c(emp.dist.lf.ind, emp.dist.lf.ind), c(0,10), lwd = 2, lty = 2)

dev.off()

# Modified-Hellinger distance, small flocks and individual sightings:


plot.new()
jpeg('mh_dist_sf_ind.jpg',1200,1200,res = 300)

hist(null.dist.sf.ind, xlim = c(0,1), freq = F,
     col = 'gray80', cex.lab = 1.25,
     main = 'Small flock vs. individual sightings',
     xlab = 'Modified-Hellinger distance (I)')
lines(c(emp.dist.sf.ind, emp.dist.sf.ind), c(0,10), lwd = 2, lty = 2)

dev.off()
