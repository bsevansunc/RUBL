################################################################################
# Niche equivalency and background test
################################################################################
# Author: Brian S Evans
# Updated: 5/6/2014
#
# This script is used to evaluate the question: Do small/large flocks occupy the
# same environmental niche (habitat) as birds not associated with flocks? The
# script runs the niche equivalency test as described
# by Warren et al. 2008 (see ?niche.equivalency.test for citation).
# 
# Steps:
# 1. Load RUBL point data
# 2. Load environmental stack
# 3. Run Niche equivalency test 
# 4. Run Background similarity test
# 

#===============================================================================
# Set-up
#===============================================================================

# Load libraries (not all are necessary for this analysis):

library(phyloclim)
library(raster)
library(dismo)
library(rJava)
library(plyr)
library(sp)
library(RVAideMemoire)

# Set-up:

source('C:/Users/Brian/Documents/GitHub/RUBL/maxent_prep.R')

#-------------------------------------------------------------------------------
# Get data
#-------------------------------------------------------------------------------

# Gather eBird background points (same regardless of species group):
# Note: "-19" removes the k-fold column, which will not be used here.

ebird = swd.list$swd.lf.all[swd.list$swd.lf.all$sp == 0,-19]


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
  # Determine the proportion of species 1:
    prob.s1 = length(s1[,1])/length(pres[,1])
  # Generate random value of 1 or 0 with the probability of obtaining
  # a 1 related to the # of s1 observations:
    pres$s.rand = rbinom(length(pres[,1]),1,prob.s1)
  # Select species randomly assigned as "1" and remove "s.rand" and k
    pres = pres[pres$s.rand == 1,]
    pres[,-c(19,20)]
  }

head(random.swd.pair('swd.lf.all','swd.sf.all'))


#-------------------------------------------------------------------------------
# Model running functions
#-------------------------------------------------------------------------------

# Function to run maxent models for niche overlap/equivalency analysis:

mod.run = function(sp, make.map){
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
  # If "make.map" is specified (yes), make a map:
      if (make.map == 'yes') map.out = predict(mod, env.stack, args = c('outputformat=raw'))
        else map.out = 'NULL'
  # Create list of model, evaluation object, and predicted values:
    mod.list = list(mod, eval, max.in, map.out)
      names(mod.list) = c('mod','eval','outframe', 'map.out')
    mod.list
    }


#-------------------------------------------------------------------------------
# Function to calculate modified Hellinger distances for a given model run
#-------------------------------------------------------------------------------

# Note: I'm using "run.e" to refer to running empirical data and "run.p" for
# permuted data

h.dist = function(run.x, run.y){
  # Load raw probabilities:
    p.x = run.x$outframe$predictions
    p.y = run.y$outframe$predictions
  # Calculate the Hellinger distance (Warren 2008, pg. 2870, EQ 2)
    h = sqrt(sum((sqrt(p.x)-sqrt(p.y))^2))
  # Calculate the modified Hellinger distance (pg. 2870, EQ 3)
    I = 1 -.5*h
    return(I)
}

#-------------------------------------------------------------------------------
# Function to calculate niche overlap about a given environmental variable
#-------------------------------------------------------------------------------

n.overlap = function(run.x, run.y, env.var){
  # Load raw probabilities:
    p.x = run.x$outframe$predictions
    p.y = run.y$outframe$predictions
  # Load environmental variables
    env.x = run.x$outframe[,env.var]
    env.y = run.y$outframe[,env.var]
  env.x
}


t1 = m.lf$outframe[,'dev_hi']

tr = mod.run('swd.lf.all', 'yes')
tr = mod.run('swd.lf.all', 'yes')

trcut = cut(tr$map.out, breaks =10)

tr2 = tr$map.out-

####################################

m.lf  = mod.run('swd.lf.all', 'no')
m.sf  = mod.run('swd.sf.all', 'no')
m.ind  = mod.run('swd.ind.all', 'no')


t2 = mod.run(c('swd.sf.all','swd.lf.all'), 'no')

h.dist(m.lf, m.sf)

h.dist(m.lf, m.ind)

h.dist(m.sf, m.ind)


h.dist.env(m.sf,m.ind,'dev_hi')


test = m.sf$outframe[,'environment']





####### BELOW: Trying something out
density(test)
lines(density(test[test$sp == 0,'dev_li']),add = T, col = 'red')

mean(test[[3]][test[[3]]$sp ==1,'pasture'])

plot(density(test[[3]][test[[3]]$sp ==1,'pasture']))

# library = sm

t2 = test[[3]]

sm.density.compare(t2$tmin,t2$sp, h = 0.5, xlim = c(-15,15),
                   xlab = 'Minimum monthly temperature, deg. C',
                   ylab = 'Density (Bandwith = 0.5)',
                   cex.lab = 1.5, bty = 'l')

mean(test[[3]][test[[3]]$sp ==0,'pasture'])
lines(density(test[[3]][test[[3]]$sp ==0,'pasture']))


##############################################################


#######################################################################################

# Compare the distribution about a given environemntal variable



test = random.swd.pair('swd.lf.all','swd.sf.all')
summary(random.swd.pair('swd.lf.all','swd.sf.all')[['tmin']])


lf = swd.list[['swd.lf.all']]
lf = lf[lf$sp == 1,]

test

# Make a new data frame with presence points and names for each flock size:

head(swd.list$swd.lf.all)

extract.pts.sp = function(data.sub, flock.size){
  dsub = swd.list[[data.sub]]
  if (flock.size!= 'bg') sp.num = 1 else sp.num = 0
  dframe = dsub[dsub$sp == sp.num,]
  dframe$sp = rep(flock.size,dim(dframe)[1])
  dframe
}

lf = extract.pts.sp('swd.lf.all','lf')
sf = extract.pts.sp('swd.sf.all','sf')
ind = extract.pts.sp('swd.ind.all','ind')

# Combine files:

pts = rbind(lf,sf,ind, ebird)[,1:3]

pts[,1] = as.factor(pts[,1])

colnames(pts) = c('spec','long','lat')

pts.lf.sf = pts[pts$sp == 'sf'|pts$sp == 'lf',]

# Convert to a spatial points data frame:

pts = SpatialPointsDataFrame(pts[,2:3],pts[1])

#
#===============================================================================
# Run hypothesis tests
#===============================================================================

# Set the number of permutations of the data:

reps = 9

# Run niche equivalency test:

test = maxent(env.stack, pts.lf.sf[,2:3])

net = niche.equivalency.test(pts.lf.sf, env.stack, maxent.exe)

net
plot(net)


bst = bg.similarity.test(samples, preds, reps, app = maxent.exe)

bst
plot(bst)
