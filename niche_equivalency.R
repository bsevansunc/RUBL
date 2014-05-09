################################################################################
# Niche equivalency and predicted niche occupancy tests
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
# 
<<<<<<< HEAD
=======

>>>>>>> 18ff21612a26037285ea03a547276c395b4b8d9e
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
<<<<<<< HEAD
# Niche equivalency analysis
#===============================================================================


# Function to run maxent models for niche equivalency analysis:
#-------------------------------------------------------------------------------
=======
# Model running functions
#===============================================================================
>>>>>>> 18ff21612a26037285ea03a547276c395b4b8d9e

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
<<<<<<< HEAD
# This can be used to calculate modified Hellinger distances of empirical or
# null distributions (based on randomization).

I.dist = function(run.x, run.y){
=======
# This can be used for the mHd of empirical or null distributions

h.dist = function(run.x, run.y){
>>>>>>> 18ff21612a26037285ea03a547276c395b4b8d9e
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
#-------------------------------------------------------------------------------
<<<<<<< HEAD
null.dist = I.lf.sf.null
emp.dist = I.lf.sf

ci = function(null.dist){
  x = mean(null.dist)
  error = qnorm(.975)*sd(null.dist)/sqrt(length(null.dist))
  ci.frame = data.frame(x-error,x+error)
  names(ci.frame) = c('lower.ci','upper.ci')
  ci.frame
}

null.ci = ci(null.dist)
dens.null = density(null.dist)
dn = data.frame(dens.null$x,dens.null$y)
  colnames(dn) = c('x','y')
dn = dn[dn$x>=ci.frame$lower.ci&dn$x<ci.frame$upper.ci,]

hist(null.dist, freq = F, col = 'white',border = 'white', xlim = c(0,1))
lines(density(null.dist), lwd = 1.5)
abline(v = I.lf.sf, lty = 2, lwd = 2)

lines(c(max(dn$x),max(dn$x)),c(0,dn[dn$x == max(dn$x),]$y), lty =3)
lines(c(min(dn$x),min(dn$x)),c(0,dn[dn$x == min(dn$x),]$y), lty =3)

pval = sum(null.dist >= emp.dist)/ length(null.dist)


abline(v = min(dn$x), lty =3)

polygon(c(dn$x,dn$x),c(rep(0,length(dn$y)),dn$y),col = "grey90", border = NA)

#-------------------------------------------------------------------------------
# Predicted niche overlap
#===============================================================================

mod.stack = stack(run.mod('swd.lf.all'), run.mod('swd.sf.all'))

pno(env.stack[['tmin']], mod.stack)
=======
>>>>>>> 18ff21612a26037285ea03a547276c395b4b8d9e
# This function is not complete

n.overlap = function(run.x, run.y, env.var){
  # Load raw probabilities:
    p.x = run.x$outframe$predictions
    p.y = run.y$outframe$predictions
  # Load environmental variables
    env.x = run.x$outframe[,env.var]
    env.y = run.y$outframe[,env.var]
  env.x
}

##################################################################################
# SCRATCH SPACE
##################################################################################
head(random.swd.pair('swd.lf.all','swd.sf.all'))

t1 = m.lf$outframe[,'dev_hi']

trl = mod.run('swd.lf.all', 'yes')
trs = mod.run('swd.sf.all', 'yes')
tri = mod.run('swd.ind.all', 'yes')

tri.cut = cut(tri$map.out, breaks = 50)

plot(tri.cut)
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
