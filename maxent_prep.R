################################################################################
# Maxent Prep
################################################################################
# Author: Brian S Evans
# Updated 5/7//2014
#
# This script is contains all of the functions used to prepare to run MaxEnt 
# models for various tasks.
#
################################################################################

#===============================================================================
# Set-up
#===============================================================================

# Load libraries:

library(phyloclim)
library(raster)
library(dismo)
library(rJava)
library(plyr)
library(sp)
library(RVAideMemoire)


# Tell dismo where MaxEnt is:

maxent.exe <- paste(system.file(package="dismo"), 
                    "/java/maxent.jar", sep = "")

#-------------------------------------------------------------------------------
# Get data
#-------------------------------------------------------------------------------
# The following sources external functions to load the environmental rasters and 
# the sample-with-data observation files (separated by observation type and
# flock size)

# Rasters (takes about 20 seconds to run):

source('C:/Users/Brian/Documents/GitHub/RUBL/load_env.R')

env.stack = load.env()

# Samples with data:

source('C:/Users/Brian/Documents/GitHub/RUBL/load_swd.R')

swd.list = load.swd()

#----------------------------------------------------------------------------
# SWD preparation functions
#----------------------------------------------------------------------------

# Get SWD file:

swd.find = function(swd){
  swd1 = swd.list
  swd1[[swd]]
}

# Function to prepare swd for model running (includes partition):

swd.prep = function(swd, k){
  swd1 = swd.find(swd)
  # Load training files
  train.pres = swd1[swd1$sp == 1&swd1$k != k,]
  train.bg = swd1[swd1$sp == 0&swd1$k !=k,]
  # Prepare training and background files for maxent
  pa.env = rbind(train.pres, train.bg)
  pa = pa.env[,1]
  env = pa.env[,-1]
  # Make list of files:
  max.in = list(pa.env, pa, env)
  names(max.in) = c('pa.env','pa','env')
  # Remove longlat and k from the pa.env file
  max.in[[1]] = max.in[[1]][,-c(2,3)]
  max.in[[3]] = max.in[[3]][,-c(1,2)]
  max.in[[1]]$k = NULL
  max.in[[3]]$k = NULL
  return(max.in)
}

# Function to create a spatial points object from presences:

sp.pts = function(swd){
  swd1 = swd.find(swd)
  swd2 = swd1[swd1$sp!=0,]
  SpatialPoints(swd2[,c(2,3)])
}


sp.test.pres = function(swd, k){
  swd1 = swd.list[[swd]]
  test.pres = swd1[swd1$sp == 1&swd1$k == k,]
  SpatialPoints(test.pres[,c(2,3)])
}
