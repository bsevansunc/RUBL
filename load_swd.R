##############################################
# Load swd files
##############################################
# The following is one big function that:
# 1. Sources the samples with data (SWD) files
# 2. Formats them for dismo-maxent
# 3. Makes a presence-absence column (pa)
# 4. Assembles as a list of data frames

load.swd = function(){
# Find the SWD data (searches for and ID's all files that end in ".csv":

swd_data <- list.files('C:/Users/Brian/Dropbox/rubl_12_15/swd',
                          pattern='\\.csv$', full=T)

# Add data to memory:

swd.list = lapply(swd_data, read.csv)

# Assign names to each of the files:

pts.all = swd.list[[1]]
pts.bg = swd.list[[2]]
pts.bz = swd.list[[3]]
pts.eb = swd.list[[4]]

# Some fixes on the background points:
library(dplyr)

pts.bg <- pts.bg %>% dplyr::select(-c(ID:woodland.1)) %>%
  dplyr::mutate(sp = 'bg') %>%
  dplyr::select(sp, lon:woodland)

#-------------------------------------------------------------------------------
# Prepare swd files for model running
#-------------------------------------------------------------------------------

# The following function:
# 1) Subsets swd file to a given flock size
# 2) Replaces the sp column (flock size) with 1's or 0's
# 3) Adds a kfold partition

swd.prep = function(swd.file, flock.size){
  swd = swd.file[swd.file$sp == flock.size,] # Subset by flock
  swd$sp = if (flock.size == 'bg'){rep(0, length(swd$sp))
    } else {
      (rep(1, length(swd$sp)))} # Adds counts
  swd$k = kfold(swd, k=5) # Adds kfold partition ID
  swd
}

# SWD function: Binds positive counts (for a flock of a given size)
# and the background files

swd.fun = function(swd.file, flock.size){
  swd1 = swd.prep(swd.file, flock.size)
  swd0 = swd.prep(pts.bg, 'bg')
  rbind(swd0, swd1)
}


# Run to create swd files:

swd.lf.bz = swd.fun(pts.bz,'lf')
swd.lf.eb = swd.fun(pts.eb,'lf')
swd.lf.all = swd.fun(pts.all,'lf')

swd.sf.bz = swd.fun(pts.bz,'sf')
swd.sf.eb = swd.fun(pts.eb,'sf')
swd.sf.all = swd.fun(pts.all,'sf')

swd.ind.bz = swd.fun(pts.bz,'ind')
swd.ind.eb = swd.fun(pts.eb,'ind')
swd.ind.all = swd.fun(pts.all,'ind')

# Convert to a list:

swd.list = list(swd.lf.bz, swd.lf.eb, swd.lf.all,
                  swd.sf.bz,swd.sf.eb,swd.sf.all,
                  swd.ind.bz, swd.ind.eb, swd.ind.all)

# Add names to the list items:

list.names = c('swd.lf.bz','swd.lf.eb','swd.lf.all',
              'swd.sf.bz','swd.sf.eb','swd.sf.all',
              'swd.ind.bz','swd.ind.eb','swd.ind.all')

names(swd.list) = list.names

# Return swd.list
swd.list
}
