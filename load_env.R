##############################################
# Load environment ascii files
##############################################
# The following is one big function that:
# 1. Sources the asc raster files
# 2. Combines rasters into a raster stack
# 3. Assigns values of all rasters into memory
# 4. Defines the projection of the rasters


load.env = function(){
# Find the raster data (searches for and ID's all files that end in ".asc":

raster_data <- list.files('C:/Users/Brian/Dropbox/rubl_12_15/lc_asc',
                          pattern='\\.asc$', full=T)    

# Create a raster stack of raster layers:

env.stack = stack(raster_data)

# Add raster stack values to memory:

values(env.stack) = getValues(env.stack)

# Add projection information to the raster stack:

newproj = CRS('+proj=longlat +datum=WGS84')

projection(env.stack) = newproj
names(env.stack) = c('dev_hi','dev_li','flood','forh', 'form', 'grass',
                     'pasture','ppt','rowcrop', 'shrub','tmin', 'upfor',
                     'weth', 'wetw', 'woodland')
return(env.stack)

}

env.stack <- load.env()
