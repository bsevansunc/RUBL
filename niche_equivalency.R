library(phyloclim)
# load PNOs for Oxalis sect. Palmatifoliae
data(PNO)

class(PNO)

test = PNO$MinTemperatureColdestMonth


# plot predicted niche occupany for annual mean temperature
plotPNO(x = PNO$AnnualMeanTemperature,
        xlab = "Annual Mean Temperature (degree C)")
# same plot, but with weighted means added
plotPNO(x = PNO$AnnualMeanTemperature,
        xlab = "Annual Mean Temperature (degree C)", wm = TRUE)

###############################################


# path to MAXENT
# --------------
maxent.exe <- paste(system.file(package="dismo"), 
                    "/java/maxent.jar", sep = "")

# a data frame of coordinates where two species 
# have been detected ('presence points') and
# a raster stack of environmental covariables
# --------------------------------------
species <- c("enneaphylla", "laciniata")
data(sites)
samples <- sites[grep(paste(species, collapse = "|"), sites$spec), ]
data.path <- system.file("extdata", package = "phyloclim")
preds <- list.files(path = data.path, pattern = "[.]asc")
preds <- paste(data.path, preds, sep = "/")
preds <- stack(lapply(X = preds, FUN = raster))

# testing against 9 permutations of the data
# -------------------------------------------
reps <- 9

# run hypothesis tests
# --------------------
if (file.exists(maxent.exe)){
  net <- niche.equivalency.test(samples, preds, reps, maxent.exe)
  net; plot(net)
  bst <- bg.similarity.test(samples, preds, reps, app = maxent.exe)
  bst; plot(bst)
} else {
  message("get a copy of MAXENT (see Details)")
}