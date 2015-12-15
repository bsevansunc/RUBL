#############################################################################
# MODEL CALIBRATION AND RUNNING
#############################################################################
# Brian S. Evans
# Updated 3/23/2014
#
# This script is the MaxEnt dashboard and is used for:
#   1) Model calibration: Calculate the optimal beta coefficient value
#   2) Model running: Run the final models
#   3) Model evaluation

# Note: This script is best run in RStudio, because of section headers

#----------------------------------------------------------------------------
# Source model running and evaluation functions file
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
# BETA FUNCTIONs
#----------------------------------------------------------------------------
# Predict values at test locations

mod.predict = function(mods, swd, k){
  # Set value for k
   # k = i
  # Load model
    model = mods[[1]]
  # Load testing files (and subset to just env data)
    swd1 = swd.list[[swd]]
    swd.p = swd1[swd1$sp == 1&swd1$k == k,]
    swd.a = swd1[swd1$sp == 0&swd1$k == k,]
    swd.p = swd.p[,-c(1:3,19)]
    swd.a = swd.a[,-c(1:3,19)]
    #p = swd.p[,c(2:3)]
    #p = SpatialPoints(p)
  # Prediction surface
    #r = predict(model,env.stack, args=c('outputformat=raw'), progress='text')
  # Outputs
    #pt.vals = extract(r,p)
    #probsum = cellStats(r, 'sum')
    pt.vals = predict(model, swd.p,args=c('outputformat=raw'))
    a.vals = predict(model, swd.a,args=c('outputformat=raw'))
    probsum = sum(pt.vals, a.vals)
    list(length(swd.p[,1]),pt.vals,probsum)
  }

# Calculate AIC:

calcAIC <- function(mods, swd, beta, k) {
  # Extract a  model:
    model = mods[[1]]
  # Extract lambdas file and convert to a data frame
    lambdas = model@lambdas
    lambda.df = data.frame(do.call('rbind', strsplit(lambdas,',',fixed=TRUE)))
  # Determing the number of parameters that had a lambda of zero:in the model from lambdas
  # Note: Using n.lz = Number of Lambdas that equals Zero
    n.lz = length(lambda.df[as.numeric(as.character(lambda.df[,2])) == 0,1])
  # Calculate the number of model parameters
    kn = length(lambdas)- n.lz - 4
  # Predict suitability
    preds = mod.predict(mods, swd, k)
  # Sum probabilities of predicted values
    probsum <- preds[[3]]
  # How many points?
    n = preds[[1]]
  # Extract the raw probabilities at point locations
    pt.vals = preds[[2]]
    loglik = sum(log(pt.vals / probsum))
  # Calculate the AICc
    AICc = -2*loglik + 2*kn + ((2*kn*(kn+1))/(n-kn-1))
  # Output
    df1 = data.frame(beta,AICc)
    colnames(df1) = c('beta','AICc')
    return(df1)
  }


# Statistical table of AIC output for a given beta

AICstats = function(swd, beta){
  mods = out.frame(swd, beta)
  df0 = data.frame(matrix(ncol = 2))
  for (i in 1:5){
    df0[i,] = calcAIC(mods[[i]],swd, beta, i)
  }
  df = data.frame(beta, mean(df0[,2]),se(df0[,2]),min(df0[,2]),max(df0[,2]))
  colnames(df) = c('beta','mean','se','min','max')
  df
}

# Function that assesses across beta values and output a table of results

beta.finder = function(swd, beta.seq){
  out = data.frame(matrix(nrow = length(beta.seq),ncol = 5))
  for (i in 1:length(beta.seq)){
    out[i,] = AICstats(swd, beta.seq[i])
  }   
  colnames(out) = c('beta','mean','se','min','max')
  out
  }

#----------------------------------------------------------------------------
# CALIBRATE MODELS
#----------------------------------------------------------------------------

setwd("C:/Users/Brian/Dropbox/rubl_12_15/scratch_out")

lf.betas2 = beta.finder('swd.lf.all', seq(0,20, by = 0.5))
sf.betas2 = beta.finder('swd.sf.all', seq(0,20, by = 0.5))
ind.betas2 = beta.finder('swd.ind.all', seq(0,20, by = 0.5))

lf.betas2[order(lf.betas2$mean),]
sf.betas2[order(sf.betas2$mean),]
ind.betas2[order(ind.betas2$mean),]

lf.betas2.eb = beta.finder('swd.lf.eb', seq(0,20, by = 0.5))
sf.betas2.eb = beta.finder('swd.sf.eb', seq(0,20, by = 0.5))
ind.betas2.eb = beta.finder('swd.ind.eb', seq(0,20, by = 0.5))

lf.betas2.eb[order(lf.betas2.eb$mean),]
sf.betas2.eb[order(sf.betas2.eb$mean),]
ind.betas2.eb[order(ind.betas2.eb$mean),]

lf.betas2.bz = beta.finder('swd.lf.bz', seq(0,20, by = 0.5))
sf.betas2.bz = beta.finder('swd.sf.bz', seq(0,20, by = 0.5))
ind.betas2.bz = beta.finder('swd.ind.bz', seq(0,20, by = 0.5))

lf.betas2.bz[order(lf.betas2.bz$mean),]
sf.betas2.bz[order(sf.betas2.bz$mean),]
ind.betas2.bz[order(ind.betas2.bz$mean),]

#----------------------------------------------------------------------------
# RUN BEST MODELS FOR FURTHER ANALYSIS
#----------------------------------------------------------------------------

mods.lf = out.frame('swd.lf.all', 7.5)
mods.sf = out.frame('swd.sf.all', 5)
mods.ind = out.frame('swd.ind.all', 5.5)

mods.lf.eb = out.frame('swd.lf.eb', 7.5)
mods.sf.eb = out.frame('swd.sf.eb', 5)
mods.ind.eb = out.frame('swd.ind.eb', 5.5)

mods.lf.bz = out.frame('swd.lf.bz', 7.5)
mods.sf.bz = out.frame('swd.sf.bz', 5)
mods.ind.bz = out.frame('swd.ind.bz', 5.5)




