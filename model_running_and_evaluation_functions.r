################################################################################
# Model running and evaluation functions
################################################################################
# Author: Brian S Evans
# Updated 3/23/2014
#
# This script is contains all of the functions used in MaxEnt analyses. The 
# functions are used to:
#   1) Run models
#   2) Provide predictive surface of outputs
#   3) Calibrate models
#   4) Evaluate models
#
################################################################################


# Set-up:

source('C:/Users/Brian/Documents/GitHub/RUBL/maxent_prep.R')

#============================================================================
# Model running Functions
#============================================================================


#----------------------------------------------------------------------------
# Function to run model for one partition
#----------------------------------------------------------------------------

mod.run = function(swd, k, beta.multiplier){
  max.in = swd.prep(swd, k)
  env = max.in[[3]]
  pa = max.in[[2]]
  # Set model arguments
  beta.r = paste('betamultiplier=',beta.multiplier,sep = '')
  mod.args = c('nothreshold', 'nohinge', 'noproduct','noquadratic',
               beta.r, 'addallsamplestobackground',
               'writebackgroundpredictions','writeplotdata',
               'noautofeature','nooutputgrids',
               'maximumiterations=10000', 'verbose')
  # Run maxent model with training and background data
  m.out = maxent(env, pa, args = mod.args)
  # Load testing files
  swd1 = swd.list[[swd]]
  test.pres = swd1[swd1$sp == 1&swd1$k == k,]
  test.bg = swd1[swd1$sp == 0&swd1$k == k,]
  # Prediction frame
  # Prepare the test data file for evaluation:
  p.env = test.pres[,-1]
  a.env = test.bg[,-1]
  # Evaluation data frame (predicted vs. observed):
  eval1 = evaluate(p.env,a.env, m.out)
  e.predicted = c(eval1@presence, eval1@absence)
  e.observed = c(rep(1, length(eval1@presence)), 
                 rep(0, length(eval1@absence)))
  eval.frame = data.frame(e.observed, e.predicted, eval1@auc)
  # Make the test presence and background file into spatial points
    test.pres.sp = SpatialPoints(test.pres[,c(2:3)])
    test.bg.sp = SpatialPoints(test.pres[,c(2:3)])
  # Output:
  out.list =  list(m.out, eval.frame, eval1)
  names(out.list)=c('m.out','eval.frame','eval1','test.pres.sp','test.bg.sp')
  return(out.list)
}

#----------------------------------------------------------------------------
# Function to run models across partitions:
#----------------------------------------------------------------------------

out.frame = function(swd, beta.multiplier){
  df1 = list()
  for (i in 1:5){
    df1[[i]] = mod.run(swd, i, beta.multiplier)
  }
  df1
}

#============================================================================
# CALIBRATION FUNCTIONs
#============================================================================
# GOAL IS THE MODEL WITH THE GREATEST EXPLANATORY POWER WITH THE LEAST 
# NUMBER OF VARIABLES.  

#----------------------------------------------------------------------------
# Standard error function:
#----------------------------------------------------------------------------

se = function(x) {sd(x)/sqrt(length(x))}

#----------------------------------------------------------------------------
# Predict values at test locations:
#----------------------------------------------------------------------------

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

#----------------------------------------------------------------------------
# Calculate AIC:
#----------------------------------------------------------------------------

calcAIC <- function(mods, swd, beta, k) {
  # Extract a  model:
  model = mods[[1]]
  # Extract lambdas file and convert to a data frame
  lambdas = model@lambdas
  lambda.df = data.frame(do.call('rbind', strsplit(lambdas,',',fixed=TRUE)))
  # Determing the number of parameters that had a lambda of zero 
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

#----------------------------------------------------------------------------
# Statistical table of AIC output for a given beta:
#----------------------------------------------------------------------------

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

#----------------------------------------------------------------------------
# Function that assesses across beta values and output a table of results:
#----------------------------------------------------------------------------

beta.finder = function(swd, beta.seq){
  out = data.frame(matrix(nrow = length(beta.seq),ncol = 5))
  for (i in 1:length(beta.seq)){
    out[i,] = AICstats(swd, beta.seq[i])
  }   
  colnames(out) = c('beta','mean','se','min','max')
  out
}

#============================================================================
# EVALUTATION FUNCTIONS
#============================================================================

#----------------------------------------------------------------------------
# Model prediction functions 
#----------------------------------------------------------------------------

# Probability surface for each fold:

mod.predict.r = function(model, outformat){
  pred.stack = stack()
  for(i in 1: length(model)){
  pred.stack[[i]] = predict(model[[i]][[1]],env.stack, 
          args=c(paste('outputformat=',outformat, sep = '')), 
          progress='text')
  }
  pred.stack
}

# Probability surface stack across folds:

prob.r.stack = function(model, outformat){
  r = stack()
  for (i in 1:5){
    r1 = predict(model[[i]][[1]],env.stack,
                 args=c(paste('outputformat=',outformat, sep = '')), 
                 progress='text')
    r = stack(r, r1)
  }
  r
}

# Average probability surface:

prob.r = function(model, outformat){
  mean(prob.r.stack(model, outformat) 
}

#----------------------------------------------------------------------------
# Function to calculate the AUC across runs
#----------------------------------------------------------------------------

auc.extract = function(obs.class,model){
  aucs = numeric(0)
  for (i in 1:5) aucs[i] = model[[i]][[3]]@auc
  df1 = data.frame(obs.class,mean(aucs),se(aucs))
  colnames(df1) = c('obs.class','auc.m','auc.se')
  df1$auc.l95 = df1$auc.m - df1$auc.se*1.96
  df1$auc.u95 = df1$auc.m + df1$auc.se*1.96
  df1$auc.l90 = df1$auc.m - df1$auc.se*1.645
  df1$auc.u90 = df1$auc.m + df1$auc.se*1.645
  df1
}

auc.tab.fs = rbind(auc.extract('lf',mods.lf),auc.extract('sf',mods.sf),
                   auc.extract('ind',mods.ind))

auc.tab.fs
auc.extract(mods.sf)
auc.extract(mods.ind)

names(mods.lf[[1]])

(mods.lf[[1]][[3]])
mods.lf[[1]][[3]]@auc

#----------------------------------------------------------------------------
# Function to calculate the point biserial correlation across runs
#----------------------------------------------------------------------------

cor.extract = function(obs.class,model){
  cors = numeric(0)
  for (i in 1:5) cors[i] = model[[i]][[3]]@cor
  df1 = data.frame(obs.class,mean(cors),se(cors))
  colnames(df1) = c('obs.class','cor.m','cor.se')
  df1$cor.l95 = df1$cor.m - df1$cor.se*1.96
  df1$cor.u95 = df1$cor.m + df1$cor.se*1.96
  df1$cor.l90 = df1$cor.m - df1$cor.se*1.645
  df1$cor.u90 = df1$cor.m + df1$cor.se*1.645
  df1
}

#----------------------------------------------------------------------------
# Function to create stats summary table with AUC, pbcor
#----------------------------------------------------------------------------

out.table = function(model, mean.prob, swd){
  df1 = cbind(cor.extract(model),auc.extract(model))
  df1
}

#----------------------------------------------------------------------------
# Function to analyze contribution (%) of variable to model performance
#----------------------------------------------------------------------------

var.contribution.fun = function(model){
  rf.fun1 = function(model, i){
    results.frame = data.frame(model[[i]][[1]]@results)
    results.frame = cbind(rownames(results.frame),results.frame)
    colnames(results.frame) = c('stat','value')
    #rownames(results.frame) = NULL
    results.frame = results.frame[agrep('contribution',results.frame[,1]),]
    results.frame
  }
  
  results.frame = rf.fun1(model,1)[2]
  
  for (i in 1:5){
    results.frame[,i] = rf.fun1(model,i)[2]
  }
  
  results.frame$mean = apply(results.frame,1,mean)
  results.frame$se = apply(results.frame,1,se)
  results.frame$rank = rank(results.frame$mean)
  results.frame = results.frame[,-c(1:5)]
  results.frame[order(results.frame$rank, decreasing = T),]
}

var.contribution.fun(mods.lf)
var.contribution.fun(mods.sf)
var.contribution.fun(mods.ind)


#----------------------------------------------------------------------------
# Function to calculate Tjur's D (Coefficient of discrimination) 
#----------------------------------------------------------------------------

D = function(obs.class,model){
  # Function to calculate D for each model:
  d.fun = function(model, i){
    df1 = model[[i]][[2]] # presence-absence and prediction
    p1 = mean(df1[df1$e.obs == 1,'e.predicted']) # Mean predicted at pres.
    p0 = mean(df1[df1$e.obs == 0,'e.predicted']) # Mean predicted at background
    d = p1-p0 # Calculate d for a given model
  }
  # Calculate for each model run:
  d1 = numeric(0)
  for (i in 1:5) {
    d1[i] = d.fun(model, i)
  }
  # Average across models:
  df2 = data.frame(obs.class, mean(d1),se(d1))
  colnames(df2) = c('obs.class','d.m','d.se')
  df2$d.l95 = df2$d.m - df2$d.se*1.96
  df2$d.u95 = df2$d.m + df2$d.se*1.96
  df2$d.l90 = df2$d.m - df2$d.se*1.645
  df2$d.u90 = df2$d.m + df2$d.se*1.645
  df2
}

#----------------------------------------------------------------------------
# Continuous Boyce Index
#----------------------------------------------------------------------------

cbi.fun = function(swd, p.surf){
  # See Hirzel et al. 2006
  # Set moving windows parameters:
  step = .01 # Number of stops in the moving window
  w = .1 # Moving window width
  suit =  seq(from = 0, to = 1, by = step) # suitability
  n = length(suit)
  # Function to calculate the number of points or raster cells
  # in a suitability class relative to the number of samples 
  # across classes:
  p.e.fun = function(p.e, j){
    length(p.e[p.e > suit[j] - w & p.e<suit[j] + w])/length(p.e)
  }
  # Function to calculate Boyce's F index for one partition:
    boyce.f.fun = function(i){
      # Get point and raster data for the partition:
        pts = sp.test.pres(swd, i)
        r = p.surf[[i]]
      # Extract data for that partition:
        p.vals = extract(r, pts)
      # Create empty data to fill:
        p = numeric()
        e = numeric()
        F = data.frame()
      # Run for loop to calculate p and e's:
      for (j in seq_len(n)) {
        p[j] = p.e.fun(p.vals, j)
        e[j] = p.e.fun(r, j)  
      }
      # Output data frame:
        F = data.frame(suitability,p, e, p/e)        
        F[!is.na(F[,3]),]
      }
  # Run across partitions and bind into one data frame:
    df = ldply(1:5, .fun = boyce.f.fun)
    df1 = data.frame(suit,as.vector(tapply(df[,4],df[,1],mean)),
        as.vector(tapply(df[,4],df[,1],se)))
    names(df1) = c('suitability', 'f.mean','f.se')
  # Calculate the Spearman correlation coefficient (rho) with confidence intervals
    # Rank of suitability:
      r.suit = rank(df$suitability)
    # Rank of p:e ratio:
      r.pe = rank(df$p.e)
    # Calculate Pearson correlation between the ranked lists:
    cor.out = cor.test(r.suit,r.pe)
  # Output
    l.out =list(df, df1, cor.out)
    names(l.out) = c('raw.data','condensed.data','cor.out')
  l.out
  }

  
cbi.plot = function(cbi.in,main){
  plot(cbi.in[[1]]$f.mean~cbi.in[[1]]$suitability, type = 'l', lwd = 2, bty = 'o',
       xlab = '',ylab = '',cex.lab = 1.5)
  title(ylab = 'Predicted / Expected ratio',line = 2.4, cex.lab =1.5)
  title(xlab = 'Suitability',line = 2.7, cex.lab =1.5)
  title(main = main,line = 1, cex.main =2)
  abline(h = 1, lwd =2 , lty = 2)
  points(cbi.in[[1]]$f.mean+1.96*cbi.in[[1]]$f.se~cbi.in[[1]]$suitability, type = 'l', lwd = 1.5,lty =3)
  points(cbi.in[[1]]$f.mean-1.96*cbi.in[[1]]$f.se~cbi.in[[1]]$suitability, type = 'l', lwd = 1.5,lty =3)
}



#--------------------------------------------------
# Construct a data frame of the p-to-e ratios by
# fold and calculate statistics
#--------------------------------------------------

df1 = data.frame(f0,f1,f2,f3,f4)

f = apply(df1, 1, mean)
se = apply(df1,1,sd)/sqrt(dim(df1)[2])
f.uci = f + qnorm(0.975)*se
f.lci = f - qnorm(0.975)*se

df2 = data.frame(suitability, f, se, f.uci, f.lci)

df3 = df2[df2$f > 0,]

plot(df3$suitability,df3$f, type = 'l', lwd = 2,
     xlab = 'Habitat suitability',
     ylab = 'Predicted:Expected',
     cex.lab = 1.5)
lines(df3$suitability, df3$f.uci, lty = 2)
lines(df3$suitability, df3$f.lci, lty = 2)
abline(h = 1, lty = 3)

cor.test(suitability, f, method = 'spearman')

#--------------------------------------------------
# Plotting suitability
#--------------------------------------------------

r = raster('lf_avg.asc')
plot(r)

rclmat = matrix(c(-Inf,.05,0,.06,.16,1,.36,Inf, 2), byrow = T, nrow = 3)
r.rc = reclassify(r, rclmat)

plot(r.rc, col = c('gray90','blue','red'))
legend('bottomright', legend = c('Unsuitable','Marginal','Suitable'),
       fill = c('gray90','blue','red'), cex = 1.5, bty = 'n')




