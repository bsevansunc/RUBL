# DASHBOARD

#----------------------------------------------------------------------------
# BEST MODELS FOR FURTHER ANALYSIS
#----------------------------------------------------------------------------

mods.lf = out.frame('swd.lf.all', 7.5)
mods.sf = out.frame('swd.sf.all', 5)
mods.ind = out.frame('swd.ind.all', 5.5)

# Probability surfaces

indProbStack <- prob.r.stack(mods.ind, 'logistic')
sfProbStack <- prob.r.stack(mods.sf, 'logistic')
lfProbStack <- prob.r.stack(mods.lf, 'logistic')

# Lambda values:

mods.lf[[1]]@lambda



# Variable contribution:

var.contribution.fun(mods.lf)
var.contribution.fun(mods.sf)
var.contribution.fun(mods.ind)