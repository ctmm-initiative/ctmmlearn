################
# Resource Selection Functions
# (c) Christen Fleming & Björn Reineking
# (paper in press at MEE)
################

library(ctmm)
data(buffalo)
projection(buffalo) <- median(buffalo)

# for the moment rsf.fit only uses isotropic models
# you can feed in an anisotropic model, but it will be refit
FITS <- list()
for(i in 1:length(buffalo))
{
  GUESS <- ctmm.guess(buffalo[[i]],CTMM=ctmm(isotropic=TRUE),interactive=FALSE)
  FITS[[i]] <- ctmm.select(buffalo[[i]],GUESS,trace=3)
}
names(FITS) <- names(buffalo)
# save(FITS,file="buffalo-iso.rda")
load("data/buffalo-iso.rda")

AKDES <- akde(buffalo,FITS,weights=TRUE)
# time to mention the benefits of rsf.fit while this is running
# * available area is estimated - uncertainty is propagated
# * available points are randomly sampled until numerical convergence
# * log-likelihood is downweighted to account for autocorrelation and irregular sampling

# load environmental data for buffalo
load("data/buffalo_env.rda")

# environmental variables
names(buffalo_env)

# pull out elevation data
ELEV <- raster(buffalo_env,"elev")

# plot to make sure we have appropriate environmental data & projection
plot(buffalo,AKDES,R=ELEV,col.grid=NA,col.level=NA)

help("rsf.fit")
# raster covariates mustbe in a named list

RSF <- list()
for(i in 1:length(buffalo))
{
  RSF[[i]] <- rsf.fit(buffalo[[i]],AKDES[[i]],R=list(elevation=ELEV),integrator="Riemann",trace=2)
}
names(RSF) <- names(buffalo)

# inspect one model fit
i <- 4
summary(RSF[[i]]) # slightly significant
# the rest are insignificant

# if you had more individuals and more significance
help("mean.ctmm")

# suitability maps
help("suitability")

# RSF-informed AKDE
i <- 1
RAKDE <- akde(buffalo[[i]],RSF[[i]],R=list(elevation=ELEV),weights=TRUE)

plot(buffalo[[i]],RAKDE)
