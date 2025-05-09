################
# Resource Selection Functions
# © Christen Fleming & Björn Reineking
# Alston & Fleming et al., Methods in Ecology and Evolution 4:2 643-654 (2023)
################

library(ctmm)
load("data/tapir.rda")
# E.P. Medici, Data from: Study "Lowland tapirs, Tapirus terrestris, in Southern Brazil", Movebank Data Repository (2023)
# tree cover data from the Hansen forest map based on Landsat 7

# plot one tapir with treecover raster, to make sure we have appropriate environmental data & projection
i <- 1
DATA <- tapir[[i]]
projection(DATA) <- median(DATA)
plot(DATA,error=2,R=treecover,main="Lowland tapir under tree cover")

# select an autocorrelation model
# for the moment rsf.fit only uses isotropic models
GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=TRUE,isotropic=TRUE),interactive=FALSE)
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save(FIT,file="data/tapir-iso.rda")
load("data/tapir-iso.rda")

# raster covariates must be in a named list
R <- list(tree=treecover)
# see raster::as.factor() for categorical variables

# AKDE (no RSF)
AKDE <- akde(DATA,FIT,weights=TRUE)
plot(DATA,error=2,UD=AKDE,R=treecover,col.grid=NA,main="AKDE")

# fit IID model for comparison
IID <- ctmm.fit(DATA,CTMM=ctmm(isotropic=TRUE))
KDE <- akde(DATA,IID)

help("rsf.fit")

# assigned weight without autocorrelation
plot(DATA$timestamp,mean(KDE$DOF.area) * KDE$weights,xlab='time',ylab="weight",ylim=c(0,1.2))
# How many points do you need for an IID RSF estimate?
# iRSF without autocorrelation: iterates until the default 1% error threshold
RSF.IID <- rsf.fit(DATA,KDE,R=R)

# assigned weight with autocorrelation
plot(DATA$timestamp,mean(AKDE$DOF.area) * AKDE$weights,xlab='time',ylab="weight")
# How many points do you need for a autocorrelation-weighted RSF estimate?
# iRSF with autocorrelation: iterates until the default 1% error threshold
RSF <- rsf.fit(DATA,AKDE,R=R)
# if you don't have a time-dependent model, integrator="Riemann" is much faster
RSF <- rsf.fit(DATA,AKDE,R=R,integrator="Riemann")

summary(RSF)

# Advantages of rsf.fit() iRSFs over regular RSFs:
# * log-likelihood is down-weighted to account for autocorrelation and irregular sampling
# * available points are randomly sampled until numerical convergence
# * available area is estimated - uncertainty is propagated (iRSF)

# iRSF or iSSF, which to choose? (for iSSFs, see the 'amt' R package)
# * RSFs requires range residence, SSFs do not
# * SSFs can model fine scale selection, and may have larger DOFs for fine-scale data
#     DOF[RSF] ~ DOF[area]
#     DOF[SSF] ~ DOF[diffusion]
# * SSFs are discrete time and cannot handle irregular data
# * RSFs directly output utilization-distribution (UD) information,
#     SSF selection parameters have a different meaning, and their UD is non-trivial

## rsf.select() can do model selection on multiple predictors
RSFS <- rsf.select(DATA,AKDE,R=R,formula=~I(sqrt(tree))+tree+I(tree^2),integrator="Riemann",verbose=TRUE,trace=TRUE)
summary(RSFS)

# selected model
RSF <- RSFS[[1]]
summary(RSF)

treecover # 0-1 valued
# relative selection of tree cover versus no tree cover
exp( summary(RSF)$CI[1,] * (sqrt(1)-sqrt(0)) )

# if you had more individuals and more significance (and transferable models)
help("mean.ctmm")

# The iRSF distribution that was fit
help("agde")

AGDE <- agde(DATA,RSF,R=R)
# note the finite available area that was estimated
plot(DATA,AGDE,main='iRSF')

# suitability maps
help("suitability")

SUIT <- suitability(DATA,CTMM=RSF,R=R,grid=AKDE)
names(SUIT) # brick with 3 layers (lower, point estimate, upper)
plot(DATA,error=2,R=SUIT[['est']],col.grid=NA,main="suitability")

# RSF-informed AKDE
help('akde')

RAKDE <- akde(DATA,RSF,R=R,weights=TRUE)
plot(DATA,error=2,UD=RAKDE,col.grid=NA,main="iRSF-AKDE")

# you can also add boundaries at the kernel level