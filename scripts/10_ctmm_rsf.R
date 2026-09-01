################
# Resource Selection Functions (Habitat suitability modeling)
# © Christen Fleming & Björn Reineking
# Alston & Fleming et al., Methods in Ecology and Evolution 4:2 643-654 (2023)
################

# RSFs: parametric estimation of animal resource use (why does animal live in a certain place)
## what drives where the animal is located
## Specify a probability distribution (log link) for a model of habitat selection 
### where (+) coefficient means attraction to resource and (-) coefficient means repulsion from resource 
### (normalize the function, by dividing by Monte Carlo integration for a probability distribution likelihood function)
## traditional methods can be used as an "approximation" of the more rigorous model


#######################
# IMPORT AND VISUALIZE DATA
#######################

# Load ctmm package
library(ctmm)

# E.P. Medici, Movebank Data Repository (2023)
# Tree cover data from the Hansen forest map based on Landsat 7
load("Data/tapir.rda")  # tapir, treecover
summary(tapir)  # 29 individuals
class(treecover)  # RasterLayer (`raster` package)

# Let's make sure we have appropriate environmental data & projection
i <- 1
DATA <- tapir[[i]]  # subset tapir 1
projection(DATA) <- median(DATA)  # center projection on median of data

# Plot one tapir with tree cover raster
plot(DATA, error = 2, R = treecover, main = "Lowland tapir under tree cover")
## green = tree cover, white = mostly grassland (possibly wetlands), red = tapirs

# Select an autocorrelation model
## for now, rsf.fit() only uses isotropic models
GUESS <- ctmm.guess(DATA, CTMM = ctmm(error = TRUE, isotropic = TRUE), interactive = FALSE)
FIT <- ctmm.select(DATA, GUESS, trace = 3)
save(FIT, file = "Data/tapir-iso.rda")  # save

# Load model fit results
load("Data/tapir-iso.rda")
summary(FIT)


###############################
# INTEGRATED RESOURCE SELECTION FUNCTION (iRSF)
###############################

# AKDE (without RSF)
AKDE <- akde(DATA, FIT, weights = TRUE)  # wAKDE

# Plot AKDE with tree cover base map
plot(DATA, error = 2, UD = AKDE, R = treecover, col.grid = NA, main = "AKDE")
## independent of covariates (not considering preference for/against tree cover)

# Fit IID model for comparison
IID <- ctmm.fit(DATA, CTMM = ctmm(isotropic = TRUE))
KDE <- akde(DATA, IID)  # regular KDE

help("rsf.fit")

# Raster covariates must be in a named list
R <- list(tree = treecover)
## See raster::as.factor() for categorical variables

# Assigned weights without autocorrelation (IID model)
plot(DATA$timestamp, mean(KDE$DOF.area) * KDE$weights,  # subtracted 1 from the mean
     xlab = 'time', ylab = "weight", ylim = c(0,1.2),
     main = "Uniform Weights")

# How many points do you need for an IID RSF estimate?

# iRSF without autocorrelation: iterates until the default 1% error threshold
RSF.IID <- rsf.fit(DATA, KDE, R = R)
## assuming independently sampled data (sampling "available" points)
## verbose shows change in log likelihood and betas
save(RSF.IID, file = "Data/tapir_rsf-iid.rda")  # save

load(file = "Data/tapir_rsf-iid.rda")  # load saved IID RSF (assumes independence)
summary(RSF.IID)

# Assigned weights with autocorrelation
plot(DATA$timestamp, mean(AKDE$DOF.area) * AKDE$weights, 
     xlab = 'time', ylab = "weight", main = "Autocorrelation-Informed Weights")
# How many points do you need for a autocorrelation-weighted RSF estimate?
## points near gaps have higher weights

# iRSF with autocorrelation: iterates until the default 1% error threshold
tictoc::tic()
RSF <- rsf.fit(DATA, AKDE, R = R)
## Monte-Carlo integration used by default (`integrator = "MonteCarlo"`)
tictoc::toc()  # 107.488 sec elapsed

# If you don't have a time-dependent model, integrator="Riemann" is much faster
RSF <- rsf.fit(DATA, AKDE, R = R, integrator = "Riemann")
## no reason to use Monte-Carlo integration (only if you want to integrate once per point in time)

summary(RSF)
## effective sample size of ~11

help("rsf.select")


#################################
# MODEL SELECTION ACROSS COVARIATES
#################################

# If you include interaction terms in your formula, you must first standardize your data

# rsf.select() can do model selection on multiple predictors
RSFS <- rsf.select(DATA, AKDE, R = R, formula = ~I(sqrt(tree)) + tree + I(tree^2),
                   integrator = "Riemann", verbose = TRUE, 
                   trace = FALSE)  # keep trace = TRUE to track the progress
summary(RSFS)

# Keep selected model
RSF <- RSFS[[1]]
summary(RSF)  # ~3.5 selection strength for tree cover

treecover # 0-1 valued

# Relative selection of tree cover versus no tree cover
exp( summary(RSF)$CI[1,] * (sqrt(1)-sqrt(0)) )  # exponentiate

# Could get a population mean of RSF estimates
## if you had more individuals and more significance (and transferable models)
help("mean.ctmm")


##################################
# WHICH METHOD TO USE?
##################################

# "Availability model" Gaussian distribution for home range selection is fit at the same time as the RSF
# Estimators correspond to two diff orders of selection 
## (Gaussian for home range selection, RSF for tree cover)

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
# * RSFs directly output utilization-distribution (UD) information (resource utilization and space utilization),
#     SSF selection parameters have a different meaning, and their UD is non-trivial


###############################
# VISUALIZING HABITAT SUITABILITY
###############################

# The iRSF distribution that was fit
help("agde")

# Autocorrelated Gaussian-distributed, RSF-informed home range
AGDE <- agde(DATA, RSF, R = R)
## note the finite available area that was estimated

plot(DATA, AGDE, main = 'iRSF')
## actual model fitted

# Calculate suitability maps
help("suitability")

# Calculate suitability raster from rsf.fit/rsf.select
SUIT <- suitability(DATA, CTMM = RSF, R = R, grid = AKDE)
names(SUIT) # raster brick with 3 layers (lower, point estimate, upper)

# Suitability maps
raster::plot(SUIT)  # plot the suitability point estimate and CIs

# Plot telemetry data with suitability base map
plot(DATA, error = 2, R = SUIT[['est']], col.grid = NA, main = "Suitability")
## only plotting the telemetry and suitability point estimate


##########################
# HABITAT-INFORMED HOME-RANGE ESTIMATION
##########################

# RSF-informed AKDE
help('akde')

# AKDE distribution informed by RSF and movement model
RAKDE <- akde(DATA, RSF, R = R, weights = TRUE)  # kernel density dependence
plot(DATA, error = 2, UD = RAKDE, col.grid = NA, main = "iRSF-AKDE")

## NOTE: you can also add boundaries at the kernel level

