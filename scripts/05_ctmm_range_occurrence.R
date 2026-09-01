###########
# RANGE VERSUS OCCURRENCE DISTRIBUTIONS
# https://doi.org/10.1101/2022.09.29.509951
###########

# Load ctmm package
library(ctmm)


#####################
# IMPORT AND VISUALIZE DATA
#####################

# Import data
data(buffalo)
projection(buffalo) <- median(buffalo)  # center projection on geometric median

# Let's focus on the first buffalo Cilla
DATA <- buffalo$Cilla


# Q: What is the occurrence distribution?
# A: Given a random time *in the sampling period*, where was the animal

# Q: What is the range distribution?
# A: At some time in the future/past *under the same behaviors* where will the animal be
# A: Long-term space use *for continuing behaviors*

####################
# OCCURRENCE DISTRIBUTION
####################

# Load her model selection results
load("Data/cilla.rda")

# Let's store the fitted movement models in a list
FITS <- list("OUF anisotropic" = FIT)  # selected OUF anisotropic model

# Include Brownian motion models

# Isotropic Brownian motion
FITS[["BM"]] <- ctmm.fit(DATA, ctmm(tau = Inf, isotropic = TRUE))  
# Anisotropic Brownian motion
## Note: this one is not as commonly used, but let's throw it in
FITS[["BM anisotropic"]] <- ctmm.fit(DATA, ctmm(tau = Inf))
# square distance moving away proportional to time taken
## fractal/infinite movement path (most appropriate for coarser data)
## doesn't stay w/in HR (diffuses forever), no more appropriate if not necessarily range resident

# Cannot compare stationary and conditionally stationary models with likelihood
summary(FITS)
# but you can compare within
summary(FITS[c("BM", "BM anisotropic")])

SVF <- variogram(DATA, CI = "Gauss")  # fit variogram

# Again, the selected model looks okay (within CIs)
plot(SVF, FITS[[1]])
zoom(SVF, FITS[[1]], frac = 0.01)  # zoomed in

# However, the Brownian motion model fits poorly
plot(SVF, FITS$BM)
# why? zoom in (doesn't asymptote, keeps diffusing infinitely, and no initial quadratic (not ballistic))
zoom(SVF, FITS$BM, frac = 0.01)

# Conventional (non-dynamic) Brownian bridge (not the selected model)
BB <- occurrence(DATA, FITS$BM)

# occurrence distribution - using the selected model
OD <- occurrence(DATA, FITS[[1]])  
## Negatively biased, biases too small for HR, but still better than the BM model

# Let's visualize the two occurrence distributions
par(mfrow = c(1,2))  # arrange plots into a 1x2 grid (row of 2)
EXT <- extent(list(DATA, BB, OD))  # make extent the largest of the diff distributions

# Plot BM occurrence distribution (BB)
## Assumes buffalo moves randomly btwn any 2 points (biases small, only good for super coarse data)
plot(BB, col.level = NA, ext = EXT, main = "BM Krige (BB)")  

# Plot OUF occurrence distribution
## Would estimate buffalo to be somewhere along the track (especially for finely sampled data)
plot(OD, col.level = NA, ext = EXT, main = "OUF Krige")  


########################
# RANGE DISTRIBUTIONS
########################

# Range distribution
RD <- akde(DATA, FITS[[1]])  # using the selected model

# Let's visually compare the data and 3 distributions
par(mfrow = c(2,2))  # arrange plots into a 2x2 grid
EXT <- extent(list(DATA, OD, RD))  # make extent the largest of the diff distributions

# Plot the raw telemetry data
plot(DATA, ext = EXT, main = "Raw Location Points")

# Plot BM occurrence distribution (BB)
## Assumes buffalo moves randomly between any 2 points (biases small, only good for coarse data)
plot(BB, col.level = NA, ext = EXT, main = "BM Krige (BB)")  

# plot OUF occurrence distribution
## Would estimate buffalo to be somewhere along the track (especially for finely sampled data)
plot(OD, col.level = NA, ext = EXT, main = "OUF Krige")  

# Plot OUF range distribution
## Fuzzy and large due to small effective sample size
plot(RD, col.level = NA, col.grid = NA, ext = EXT, main = "OUF AKDE")  

# Overlay them (`add = TRUE` plots on top of previous)
par(mfrow = c(1,1))  # return to single plot grid
plot(RD, col.level = NA, col.grid = NA, ext = EXT, col.UD = "blue")  # OUF range
plot(BB, col.level = NA, ext = EXT, col.UD = "red", add = TRUE)  # BM occurrence (BB)
plot(OD, col.level = NA, ext = EXT, col.UD = "green3", add = TRUE)  # OUF occurrence
title("Overlayed Distributions")


#########################
# SAMPLING DEPENDENCY
#########################

# SAMPLING INTERVAL
# Impact of COARSENING the data
SUB <- DATA

### Repeat the code chunk below ###
#########################

# Remove every other time
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]  # 2-hr sampling

# Fit occurrence and range distributions
RD <- akde(SUB, FITS[[1]])
OD <- occurrence(SUB, FITS[[1]])

par(mfrow = c(1,2))  # plot both side-by-side

plot(RD, col.level = NA, col.grid = NA, ext = EXT)  # range distribution
title("Range distribution")
plot(OD, col.level = NA, ext = EXT)  # occurrence distribution
title("Occurrence distribution")

#########################
# repeat the above until they look similar

par(mfrow = c(1,1))

# how much data when they look similar?
nrow(DATA)
nrow(SUB)

####################################

# SAMPLING DURATION
# Impact of TRUNCATING the data
SUB <- DATA

### Repeat the code chunk below ###
#########################

# Remove the second half of the data
SUB <- SUB[1:round(nrow(SUB)/2),]  # cutting data in half

# Fit occurrence and range distribution
RD <- akde(SUB, FITS[[1]])
OD <- occurrence(SUB, FITS[[1]])

par(mfrow = c(1,2))  # plot both side-by-side

plot(RD, col.level = NA, col.grid = NA, ext = EXT)  # range distribution
title("Range distribution")
plot(OD, col.level = NA, ext = EXT)  # occurrence distribution
title("Occurrence distribution")

####################################
# repeat the above
par(mfrow=c(1,1))

# Range area = predicted space use, given the same behaviors (biological)
# Occurrence area = uncertainty (sampling dependent and limited to the sampling period)
# neither estimate the amount of space used during the sampling period!!!
