###########
# RANGE VERSUS OCCURRENCE DISTRIBUTIONS
# https://doi.org/10.1101/2022.09.29.509951
###########

library(ctmm)
data(buffalo)
projection(buffalo) <- median(buffalo)
DATA <- buffalo$Cilla
load("data/cilla.rda")

FITS <- list("OUF anisotropic"=FIT)
# include Brownian motion models
FITS[["BM"]] <- ctmm.fit(DATA,ctmm(tau=Inf,isotropic=TRUE))
# this one is not as commonly used, but let's throw it in
FITS[["BM anisotropic"]] <- ctmm.fit(DATA,ctmm(tau=Inf))

# you can't compare stationary (IID,OU,OUF) and conditionally stationary (BM,IOU) models with likelihood
summary(FITS)
# but you can compare within
summary(FITS[c("BM","BM anisotropic")])

SVF <- variogram(DATA,CI="Gauss")

# again, the selected model looks okay
zoom(SVF,FITS[[1]])

# the Brownian motion model looks...
zoom(SVF,FITS$BM)
# why? zoom in

# range distribution - using the selected model
RD <- akde(DATA,FITS[[1]])

# occurrence distribution - using the selected model
OD <- occurrence(DATA,FITS[[1]])

# conventional (non-dynamic) Brownian bridge
BB <- occurrence(DATA,FITS$BM)

# plot them
EXT <- extent(list(DATA,OD,RD))
plot(RD,col.level=NA,col.grid=NA,ext=EXT)
title("OUF AKDE")
# plot OUF occurrence distribution
plot(OD,col.level=NA,ext=EXT)
title("OUF Krige")
# plot BM occurrence distribution (BB)
plot(BB,col.level=NA,ext=EXT)
title("BM Krige (BB)")

# Q: What is the occurrence distribution?
# A: Given a random time *in the sampling period*, where was the animal

# Q: What is the range distribution?
# A: At some time in the future/past *under the same behaviors* where will the animal be
# A: Long-term space use *for continuing behaviors*

# Impact of coarsening the data
SUB <- DATA

#########################
# remove every other time
#########################
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]
par(mfrow=c(1,2))
RD <- akde(SUB,FITS[[1]])
OD <- occurrence(SUB,FITS[[1]])
plot(RD,col.level=NA,col.grid=NA,ext=EXT)
title("Range distribution")
plot(OD,col.level=NA,ext=EXT)
title("Occurrence distribution")
#########################

# repeat the above until they look similar
# how much data when they look similar?
nrow(DATA)
nrow(SUB)

# Impact of truncating the data
SUB <- DATA

####################################
# remove the second half of the data
####################################
SUB <- SUB[1:round(nrow(SUB)/2),]
par(mfrow=c(1,2))
RD <- akde(SUB,FITS[[1]])
OD <- occurrence(SUB,FITS[[1]])
plot(RD,col.level=NA,col.grid=NA,ext=EXT)
title("Range distribution")
plot(OD,col.level=NA,ext=EXT)
title("Occurrence distribution")
####################################

# repeat the above
par(mfrow=c(1,1))

# range area = predicted space use, given the same behaviors (biological)
# occurrence area = uncertainty (sampling dependent and limited to the sampling period)
# neither estimate the amount of space used during the sampling period!!!
