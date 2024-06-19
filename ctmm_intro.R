###################
# HELP
###################

# help files
help(package="ctmm")

# vignettes
browseVignettes(package="ctmm")

# FAQ
help("ctmm-FAQ",package="ctmm")

# ctmm user group for any questions or help
browseURL("https://groups.google.com/g/ctmm-user")

# issue reporting
browseURL("https://github.com/ctmm-initiative/ctmm/issues")

# ctmm learning material - where this script is from
browseURL("https://github.com/ctmm-initiative/ctmmlearn")

# ctmm manuscripts
browseURL("https://www.dropbox.com/sh/55ylq4rbm9pl4d9/AAC2WlRCfgQDYrVRpu5pgrfFa?dl=0")

# development branch of ctmm (more recent than CRAN)
remotes::install_github("ctmm-initiative/ctmm")

# what's new in ctmm
news(package="ctmm")

# ctmm point-and-click app - if you know anyone that doesn't user R
# remotes::install_github("ctmm-initiative/ctmmweb")
# ctmmweb::app()

###################
# IMPORT AND VISUALIZE
###################

#! load the ctmm package
library(ctmm)

# STEP 1: Get data through MoveBank
# STEP 2: Import data with as.telemetry()
help("as.telemetry")

# loading data from Movebank CSV (which can be compressed)
Buffalo <- as.telemetry('data/Kruger African Buffalo, GPS tracking, South Africa.zip')
# you can also import from a move object, data.frame, etc.

#! load buffalo dataset from ctmm
data(buffalo)
help("buffalo")

# this is a list of buffalo telemetry objects
class(buffalo)

# number of buffalo datasets
length(buffalo)

class(buffalo[[1]])
head(buffalo[[1]])

# names of buffalo
names(buffalo)

# summary of buffalo data
summary(buffalo)

###################
# PLOT TELEMETRY
###################

help("plot.telemetry")

# plot all buffalo
plot(buffalo,main="6 African buffalo")
# but all the same color

# plot buffalo with list-sorted rainbow of colors
COL <- rainbow(length(buffalo))
plot(buffalo,col=COL,main="Rainbow colors")

# plot buffalo with spatially-separated rainbow of colors
COL <- color(buffalo,by='individual')
plot(buffalo,col=COL,main="Spatial color separation")

# many other built in coloring options for telemetry objects
help("color")
# you can color by sunlight, moonlight, season, time, ...

####################
# PROJECTIONS
####################

# what projection are the buffalo in
projection(buffalo)

# You want a projection that is locally flat over your data (to minimize distortion).
# By default, as.telemetry() will choose a two-point equidistant projection, which is
# safer for migratory species, but does not preserve North=up.
# The algorithm can be found in:
ctmm:::median_longlat
# and automates the estimation of k=2 geometric median (robust) clusters

# show north on plot
compass()

#! center the projection on the geometric median of the data
projection(buffalo) <- median(buffalo)

projection(buffalo)

# now North=up, which is fine for this dataset
plot(buffalo,col=COL,main="Azimuthal-equidistant projection")
compass()

###################
# VARIOGRAM
###################

# names of buffalo
names(buffalo)

#! select buffalo Cilla
DATA <- buffalo$Cilla

# plot telemetry object
plot(DATA,main="Cilla")

# color by time
COL <- color(DATA,by='time')
plot(DATA,col=COL)
# easier to see migrations/dispersals

#! calculate a variogram object (named SVF) from the telemetry object
SVF <- variogram(DATA)
plot(SVF,main="Variogram")
# on average how far apart (in distance^2) given a time lag between any two points

# help file for variogram
help("variogram")
# there are some options in here if you have very irregular data:
#   fast, dt, res
vignette('variogram')
# Sec. "Irregular Sampling Schedules"

# more accurate CIs, too slow for larger datasets
SVF <- variogram(DATA,CI="Gauss")

# frequently you want to zoom in to the beginning of the variogram
# plot with zoom slider
zoom(SVF,main="Variogram with good CIs")
# things to look for
# * the asymptote (if any)
# * how long does it take to asymptote
# * initial curvature or initial linear?


###################
# MODEL SELECTION
###################

# model guesstimate function
help("ctmm.guess")
# variogram will be calculated automatically (with default arguments)
# this is interactive mode
ctmm.guess(DATA,variogram=SVF)
# notice how much work I spent automating the units of every plot

# this is noninteractive mode
GUESS <- ctmm.guess(DATA,interactive=FALSE)

# automated model selection
help("ctmm.select")

# fit a bunch of models, tell me what models are being fit, return all models, and use all but one CPU core
FITS <- ctmm.select(DATA,GUESS,trace=3,verbose=TRUE,cores=-1)
# candidate models: OUF, OUf, OUΩ, IOU, BM, IID, inactive
# I've already run this code for you
# save(FITS,file="cillas.rda")
load("data/cillas.rda")

# lets look at the results
summary(FITS)

# IID was not attempted because the nested-model hierarchy is OUF -> OU -> IID
# so let's include the IID models
FITS[["IID anisotropic"]] <- ctmm.fit(DATA)
FITS[["IID"]] <- ctmm.fit(DATA,ctmm(isotropic=TRUE))

# now including IID model
summary(FITS)

# lets look at individual models
# IID  anisotropic model
summary(FITS$`IID anisotropic`)

# compare mean and covariance to data
plot(DATA,FITS$`IID anisotropic`,main="IID Gaussian Distribution")

# compare empirical variogram to that of model
zoom(SVF,FITS$`IID anisotropic`,main="IID Variogram")

# calculate residuals
RES <- residuals(DATA,FITS$`IID anisotropic`)

# scatter plot of residuals
plot(RES,main="IID Residuals")

# calculate correlogram of residuals
ACF <- correlogram(RES,res=10)
# res=10 is for drifting sampling rate
# alternatively, fast=FALSE

zoom(ACF,main='ACF of "IID" Residuals')

# The first model is the selected model
summary(FITS)
# The selected OUF anisotropic model
summary(FITS[[1]])
# area here is Gaussian area
# speed here is Gaussian RMS speed

summary(DATA)
(4.967566 %#% 'months') / (7.505372 %#% 'days')
help("%#%")

plot(DATA,FITS[[1]],main="Anisotropic Gaussian") # anisotropic
plot(DATA,FITS[[2]],main="Isotropic Gaussian") # isotropic

zoom(SVF,FITS[[1]],main="OUF Variogram")
# not perfect, but much better

# residuals
RES2 <- residuals(DATA,FITS[[1]])

plot(RES2,main="OUF Residuals")

# residual ACF
ACF2 <- correlogram(RES2,res=10)

zoom(ACF2,main='ACF of "OUF" Residuals')

# you can do well by hand
ctmm.guess(DATA,variogram=SVF)
# why is this model fit deflected down?
zoom(SVF,FITS$`OU anisotropic`,main='ACF of "OU" Residuals')

################
# TEASER
################

# simulate data from the selected model with same times
SIM <- simulate(FITS[[1]],t=DATA$t)

# plot data
plot(SIM,main="Cilla Simulacrum")
# what areas does this individual like/dislike?

# SPOILER
plot(SIM,FITS[[1]],level=NA)
