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

# ctmm MoveApps
browseURL("https://www.moveapps.org/apps/browser?q=ctmm")


###################
# IMPORT DATA
###################

# Load the ctmm package
library(ctmm)

# STEP 1: Get data through MoveBank
# STEP 2: Import data with as.telemetry()
help("as.telemetry")

# Load data from Movebank CSV (which can be compressed)
Buffalo <- as.telemetry("Data/Kruger African Buffalo, GPS tracking, South Africa.zip")
## You can also import from a move object, data.frame, etc.
## GPS location error can be an issue if greater than the sampling timesteps

# Load buffalo dataset from ctmm
data(buffalo)  # this data is available within the package
# help("buffalo")  # information on this data can be found at its help page

# This is a list of buffalo telemetry objects 
class(buffalo)  # all listed objects are telemetry objects

# Number of buffalo datasets
length(buffalo)  # There are 6 individuals

class(buffalo[[1]])  # ctmm telemetry object
head(buffalo[[1]])

# Names of buffalo
names(buffalo) 

# Summary of buffalo data
summary(buffalo)
## Note: 1 hour sampling for all individuals except Pepper's collar malfunctioned (2 hours)


###################
# VISUALIZE TELEMETRY DATA
###################

help("plot.telemetry")

# Plot all buffalo
plot(buffalo, main = "6 African buffalo")  # telemetry tracking data for all 6 indivs
## but they are all the same color

# Plot buffalo with list-sorted rainbow of colors
COL <- rainbow(length(buffalo))
plot(buffalo, col = COL, main = "Rainbow colors")  # rainbow by default
## includes color function, which can color by individual 
## (spatially closer indivs are more colorly distinct)

# Plot buffalo with spatially-separated rainbow of colors
COL <- color(buffalo, by = 'individual')
plot(buffalo, col = COL, main = "Spatial color separation")

# Many other built in coloring options for telemetry objects
help("color")
## You can color by sunlight, moonlight, season, time, ...


####################
# PROJECTIONS
####################

# What projection are the buffalo in?
projection(buffalo)
## You want a projection that is locally flat over your data (to minimize distortion).
## The further out in projection, the more distorted (better at the focal point)
## Only local projections are good, unless working w/ remote sensing data (project it to that data)
## By default, as.telemetry() will choose a two-point equidistant projection, which is
## safer for migratory species, but does not preserve North = up.

# The algorithm can be found in:
ctmm:::median_longlat
## and automates the estimation of k = 2 geometric median (robust) clusters

# Geometric median of data
median(buffalo)
# Object of class "telemetry"
# longitude  latitude          x          y
# 31.82024  -24.73712   8897.906  -2470.149

# Show north on plot (puts North facing to the side due to foci on horizontal)
compass()

# Center the projection on the geometric median of the data
projection(buffalo) <- median(buffalo)

projection(buffalo)  # new projection centered on geometric median of telemetry data

# Changes the method so that North is now up
plot(buffalo, col = COL, main = "Azimuthal-equidistant projection")
compass()


###################
# VARIOGRAM
###################

# Names of buffalo
names(buffalo)

# Select buffalo Cilla
DATA <- buffalo$Cilla

# Plot telemetry object
COL <- color(DATA, by = 'time')  # color by time (easier to see migrations/dispersals)
plot(DATA, col = COL)
## easier to see migrations/dispersals
## range resident (stays in area for the entire sampling period)

# Calculate a variogram object (named SVF) from the telemetry object
SVF <- variogram(DATA)
plot(SVF, main = "Variogram", level = c(0.5,0.95))
# on average how far apart (in distance^2) given a time lag between any two points
## spatial autocorrelation across time (time-lag on x-axis instead of distance)
## variance: asymptote of variogram
## time to independence: time to reach asymptopte (no autocorrelation when it asymptotes)
## Can also see how long it takes the buffalo to cross its home range

# Documentation for variogram function
help("variogram")
## there are some options in here if you have very irregular data: fast, dt, res

# Vignette/tutorial
vignette('variogram')
## See section "Irregular Sampling Schedules"

# More accurate confidence intervals (CIs)
SVF <- variogram(DATA, CI = "Gauss")
## (n^2)log(n) algorithm for how long it takes to run
## too slow for larger datasets, not good on very irregular datasets

plot(SVF, main = "Cilla's Variogram with more accurate CIs")

# frequently you want to zoom in to the beginning of the variogram
# Plot with zoom slider
zoom(SVF, main = "Variogram with good CIs", level = c(0.95,0.96))  # Useful zoom to see nugget effect

# Here are some details to look out for when checking our variogram:
## Is there are asymptote?
## How long does it take for the variogram to asymptote?
## Is there an initial curvature or is it linear? 
### -- DIFFUSION (amore later) of square distance/time
### -- initial quadratic curvature (if clean, finely sampled data) = mean speed of animal (or square of mean speed of animal)
## possible nugget effect if location errors are not correlated (would give nugget)
## want nugget = 0

# How many square km away at a time-lag of a month did the buffalo get away?
## Asymptotes at about 0.5 month -- autocorrelated data until ~0.5 months
## the longer you wait, the further the animal moves until asymptote (corresponds to home range scale)
## straight line increase corresponds to animal speeds

## Variogram is unbiased estimate

# Show variogram with ACF for residuals of IID model
IID <- ctmm.fit(DATA)
RES <- residuals(DATA, IID) # extract residuals
ACF <- correlogram(RES, res = 10)  # correlogram more robust than acf function
## Ideally, we want the residuals to drop down to 0 autocorrelation immediately
## Biased estimator if there's autocorrelation
## Shows autocorrelation variogram w/ where 0 autocorrelation and 95% CI are
### essentially an upside-down variogram on diff scale

zoom(ACF)  # plot ACF 
## for testing if there's autocorrelation (biased estimator though)
## Fourier transform from time to frequency (per time) gives correlogram

# Periodogram for autocorrelation
LSP <- periodogram(DATA)
plot(LSP)   # across different timescales
## Looking for periodic movement patterns (autocorrelated movement)
## can see patterns in their movement periodicity 
## (e.g., albatross hunts once/month = spike in activity around 1 month)

# Less common in movement ecology are spectrograms
## heat map of energy (intensity) plotted on freq ~ absolute time scale


###################
# MODEL SELECTION
###################

# Model guesstimate function
help("ctmm.guess")
# variogram will be calculated automatically (with default arguments)

# Fit autocorrelation model to variogram 
## Interactive mode
ctmm.guess(DATA, variogram = SVF)  # automates guesses for parameters
## See the zoom gear for parameter estimates: variance (km^2), tau_p (position, day), tau_v (velocity, min)

## timescale for autocorrelation in position (tau_p): how long it takes animal to cross range (i.e. home range if range resident)
### -- time taken to asymptote
## timescale for autocorrelation in velocity (tau_v): how much time is animal going in same direction at same speed (straight line movement time)
### -- changes linearity of initial curvature (i.e. coursely-sampled data would be linear, can't see animal's finite speed)
### -- Brownian motion model is okay for course data (fractal movement), but finite speeds models are better if finer data

# Fit autocorrelation model to variogram 
## Non-interactive mode
GUESS <- ctmm.guess(DATA, interactive = FALSE)

# Automated model selection
help("ctmm.select")

# Fit a bunch of autocorrelation models
## 10x more data = 10x longer to run (computation time proportional to sample size)
FITS <- ctmm.select(DATA, GUESS, trace = 3, verbose = TRUE, cores = -1)
## Candidate models: OUF, OUf, OUΩ, IOU, BM, IID, inactive
## verbose = TRUE --> returns all models (only top models returned by default)
## cores = -1 --> use all computer cores but 1 (only for Unix-based systems)

# We have already run this code for you (slowest step of analysis)
# save(FITS, file = "Data/cillas.rda")  # store results

load("Data/cillas.rda")  # All autocorrelation models stored

# Let's look at the results
summary(FITS)
## OUF: model with position autocorrelation AND velocity autocorrelation
## Anisotropic: distribution can be elongated
## Lower AICc is better (AICc increased as features/parameters were dropped --> nested models)

# IID was not attempted because the nested-model hierarchy is OUF -> OU -> IID
## but let's include the IID models to see how they compare
# Independent and identically distributed (IID) model (assumes no autocorrelation)
FITS[["IID anisotropic"]] <- ctmm.fit(DATA)  # anisotropic version
FITS[["IID"]] <- ctmm.fit(DATA, ctmm(isotropic = TRUE))  # isotropic version

# Now including IID model
summary(FITS)

# Lets look at individual models
summary(FITS$`IID anisotropic`)  # IID  anisotropic model
## CIs are pretty narrow
## DOF = effective sample size
## DOP = dilution of precision, gives error estimates for each point

# Compare mean and covariance to data
zoom(DATA, FITS$`IID anisotropic`, main = "IID Gaussian Distribution")

# Compare empirical variogram to that of the IID model
zoom(SVF, FITS$`IID anisotropic`, main = "IID Variogram")
# Non-overlapping CIs is bad
## IID model poorly fits data

# Calculate model residuals
RES <- residuals(DATA, FITS$`IID anisotropic`)

# Scatter plot of residuals
plot(RES, main = "IID Residuals")
## Residuals not normally distributed

# Calculate correlogram of residuals
ACF <- correlogram(RES, res = 10)
## res = 10 is for drifting sampling rate (increased time intervals)
## Alternatively, fast = FALSE

zoom(ACF, main = 'ACF of "IID" Residuals')
## RE: red bands for 95% CI for no autocorrelation

# The first model is the selected model (lowest AICc)
summary(FITS)

# The selected OUF anisotropic model
summary(FITS[[1]])
## Note DOF for different parameters: mean, area, diffusion, speed
## Estimates and CIs: Gaussian area, Gaussian RMS speed, diffusion rate

# 95% location autocorrelation remaining
exp(-3)
summary(FITS[[1]])$CI[2,] * 3  # position autocorrelation estimate: 7.504067
## position autocorrelation disappears in ~22.5 days

summary(DATA)
## Cilla sampled for ~5 months w/ position autocorrelation point estimate at ~1 week

# `ctmm` automatically gives nice round SI units for timescales
(4.967566 %#% 'months') / (7.505372 %#% 'days')
## = ~20, close to effective sample size (~18)

help("%#%")  # converts dimensional quantities to an from SI units
help(sigfig)  # gives nice significant figures

# How long would it take for there to be <5% position autocorrelation?
sigfig(summary(FITS[[1]])$CI[2,] * 3)

plot(DATA, FITS[[1]], main = "Anisotropic Gaussian")  # anisotropic
plot(DATA, FITS[[2]], main = "Isotropic Gaussian")  # isotropic
## ERROR: multiple projections not yet supported

zoom(SVF, FITS[[1]], main = "OUF Variogram")
## Not perfect, but much better

Residuals
RES2 <- residuals(DATA, FITS[[1]])

plot(RES2, main = "OUF Residuals")
## Residuals look better (less clear patterning/structure)

# Residual ACF
ACF2 <- correlogram(RES2, res = 10)

zoom(ACF2, main = 'ACF of "OUF" Residuals')
## Most of autocorrelation is explained by model

# You can do this process well by hand
ctmm.guess(DATA, variogram = SVF)
## This is a stationary model (parameters don't change with periods of time/activity)
## - ideally need one stationary model per behaviour (movement behaviour)

# Why is this model fit deflected down?
plot(SVF, FITS$`OU anisotropic`, main = 'ACF of "OU" Residuals') 
## This model doesn't fit well at all, wrong type of autocorrelation model
## Brownian motion for short periods of time under OU model,
## but buffalo has persistence of motion (empirical variogram has initial quadratic shape)

######################
# Non-resident models
######################

# Non-resident models
ctmm.guess(DATA, ctmm(range = FALSE)) # `range = FALSE` for non-resident indivs
## fits very well for first few days, but much worse at longer timescales

GUESS2 <- ctmm.guess(DATA, ctmm(range = FALSE), interactive = FALSE)

FITS2 <- ctmm.select(DATA, GUESS2, verbose = TRUE, trace = 1)
## Remember that this is the slow step of the analysis

summary(FITS2)
zoom(SVF, FITS2[["IOU anisotropic"]])
zoom(SVF, FITS2[["BM anisotropic"]])
## Generally don't need these models (3 fewer parameters)
### Can be useful if very little data and have 3 less useful parameters

# Likelihoods/AICs cannot be compared
summary(c(FITS, FITS2))
## Can use a specific likelihood model validation method (lead-one-out cross validation, LOOCV)
## see help("ctmm.select") IC = "LOOCV" argument for tiny tracks
## Might have to manually judge the model fit


################
# TEASER
################

# Simulate data from the selected model with same times (same timescale)
SIM <- simulate(FITS[[1]], t = DATA$t)  # not conditioned on the data
## Simulated utilization distribution estimation

# Plot simulated data
plot(SIM, main = "Cilla Simulacrum")
# What areas does this individual like/dislike? 
## Generally areas where there are more data points, but RE: spatial autocorrelation in data
## diffusion and speed data more trustworthy
## Gaussian model

# SPOILER
plot(SIM, FITS[[1]], level = NA) # did not set seed so it'll be random
