#############
# TELEMETRY ERROR MODELING
# https://www.biorxiv.org/content/10.1101/2020.06.12.130195v2.full
#############

#############
## STEP 0: Do you need to model error?
# How do the scales of error compare to the scales of movement?

#############
## STEP 1: Do you have calibrated data or calibration data?
# Calibration data can be collected or opportunistic.
# Without calibration data, you should supply a prior

#############
## IF YOUR DATA NEEDS CALIBRATION
## STEP 1B: What columns do you have in your data?
# DOP values? (HDOP, VDOP, PDOP, GDOP, ...)
# location classes?
# number of satellites?
# time-to-fix timeout?
# ctmm will try to pick out the best data

# load ctmm library
library(ctmm)

# load turtle data
data(turtle)
# or as.telemetry on the turtle data filename

##############
## IF YOUR DATA NEEDS CALIBRATION
## AND ESPECIALLY IF YOUR DATA HAVE NUMEROUS ERROR-RELATED COLUMNS
## STEP 1C: Error model selection

# turtle datasets
names(turtle)
# first two are calibration data - not turtles

# second look at columns
head(turtle[[1]])
# HDOP: horizontal dilution or precision -- proportional to RMS error
# location class:

## Assuming all of the information is good

help("uere.fit")

# fit error parameters to calibration data
UERE <- uere.fit(turtle[1:2])
# do not run uere.fit on tracking data

# estimated error model parameters
summary(UERE)

# apply error model to data
uere(turtle) <- UERE

head(turtle$F231)

plot(turtle$F231)

## If we aren't sure about the error data:
## QUESTION 1: Are the HDOP and location class values informative?
data(turtle)

# make a list to store error models
UERES <- list()

# first attempt: let's use everything
UERES$all <- uere.fit(turtle[1:2])
# do not run uere.fit on tracking data

# summarize error model
summary(UERES$all)

# second attempt: let's drop the location class information
# copy of calibration data
test <- turtle[1:2]
# delete location class column
test[[1]]$class <- NULL
test[[2]]$class <- NULL
uere(test) <- NULL
# store error-model fit (HDOP only)
UERES$HDOP <- uere.fit(test)

# summarize error model
summary(UERES$HDOP)

# third attempt: let's further drop the HDOP values
# delete HDOP column
test[[1]]$HDOP <- NULL
test[[2]]$HDOP <- NULL
# store error-model fit
UERES$nada <- uere.fit(test)

# summarize error model
summary(UERES$nada)

# compare error-models
summary(UERES)
# AICc: super-fancy AIC values
# reduced Z-squared statistic (goodness of fit)
# compare to reduced chi-squared statistic (1 is good)

## PROBLEM 2: Are these GPS tags identical?

# create a list to store individualized error models
indiv <- list()

# calculate individual UEREs
indiv[[1]] <- uere.fit(turtle[[1]])
indiv[[2]] <- uere.fit(turtle[[2]])

# compare calibration parameters
summary(UERES$all) # joint model
summary(indiv[[1]])
summary(indiv[[2]])

# store with joint models
UERES$indiv <- indiv

# compare to joint models
summary(UERES)

#############
# ERROR CALIBRATION
#############

# calibrate turtle data with best error model
uere(turtle) <- UERES$all

# error columns now in data
head(turtle[[1]])

#############
# ERROR-MODEL RESIDUALS
#############

# calculate residuals of calibration data w.r.t best error model
RES <- lapply(turtle[1:2],residuals)
# plot residuals
plot(RES)

# calculate residuals of calibration data w.r.t. worst error model
uere(test) <- UERES$nada

RES2 <- lapply(test,residuals)
plot(RES2)

#############
# ERROR-INFORMED MOVEMENT ANALYSIS
#############

# turtle data again
names(turtle)

# take female turtle 231
DATA <- turtle$F231

# plot data
plot(DATA)

help('outlie')

# look for outliers
OUT <- outlie(DATA)

plot(OUT)
# you may get other useful information here
head(OUT)

# good location estimates
GOOD <- OUT$speed < 0.05 # biological threshold

# take only good location estimates
DATA <- DATA[GOOD,]

# re-check
plot(DATA)
OUT <- outlie(DATA)
plot(OUT)

# create guesstimate interactively
ctmm.guess(DATA)
# * check the error box

# create guesstimate non-interactively
GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=TRUE),interactive=FALSE)
# new argument CTMM, which can contain extra parameters,
# here error=TRUE

# fit models
FITS <- ctmm.select(DATA,GUESS,verbose=TRUE,trace=3,cores=-1)
# verbose=TRUE returns all candidate models
# I've already run this code for you
# save(FITS,file="data/turtle.rda")
load("data/turtle.rda")

# look at all models
summary(FITS)

# look at best model
summary(FITS[[1]])

# compare to
summary(uere(DATA))

## Smoothing data for other packages (not ctmm)

help('predict',package="ctmm")

SMOOTH <- predict(DATA,FITS[[1]])

plot(DATA)
plot(SMOOTH)

SIM <- simulate(DATA,FITS[[1]])
plot(SIM)

## IF YOU DIDN'T HAVE CALIBRATION DATA, SUPPLY A PRIOR

# load un-calibrated datas
data(turtle)

# will need to match the class structure (2D,3D here)
summary(uere(turtle))

# supply point estimates
# 20-meter 2D error at HDOP=1
# 10-meter 3D error at HDOP=1
uere(turtle) <- c(20,10)

# extract calibration object
PRIOR <- uere(turtle)
# the default uncertainty when assigning numerical error is zero
summary(PRIOR)
PRIOR$DOF

# set DOF for wide credible intervals
PRIOR$DOF[] <- 2
summary(PRIOR)

# assign prior to data
uere(turtle) <- PRIOR

# automated guesstimate for calibrated data
GUESS <- ctmm.guess(turtle[[3]],CTMM=ctmm(error=TRUE),interactive=FALSE)
FIT.PRIOR <- ctmm.select(turtle[[3]],GUESS,trace=3,cores=-1)
# this will take a while, but comes out consistent
# save(FIT.PRIOR,file="data/turtle-prior.rda")
load("data/turtle-prior.rda")

summary(FIT.PRIOR)

# compare update to prior
summary(PRIOR)
