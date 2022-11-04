#############
# TELEMETRY ERROR MODELING
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

## PROBLEM 1: Are the HDOP and location class values informative?

# make a list to store error models
UERE <- list()

# first attempt: let's use everything
UERE$all <- uere.fit(turtle[1:2])
# do not run uere.fit on tracking data

# summarize error model
summary(UERE$all)

# second attempt: let's drop the location class information
# copy of calibration data
test <- turtle[1:2]
# delete location class column
test[[1]]$class <- NULL
test[[2]]$class <- NULL
uere(test) <- NULL
# store error-model fit
UERE$HDOP <- uere.fit(test)

# summarize error model
summary(UERE$HDOP)

# third attempt: let's further drop the HDOP values
# delete HDOP column
test[[1]]$HDOP <- NULL
test[[2]]$HDOP <- NULL
# store error-model fit
UERE$nada <- uere.fit(test)

# summarize error model
summary(UERE$nada)

# compare error-models
summary(UERE)
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
summary(UERE$all) # joint model
summary(indiv[[1]])
summary(indiv[[2]])

# store with joint models
UERE$indiv <- indiv

# compare to joint models
summary(UERE)

#############
# ERROR CALIBRATION
#############

# calibrate turtle data with best error model
uere(turtle) <- UERE$all

# error columns now in data
head(turtle[[1]])

#############
# ERROR-MODEL RESIDUALS
#############

# calculate residuals of calibration data w.r.t best error model
RES <- list()
RES[[1]] <- residuals(turtle[[1]])
RES[[2]] <- residuals(turtle[[2]])
# plot residuals
plot(RES)

# calculate residuals of calibration data w.r.t. worst error model
uere(test) <- UERE$nada

RES <- list()
RES[[1]] <- residuals(test[[1]])
RES[[2]] <- residuals(test[[2]])
plot(RES)

#############
# ERROR-INFORMED MOVEMENT ANALYSIS
#############

# turtle data again
names(turtle)

# take female turtle 231
DATA <- turtle$F231

# plot data
plot(DATA)

# look for outliers
OUT <- outlie(DATA)
plot(OUT)

# good location estimates
BAD <- OUT$speed > 0.05

# take only good location estimates
DATA <- DATA[!BAD,]

# re-check
plot(DATA)
OUT <- outlie(DATA)
plot(OUT)

# create guesstimate interactively
ctmm.guess(DATA)
# check the error box

# create guesstimate non-interactively
GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=TRUE),interactive=FALSE)
# new argument CTMM, which can contain extra parameters,
# here error=TRUE

# fit models
FITS <- ctmm.select(DATA,GUESS,verbose=TRUE,trace=3,cores=-1)
# verbose=TRUE returns all candidate models
# I've already run this code for you
# save(FITS,file="turtle.rda")
load("turtle.rda")

# look at all models
summary(FITS)

# look at best model
summary(FITS[[1]])

# compare to
summary(uere(DATA))

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
UERE <- uere(turtle)
# the default uncertainty when assigning numerical error is zero
summary(UERE)
UERE$DOF

# set DOF for wide credible intervals
UERE$DOF[] <- 2
summary(UERE)

# assign prior to data
uere(turtle) <- UERE

# automated guesstimate for calibrated data
GUESS <- ctmm.guess(turtle[[3]],CTMM=ctmm(error=TRUE),interactive=FALSE)
FIT.PRIOR <- ctmm.select(turtle[[3]],GUESS,trace=3,cores=-1)
# this will take a while, but comes out consistent

## what happens when error is not modeled
GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE)
# error=FALSE is default

# fit models with no location error
FIT.NO <- ctmm.select(DATA,GUESS,trace=3,cores=-1)

# compare
summary(FITS[[1]])
summary(FIT.NO)
