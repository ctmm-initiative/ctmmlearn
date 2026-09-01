## LOCATION ERROR AND OUTLIERS ##

# Before beginning: Do you need an error model?
# How do the scales of error compare to the scales of movement for your study species?
## Location error may account for more variation in tracked locations than the animal's actual movement
## We still recommend calibrating your telemetry data when possible

# Calibration data can be collected (i.e., leaving a device outside to collect locations untouched)
# Calibration data can be opportunistic (e.g., data collected after an individual dies and is not moved)

# RMS: Root-mean-square — statistic that is equivalent to standard deviation in 1 dimension
# DOP: Dilution of precision — this is proportional to root-mean-squared (RMS) error.
# HDOP: Horizontal dilution of precision — this is proportional to RMS horizontal error, recorded as location error with location class.
# VDOP: Vertical dilution of precision — relative measure of RMS vertical error.
# PDOP: Position dilution of precision — aggregates HDOP and VDOP.
# TDOP: Time dilution of precision — relative measure of RMS temporal error.
# GDOP: Geometric dilution of precision — aggregates PDOP and TDOP.
# UERE: User equivalent range error — location error after partial standardization using the DOP value
# Location class: This is how different devices will categorize/classify locations of different quality.


#########################
# IMPORT DATA
#########################

# Load ctmm package
library(ctmm)

# Load turtle data
data(turtle)  # or as.telemetry on the turtle data file

names(turtle)  # first two entries are calibration data - not turtles

# Closer look at columns
head(turtle[[1]])
# HDOP: horizontal dilution of precision (want proportionality constant close to 1)
# location class: 3D (or 2D)

# Higher DOP values mean there is relatively more uncertainty in where the animal was (more location error)
# Lower DOP values mean that there is relatively less uncertainty in the animal's position (less location error)


##############################
# FITTING AN ERROR MODEL TO CALIBRATION DATA
##############################

help("uere.fit")  # user equivalent range error

# Fit error parameters to calibration data (informs error model)
UERE <- uere.fit(turtle[1:2])
## Do NOT run uere.fit on tracking data (use calibration data, not moving turtles)

# Estimated error model parameters
summary(UERE)
## 3D fixes have ~7 at HDOP 1
## 2D fixes have ~ 30 at HDOP 1 (diff scales)

# Apply error model to data
uere(turtle) <- UERE

# Let's take a look at our turtle data now
head(turtle$F231) 

plot(turtle$F231, ylim = c(-650,2300))  
## Error circles at each tracked location accounting for calibration data


############################
# TEST WHICH ERROR DATA TO USE
############################

# Question 1: Which error measurements (e.g., HDOP, VDOP, location class) are the most informative?

# Re-load turtle data
data(turtle)

# Make a list to store error models
UERES <- list()

# Attempt 1: let's use everything
UERES$all <- uere.fit(turtle[1:2])
## Do NOT run uere.fit on tracking data

# Summarize Attempt 1 error model
summary(UERES$all)

# Attempt 2: let's drop the location class information
test <- turtle[1:2]  # copy of calibration data

# Delete location class column
test[[1]]$class <- NULL
test[[2]]$class <- NULL
uere(test) <- NULL

# Store error-model fit 
UERES$HDOP <- uere.fit(test)  # HDOP only

# Summarize Attempt 2 error model
summary(UERES$HDOP)  # removing location class data (only HDOP), ~10.6 m error

# Attempt 3: let's drop the HDOP values too

# Delete HDOP column
test[[1]]$HDOP <- NULL
test[[2]]$HDOP <- NULL

# Store error-model fit
UERES$nothing <- uere.fit(test)  # homoskedastic error model

# Summarize error model
summary(UERES$nothing)  # removing HDOP as well (large location error estimate)

# Compare error models
summary(UERES)  # AICc selects model with all error parameters
## But all have poor Z[red]^2 values because the HDOP values aren't great


# Question 2: Are these GPS tags identical?

# Create a list to store individualized error models
indiv <- list()

# Calculate individual UEREs for each GPS tag
indiv[[1]] <- uere.fit(turtle[[1]])  # tag 1
indiv[[2]] <- uere.fit(turtle[[2]])  # tag 2

# Compare calibration parameters
summary(UERES$all)  # joint model
## ~7m error for 3D, ~30m for 2D
summary(indiv[[1]])  # but pretty similar if fit separately
summary(indiv[[2]])

# Store individual models with joint models
UERES$indiv <- indiv

# Compare individual to joint models
summary(UERES)


#########################
# CALIBRATE TELEMETRY WITH ERROR MODEL
#########################

# Calibrate turtle data with best error model
uere(turtle) <- UERES$all  # assign error model to be the error of dataset

# Error columns now in data
head(turtle[[1]])
## also now includes a per-time error estimate (error circle)

# We can now see error in individual turtle movement
plot(turtle[[1]])  # calibration data (device not moved)
## a lot of the variation in the position of the device is explained by error


############################
# CHECKING MODEL WITH FIT WITH MODEL RESIDUALS
############################

# Calculate residuals of calibration data with respect to BEST error model
RES <- lapply(turtle[1:2], residuals)  # apply function over items in list

# Plot residuals from best model
plot(RES)  # residuals in red
## Bad error models would give heavy tail

# Calculate residuals of calibration data with respect to WORST error model
uere(test) <- UERES$nothing  # homoskedastic error model
RES2 <- lapply(test, residuals)

# Plot residuals from worst model
plot(RES2)


#############################
# ERROR-INFORMED MOVEMENT ANALYSIS
#############################

# Re-load turtle data again
names(turtle)

# Let's focus on female turtle 231
DATA <- turtle$F231

# Plot data
plot(DATA, ylim = c(-650,2300))


## FIND OUTLIERS

help('outlie')  # pick out outliers

# Look for outliers
OUT <- outlie(DATA)  # side effect plot 
## blue segment = highest speed (used to determine outliers), note larger red points

# Plot outlier information to see deviations
plot(OUT)

# Summary statistics with other useful info
head(OUT)  # error-informed speed and distance estimates

# Biological decision to reject all speeds > 5 cm/s for this wood turtle
MAX <- 5 %#% 'cm/s'  # '%#%' convenient units function
MAX  # 5 cm/s in SI units

# Alternatively
MAX <- 0.05 # in meters/second

# Keep good location estimates
GOOD <- OUT$speed < MAX  # keep only data where speeds are less than 5 cm/s
## This is the condition we are setting so that we can subset out data

# Take only good location estimates in out data
DATA <- DATA[GOOD,]  # subset data based on threshold condition

# Re-check data
plot(DATA, ylim = c(-450,420))
OUT <- outlie(DATA)
plot(OUT)


########################
# MODEL SELECTION WITH ERROR
########################

# Guesstimate interactively
ctmm.guess(DATA)
## Check the error box

# Guesstimate non-interactively
GUESS <- ctmm.guess(DATA, CTMM = ctmm(error = TRUE), interactive = FALSE)
## here error = TRUE

# Fit models with model selection
FITS <- ctmm.select(DATA, GUESS, verbose = TRUE, trace = 3, cores = -1)
## verbose = TRUE returns all candidate models
save(FITS, file = "Data/turtle.rda")  # save

# We've already run the model selection for you
load("Data/turtle.rda")

# Inspect all models
summary(FITS)
## Location error model turned on

# Best model
summary(FITS[[1]])
## updated error parameter estimates (still similar to what was fed in)

# Compare to prior model (error parameters from calibration)
summary(uere(DATA))

# Compare to movement model without error model 
GUESS <- FITS[[1]]
GUESS$error <- FALSE  # error is not included if set to FALSE
FIT.NE <- ctmm.fit(DATA, GUESS, trace = 2)  # fits quickly w/out error model turned on

summary(FITS[[1]])$CI  # with error model

summary(FIT.NE)$CI  # without error model
## Without error model, we assume all of the variation is explained by animal movement


#############################
# PREDICTING FROM MODELS THAT INCLUDE ERROR
#############################

help('predict', package = "ctmm")

# Smoothed data to update
SMOOTH <- predict(DATA, FITS[[1]])
## Removes some error because it feeds in the movement model

plot(DATA, ylim = c(-450,420))
plot(SMOOTH, ylim = c(-450,420))  # updated estimates of turtle locations
## uses error ellipses, not circles

# Simulate based on movement + error model
SIM <- simulate(DATA, FITS[[1]])
plot(SIM)


#############################
# NO CALIBRATION? SUPPLY A PRIOR
#############################

# Re-load the un-calibrated data
data(turtle)

# These data have two location classes: 2D & 3D
## need to match this class structure
summary(uere(turtle))  # guesses for error (10m)

# Supply point estimates (assign numbers to error)
## 20-meter 2D error at HDOP=1
## 10-meter 3D error at HDOP=1
uere(turtle) <- c(20,10)

# Extract calibration object
PRIOR <- uere(turtle)

summary(PRIOR)  # the default uncertainty when assigning numerical error is zero
PRIOR$DOF  # will also need to set DOF, currently infinite

# Set DOF for wide credible intervals
PRIOR$DOF[] <- 2  # Bayesian: error is worth 2 data points
summary(PRIOR)  # data calibrated w/ prior (much more uncertainty due to few data points)

# Assign prior to data
uere(turtle) <- PRIOR

# Automated guesstimate for calibrated data
GUESS <- ctmm.guess(turtle[[3]], CTMM = ctmm(error = TRUE), interactive = FALSE)
FIT.PRIOR <- ctmm.select(turtle[[3]], GUESS, trace = 3, cores = -1)
## This will take a while, but comes out consistent
save(FIT.PRIOR, file = "Data/turtle-prior.rda")

load("Data/turtle-prior.rda")  # load saved model selection results

summary(FIT.PRIOR)  # using prior and updating it

# Compare update to prior
summary(PRIOR)


