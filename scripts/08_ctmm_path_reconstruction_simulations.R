## PATH RECONSTRUCTION AND SIMULATIONS ##

########################
# IMPORT AND VISUALIZE DATA
########################

# Load ctmm package
library(ctmm)

# Load buffalo data from package
data(buffalo)
projection(buffalo) <- median(buffalo)  # center projection on median

names(buffalo)

# Subset to Cilla
DATA <- buffalo$Cilla
plot(DATA, col = color(DATA, by = "time"))


########################
# MODEL SELECTION
########################

# Autocorrelation model selection
GUESS <- ctmm.guess(DATA, interactive = FALSE)
FIT <- ctmm.select(DATA, GUESS, trace = 3)
save(FIT, file = "Data/cilla.rda")

# Load saved model selection results
load("Data/cilla.rda")


############################
# PREDICT
############################

# Take the first 10 locations (first 10 hrs)
SUB <- DATA[1:10,]

# Plot subset
plot(SUB, ylim = c(-26500,-21000))
# If working with this amount of data, you might consider alternative ICs
## in particular IC = "LOOCV"
help("ctmm.select")

# Convenient conversion function (help("%#%"))
1 %#% 'hr'

# Make an array of times over the same period, but 5 min apart
SEQ <- seq(from = SUB$t[1], to = SUB$t[10], by = 5 %#% 'min')

help('predict.ctmm')  # makes conditional predictions based on movement/location data

# Predict locations at those times
PRED <- predict(SUB, FIT, t = SEQ)  # predict for every 5 mins
## can allow smoothing for better location estimates 
## (for when data points have greater or lesser uncertainty)

# Plot predictions & data
plot(list(PRED, SUB), col = c('blue','red'), ylim = c(-26500,-21000))
## uncertainty ellipses

# Plot most-likely path
plot(list(PRED, SUB), col = c('gray','red'), ylim = c(-26500,-21000))
plot(PRED, error = FALSE, type = "l", col = "blue", add = TRUE)


#######################
# SIMULATE
#######################

help('simulate.ctmm')  # simulate things freely or conditional on data

#######################
# CONDITIONAL SIMULATIONS
#######################

# 1 minute sequence
SEQ <- seq(from = SUB$t[1], to = SUB$t[10], by = 1 %#% 'min')

# Simulate locations at those time (simulation 1)
SIM <- simulate(SUB, FIT, t = SEQ)  # simulate path for every min

# Plot conditional simulation & data
plot(list(SIM, SUB), col = c('blue','red'), type = c('l','p'), 
     ylim = c(-26500,-21000))

# Simulate again (simulation 2)
SIM2 <- simulate(SUB, FIT, t = SEQ)

# Plot conditional simulation & data
plot(list(SIM, SIM2, SUB), col = c('blue','orange','red'), type = c('l','l','p'), 
     ylim = c(-26500,-21000))
## some variation in simulated trajectories based on the data

# We can also include parameter uncertainty
help('emulate')  # simulating movement model itself from posterior of estimate of movement model

# Include parameter uncertainty
SIM3 <- simulate(SUB, emulate(FIT, fast = T), t = SEQ)  
## emulate(FIT) for sample movement model to approximate unknown movement model

plot(list(SIM, SIM2, SIM3, SUB), 
     col = c('blue','orange','black','red'), type = c('l','l','l','p'), 
     ylim = c(-26500,-21000))


#####################
# GENERIC SIMULATIONS
#####################

# Position autocorrelation timescale (range crossing time)
tau_p <- 1 %#% 'day'

# Velocity autocorrelation timescale (directional persistence)
tau_v <- 1 %#% 'min'

# Spatial variance
## Note the same as the 95% HR area
## 95% HR area = -2*log(0.05)*pi*sig
sig <- 1 %#% 'kilometers^2'

# Specify an OUF model for simulation
MODEL <- ctmm(tau = c(tau_p, tau_v), isotropic = TRUE, sigma = sig)

# Define the sampling duration/schedule
t <- seq(0,1 %#% 'month', 1 %#% 'hr')  # 1-hour sampling over 1 month
length(t)  # 709 location points to simulate

# Simulate from the defined model and sampling schedule
SIM <- simulate(MODEL, t = t)

# Fit the movement models
GUESS <- ctmm.guess(SIM, interactive = FALSE)
FITS <- ctmm.select(SIM, GUESS, cores = -1)

# Summary of the fit
summary(FITS)  # NOTE the DOFs (DOF[speed] = 0)
## We did not sample finely enough to capture the velocity autocorrelation timescale

# Let's try a finer sampling schedule
# Define the sampling duration/schedule
t <- seq(0,4 %#% 'days', 5 %#% 'min')  # 5-minute sampling over 4 days
length(t)  # 1153 locations points to simulate

# Simulate from the defined model and sampling schedule
SIM <- simulate(MODEL, t = t)

# Fit the movement models
GUESS <-  ctmm.guess(SIM, interactive = FALSE)

FITS <- ctmm.select(SIM, GUESS)  # this takes a long time
save(FITS, file = "Data/generic_simulation.rda")  # save

load(file = "Data/generic_simulation.rda")  # load saved model results

# Summary of the fit (NOTE the DOFs)
summary(FITS)

# Home range estimate based on the simulated locations
HR <- akde(SIM, FITS)

# Visualize the results
plot(SIM, UD = HR, main = "Simulation-Based Home-Range Estimate")


##########################
# COATI CASE STUDY
##########################

# Load in the dataset from ctmm
data("coati")
summary(coati)  # white-nosed coatis (Nasua narica)

projection(coati) <- median(coati)

# Subset to single individual
DATA <- coati[[1]]  # Let's use Aleja

# Visualize the data
plot(DATA, ylim = c(-1200,1650))
dt.plot(DATA)  # diagnose the sampling schedule
## There are gaps in the data

# Generate the variogram and initial parameter guesses
vg <- variogram(DATA)
GUESS <- ctmm.guess(DATA, variogram = vg, interactive = FALSE)

# Visually inspect the fit
plot(vg, GUESS)

# Model selection
FIT <- ctmm.select(data = DATA, CTMM = GUESS, trace = 3)
save(FIT, file = "Data/coati_aleja.rda")  # save

# Load saved model selection results
load(file = "Data/coati_aleja.rda")

# Summary of the fit
summary(FIT)  # OUF anisotropic model selected

# Simulate from the fitted model, with the original sampling times
SIM <- simulate(FIT, t = DATA$t)

# Visualize the results
plot(list(DATA, SIM), col = c("red", "blue"))

# Draw model parameters from the sampling distribution of the fitted model
FIT2 <- emulate(FIT, fast = TRUE)

# Simulate from the model
SIM2 <- simulate(FIT2, t = DATA$t)

# Visualize the results
plot(list(DATA, SIM, SIM2), col = c("red", "blue", "green4"))

# Fill in the gaps 
SIM3 <- simulate(DATA, CTMM = FIT, dt = 60)  # simulate for every 60 sec (1 min)

# Visualize the results
plot(list(DATA, SIM3), type = c("p", "l"), col = c("red", "blue"))

# Generate the most likely path (conditioned on the fitted model)
MLP <- predict(DATA, FIT, dt = 60)

plot(DATA, ylim = c(-1200,1650))
plot(MLP, error = FALSE, type = "l", col = "blue", add = TRUE)

# Export the simulations to work with other packages
SIM_EXPORT <- simulate(DATA, CTMM = FIT, complete = TRUE)

EXPORT <- SpatialPoints.telemetry(SIM_EXPORT)  # we can export this to other formats

class(EXPORT)  # `SpatialPoints` object for `sp` package

# Different formats/object types allow us to work with our data in other packages
mapview::mapView(EXPORT, alpha = 0.45)


