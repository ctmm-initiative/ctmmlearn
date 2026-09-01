#############################
# Speed, distance, diffusion
# https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-019-0177-1
#############################

# Load ctmm package
library(ctmm)

# Load buffalo dataset from ctmm
data(buffalo)

# Projection with North = up
projection(buffalo) <- median(buffalo)  # center on geometric median of locations

# Let's consider just first buffalo (Cilla)
DATA <- buffalo[[1]]

# Plot Cilla's tracks
plot(DATA, col = color(DATA, by = "time"),  # color by time
     main = "Cilla's tracks colored by time")

# Model selection
## Guesstimate autocorrelation parameters
# GUESS <- ctmm.guess(data = DATA, interactive = FALSE)  
# FIT <- ctmm.select(data = DATA, CTMM = GUESS)  # fit and select movement models
# save(FIT, file = "Data/Cilla.rda")  # save results

# Load model fits from ctmm.select
load("Data/cilla.rda")

# Remember our handy units operator?
help(`%#%`)  # conversion to SI units
?`%#%`  # this works too

1 %#% 'day'  # 1 day in seconds
1 %#% 'year'  # 1 year in seconds

# For the sake of time, we will demonstrate only on the first week of data
DATA <- DATA[DATA$t <= DATA$t[1] + 1 %#% 'week',]  # subset to first week

# Plot without errors (easier to see the location points)
plot(DATA, col = color(DATA, by = 'time'),  # color by time
     error = FALSE, main = "First week of tracking data")
## Less data so ctmm.select should fit relatively quicker

# Re-fit to first week only
FIT <- ctmm.select(DATA, FIT, trace = 1)
summary(FIT)$name  # OUF anisotropic was the best fit model

# Gaussian RMS speed from summary statistics
summary(FIT)
## contains Gaussian root-mean squared (RMS) speed -- proxy of speed
## mean speed: v^- = E[v] = 1/(tn-t1)*integral of speed over time
## RMS speed: v_rms = root(E[v^2])

# Calculate Gaussian (regular mean speed - not RMS)
speed(FIT, trace = TRUE)  # keep trace = TRUE to see the progress bar


#######################
# CTSD Speed Estimation
#######################

# Non-parametric speed estimation (CTSD: Noonan, et al., 2019)
speed(DATA, CTMM = FIT, trace = FALSE)
## This gives mean of mean speeds (from simulations)
## For species with very distinct movement behaviours, speed calculations should be segmented
### e.g., birds stationary/feeding on ground VS flying around
## CTSD speed is fairly insensitive to the effect of coarsening data


#########################
# SENSITIVITY ANALYSIS CASE STUDY
#########################

# Impact of coarsening the data
SUB <- DATA  # duplicate so we don't lose the original data
FIT.SUB <- FIT

### Repeat the code chunk below ###
#########################

# Remove every other time
## This coarsens the data by removing half of it, while maintaining the duration
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]
FIT.SUB <- ctmm.select(SUB, FIT.SUB, trace = FALSE)  # fits more quickly w/ less data
## the speed estimate here is RMS Gaussian
## won't get speed estimate if too coarse (resolution of data not high enough)

summary(FIT.SUB)  # wider CIs
## Gaussian (regular speed - not RMS)
## Note the diffusion rate

# Mean speed
speed(FIT.SUB, trace = FALSE)  
## similar estimates to finer sampled data, but more uncertainty (larger CIs)

# non-parametric speed estimation
speed(SUB, FIT.SUB, trace = FALSE)  # fits more slowly w/ less data
## Note lower DOF[speed], effective sample size for speed estimation
## Movement model may switch when data is too coarse

#########################
# repeat until data become too coarse
## Can look instead at diffusion rate if data too coarse 
## Can also look at location error

# keep in mind the stationary assumption of the model
# see the appendix of Noonan et al.

###########################
# Population meta-analysis
###########################

help('meta')  # population meta-analysis for movement parameters

# Load in the fitted movement models for all 6 buffalo
load("Data/buffalo.rda")

# Estimate mean speed for each buffalo
SPEEDS <- list()  # empty list to store results
for(i in 1:length(buffalo)) {  # open for loop
  
  # Estimate mean speed and store in the list
  SPEEDS[[i]] <- speed(buffalo[[i]], FITS[[i]])  # this can be slow
}  # close for loop

names(SPEEDS) <- names(buffalo)  # match names to the buffalo

# save(SPEEDS, file = "Data/buffalo_speeds.rda")  # save the results

# Since it can be a slower process, let's load the saved file
load("Data/buffalo_speeds.rda")  # mean speed estimates for each buffalo

# Inspect the results for each individual by printing the `speeds` output
SPEEDS

# Speed meta-analysis
meta(SPEEDS, sort = TRUE)  # mean = population mean of mean speeds

# Diffusion rate meta-analysis
meta(FITS, variable = "diffusion", sort = TRUE)  # mean = population mean diffusion rate


###########################
# Instantaneous speeds
###########################

# Speed at a particular given time
INST_SPEEDS <- speeds(buffalo[[1]], FITS[[1]])  # Cilla's instantaneous speeds

head(INST_SPEEDS)  # inspect first 6 times (rows) of the output

# Distance = mean speed x total time
nrow(buffalo$Cilla)  # 3527 total tracked locations
mean_speed <- SPEEDS$Cilla$CI / 1 %#% "day"  # convert to km/s
mean_speed * (buffalo$Cilla$t[3527]-buffalo$Cilla$t[1])  # total distance traveled
## Ignore "speed (kilometers/day)" relic from SPEEDS object
