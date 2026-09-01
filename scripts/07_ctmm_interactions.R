# Methods that can be used for studying interactions between individuals
# This includes:
# - Home-range overlap
# - Encounter location distributions (CDE)
# - Pairwise distances
# - Proximity ratios
# - Encounter rates


# These analyses are conditional on fitted movement models and HR estimates
# (see: https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_akde.R)
library(ctmm)

# Load buffalo data
data(buffalo)
projection(buffalo) <- median(buffalo)  # center projection on median

# Load in saved model selection results and HR estimates
load("Data/buffalo.rda")  # fitted movement models (FITS)
load("Data/buffalo_akdes.rda")  # estimated HR areas (AKDES)

#-----------------------------------------------------
# Home-range overlap
#-----------------------------------------------------

# Do individuals share the same space?

# Relevant paper: https://doi.org/10.1111/2041-210X.13027
help("overlap")  # overlap of distributions 
## (measure of similarities btwn distributions as % overlap of PMF)

# Estimate HR overlap for all pairs
OVER <- overlap(AKDES)  # statistical measure of overlap with bias correction
## Things tend to appear more dissimilar, so overlap is underestimated w/out bias correction

# Generate an error because of incompatible grids (due to pixel by pixel calculation)
overlap(list(akde(buffalo$Pepper, FITS$Pepper), akde(buffalo$Queen, FITS$Pepper)))
## Do NOT do this!

# This works because HRs are estimated simultaneously (and consistently)
overlap(akde(list(buffalo$Pepper, buffalo$Queen), list(FITS$Pepper, FITS$Queen)))
## can also estimate all the AKDEs together first, then feed into overlap

# All overlaps
OVER

# Pairwise confidence intervals (CIs) 
OVER$CI["Pepper","Toni",]
OVER$CI["Queen","Toni",]

# Point estimates for HR overlap
OVER$CI[,,"est"]

# NOTE: Home-range overlap does not tell us if animals shared the same space at the same time


########################################
# ENCOUNTER LOCATION DISTRIBUTIONS (CDE)
########################################

# where encounters are expected to take place?
## assuming that individuals are moving independently when not encountering

# Relevant paper: https://doi.org/10.1111/2041-210X.13597
help("cde")  # estimated area of where animals are likely to encounter each other

# Plot the data and HR estimates
plot(buffalo[c("Pepper", "Queen")], UD = AKDES[c("Pepper", "Queen")],
     col = c("#e76f51", "#264653"), col.UD = c("#f4a261", "#2a9d8f"), 
     col.grid = NA, ylim = c(8000,80000))
## orange = Pepper, green = Queen

# Estimate the home range overlap
overlap(AKDES[c("Pepper", "Queen")])

# Estimate the conditional distribution of encounters (CDE) 
## where we expect majority of encounter to take place
CDE <- cde(AKDES[c("Pepper", "Queen")])  # can weight indivs separately

# Visualize the CDE
plot(buffalo[c("Pepper", "Queen")], UD = CDE,
     col = c("#e76f51", "#264653"), col.UD = "gold2", 
     col.grid = NA, ylim = c(8000,80000))

# CDE corresponds with areas where the individual home ranges overlap


##############################
# PAIRWISE METRICS
##############################

# Metrics that take time into account (paper coming)
help("proximity")  # ratio to determine if pairwise proximity differs from expectation
help("distances")  # estimate pairwise separation distances
help("difference")  # predict pairwise location differences, given data and ctmm models
help("midpoint")  # predict the midpoints between pairwise locations

##############################
# PAIRWISE DISTANCE
##############################

# Cilla and Mvubu telemetry data
plot(list(buffalo$Cilla, buffalo$Mvubu), col = c("red2", "blue"))
## red = Cilla, blue = Mvubu

# Pairwise separation distances
DISTS <- distances(buffalo[c("Cilla","Mvubu")], FITS[c("Cilla","Mvubu")])
## predicts distance btwn 2 individuals at given time

# First 6 entries
head(DISTS)  # dataframe with estimates and CIs at all timestamps
## Includes estimate of potential encounter events

# Visualize the separation distances
plot(DISTS$est ~ DISTS$timestamp, type = "l", col = "#5e548e",
     xlab = "Time", ylab = "Separation Distance (m)",
     main = "Cilla-Mvubu Pairwise Separation Distances")

# Internal plotting function [IN DEVELOPMENT]
ctmm:::ts.plot(DISTS)  # work in progress (not yet an exported function in ctmm)

# What would totally independent motion look like?
## Simulate independent motion (at same timestamps, without the original data)
cilla_sim <- simulate(FITS$Cilla, t = buffalo$Cilla$t)
mvubu_sim <- simulate(FITS$Mvubu, t = buffalo$Mvubu$t)

# Pairwise distances if independent motion
sim_dists <- distances(list(cilla_sim, mvubu_sim), FITS[c("Cilla","Mvubu")])

# Plot the data
par(mfrow = c(2,2))  # plot in 2x2 grid

## Plot real tracking data
plot(buffalo[c("Cilla", "Mvubu")], col = c("red2", "blue"),
     main = "Empirical data")

## Plot simulated tracked locations
plot(list(cilla_sim, mvubu_sim), col = c("red2", "blue"),
     main = "Simulated data")

## Plot empirical pairwise distances
plot(DISTS$est ~ DISTS$timestamp, type = "l", col = "#5e548e",
     main = "Empirical distances", ylab = "Distance (m)", xlab = "Time",
     ylim = c(0,max(sim_dists$est)))

## Plot simulated pairwise distances
plot(sim_dists$est ~ sim_dists$timestamp, type = "l", col = "#5e548e",
     main = "Simulated distances", ylab = "Distance (m)", xlab = "Time",
     ylim = c(0,max(sim_dists$est)))


###########################
# PROXIMITY RATIO
###########################

help('proximity')  # proximity ratio (Note: can be slow)

PROXIMITY <- proximity(buffalo[c("Cilla","Mvubu")],
                       FITS[c("Cilla","Mvubu")])

load("Data/buffalo_proximity.rda")  
## statistic only works if they are moving tog or actually avoiding each other

## <1 = The two individuals are **closer** on average than expected from independent movement
## 1 = There is no particular relationship between the movement of individuals (independent movement)
## >1 = The two individuals are **farther** from each other on average than expected for independent movement

# Proximity ratio for simulated animals
SIM_PROXIMITY <- proximity(list(cilla_sim, mvubu_sim), FITS[c("Cilla","Mvubu")])

load("Data/simulated_proximity.rda")

SIM_PROXIMITY


#########################
# ENCOUNTER RATE
#########################

help("encounter")  # encounters, encounter rates/frequencies
# Relevant paper: https://doi.org/10.1101/2023.06.07.544097
## Using proximity as a measure of encounter probability can be sensitive
## (depends on the threshold distance we choose to set for what is counted as an encounter)
## The other issue is that this misses a lot of potential encounters if data is too coarse

help("encounter")  # encounters, encounter rates/frequencies

# Empirical encounters
DISTS$encounter <- ifelse(DISTS$est <= 100, 1, 0)  
## If pairwise distance is less than or equal to 100m, we assume an encounter occurred

# Visualize the results
plot(DISTS$encounter ~ DISTS$timestamp,   # encounters over time
     xlab = "Time", ylab = "Encounter (yes = 1, no = 0)")
cdplot(as.factor(DISTS$encounter) ~ DISTS$timestamp,  # conditional density plot
       xlab = "Time", ylab = "Encounter (yes = 1, no = 0)")
## how does the conditional distribution of encounters change over time

# Empirical Encounter rate (n/day)
n <- sum(DISTS$encounter)  # number of encounters
n
t <- "day" %#% (DISTS$t[nrow(DISTS)] - DISTS$t[1])  # days in the sampling period
n/t  # encounters per day


# Choosing a 100 m encounter radius is arbitrary
## number of encounters estimated in the data can depend on the size of the encounter radius
# We should do a sensitivity analysis to check the impact of the encounter radius size

# Define a range of distances to test for the encounter radius
enc_rad <- 1:1000

N <- vector("numeric", 1000)  # empty vector to store counted encounters
for(i in 1:length(enc_rad)){
  
  # Encounter occurs if pairwise distance is less than or equal to the encounter radius
  N[i] <- sum(ifelse(DISTS$est <= enc_rad[i], 1, 0))
}

# Visualize the results
plot(N ~ enc_rad, col = "#5e548e", type = "l",
     ylab = "Encounters", xlab = "Encounter radius")


# Estimate relative encounter rates
RATES <- encounter(AKDES, method = "PDF")
RATES  # across all individuals

# Relative encounter rate for Cilla and Mvubu
RATES$CI["Cilla","Mvubu",] * 1000^2  
## good for small distances (spend about 1.2% of their time tog)
RATES$CI["Pepper","Toni",] * 1000^2  

# This transformation can provide a more reliable estimate
tanh(sqrt(RATES$CI["Cilla","Mvubu",]) * 1000)^2
## strong assumption that when they're not together, they're avoiding each other


