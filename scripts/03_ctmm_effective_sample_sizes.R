###########
# SMALL EFFECTIVE SAMPLE SIZES
###########

# What is an "effective sample size" (ESS)?
# also called "degrees of freedom" (DOF)
# Example: consider the standard error of the IID mean estimate from Statistics 101:
# S.E. = S.D./sqrt(n)
# S.E. = the standard error (uncertainty) of the mean estimate
# S.D. = the standard deviation of the process itself
# n    = the nominal sample size (the actual number of data points)
# With larger 'n', the standard error (uncertainty) is smaller 
# But with autocorrelated data, this exact relationship does not hold
# Given S.E., then inverting the relationship S.E. = S.D./sqrt(N)
# to get N = S.D.^2/S.E.^2 then 'N' is the "effective sample size"
# N is a relative measure of the certainty
# For IID data, N=n. The effective sample size is the nominal sample size
# For autocorrelatd data, N<n. The effective sample size is less than the nominal sample size
# If you had N IID data points, then you would get the same quality estimate

######################
# IMPORT AND VISUALIZE DATA
######################

# Load ctmm package
library(ctmm)

# Import gazelle data
data(gazelle)

# Select the 18th gazelle individual
DATA <- gazelle[[18]]

# Summarize the data
summary(DATA)  # longitude and latitude not publicly shared

# Let's visualize this individual's tracks
COL <- color(DATA, by = 'time')  # color location points by time
plot(DATA, col = COL)
## seasonal nomadic range crossing time
## weak site fidelity
## this particular indiv crosses range 2-3 times

plot(DATA, col = COL, error = FALSE,  # can be hard to see points if error = TRUE
     pch = 16, cex = 0.6)  # solid fill data points


#####################
# MODEL SELECTION
#####################

# Guestimate model parameters
GUESS <- ctmm.guess(DATA, interactive = FALSE)

# Select best model
FIT <- ctmm.select(DATA, GUESS, trace = 3)
# save(FIT, file = "Data/gazelle.rda")

load("Data/gazelle.rda")  # load saved model selection results


#######################
# EFFECTIVE SAMPLE SIZE
#######################

# Note the sampling period
summary(DATA)[3]  # ~1 year

# Absolute sample size
nrow(DATA)

# Summarize model
summary(FIT)  # OUF anisotropic selected
## Note the effective sample sizes (DOF[mean] and DOF[area])
## DOF[mean] = central location ESS
## DOF[area] = size of area ESS
## here, approx. 2 effective data points for mean and area, but ~372 for SSFs

# SI units converter
1 %#% 'hr'  # how many seconds in an hour

# Approximate effective sample size
## period of data / HR estimation autocorrelation (should be about the # of HR crossings)
(12.22181 %#% "month") / (5.741925 %#% "month")  # N_area = T / tau_p

# Compare to estimate
summary(FIT)$DOF
## high bias (only 2 effective data points)

# ctmm.fit/ctmm.select help file
help("ctmm.fit")
## Note the "methods" argument
## Default method is not maximum likelihood (ML), 
### need specific minimum effective sample sizes to work properly

# For a target bias of O(5%):
## ctmm.fit method="ML" requires DOF[area] >= 20 points       (CONVENTIONAL)
## ctmm.fit method="pHREML" requires DOF[area] >= 4-5 points  (DEFAULT)
## ctmm.boot method="pHREML" requires DOF[area] >= 2-3 points (SLOW)


####################
# BOOTSTRAPPING
####################

help("ctmm.boot")  # bootstrapping function

# This will take a long time if running on full data
BOOT <- ctmm.boot(DATA, FIT, cores = -1)
## fitting as many times as needed to reach threshold


