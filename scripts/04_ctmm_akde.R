##########
# AKDE
##########

# Load ctmm package
library(ctmm)

help("akde") # main function and new `pkde()` function for population-wide data
## less weight on over-sampled points and more weight on under-sampled points 
## (number of weights (optimization parameters) = number of data points)

help("bandwidth")  # bandwidth = spread of kernels
## kernels will spillover into areas where animal can't go, can set boundaries if kernels are smaller than the polygon
## uses Gaussian optimization function


###################
# IMPORT AND VISUALIZE DATA
###################

# Load buffalo data
data(buffalo)
projection(buffalo) <- median(buffalo)  # center projection on median of locations

names(buffalo)

# Here we will work with Pepper
DATA <- buffalo$Pepper

COL <- color(DATA, by = "time")  # color location points by time
plot(DATA, col = COL, main = "Pepper")

# This dataset has problems
dt.plot(DATA)  # diagnose sampling schedule
## time intervals in data sorted by size, 
## most of the sampling intervals are 2 hours, some greater, some around 1 hour


####################
# MODEL SELECTION
####################

# Selected autocorrelation model
GUESS <- ctmm.guess(DATA, interactive = FALSE)
FIT <- ctmm.select(DATA, GUESS, trace = 3)
# save(FIT, file = "Data/pepper.rda")

# Load saved model selection results
load("Data/pepper.rda")

summary(FIT)
## velocity autocorrelation ~ 30-40 mins
## position autocorrelation ~ 7-24 days

# Analogous IID model
IID <- ctmm.fit(DATA)

summary(IID)


#####################
# HOME RANGE ESTIMATION (AKDE)
#####################

# Regular KDE
KDE <- akde(DATA, IID)  # with IID model
## Anisotropic kernel
## would not want symmetric kernels here (want elongated kernels)

plot(DATA, KDE, main = "KDE")
summary(KDE)

# Default AKDE
AKDE <- akde(DATA, FIT)  # with selected autocorrelation model (OUF anisotropic)

# Plot the home range
plot(DATA, AKDE, main = "AKDE")
summary(AKDE)
## Larger point estimates and CIs


#######################
# WEIGHTED AKDE
#######################

# Optimally weighted AKDE
wAKDE <- akde(DATA, FIT, weights = TRUE)
## Only needed for irregular sampling - can be slow

# Pepper's optimal weights
plot(DATA$timestamp, wAKDE$weights, 
     xlab = "time", ylab = "weight", main = "Optimal Weights")

# Zoom into the lower weights
plot(DATA$timestamp, wAKDE$weights, ylim = c(0,0.005),
     xlab = "time", ylab = "weight", main = "Optimal Weights (zoomed in)")
## More weight ascribed to 2 hour data
## higher weighted/more unique datapoints are more valuable

# Pepper's sampling interval
plot(DATA$timestamp, 'hour' %#% c(0, diff(DATA$t)), ylim = c(0,20),
     xlab = "Date", ylab = "Sampling interval (hours)", main = "Pepper's Sampling Intervals")

# Match extent for plotting to the largest of the three distributions
EXT <- extent(list(KDE, AKDE, wAKDE))

# Plot the IID KDE
plot(DATA, KDE, ext = EXT, main = "Regular IID KDE")
summary(KDE)

# Plot the AKDE
plot(DATA, AKDE, ext = EXT, main = "Uniformly-weighted AKDE")  # uniform weights
summary(AKDE)

# Plot the weighted AKDE
plot(DATA, wAKDE, ext = EXT, main = "Optimally-weighted AKDE")  # optimal weights
summary(wAKDE)
## Note diff shape for akde: 1-hour data further up (less weight), more weight on the 2-hour sampling lower down
## more representative visualization of movement data instead of sampling data


# Let's compare the 3 distributions more directly by overlaying them
# Create custom colors for data by sampling interval
DATA$interval <- "hour" %#% c(3600, diff(DATA$t))  # sampling intervals
DATA$color <- NA  # empty column to fill with colors

DATA$color[0.94 < DATA$interval & DATA$interval < 1.05] <- "gold"  # ~1-hour sampling
DATA$color[1.85 < DATA$interval & DATA$interval < 2.15] <- "red2"  # ~2-hour sampling
DATA$color[DATA$interval > 2.5] <- "darkgray"        # >2 hours
DATA$color[DATA$interval < 0.94] <- "lightgray"      # <1 hour

# Plot all distributions overlayed
plot(DATA, KDE, ext = EXT, col = DATA$color, level.UD = c(0.5,0.95), level = NA,
     col.level = "gray", col.grid = NA, col.UD = "transparent", 
     main = "Overlayed KDE, AKDE, wAKDE") 
legend("topright", inset = 0.05, legend = c("1-hour", "2-hour", "other"), 
       col = c("gold", "red2", "gray"), bty = "n", pch = 16) 
text(-22, 68, labels = "KDE", col = "gray") 
text(19, 50, labels = "AKDE", col = "blue") 
text(-20, 20, labels = "wAKDE", col = "red")
plot(AKDE, ext = EXT, level.UD = c(0.5,0.95), level = NA, col.level = "blue",
     col.grid = NA, col.UD = "transparent", add = TRUE)
plot(wAKDE, ext = EXT, level.UD = c(0.5,0.95), level = NA, col.level = "red",
     col.grid = NA, col.UD = "transparent", add = TRUE)


#######################
# OVER-SMOOTHING BIAS CORRECTION
#######################

# Over-smoothing bias 
osAKDE <- akde(DATA, FIT, weights = TRUE, debias = FALSE)  # no bias correction
## Gaussian reference function akde over-smooths

plot(DATA, osAKDE, main = "Uncorrected wAKDE")
## GRF coverage area is too big


###########################
# HOME-RANGE META-ANALYSIS
###########################

help("meta")
help('meta', package = "ctmm")
## meta-analysis: hierarchical model that propagates individual parameters into population-wide

# Model selection for each buffalo
FITS <- list()
for(i in 1:length(buffalo))
{
  GUESS <- ctmm.guess(buffalo[[i]], interactive = FALSE)
  FITS[[i]] <- ctmm.select(buffalo[[i]], GUESS, trace = 3)
}
names(FITS) <- names(buffalo)
# save(FITS, file = "Data/buffalo.rda")

# calculate AKDES on a consistent grid
AKDES <- akde(buffalo, FITS, weights = TRUE)
# save(AKDES, file = "Data/buffalo_akdes.rda")

# Load in saved model selection results
load("Data/buffalo.rda")

# Load in saved AKDE results
load("Data/buffalo_akdes.rda")

# Color individuals to be spatially distinct
COL <- color(AKDES, by = 'individual')
## individuals closer together in space have more distinct colours

# Plot individual AKDEs
plot(AKDES, col.UD = COL, col.level = COL, col.grid = NA, level = NA, 
     main = "African buffalo AKDEs")

# Calculate manually
# Mean buffalo HR "the old way"
AREA <- vector("numeric", length = length(AKDES))  # empty vector to store HR areas

for(i in 1:length(AKDES)) { 
  AREA[i] <- summary(AKDES[[i]], units = FALSE)$CI[2]  # turn off units when making tables (units will be diff)
}
AREA  # vector of HR areas

mean(AREA) # mean
sqrt(var(AREA)/length(AREA)) # SE

# meta-analysis of buffalo home-range areas
meta(AKDES, col = c(COL,'black'), sort = TRUE)
## Model selection: Dirac-delta > inverse-Gaussian for pop-level parameters
### -- here Dirac-delta singular (no variance)

## Forest plot
## individual HR estimates (a lot of uncertainty)
## hierarchical meta-analysis model that better estimates mean

# Force inverse-Gaussian population distribution
meta(AKDES, plot = FALSE, IC = NA)
## Since CoV is not a selected feature, its underestimated here

# Compare sub-groups (North vs South don't look significantly different)
BUFFALO <- list(South = AKDES[1:3], North = AKDES[4:6])
META <- meta(BUFFALO)

META
META['South/','/North',]  # ratio of mean southern HR to mean northern HR
# Not significantly different (CIs overlap)

# General meta-analytic regressions
help("Log")  # log-transform to make estimates more normal for `metafor`
## then you can use the 'metafor' R package

# Speed example
## log-transform speed estimates
Log(FITS, variable = "speed")  # then you can use the 'metafor' R package


#########################
# POPULATION RANGE ESTIMATION
#########################

# Mean of individual densities doesn't model population variance
help("mean.UD")  # doesn't model population variance
## note the 'sample' argument for correct CIs
## e.g., averaging summer and winter ranges (estimate separately then average)

# Straight mean of HRs - for a population of 6 buffalo
MEAN <- mean(AKDES, sample = FALSE)

plot(buffalo, MEAN, col = COL, main = "Mean African buffalo AKDE")

# Population kernel density estimate (Anand et al., 2025, bioRxiv)
help("pkde")  # population KDE: bandwidth optimization after choosing hierarchical model

# Estimate population range
PKDE <- pkde(buffalo, AKDES, trace = FALSE)  # runs mean on fitted movement models
## does model selection across many parameters due to all the parameters for each indiv
## tests which correlations btwn parameters can be supported (outputs delta AIC)

plot(buffalo, PKDE, col = COL, main = "African buffalo PKDE")
## much larger and much more uncertain due to low sampling (but at least doesn't bias too small)
## other methods tend to estimate too small or look at saturation curves of crude pop range estimate



