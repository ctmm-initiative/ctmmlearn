# Investigating potential periodic patterns in space use through autocorrelation
## Lomb-Scargle periodogram
## Péron et al. (2016): https://doi.org/10.1186/s40462-016-0084-7
## Péron et al. (2017): https://doi.org/10.1002/ecm.1260

# Load ctmm package
library(ctmm)

# Import maned wolf data
data(wolf)

# Subset to single individual
DATA <- wolf$Gamba
plot(DATA, col = color(DATA, by = "time"))  # color by time

# Calculate variogram
SVF <- variogram(DATA)
zoom(SVF)  # zoom in

# Calculate variogram with better confidence intervals
SVF <- variogram(DATA, CI = "Gauss")
zoom(SVF)  # zoom in

?acf  # or help(acf)
?correlogram  # or help(correlogram)

# Guesstimate the autocorrelation parameters
ctmm.guess(DATA, variogram = SVF)  # interactive mode
GUESS <- ctmm.guess(DATA, variogram = SVF, interactive = FALSE)  # non-interactive mode
plot(SVF, GUESS)  # inspect variogram and model guess parameters
## Note the fluctuations in the autocorrelation

# Model selection
FITS <- ctmm.select(DATA, GUESS, verbose = TRUE, trace = 2)  # smaller data set (faster)
## verbose = TRUE to return all models

summary(FITS)  # OUF anisotropic model selected

# Let's look at the best-fit model
summary(FITS[[1]])
plot(SVF, CTMM = FITS[[1]])  # variogram with best-fit model

# Other models
summary(FITS[[2]])
summary(FITS[[3]])

# Home range estimate (AKDE)
UD <- akde(DATA, FITS[[1]], weights = TRUE, PC = "direct", fast = FALSE, trace = 2)
# Plot home range estimate with data
plot(DATA, UD)

# Compare to IID model (assumes independence)
FITS$IID <- ctmm.fit(DATA)

# Regular KDE
UD.IID <- akde(DATA, FITS$IID)  # NOT accounting for autocorrelation
plot(DATA, UD.IID)

# Compare both models
summary(UD)  # much smaller ESS
summary(UD.IID)  # underestimates home range area


# We have a vignette for the periodogram function
vignette("periodogram")

# Calculate Lomb-Scargle periodogram (log spectral density)
LSP <- periodogram(DATA, fast = FALSE, res.time = 2, res.freq = 2)

# Visualize periodic patterns in autocorrelation
plot(LSP)  # note the spike at the per day mark

# Compare with the sampling schedule
plot(LSP, diagnostic = TRUE, max = TRUE)
## diagnostic = TRUE plots sampling schedule to help check for artifacts
## max = TRUE plots the local maxima of the periodogram (use only for res > 1)

# Add periodic parameter to model
PROTO <- ctmm(mean = "periodic", period = c(24 %#% "hours", 1 %#% "month"),
              circle = TRUE)
GUESS <- ctmm.guess(DATA, variogram = SVF, CTMM = PROTO, interactive = FALSE)
## Guesstimate with the added periodicity and stochastic circulation parameters

# Periodic movement model selection
PFITS <- ctmm.select(DATA, GUESS, verbose = TRUE, trace = 2)  # return all models
## checks for harmonics
# save(PFITS, file = "Data/wolf_periodic.rda")

# Load saved model selection results
load(file = "Data/wolf_periodic.rda")

# Model selection results
summary(PFITS)

# How do the models fit to the variogram?
plot(SVF, CTMM = PFITS[[1]])  # best-fit model
plot(SVF, CTMM = PFITS[[9]])  # OUF anisotropic harmonic 3 1 (poorer fit)

# Selected model
summary(PFITS[[1]])  # OUF anisotropic harmonic 2 1
## Periodic pattern of space use at 1 day mark (daily periodic pattern in movement)

# Re-fit AKDE for home range
UD_period <- akde(DATA, PFITS[[1]], weights = TRUE, 
                  PC = "direct", fast = FALSE, trace = FALSE)

plot(DATA, CTMM = UD_period)  
## Fairly small difference in the home range
## Would have been hard to notice the periodicity without the periodogram

###

names(UD)
UD@info
image(UD$PDF)
image(UD$CDF)
UD$r
