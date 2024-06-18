# This script details methods that can be used for studying
# interactions between individuals. This includes:
# - Home-range overlap
# - Encounter location distributions (CDE)
# - Pairwise distances
# - Proximity ratios
# - Encounter rates


# These analyses are conditional on fitted movement models and HR estimates
# (see: https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_akde.R)
library(ctmm)
data("buffalo"); projection(buffalo) <- median(buffalo)
load("data/buffalo.rda") # Fitted movement models; object called 'FITS'
load("data/buffalo_akdes.rda") # Estimated HR areas; object called 'AKDES'


#-----------------------------------------------------
# Home-range overlap
#-----------------------------------------------------

# Do individuals share the same space?
# Relevant paper: https://doi.org/10.1111/2041-210X.13027
help("overlap")

#Estimate HR overlap for all pairs
#Note: these all must have compatible resolutions and alignments
OVER <- overlap(AKDES)

# This will generate an error because of incompatible grids
overlap(list(akde(buffalo$Pepper, FITS$Pepper),
             akde(buffalo$Queen, FITS$Pepper)))

# But this works because HRs are estimated simultaneously (and consistently)
overlap(akde(list(buffalo$Pepper,buffalo$Queen),
             list(FITS$Pepper, FITS$Queen)))


# look at everything
OVER

# pairwise CIs 
OVER$CI["Pepper","Toni",]
OVER$CI["Queen","Toni",]

# point estimates
OVER$CI[,,"est"]


#-----------------------------------------------------
# Encounter location distributions (CDE)
#-----------------------------------------------------

# where encounters are expected to take place

# Relevant paper: https://doi.org/10.1111/2041-210X.13597
help("cde")


#Plot the data and HR estimates
plot(buffalo[c("Pepper", "Queen")],
     UD=AKDES[c("Pepper", "Queen")],
     col = c("#e76f51", "#264653"),
     col.DF=c("#f4a261", "#2a9d8f"),
     col.grid = NA)


#Estimate the home range overlap
overlap(AKDES[c("Pepper", "Queen")])


#Estimate the CDE
CDE <- cde(AKDES[c("Pepper", "Queen")])

#Visualise the CDE
plot(buffalo[c("Pepper", "Queen")],
     col=c("#e76f51", "#264653"),
     UD=CDE,
     col.DF="#046C9A",
     col.grid = NA)


#-----------------------------------------------------
# Pairwise proximity and distance metrics
#-----------------------------------------------------

# metrics that takes time into account (paper coming)
help("proximity")

#Pairwise separation distances
DISTS <- distances(buffalo[c("Cilla","Mvubu")],
                   FITS[c("Cilla","Mvubu")])


#Visualise the separation distances
plot(DISTS$est ~ DISTS$timestamp,
     type = "l",
     col = "#5e548e")


#Internal plotting function (work in progress)
ctmm:::ts.plot(DISTS)


cilla_sim <- simulate(FITS$Cilla, t = buffalo$Cilla$t)
mvubu_sim <- simulate(FITS$Mvubu, t = buffalo$Mvubu$t)


sim_dists <- distances(list(cilla_sim, mvubu_sim),
                       FITS[c("Cilla","Mvubu")])

#Plot the data
par(mfrow = c(2,2))
plot(buffalo[c("Cilla", "Mvubu")],
     col = c("#e76f51", "#264653"),
     main = "Empirical data")

plot(list(cilla_sim, mvubu_sim),
     col = c("#e76f51", "#264653"),
     main = "Simulated data")

plot(DISTS$est ~ DISTS$timestamp,
     type = "l",
     col = "#5e548e",
     main = "Empirical distances",
     ylab = "Distance (m)",
     xlab = "Time",
     ylim = c(0,max(sim_dists$est)))

plot(sim_dists$est ~ sim_dists$timestamp,
     type = "l",
     col = "#5e548e",
     main = "Simulated distances",
     ylab = "Distance (m)",
     xlab = "Time",
     ylim = c(0,max(sim_dists$est)))



# Proximity ratio (note: can be slow)
PROXIMITY <- proximity(buffalo[c("Cilla","Mvubu")],
                       FITS[c("Cilla","Mvubu")])
load("data/buffalo_proximity.rda")
PROXIMITY

# Proximity ratio for simulated animals
SIM_PROXIMITY <- proximity(list(cilla_sim, mvubu_sim),
                           FITS[c("Cilla","Mvubu")])
load("data/simulated_proximity.rda")
SIM_PROXIMITY

#-----------------------------------------------------
# Encounters
#-----------------------------------------------------

help("encounter")
# Relevant paper: https://doi.org/10.1101/2023.06.07.544097

#Empirical encounters
DISTS$encounter <- ifelse(DISTS$est <= 100, 1, 0)

#Visualise the results
par(mfrow = c(1,1))
plot(DISTS$encounter ~ DISTS$timestamp)
cdplot(as.factor(DISTS$encounter) ~ DISTS$timestamp)

#Empirical Encounter rate (n/day)
n <- sum(DISTS$encounter)
t <- "day" %#% (DISTS$t[nrow(DISTS)] - DISTS$t[1])
n/t


#If you do this, run a sensitivity analysis
enc_rad <- 1:1000
N <- vector("numeric", 1000)
for(i in 1:length(enc_rad)){
  N[i] <- sum(ifelse(DISTS$est <= enc_rad[i], 1, 0))
}

#visualise the results
plot(N ~ enc_rad,
     ylab = "Encounters",
     xlab = "Encounter radius",
     type = "l",
     col = "#5e548e")


#Estimate relative encounter rates
RATES <- encounter(AKDES)
RATES$CI["Cilla","Mvubu",] * 100^2 # good for small distances
tanh(sqrt(RATES$CI["Cilla","Mvubu",])*100)^2 # more reliable
