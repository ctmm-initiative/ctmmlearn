
#Load the pre-requsite package
library(ctmm)


#############################################################
####    Simulations conditioned off of a fitted model    ####
#############################################################

#Load in the dataset from ctmm
data("coati")


#Visualise the data
plot(coati[[1]])

#Generate the variogram
vg <- variogram(coati[[1]])


GUESS <- variogram.fit(vg,
                       interactive = FALSE)

#Visually inspect the fit
plot(vg, GUESS)


#Fit the models
FIT <- ctmm.select(coati[[1]],
                   GUESS,
                   cores = -1,
                   trace = 3)


#Summary of the fit
summary(FIT)


#Simulate from the fitted model, with the original sampling times
SIM <- simulate(FIT,
                t = coati[[1]]$t)


#Visualise the results
plot(list(coati[[1]],
          SIM),
     col = c("red",
             "blue"))


#Draw model parameters from the sampling distribution of the fitted model
FIT2 <- emulate(FIT,
                fast = TRUE)



#Simulate from the model
SIM2 <- simulate(FIT2,
                 t = coati[[1]]$t)


#Visualise the results
plot(list(coati[[1]],
          SIM,
          SIM2),
     col = c("red",
             "blue",
             "grey30"))


#Fill in the gaps 
SIM3 <- simulate(coati[[1]],
                 CTMM = FIT,
                 dt = 60)


#Visualise the results
plot(list(coati[[1]],
          SIM3),
     type = c("p",
              "l"),
     col = c("red",
             "blue"))


#######
#Generate the most likely path (conditioned off of the fitted model)
MLP <- predict(coati[[1]],
               FIT,
               dt = 60)

plot(coati[[1]])
plot(MLP,
     error = FALSE,
     type = "l",
     col = "blue",
     add = TRUE)


#Export the simulations to work with other packages
SIM_EXPORT <- simulate(coati[[1]],
                       CTMM = FIT,
                       complete = TRUE)

EXPORT <- SpatialPoints.telemetry(SIM_EXPORT)

class(EXPORT)
plot(EXPORT)

#############################################################
##########            Generic simulations          ##########
#############################################################

#Simulations can be used to guide study design

#Define model parameters for the species of interest

#The position autocorrelation timescale (range crossing time)
tau_p <- 1 %#% 'day'

#The velocity autocorrelation timescale (directional persitence)
tau_v <- 1 %#% 'min'


#Spatial variance
sig <- 1 %#% 'kilometers^2'


#Specify an OUF model for simulation
MODEL <- ctmm(tau=c(tau_p, tau_v),
              isotropic=TRUE,
              sigma=sig)


#Define the sampling duration/schedule
t <- seq(0,1 %#% 'month', 1 %#% 'hr')

#Simulate from the defined model and sampling schedule
SIM <- simulate(MODEL,
                t = t)

#Fit the movement models
GUESS <- ctmm.guess(SIM, interactive = FALSE)

FITS <- ctmm.select(SIM,
                    GUESS,
                    cores = -1)

#Summary of the fit (NOTE the DOFs)
summary(FITS)


#Define the sampling duration/schedule
t <- seq(0,4 %#% 'days', 5 %#% 'min')


#Simulate from the defined model and sampling schedule
SIM <- simulate(MODEL,
                t = t)


#Fit the movement models
GUESS <-  ctmm.guess(SIM, interactive = FALSE)

FITS <- ctmm.select(SIM,
                    GUESS)

#Summary of the fit (NOTE the DOFs)
summary(FITS)


#Home range estimate
HR <- akde(SIM,
           FITS)


#Visualise the results
plot(SIM,
     UD = HR)

