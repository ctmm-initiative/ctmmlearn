#############################
# Speed, distance, diffusion
# https://movementecologyjournal.biomedcentral.com/articles/10.1186/s40462-019-0177-1
#############################

library(ctmm)

#! load buffalo dataset from ctmm
data(buffalo)

# north-up projection
projection(buffalo) <- median(buffalo)

# consider first buffalo
DATA <- buffalo[[1]]

# load model fits from ctmm.select
load("data/cilla.rda")

# units operator
?`%#%`

1 %#% 'day' # day in seconds
1 %#% 'year' # year in seconds

# for time,  will consider the first week of data
DATA <- DATA[DATA$t <= DATA$t[1] + 1%#%'week',]
plot(DATA,col=color(DATA,by='time'),error=FALSE)

# fit to first month only
FIT <- ctmm.select(DATA,FIT,trace=3)

# the speed estimate here is RMS Gaussian
summary(FIT)

# Gaussian (regular speed - not RMS)
speed(FIT)

# non-parametric speed estimation
# "2019 Noonan Fleming Akre ... Calabrese.pdf" in Readings/Continuous_Time folder
SPD <- speed(DATA,FIT)
SPD

# Impact of coarsening the data
SUB <- DATA
FIT.SUB <- FIT
#########################
# remove every other time
#########################
SUB <- SUB[as.logical(1:nrow(SUB)%%2),]
plot(SUB,col=color(SUB,by='time'),error=FALSE)
FIT.SUB <- ctmm.select(SUB,FIT.SUB,trace=3)
# the speed estimate here is RMS Gaussian
summary(FIT)
summary(FIT.SUB)
# Gaussian (regular speed - not RMS)
speed(FIT)
speed(FIT.SUB)
# non-parametric speed estimation
SPD
speed(SUB,FIT.SUB)
#########################
# repeat until data become too coarse

# keep in mind the stationary assumption of the model
# see the appendix of Noonan et al.

###########################
# Population meta-analysis
###########################

help('meta')

#Load in the fitted movement models
load("data/buffalo.rda")

#Estimate mean spead for each animal
SPEEDS <- list()
for(i in 1:length(buffalo))
{
  SPEEDS[[i]] <- speed(buffalo[[i]],FITS[[i]])
}
names(SPEEDS) <- names(buffalo)
# save(SPEEDS,file="data/buffalo_speeds.rda")
load("data/buffalo_speeds.rda")


meta(SPEEDS,sort=TRUE)


###########################
# Instantaneous speeds
###########################


INST_SPEEDS <- speeds(buffalo[[1]],FITS[[1]])

head(INST_SPEEDS)
