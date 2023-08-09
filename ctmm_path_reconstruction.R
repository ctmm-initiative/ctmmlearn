# load ctmm package
library(ctmm)

# load buffalo data from package
data(buffalo)
names(buffalo)

# look at Cilla
DATA <- buffalo$Cilla

# calculate guesstimate
ctmm.guess(DATA)
GUESS <- ctmm.guess(DATA,interactive=FALSE)

# select model
FIT <- ctmm.select(DATA,GUESS)
# You will want to use ctmm.select here
# I am cheating, as I know the selected model!
FIT <- ctmm.fit(DATA,GUESS,trace=TRUE)

#############
# PREDICT
#############

# take the first 10 locations
SUB <- DATA[1:10,]

# plot subset
plot(SUB)

# if working with this amount of data, you might consider alternative ICs
# in particular IC="LOOCV"
help("ctmm.select")
# I'm just sub-setting for purposes of visualization and speed

# convenience function
help("%#%")
1 %#% 'hr'

# make an array of times over the same period, but 5 min apart
SEQ <- seq(from=SUB$t[1],to=SUB$t[10],by=5 %#% 'min')

# predict locations at those times
help('predict.ctmm')
PRED <- predict(SUB,FIT,t=SEQ)

# plot predictions & data
plot(list(PRED,SUB),col=c('blue','red'))

#############
# CONDITIONAL SIMULATION
#############

# 1 minute sequence
SEQ <- seq(from=SUB$t[1],to=SUB$t[10],by=1 %#% 'min')

# simulate locations at those time
help('simulate.ctmm')
SIM <- simulate(SUB,FIT,t=SEQ)

# plot conditional simulation & data
plot(list(SIM,SUB),col=c('blue','red'),type=c('l','p'))


SIM2 <- simulate(SUB,FIT,t=SEQ)
# plot conditional simulation & data
plot(list(SIM,SIM2,SUB),col=c('blue','orange','red'),type=c('l','l','p'))

# that is only trajectory uncertainty
# can also include parameter uncertainty
help('emulate')

SIM3 <- simulate(SUB,emulate(FIT, fast = T),t=SEQ)

plot(list(SIM,SIM2,SIM3,SUB),col=c('blue','orange','black','red'),type=c('l','l','l','p'))

##########################
# Occurrence distributions
##########################

library(ctmm)
data(buffalo)
projection(buffalo) <- median(buffalo)
DATA <- buffalo$Cilla
load("data/cilla.rda")

plot(DATA)

OD <- occurrence(DATA,FITS[[1]])
plot(OD,col.level=NA)

SIM <- simulate(DATA,FITS[[1]],dt=5 %#% 'min')
plot(SIM)

# If you have habitat values and want to know how much
# time an animal spent you can calculate the weighted average:

# sum(RASTER*OD) = E[RASTER]
