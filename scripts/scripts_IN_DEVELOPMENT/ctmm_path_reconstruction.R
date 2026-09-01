# load ctmm package
library(ctmm)

# load buffalo data from package
data(buffalo)
projection(buffalo) <- median(buffalo)

names(buffalo)

# look at Cilla
DATA <- buffalo$Cilla

# selected autocorrelation model
GUESS <- ctmm.guess(DATA,interactive=FALSE)
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save(FIT,file="cilla.rda")
# I've already run this
load("data/cilla.rda")

#############
# PREDICT
#############

# take the first 10 locations
SUB <- DATA[1:10,]

# plot subset
plot(SUB,col=color(SUB,by='time'))

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
plot(list(PRED,SUB),col=c('blue','red'),error=FALSE) # Most Likely Path (MLP)

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

# full dataset
plot(DATA)

OD <- occurrence(DATA,FIT)
plot(OD,col.level=NA)

SIM <- simulate(DATA,FIT,dt=5 %#% 'min')
plot(SIM)

# If you have habitat values and want to know how much
# time an animal spent you can calculate the weighted average:

# sum(RASTER*OD) = E[RASTER]
# where OD is the exported 'PMF'
help('export')
