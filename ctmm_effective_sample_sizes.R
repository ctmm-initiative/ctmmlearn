###########
# SMALL EFFECTIVE SAMPLE SIZES
###########

library(ctmm)

# load gazelle data
data(gazelle)

# select 18th gazelle
DATA <- gazelle[[18]]

COL <- color(DATA,by='time')
plot(DATA,col=COL)

# guestimate model parameters
GUESS <- ctmm.guess(DATA,interactive=FALSE)

# select best model
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save("FIT",file="gazelle.rda")
load("data/gazelle.rda")

# summarize data
summary(DATA)
# note the sampling period

# summarize model
summary(FIT)
# note the effective sample sizes

# SI units converter
help("%#%")
1 %#% 'hr'

# approximate effective sample size
(12.22181 %#% "month") / (5.741925 %#% "month")

# compare to estimate
summary(FIT)$DOF

# ctmm.fit/ctmm.select help file
help("ctmm.fit")
# note the methods argument

############################
# For a target bias of O(5%)
# ctmm.fit method="ML" requires DOF[area]>=20       (CONVENTIONAL)
# ctmm.fit method="pHREML" requires DOF[area]>=4-5  (DEFAULT)
# ctmm.boot method="pHREML" requires DOF[area]>=2-3 (SLOW)
# but in all cases DOF[area] is an estimate

help("ctmm.boot")

# this will take a while
BOOT <- ctmm.boot(DATA,FIT,cores=-1)

# discuss daily, weekly, monthly, seasonsal home-range estimates
