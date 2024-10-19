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

library(ctmm)

# load gazelle data
data(gazelle)

# select 18th gazelle
DATA <- gazelle[[18]]

COL <- color(DATA,by='time')
plot(DATA,col=COL,error=FALSE)

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

# discuss daily, weekly, monthly, seasonal home-range estimates
