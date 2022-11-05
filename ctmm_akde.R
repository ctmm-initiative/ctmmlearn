##########
# AKDE
##########

help("akde")
help("bandwidth")

# load buffalo data
data(buffalo)
projection(buffalo) <- median(buffalo)

names(buffalo)

# here we will work with Pepper
DATA <- buffalo$Pepper

COL <- color(DATA,by="time")
plot(DATA,col=COL)

# this dataset has problems
dt.plot(DATA)

# selected autocorrelation model
GUESS <- ctmm.guess(DATA,interactive=FALSE)
FIT <- ctmm.select(DATA,GUESS,trace=3)
# save(FIT,file="pepper.rda")
# I've already run this
load("data/pepper.rda")

summary(FIT)

# analogous IID model
IID <- ctmm.fit(DATA)

summary(IID)

# regular KDE
KDE <- akde(DATA,IID)

# default AKDE
AKDE <- akde(DATA,FIT)

# optimally weighted AKDE
wAKDE <- akde(DATA,FIT,weights=TRUE)
# you only need this with irregular sampling - can be slow
# unweighted AKDE places too much density on oversampled times

# Pepper's optimal weights
plot(DATA$timestamp,wAKDE$weights)

# matching extent for plotting
EXT <- extent(list(KDE,AKDE,wAKDE))

plot(DATA,KDE,ext=EXT)
# note CIs, grid, etc...
summary(KDE)

plot(DATA,AKDE,ext=EXT)
summary(AKDE)

plot(DATA,wAKDE,ext=EXT)
summary(wAKDE)

###########################
# Home-range meta-analysis
###########################

help("meta")

FITS <- list()
for(i in 1:length(buffalo))
{
  GUESS <- ctmm.guess(buffalo[[i]],interactive=FALSE)
  FITS[[i]] <- ctmm.select(buffalo[[i]],GUESS,trace=3)
}
names(FITS) <- names(buffalo)
# save(FITS,file="data/buffalo.rda")
load("data/buffalo.rda")

# calculate AKDES on a consistent grid
AKDES <- akde(buffalo,FITS,weights=TRUE)
# save(AKDES,file="data/buffalo_akdes.rda")
load("data/buffalo_akdes.rda")

# color to be spatially distinct
COL <- color(AKDES,by='individual')

# plot AKDEs
plot(AKDES,col.DF=COL,col.level=COL,col.grid=NA,level=NA)

# Mean buffalo HR "the old way"
AREA <- vector("numeric", length = length(AKDES))
for(i in 1:length(AKDES)){
  AREA[i] <- summary(AKDES[[i]])$CI[2]
}
mean(AREA)

# meta-analysis of buffalo home-range areas
meta(AKDES,col=c(COL,'black'),sort=TRUE)
# model selection: Dirac-delta > inverse-Gaussian

# force inverse-Gaussian population distribution
meta(AKDES,plot=FALSE,IC=NA)
# since CoV isn't a selected feature, its underestimated here

###########################
# Home-range overlap etc.
###########################

help("overlap")

OVER <- overlap(AKDES)

# look at everything
OVER

# pairwise CIs
OVER$CI["Pepper","Toni",]
OVER$CI["Queen","Toni",]

# point estimates
OVER$CI[,,"est"]

# where encounters are expected to take place
help("encounter")


#Plot the data and HR estimates
plot(buffalo[c("Pepper", "Queen")],
     UD=AKDES[c("Pepper", "Queen")],
     col = c("blue", "black"),
     col.DF=COL[4:5],
     col.grid = NA)


#Estimate the home range overlap
overlap(AKDES[c("Pepper", "Queen")])


#Estimate the CDE
CDE <- encounter(AKDES[c("Pepper", "Queen")])

#Visualise the CDE
plot(buffalo[c("Pepper", "Queen")],
     col=COL[4:5],
     UD=CDE,
     col.DF="#046C9A",
     col.grid = NA)


# metric that takes time into account (paper coming)
help("proximity")

proximity(buffalo[c(1,3)],
          FITS[c(1,3)])

#########################
# Population density
#########################

# this is a straight mean of the individual densities that doesn't model population variance
help("mean.UD")
# note the 'sample' argument for correct CIs

# straight mean
MEAN <- mean(AKDES)

plot(buffalo,MEAN,col=COL)

# this is a population kernel density estimate (paper coming)
help("pkde")

PKDE <- pkde(buffalo,AKDES)

plot(buffalo,PKDE,col=COL)
