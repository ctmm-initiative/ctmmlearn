###############
# Meeting the IID assumption

data(buffalo)
DATA <- buffalo$Cilla
SVF <- variogram(DATA)
zoom(SVF) # coarsen to asymptote

FITS <- ctmm.select(DATA,GUESS,trace=3,verbose=TRUE,cores=-1)
load("data/cillas.rda")
FIT <- FITS[[1]]
dt <- -log(0.05)*FIT$tau[1] # time to 5% autocorrelation in velocity
"day" %#% dt # coarsen to

###############
# Meeting the Markovian assumption

data(buffalo)
DATA <- buffalo$Cilla
SVF <- variogram(DATA)
zoom(SVF) # coarsen to straight line - regular diffusion

FITS <- ctmm.select(DATA,GUESS,trace=3,verbose=TRUE,cores=-1)
load("data/cillas.rda")
FIT <- FITS[[1]]
dt <- -log(0.05)*FIT$tau[2] # time to 5% autocorrelation in velocity
"hr" %#% dt # coarsen to

###############
# Segmentation

### Type 1: Clustering
# k-means clustering (simple)
?kmeans
# k-medians clustering (robust)
?Gmedian::Gmedian
# GMM: Gaussian mixture models (flexible)
?rebmix::REBMIX

### Type 2: Time-series change-point detection
# time-series based
?mcp::mcp
?segclust2d::segclust

### Type 3: HMM: Hidden Markov Models
?momentuHMM::fitHMM

###############
# integrated Step Selection Functions (iSSF)
vignette("p4_SSF",package="amt")
help("random_steps",package="amt")
# increase n_control until numerical convergence in the output
# make note of rand_sl and rand_ta distributions, which are gamma and vonmises by default
# log(gamma.pdf(sl)) ~ sl + log(sl)
# log(vonmises.pdf(ta)) ~ cos(ta)
# Note that m0,m1,m2 need to contain all of the above terms, and their significance is irrelevant
# Additionally, I would consider interactions with both sl and log(sl) rather than one,
# because their correspondence with E[sl] and VAR[sl] is complex

###############
# HMM + SSF
browseURL("https://github.com/NJKlappstein/hmmSSF")
