library(ctmm)
vignette("variogram")

data(wolf)

DATA <- wolf$Gamba

plot(DATA)

SVF <- variogram(DATA)

?acf
?correlogram

zoom(SVF)

SVF <- variogram(DATA,CI="Gauss")
zoom(SVF)

ctmm.guess(DATA,variogram=SVF)
GUESS <- ctmm.guess(DATA,variogram=SVF,interactive=FALSE)

FITS <- ctmm.select(DATA,GUESS,verbose=TRUE,trace=2)

summary(FITS)
summary(FITS[[1]])
zoom(SVF,CTMM=FITS[[1]])

summary(FITS[[2]])

summary(FITS[[3]])

UD <- akde(DATA,FITS[[1]],weights=TRUE,PC="direct",fast=FALSE,trace=2)
plot(DATA,UD)

FITS$IID <- ctmm.fit(DATA)
UD.IID <- akde(DATA,FITS$IID)
plot(DATA,UD.IID)

summary(UD)
summary(UD.IID)

vignette("periodogram")

LSP <- periodogram(DATA,fast=FALSE,res.time=2,res.freq=2)
plot(LSP)
plot(LSP,diagnostic=TRUE,max=TRUE)

PROTO <- ctmm(mean="periodic")
GUESS <- ctmm.guess(DATA,variogram=SVF,CTMM=PROTO,interactive=FALSE)

PFITS <- ctmm.select(DATA,GUESS,verbose=TRUE,trace=2)

summary(PFITS)

zoom(SVF,CTMM=PFITS[[1]])
zoom(SVF,CTMM=PFITS[[4]])

summary(PFITS[[1]])

names(UD)
UD@info
image(UD$PDF)
image(UD$CDF)
UD$r
