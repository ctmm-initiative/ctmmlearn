####################
# OUTLIER DETECTION
####################
# Identify outliers consistent/inconsistent with an error model

#! load wood turtle data
data(turtle)

# names of each dataset
names(turtle)
# the first two are calibration data

#! select the third dataset (female 231)
DATA <- turtle$F231

# plot turtle
plot(DATA)
# notice the error circles

# help file for outlie function
?outlie
# note the 'by' argument in particular

#! calculate outlier statistics and generate plot
OUT <- outlie(DATA)
# red segments are for speed
# blue points are for proximity
# the speed and proximity estimates are error informed (if your data are calibrated)

# some useful error-informed statistics were calculated
head(OUT)
# you may also have some other columns, like altitude

# outlier statistics (used for coloring)
plot(OUT)
# the minimum speed required to explain the data
# deviation from the geometric median
# other axes can be plotted if available

# biological decision to reject all speeds > 5 cm/s for this species (wood turtle)
MAX <- 5 %#% 'cm/s'
?'%#%' # convenient units function
MAX # 5 cm/s in SI units
# alternatively
MAX <- 0.05 # in meters/second

#! index of fix with highest speed
GOOD <- OUT$speed < MAX

#! remove the outliers from the dataset
DATA <- DATA[GOOD,]
# negative indices remove those indices

# plot data with outlier removed
plot(DATA)

# look for outliers again
OUT <- outlie(DATA)
plot(OUT)
