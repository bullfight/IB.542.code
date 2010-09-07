# Patrick Schmitz
# p.schmitz@gmail.com
# September 6 2010

# start by seting your working directory rewrite to be appropriate
setwd("C:/Users/pschmitz/Desktop/2010.IB.542.Temperature")
# setwd("~/Desktop/2010.IB.542.Temperature") #on MAC OS X

# first clear the workspace, this is a good way to be sure
# 	that you know exactly what you are working with
rm(list=ls()) 				# clears all objects in workspace
ls() 									# check to see that the workspace is empty

# Let's read in the data and put it into an object called dat
dat <- read.csv(
		file = "2010.SoyFace.micromet.sample.csv",
		header = TRUE,
		sep = ",", 
		as.is = TRUE
)


########## ########## ########## ##########
# R is an object-ariented programming language
# 	this means that each variable is an "object"
# 	that can be queried or manipulated
#	dat is an arbitray name, I could just as easily
#	have put the data in an object called hotdogz

# read.csv is a function used to import csv files.
# 	you can read about the function by typing
?read.csv

# you can access documentation about any function in R in this manner

# Description of Data ######################
# This is a 10 minute data record from SoyFace 2010

# DOY 	- Day of Year 
# Hr 	- 24-Hour 
# Min 	- Minute  
# Hour 	- decimal Hour dat$Hr + dat$Min/60
# Ta 	- air temperature recorded at 2.2 meters above the surface
# Ts 	- soil temperature recorded 10 cm below surface

# Let's take a look at that ol' data
str(dat) 	# prints a list of the variables in the data.frame
head(dat)	# prints first six rows

# What if we want to just look at AirTemp
#		Here are some methods that all do the same thing
head(dat$Ta)
dat$Ta[1:5]
dat[1:5, "Ta"]
dat[1:5, 5]

# First you can see that "$" operator allows you to select
# columns of data by name
# When we write dat$Ta[1:5] we are accessing the index
# of the vector dat$Ta

# making an index is easy
1:100
seq(from = 1, to = 100, by = 1) # or
seq(1, 100, 1)
seq(1, 100, 2) 			# odd numbers
seq(2, 100, 2) 			# even numbers

# xyplot uses a formula for plotting with the form
#		y ~ x1 + x2 | panel factor
xyplot(
	x = Ta + Ts ~ Hour | DOY,
	data = dat,
	type = "l" # Line type, try "b" and "p"
)

# What if we want to plot in a single panel?
xyplot(
	x = Ta ~ Hour,
	data = dat
)

# This gives us a sense of the data, but we can't tell
# the difference between days
xyplot(
	x = Ta ~ Hour,
	data = dat,
	groups = DOY, 		# grouping factor
	type = "l", 			
	auto.key = list(TRUE, space = "right", points = F, lines = T)
)

# But really we want to see temperature as a single line,
# to do so we need an time vector which includes both day and time

# Create Decimal Day (also known as julian day)
DOY.dec <- dat$DOY + dat$Hour/24
DOY.dec <- dat$DOY + dat$Hr/24 + dat$Min/1440 # Same as above
length(DOY.dec) 	# this vector has 1582 values
dim(dat) 	# gives the dimensions of the ?data.frame
					# notice it has the same length as DOY.dec
					
dat <- cbind(DOY.dec, dat) 
# We have bound DOY.dec to dat, possible because they are of the same size
# Alternatively create vector and tack on to the right of the data.frame
# dat$DOY.dec <- dat$DOY + dat$Hour/24

# now
dim(dat)
head(dat)

xyplot(
	x = Ta + Ts ~ DOY.dec,
	data = dat,
	type = "l", 			# Line type, try "b" and "p"
	auto.key = list(TRUE, points = F, lines = T),
	main = "SoyFace Temperature Record",
	xlab = "Julian Day"
)



########## ########## ########## ##########
# Let's complete some more complicated tasks
# calculate Daily Maximum
Ta.max <- tapply(X = dat$Ta, INDEX = dat$DOY, FUN = max)
Ta.max  # vector of daily Temp max, with horizontal DOY index
Ta.max["205"] # or
Ta.max[6]

#Daily Min
Ta.min <- tapply(dat$Ta, dat$DOY, min)



########## ########## ########## ##########
# Next lets move to building and using the model

# Fourier Temperature Series (Cambell & Norman p.23)
# function gamma - accepts input time
gamma <- function(time){
	0.44 - 	0.46 * sin( { ( pi/12 ) * time } + 0.9 ) +
			0.11 * sin( { 2 *  ( pi/12 ) * time } + 0.9 )
}

gamma(10) # [1] 0.59295
gamma(0:10)

# Fourier Series 
fs <- function(time, Tn, Tx){
	Tx * gamma(time) + Tn * (1 - gamma(time))
}

#Example 2.3 Page 23
fs(10, Tn = 5, Tx = 23) # [1] 15.673

# Yep, thats it! The challenging part however
# is applying these functions across the vector appropriately
# We will explore these issues below

# Indicies for Ta.max and Ta.min to be applied by 
# time of Day
# 0:5			(Tx[i - 1], Tn)
# 5:14		(Tx, Tn)
# 14:24		(Tx[1], Tn[i + 1])
# NOTE: the book has a typo written as (Tx[i - 1], Tn[i + 1])

# Remember we cannot predict all times,
# On Day one there are no inputs for Tx[i -1]
# and Day 10 we have no inputs Tn[i + 1]

# to write this model we will be using a for-loop,
# to loop over the data with a particular index value

# For example
for(i in 1:10){ print(i)}

# Here we assign each unique DOY to day, and print day to the output
for( day in unique(dat$DOY) ){ print(day) }; rm(day)





# Usage of model ######## ######## ######## ########
# 	Build an index for each time set of times
#		apply each equation to match these times

# Empty Vector of Not available Values
dat$fTa <- rep(NA, length(dim(dat)[1]))

Days <- unique(dat$DOY)

for(day in Days[2:length(Days)]){
	# Time index < or = 5 
	time <- which(dat$Hour <= 5 & dat$DOY & dat$DOY %in% day) 

	dat$fTa[time] <-	Ta.max[as.character(day - 1)] * gamma( dat$Hour[time] ) + 
						Ta.min[as.character(day)] * { 1 - gamma( dat$Hour[time] ) }
}

for(day in Days[2:length(Days)]){
	# Time index > 5  & Time < or = 14
	time <- which(dat$Hour > 5 & dat$Hour <= 14 & dat$DOY %in% day)  

	dat$fTa[time] <- 	Ta.max[as.character(day)] * gamma( dat$Hour[time] ) + 
						Ta.min[as.character(day)] * { 1 - gamma( dat$Hour[time] ) }
}
	
for(day in Days[2:{length(Days) - 1}]){
	# Time index > 14
	time <- which(dat$Hour > 14 & dat$DOY %in% day) 

	dat$fTa[time] <- 	Ta.max[as.character(day)] * gamma( dat$Hour[time] ) + 
						Ta.min[as.character(day + 1)] * { 1 - gamma( dat$Hour[time] ) }
}


# There we go, Let's plot the data!

xyplot(
	x = fTa ~ DOY.dec,
	data = dat,
	type = "b", 			# Line type, try "b" and "p"
	auto.key = list(TRUE, points = F, lines = T),
	main = "SoyFace Temperature Record",
	xlab = "Julian Day"
)

xyplot(
	x = fTa + Ta ~ DOY.dec,
	data = dat,
	type = "b", 			# Line type, try "b" and "p"
	auto.key = list(TRUE, points = F, lines = T),
	main = "SoyFace Temperature Record",
	xlab = "Julian Day"
)

xyplot(
	x = fTa + Ta ~ Hour | as.factor(DOY),
	data = dat,
	type = "l", 			# Line type, try "b" and "p"
	auto.key = list(TRUE, points = F, lines = T),
	main = "SoyFace Temperature Record",
	xlab = "Julian Day"
)