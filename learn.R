# Patrick Schmitz
# p.schmitz@gmail.com
# September 6 2010

# I want to share with class, a nice introduction to R and model air temperature
# as part of that introduction.  Things may seem obscure, but I try to give examples,
# to explain how each part works.  If there are any functions I use that you don't 
# understand, it is quite simple to check the documentation, which I explain below.
# If you get stuck, feel free to email me, or message me on gchat

# As you work through this, you should copy, select code chunks from this script and 
# paste them into the console prompt

# start by seting your working directory

# In Windows, it may simpler to change the working directory from the menu bar, goto
# File > Change dir...

# On OS X you can do the same by going to 
# Misc > Change Working Directory...
# or hitting  ⌘ + D

# You can also use the command setwd() as in
# setwd("C:/Documents and Settings/Desktop/IB.542/") # On Windows
# setwd("~/Desktop/2010.IB.542/") # on MAC OS X
# setwd("~/Dropbox/classes/2010.IB.542/code/2010.IB.542.Temperature/") # Pat's directory

# now check to make sure you are in the right directory
getwd()
# and
list.files()

# Notice that all text following "#" are comments, and will be ignored by the console interpreter


# first clear the workspace, this is a good way to be sure
# that you know exactly what you are working with

rm(list=ls()) # clears all objects in workspace
ls() # check to see that the workspace is empty

# Let's read in the data and put it into an object called dat
dat <- read.csv(
	file = "2010.SoyFace.micromet.sample.csv",
	header = TRUE,
	sep = ",", 
	as.is = TRUE
)

# R is an object-ariented programming language
# this means that each variable is an "object"
# that can be queried or manipulated
# "dat" is an arbitray name, I could just as easily
# have put the data in an object named "cookies"
# It is important to note that R is case-sensitive
# cookies ≠ Cookies ≠ COOKIES

# read.csv() is a function used to import csv files.
# all functions in R follow the form of
# function()
# options for the function you are using are given
# inside of the parenthesis, separated by commas "(option 1 = 1, option2 = 2)"

# you can read about the function by typing
?read.csv
# or see the basic inputs and sometimes source code by typing
read.csv

# you can access documentation about any function in R in this manner
# The documentation is your friend, it is easy to access,
# consistent in form, and there are almost always examples at the end 
# of the documentation for any given function
# It may however take a bit to get used to they layout



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

# Here are some methods that all do the same thing
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






# PLOTTING ##### ##### ##### ##### ##### ##### ##### 

# Let's plot this data.  You will need to 
# load the library lattice, it is one of MANY packages
# available for the R language
# http://cran.r-project.org/web/packages/
library(lattice)

# xyplot uses a formula for plotting with the form
# y1 + y2 ~ x | factor1 + factor2
xyplot(
	x = Ta + Ts ~ Hour | as.factor(DOY),
	data = dat,
	type = "l" # Line type, try "b" and "p"
)

# Notice I treat DOY as a factor, this allows xyplot
# to treat DOY as a unique factor instead of a numeric 
# vector, you can try plotting DOY without, as.factor(),
# the results are similar but the panels use a marker to
# indicate progression along the numerical vector

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
	groups = DOY, # grouping factor
	type = "l", 			
	auto.key = list(TRUE, space = "right", points = F, lines = T)
)

# But really we want to see temperature as a single response across days,
# to do so we need an time vector which includes both day and time
# to assign the result to an object we use "<-" this is interchangeable with =
# however <- is less ambiguous, in that it is clearly not an option within a function

# Create Decimal Day (also known as Julian day)
DOY.dec <- dat$DOY + dat$Hour/24
DOY.dec <- dat$DOY + dat$Hr/24 + dat$Min/1440 # Same as above
length(DOY.dec) # this vector has 1582 values
dim(dat) # gives the dimensions of the data.frame

# [1] 1582   10 
# [row] length width
# notice dat has the same length as DOY.dec
					
dat <- cbind(DOY.dec, dat) #column bind function
# We have bound DOY.dec to dat, possible because they are of the same length
# Alternatively create the vector and tack on to the right of the data.frame
# dat$DOY.dec <- dat$DOY + dat$Hour/24

# now inspect
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






# Daily Max Min ##### ##### ##### ##### ##### ##### ##### 

# Let's complete some more complicated tasks
# calculate Daily Maximum Air Temperature
# we will use the functions max and min
# we are also using ?tapply, which allows us to apply
# a function along an index, here we use dat$DOY 
# which it interprets as 
unique(dat$DOY) #[1] 200 201 202 203 204 205 206 207 208 209 210
# and applies our function each day


Ta.max <- tapply(X = dat$Ta, INDEX = dat$DOY, FUN = max)
Ta.max  # vector of daily Temp max, with horizontal DOY index
Ta.max["205"] # or
Ta.max[6]

#Daily Min
Ta.min <- tapply(dat$Ta, dat$DOY, min)







# Model ##### ##### ##### ##### ##### ##### ##### 

# Next lets move to building and using the model
# Fourier Temperature Series (Cambell & Norman p.23)
# we will definite the equations as functions,
# so that we can give simply give the function
# our parameter and produce an output 

# function gamma - accepts input time
fgamma <- function(time){
	0.44 - 	0.46 * sin( { ( pi/12 ) * time } + 0.9 ) +
		0.11 * sin( { 2 *  ( pi/12 ) * time } + 0.9 )
}

# Fourier Series 
fs <- function(time, Tn, Tx){
	Tx * fgamma(time) + Tn * (1 - fgamma(time))
}

# using gamma
fgamma(10) # [1] 0.59295
fgamma(0:10)

#Example 2.3 Page 23
fs(10, Tn = 5, Tx = 23) # [1] 15.673

# Yep, thats it! The challenging part however
# is applying these functions across the time vector appropriately
# We will explore these issues below







# Indicies for Ta.max and Ta.min to be applied by 
# time of Day
# 0:5			(Tx[i - 1], Tn)
# 5:14			(Tx, Tn)
# 14:24		(Tx[1], Tn[i + 1])
# NOTE: the book has a typo for 14:24 written as (Tx[i - 1], Tn[i + 1])

# Remember we cannot predict all times,
# On Day one there are no inputs for Tx[i -1]
# and Day 10 we have no inputs Tn[i + 1]

# to write this model we will be using a for-loop,
# to loop over the data with a particular index value

# For example
for(i in 1:10){ print(i)}
# this steps through the sequence 1:10
# and assigns each number, in the sequence to i,
# outputs i to the console with print(i),
# then moves to the next number in the sequence, 
# until it reaches 10


# Here we assign each unique DOY to day, and print day to the output
for( day in unique(dat$DOY) ){ print(day) } 

# ok, but running the for loop left day in the workspace, 
# lets look
ls()
# [1] "dat"     "day"     "DOY.dec" "fs"      "fgamma"   "i"       "Ta.max"  "Ta.min" 

# ah, there are a bunch of objects in the workspace, let's get rid of a couple
rm(day, i, DOY.dec)

# now
ls()
# [1] "dat"    "fs"     "fgamma"  "Ta.max" "Ta.min"

# nice, much cleaner




# Applying the model ######## ######## ######## ########
# Build an index for each time set of times
# apply each equation to match these times

Days <- unique(dat$DOY) #index of days
Days 

# we only step through days
Days[2:{length(Days) - 1}] # [1] 201 202 203 204 205 206 207 208 209

# because we need Ta.max and Ta.min values from the first and last
# days to predict the temperature for the second and next to last day

# also
# as I showed before, the Ta.max vector has a horizontal index
# which is a label coresponding to the DOY
# so here we will write
 as.character(201 - 1) # to select the temperature record
# from the previous day

# so 
Ta.max[as.character(201 - 1)]



# Using everything we have learned up to this point, I am going to jump right in.

# Empty Vector of NA "Not Available" Values
dat$fTa <- rep(NA, length(dim(dat)[1]))
Days <- unique(dat$DOY) #index of days

for(day in Days[2:{length(Days) - 1}]){
	
	# Time index < or = 5 within the current day (dat$DOY %in% day)
	time <- which(dat$Hour <= 5 & dat$DOY %in% day) 
	
	dat$fTa[time] <-	Ta.max[as.character(day - 1)] * fgamma( dat$Hour[time] ) + 
				Ta.min[as.character(day)] * { 1 - fgamma( dat$Hour[time] ) }

	# Time index > 5  & Time < or = 14
	time <- which(dat$Hour > 5 & dat$Hour <= 14 & dat$DOY %in% day)  

	dat$fTa[time] <-	Ta.max[as.character(day)] * fgamma( dat$Hour[time] ) + 
				Ta.min[as.character(day)] * { 1 - fgamma( dat$Hour[time] ) }

	# Time index > 14
	time <- which(dat$Hour > 14 & dat$DOY %in% day) 

	dat$fTa[time] <- 	Ta.max[as.character(day)] * fgamma( dat$Hour[time] ) + 
				Ta.min[as.character(day + 1)] * { 1 - fgamma( dat$Hour[time] ) }
}





# There we go, Let's plot the data!

xyplot(
	x = fTa + Ta ~ DOY.dec,
	data = dat,
	type = "l",		# Line type, try "b" and "p"
	auto.key = list(TRUE, points = F, lines = T),
	main = "SoyFace Temperature Record",
	xlab = "Julian Day"
)


# oh, interesting, we can immediately see that on some nights temperature
# is underpredicted

# on days in which the night time temperature falls much lower than that of the previous day, 
# that the daily minimum occurs at the end of the day and not in the morning,
# now this doesn't reflect the way the model was designed, where we use the daily
# minimum and the previous day's max to determine the early morning hours in
# which the daily morning minimum is reached.

# As we have learned in class, temperature decreases over the course of the night,
# not reaching it's minimum until sometime around 4 or 5 AM, just prior to sun rise
# when solar radiation is applied to the surface.

# ok, so let's fix this by calculating Ta.min only from those times before 10 am
# this follows on the max/min weather station data collection mothod, where
# temperatures are logged around 10 AM, recording the previous day's maximum
# and the current morning minimum

Ta.min.old <- tapply(dat$Ta, dat$DOY, min)
Ta.min <- tapply(dat$Ta[dat$Hr < 10], dat$DOY[dat$Hr < 10], min)

#and compare
Ta.min.old
Ta.min


# Re run the model and plot
dat$fTa <- rep(NA, length(dim(dat)[1]))
Days <- unique(dat$DOY) #index of days

for(day in Days[2:{length(Days) - 1}]){
	
	# Time index < or = 5 within the current day (dat$DOY %in% day)
	time <- which(dat$Hour <= 5 & dat$DOY %in% day) 
	
	dat$fTa[time] <-	Ta.max[as.character(day - 1)] * fgamma( dat$Hour[time] ) + 
				Ta.min[as.character(day)] * { 1 - fgamma( dat$Hour[time] ) }

	# Time index > 5  & Time < or = 14
	time <- which(dat$Hour > 5 & dat$Hour <= 14 & dat$DOY %in% day)  

	dat$fTa[time] <-	Ta.max[as.character(day)] * fgamma( dat$Hour[time] ) + 
				Ta.min[as.character(day)] * { 1 - fgamma( dat$Hour[time] ) }

	# Time index > 14
	time <- which(dat$Hour > 14 & dat$DOY %in% day) 

	dat$fTa[time] <- 	Ta.max[as.character(day)] * fgamma( dat$Hour[time] ) + 
				Ta.min[as.character(day + 1)] * { 1 - fgamma( dat$Hour[time] ) }
}

pTemp <- xyplot(
	x = fTa + Ta ~ DOY.dec,
	data = dat,
	type = "l",		# Line type, try "b" and "p"
	auto.key = list(TRUE, points = F, lines = T),
	main = "SoyFace Temperature Record",
	xlab = "Julian Day"
)

print(pTemp)

# Much better!

# save the plot as a pdf
pdf(file = "predictedTemp.pdf")
	print(pTemp)
dev.off()

# Hey, now you are a  useR! R is a fantastic tool
# for both investigating datasets, and for plotting and modeling data
# again feel free to email me or instant message on gchat if you get stuck

# I have re-written the model to be more concise 
# take a look at run.model.R for useage, and model.Functions.R for the source