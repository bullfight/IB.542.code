# Patrick Schmitz
# p.schmitz@gmail.com
# September 6 2010

# setwd("~/Dropbox/classes/2010.IB.542/code/2010/IB.542.Temperature/") # Pat's directory
rm(list=ls())

# load
dat <- read.csv("2010.SoyFace.micromet.sample.csv", header = TRUE, as.is = TRUE )
DOY.dec <- dat$DOY + dat$Hour/24
dat <- cbind(DOY.dec, dat) 

#Load Functions
source("model.Functions.R")

# Run Fourier Transform to predict temperature 
# at an arbitrary timestep
fTa <- modelTemp(
	Ta = dat$Ta, 
	time.vect = dat$Hour, 
	time.inc = dat$DOY
)

dat <- cbind(dat, fTa)

# There we go, Let's plot the data!
xyplot(
	x = fTa ~ DOY.dec,
	data = dat,
	type = "l",
	main = "SoyFace Temperature Record",
	xlab = "Julian Day",
	auto.key = list(TRUE, points = F, lines = T)
)

xyplot(
	x = fTa + Ta ~ DOY.dec,
	data = dat,
	type = "l",
	main = "SoyFace Temperature Record",
	xlab = "Julian Day",
	auto.key = list(TRUE, points = F, lines = T)
)

DOY <- 207
xyplot(
	x = fTa + Ta ~ DOY.dec | as.factor(DOY),
	data = dat[dat$DOY %in% DOY,],
	type = "l",
	main = paste("SoyFace Temperature Record, Day : ", DOY, sep = ""),
	xlab = "Julian Day",
	auto.key = list(TRUE, points = F, lines = T)
)

xyplot(
	x = fTa + Ta ~ Hour | as.factor(DOY),
	data = dat,
	type = "l",
	main = "SoyFace Temperature Record",
	xlab = "Hour",
	auto.key = list(TRUE, points = F, lines = T)
)

daily.plot <- xyplot(
	x = fTa + Ta ~ Hour | as.factor(DOY),
	data = dat,
	type = "l",
	layout = c(1,1),
	main = "SoyFace Temperature Record",
	xlab = "Hour",
	auto.key = list(TRUE, points = F, lines = T)
)

pdf(file = "PLOTS/dailyplot.pdf")
	print(daily.plot)
dev.off()