# Patrick Schmitz
# p.schmitz@gmail.com
# September 6 2010

#setwd("C:/Users/pschmitz/Desktop/ModelFolder")
# setwd("~/Desktop/ModelFolder") #on MAC OS X
rm(list=ls())

# load
dat <- read.csv(
					file = "2010.SoyFace.micromet.sample.csv",
					header = TRUE,
					sep = ",", 
					as.is = TRUE
)

DOY.dec <- dat$DOY + dat$Hour/24
dat <- cbind(DOY.dec, dat) 

#Load Functions
source("model.Functions.R")
dat <- modelTemp(dat)

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

pdf(file = "daily.plot.pdf")
	print(daily.plot)
dev.off()