# Patrick Schmitz
# p.schmitz@gmail.com
# September 6 2010

setwd("~/Dropbox/classes/2010.IB.542/code/2010.IB.542.Temperature/") # Pat's directory\
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

library(lattice)

# There we go, Let's plot the data!
predict <- xyplot(
	x = fTa + Ta ~ DOY.dec,
	data = dat,
	type = "l",
	main = "SoyFace Temperature Record",
	xlab = "Julian Day",
	scales = list(x = list(tick.number = 15)),
	auto.key = list(TRUE, points = F, lines = T),
	panel = function(...){ 
		panel.grid(h=-1,v=-15) 
		panel.xyplot(...)
	}
)

pdf("~/Desktop/modelTempfixed.pdf", width = 10)
print(predict)
dev.off()


# Other interesting Plots


# Plot a single Day
Day <- 207
xyplot(
	x = fTa + Ta ~ DOY.dec | as.factor(DOY),
	data = dat[dat$DOY %in% Day,],
	type = "l",
	main = paste("SoyFace Temperature Record, Day : ", Day, sep = ""),
	xlab = "Julian Day",
	auto.key = list(TRUE, points = F, lines = T)
)

#Plot all days in panels
xyplot(
	x = fTa + Ta ~ Hour | as.factor(DOY),
	data = dat,
	type = "l",
	main = "SoyFace Temperature Record",
	xlab = "Hour",
	auto.key = list(TRUE, points = F, lines = T)
)

# Plot all days as panels, with each plot separate
# this can then be handed off to pdf() and each plot
# will be it's own page
# we use the layout = c(1,1) to do this
# similarly if we wanted a 4 x 4 plot we could use
# layout = c(4,4) or 2 x 1 layout = c(2,1)

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


# Calculate Growing Degree Days
source("model.Functions.R")
gdd.out <- thermalTime(
	Ta = dat$Ta, 
	Tbase = 14,
	time.inc = dat$DOY
)

xyplot(dTi + tau.n + Tx + Tn ~ time.inc, 
	gdd.out, 
	type = "b",
	auto.key = T
)