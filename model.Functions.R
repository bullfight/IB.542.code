gamma <- function(time){
	0.44 - 	0.46 * sin( { ( pi/12 ) * time } + 0.9 ) +
					0.11 * sin( { 2 *  ( pi/12 ) * time } + 0.9 )
}

fs <- function(time, Tn, Tx){
	Tx * gamma(time) + Tn * (1 - gamma(time))
}

modelTemp <- function(dat){

	# Daily Max/Min
	Ta.max <- tapply(X = dat$Ta, INDEX = dat$DOY, FUN = max)
	Ta.min <- tapply(dat$Ta, dat$DOY, min)

	# Empty Vector
	dat$fTa <- rep(NA, length(dim(dat)[1]))

	Days <- unique(dat$DOY)
	for(day in Days[2:length(Days)]){
		time <- which(dat$Hour <= 5 & dat$DOY & dat$DOY %in% day) 

		dat$fTa[time] <-		Ta.max[as.character(day - 1)] * gamma( dat$Hour[time] ) + 
												Ta.min[as.character(day)] 	* { 1 - gamma( dat$Hour[time] ) }
	}

	for(day in Days[2:length(Days)]){
		time <- which(dat$Hour > 5 & dat$Hour <= 14 & dat$DOY %in% day)  

		dat$fTa[time] <- 		Ta.max[as.character(day)] * gamma( dat$Hour[time] ) + 
												Ta.min[as.character(day)] * { 1 - gamma( dat$Hour[time] ) }
	}
	
	for(day in Days[2:{length(Days) - 1}]){
		time <- which(dat$Hour > 14 & dat$DOY %in% day) 

		dat$fTa[time] <- 		Ta.max[as.character(day)] * gamma( dat$Hour[time] ) + 
												Ta.min[as.character(day + 1)] * { 1 - gamma( dat$Hour[time] ) }
	}
	
	return(dat)
}