# (Campbell & Norman, 1998, page 23)
gamma <- function(time){
	0.44 - 	0.46 * sin( { ( pi/12 ) * time } + 0.9 ) +
	0.11 * sin( { 2 *  ( pi/12 ) * time } + 0.9 )
}

# (Campbell & Norman, 1998, page 23)
fs <- function(time, Tn, Tx){
	Tx * gamma(time) + Tn * (1 - gamma(time))
}

# (Campbell & Norman, 1998, page 23)
modelTemp <- function(Ta = NULL, time.vect = NULL, time.inc, Tx = NULL, Tn = NULL){
	
	# this model can accept data in two forms

	# continuous 
	# Ta - air temp measured continuously
	# time.vect - time vector coresponding to measurements of Ta
	# time.inc - unit incriment over which to determine max/min time and model temp

	# Daily
	# Tx - daily max
	# Tn - daily min
	# time.inc - time incriment over which to integrate

	if(!is.null(Tx) == TRUE & !is.null(Tn) == TRUE){
		names(Tx) <- time.inc
		names(Tn) <- time.inc
	}
	
	# Daily Max/Min
	if(!is.null(Ta) == TRUE){
		Tx <- tapply(Ta, time.inc, max)
		Tn <- tapply(Ta, time.inc, min)
	}
	
	# Construct Time Step when none available
	if(is.null(time.vect) == TRUE){
		time.vect <- expand.grid(Hr = 0:24, Min = (0:50)/60)
		time.vect <- time.vect$Hr + time.vect$Min
		time.vect <- time.vect[order(time.vect)]
	}
	
	# create time index
	ts <- expand.grid(time.inc = time.inc, time.vect = time.vect)
	ts <- ts[order(ts$time.inc, ts$time.vect),]

	# Empty Vector
	ts$fTa <- rep(NA, dim(ts)[1])

	time.step <- unique(time.inc)
	for(utime in time.step[2:{length(time.step) - 1}]){
		time <- which(ts$time.vect <= 5 & ts$time.inc %in% utime) 

		ts$fTa[time] <-	Tx[as.character(utime - 1)] * gamma( ts$time.vect[time] ) + 
						Tn[as.character(utime)] 	* { 1 - gamma( ts$time.vect[time] ) }
	}

	for(utime in time.step[2:{length(time.step) - 1}]){
		time <- which(ts$time.vect > 5 & ts$time.vect <= 14 & ts$time.inc %in% utime)  

		ts$fTa[time] <- 	Tx[as.character(utime)] * gamma( ts$time.vect[time] ) + 
						Tn[as.character(utime)] * { 1 - gamma( ts$time.vect[time] ) }
	}
	
	for(utime in time.step[2:{length(time.step) - 1}]){
		time <- which(ts$time.vect > 14 & ts$time.inc %in% utime) 

		ts$fTa[time] <- 	Tx[as.character(utime)] * gamma( ts$time.vect[time] ) + 
						Tn[as.character(utime + 1)] * { 1 - gamma( ts$time.vect[time] ) }
	}
	
	return(ts[ts$time.inc %in% time.step[2:{length(time.step) - 1}], ])
}

# (Campbell & Norman, 1998, page 30-32)
thermalTime <- function(time.inc, Tbase, Ta = NULL, Tx = NULL, Tn = NULL, Tmax = NULL, Tcut = NULL){

	# Daily Max/Min
	if(!is.null(Ta) == TRUE){
        Tx <- tapply(Ta, time.inc, max)
        Tn <- tapply(Ta, time.inc, min)
	}
	
	Ti <- (Tx + Tn) / 2
	dTi <- Ti - Tbase
	
	# Correct for Ti < Tbase
	dTi[Ti <= Tbase] <- 0
	
	#Correct for Tmax <= Ti < Tcut 
	sel <- which(Ti >= Tmax & Ti < Tcut)
	dTi[sel] <- { ( { Tcut - Ti[sel] } / { Tcut - Tmax } ) * Tmax } - Tbase 
	
	# Accumulated thermal time
	tau.n <- cumsum(dTi)
	
	return(list(dTi, tau.n))
}