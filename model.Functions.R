# (Campbell & Norman, 1998, page 23)
gamma <- function(time){
	0.44 - 	0.46 * sin( { ( pi/12 ) * time } + 0.9 ) +
	0.11 * sin( { 2 *  ( pi/12 ) * time } + 0.9 )
}

# (Campbell & Norman, 1998, page 23)
fs <- function(time, Tn, Tx){
	Tx * gamma(time) + Tn * (1 - gamma(time))
}

#extact formulas
extract.formula<-function(formu){
	if (!is(formu,"formula"))
		stop("X should be a valid formula of form \n y ~ x1 + x2 | conditional")
	resp <- formu[[2]]
	if( length(formu[[3]]) < 2 ){ formu <- formu }else{ formu <- formu[[3]] }
	
	flattener <- function(f) {if (length(f)<3) return(f);
	                            c(Recall(f[[2]]),Recall(f[[3]]))}

	if (formu[[1]] == '|'){
		condi 	<- formu[[3]]
		vars 	<- flattener(formu[[2]])
	}else if(formu[[1]] == '+' & length(formu) < 4){
		condi 	<- NA
		vars 	<- c(formu[[2]], formu[[3]])
	}else if(length(formu[[3]]) < 2){
		condi 	<- NA
		vars 	<- formu[[3]]
	}

	list(resp=resp,condi=condi,vars=vars)
}


Ta = dat$Ta
time.vect = dat$Hour
time.inc = dat$DOY


# (Campbell & Norman, 1998, page 23)
modelTemp <- function(Ta = NULL, time.vect = NULL, time.inc, Tx = NULL, Tn = NULL){
	
	# Daily Max/Min
	if(!is.null(Ta) == TRUE){
		Tx <- tapply(Ta, time.inc, max)
		Tn <- tapply(Ta, time.inc, min)
	}
	
	#Construct Time Step when none available
	if(is.null(time.vect) == TRUE){
		time.vect <- expand.grid(Hr = 0:24, Min = (0:50)/60)
		time.vect <- time.vect$Hr + time.vect$Min
		time.vect <- time.vect[order(time.vect)]
	}

	# Empty Vector
	fTa <- rep(NA, length(time.vect))

	time.step <- unique(time.inc)
	for(utime in time.step[2:length(time.step)]){
		time <- which(time.vect <= 5 & time.inc %in% utime) 

		fTa[time] <-	Tx[as.character(utime - 1)] * gamma( time.vect[time] ) + 
						Tn[as.character(utime)] 	* { 1 - gamma( time.vect[time] ) }
	}

	for(utime in time.step[2:length(time.step)]){
		time <- which(time.vect > 5 & time.vect <= 14 & time.inc %in% utime)  

		fTa[time] <- 	Tx[as.character(utime)] * gamma( time.vect[time] ) + 
						Tn[as.character(utime)] * { 1 - gamma( time.vect[time] ) }
	}
	
	for(utime in time.step[2:{length(time.step) - 1}]){
		time <- which(time.vect > 14 & time.inc %in% utime) 

		fTa[time] <- 	Tx[as.character(utime)] * gamma( time.vect[time] ) + 
						Tn[as.character(utime + 1)] * { 1 - gamma( time.vect[time] ) }
	}
	
	return(fTa)
}

# (Campbell & Norman, 1998, page 23)
GDD <- function(dat, Tbase, Tmax){



}