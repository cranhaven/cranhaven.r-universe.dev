
# Author: tim
###############################################################################
# contains functions related to the generalized growth balance method


#' @title calculate the root means square of the error to help find optimal age range
#' 
#' @description Called by \code{ggbgetAgesFit()} whenever the user does not want to manually determine the age range used to determine registration coverage. Probably no need to be called by top-level users. If a user would rather determine the optimal age range some other way, then look to \code{ggbcoverageFromYear()} where \code{ggbgetRMS} is called and add another condition or make it call something else.
#' 
#' @param agesi the set of ages used for this iteration
#' @param codi \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, and \code{$age}. 
#' 
#' @return the RMSE
#' 
#' @export 

ggbgetRMS <- function(agesi, codi){
	codi <- ggbFittedFromAges(codi, agesfit = agesi)
	# get root mean square of residuals
	# I'd do this with magrittr, but just to stay dependency-free...
	sqrt(
		sum(
			  (
				 (codi$leftterm[codi$age %in% agesi] - codi$fitted[codi$age %in% agesi]) ^ 2
			   )
	        ) / length(agesi)
         )
	# no checks here for NAs...
}



#' @title estimate death registration coverage for a single year/sex/region using the GGB method
#' 
#' @description Given two censuses and an average annual number of deaths in each age class between censuses, we can use stable population assumptions to estimate the degree of underregistration of deaths. The method is based on finding a best-fitting linear relationship between two modeled parameters (right term and left term), but the fit, and resulting coverage estimate, depend on exactly which age range is taken. This function either finds a nice age range for you automatically, or you can specify an exact vector of ages. Called by \code{ggb()}. Users probably don't need to call this directly. Just use \code{ggb()} instead.
#' 
#' Census dates can be given in a variety of ways: 1) using Date classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing integers. If only \code{year1} and \code{year2} are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month. 
#' 
#' @param codi \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, and \code{$age}.
#' @param exact.ages optional. use an exact set of ages to estimate coverage.
#' @param minA the minimum of the age range searched. Default 15
#' @param maxA the maximum of the age range searched. Default 75
#' @param minAges the minimum number of adjacent ages needed as points for fitting. Default 8
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' @return a \code{data.frame} with columns for the coverage coefficient, and the min and max of the age range on which it is based. 
#' 
#' @export

ggbcoverageFromYear <- function(codi, 
								exact.ages = NULL, 
								minA = 15, 
								maxA = 75, 
								minAges = 8, 
								deaths.summed = FALSE
								){
	
	# if exact.ages is given, we override other age-parameters
	if (!is.null(exact.ages) & length(exact.ages) >= 3){
		if (min(exact.ages) < minA){
			minA <- min(exact.ages)
		} 
		if (max(exact.ages) > maxA){
			maxA <- max(exact.ages)
		}
		if (minAges < length(exact.ages)){
			minAges <- length(exact.ages)
		}
	}
	
	# TR: test add this step, just in case
	codi    <- codi[ order(codi$age), ]
	
	codi    <- ggbMakeColumns(codi = codi, 
							  minA = minA, 
							  maxA = maxA, 
							  deaths.summed = deaths.summed)
	
	if (!is.null(exact.ages) & length(exact.ages) >= 3){
		agesfit <- exact.ages
	} else {                          
		agesfit <- ggbgetAgesFit(codi = codi, 
								 minA = minA, 
								 maxA = maxA, 
								 minAges = minAges, 
								 deaths.summed = deaths.summed)
	}
	
	# TR: added 17 June, 2016. Get Lambda to adjust first census:
	# TR: 3-4-2017 no longer optional

	coefs    <- slopeint(codi, agesfit)
	dif      <- yint2(codi)
	# TR: 3-4-2017 this is k1/k2
	delta    <- exp(coefs$a * dif)
	# TR: 3-4-2017 these are calulcated per the IUSSP spreadsheet
	k1       <- ifelse(delta > 1, 1, delta)
	k2       <- k1 / delta
	# this is the basic formula
	coverage <- ggbcoverageFromAges(codi = codi, agesfit = agesfit)
	result   <- data.frame(cod = unique(codi$cod), 
			   coverage = coverage, 
			   lower = min(agesfit), 
			   upper = max(agesfit))
	# can't have NULL column...
    # TR: 3-4-2017 no longer optional, also returning more goods
   
	result   <- cbind(result, a = coefs$a, b = coefs$b, delta = delta, k1 = k1, k2 = k2)
	result
}



#' @title make the growth-adjusted quasi life table columns required by GGB method
#' 
#' @description Called by \code{ggbChooseAges()} and \code{ggbcoverageFromYear()}. This simply modulates some code that would otherwise be repeated. Users probably don't need to call this function directly. If columns produced by \code{ggbMakeColumns()} are not present, then we call it here just to keep things from breaking.
#' 
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{ggb()}
#' @param agesfit an a priori set of ages for which to calculate the fit
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' @return codi, with many columns added, most importantly \code{$rightterm}, \code{$leftterm}, and \code{$exclude}.
#' 
#' @export

ggbFittedFromAges <- function(codi, agesfit, deaths.summed = FALSE){
	
	if (! "leftterm" %in% colnames(codi)){
		codi <- ggbMakeColumns(codi=codi, deaths.summed = deaths.summed)
	}
	# assumes ggbMakeColumns() has been run.
#	slope       <- with(codi, 
#			sd(leftterm[age %in% agesfit]) /  sd(rightterm[age %in% agesfit])
#	)
#	intercept   <-  with(codi, 
#			(mean(leftterm[age %in% agesfit]) * slope - mean(rightterm[age %in% agesfit]))
#	) 
	coefs       <- slopeint(codi, agesfit)
	codi$fitted <- coefs$a + codi$rightterm * 1/coefs$b 
	codi
}

#' @title given a set of ages, what is the implied death registration coverage?
#' 
#' @description For a single year/sex/region of data (formatted as required by \code{ggb()}), what is the registration coverage implied by a given age range? Called by \code{ggbcoverageFromYear()} and \code{ggbChooseAges()}.
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{ggb()}
#' @param agesfit an integer vector of ages, either returned from \code{ggbgetAgesFit} or user-supplied.
#' @return numeric. the estimated level of coverage.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.

#' @export
ggbcoverageFromAges <- function(codi, agesfit, deaths.summed = FALSE){
	if (! "leftterm" %in% colnames(codi)){
		codi <- ggbMakeColumns(codi = codi, 
				               minA = min(agesfit), 
							   maxA = max(agesfit), 
							   deaths.summed = deaths.summed)
	}
	# this is the coverage estimate
	#with(codi, sd(rightterm[age %in% agesfit])/ sd(leftterm[age %in% agesfit]))
	1 / slopeint(codi = codi, agesfit = agesfit)$b
}



#' @title make the growth-adjusted quasi life table columns required by GGB method
#' 
#' @description Called by \code{ggbChooseAges()} and \code{ggbcoverageFromYear()}. This simply modulates some code that would otherwise be repeated. Users probably don't need to call this function directly. 
#' 
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{ggb()}
#' @param minA the minimum of the age range searched. Default 15
#' @param maxA the maximum of the age range searched. Default 75
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' @return codi, with many columns added, most importantly \code{$rightterm}, \code{$leftterm}, and \code{$exclude}.
#' 
#' @export
ggbMakeColumns <- function(codi, minA = 15, maxA = 75, deaths.summed = FALSE){
	codi                   <- avgDeaths(codi = codi, deaths.summed = deaths.summed)
	AgeInt                 <- detectAgeInterval(Dat = codi, MinAge =  minA, MaxAge = maxA, ageColumn = "age")
	dif                    <- yint2(codi)
		
		# a quick recheck of classes:
#	codi <- within(codi, {
#				deathsAvg <- as.double(deathsAvg)
#				pop1 <- as.double(pop1)
#				pop2 <- as.double(pop2)
#			})
	codi$deathsAvg <- as.double(codi$deathsAvg)
	codi$pop1      <- as.double(codi$pop1)
	codi$pop2      <- as.double(codi$pop2)
			
	# group inf if necessary
	codi                   <- group01(codi)
	N                      <- nrow(codi)
	# now actual column creation
#	codi      <- within(codi, {
#			pop1cum        <- rev(cumsum(rev(pop1)))
#			pop2cum        <- rev(cumsum(rev(pop2))) 
#			deathcum       <- rev(cumsum(rev(deathsAvg)))
#			birthdays      <- c(0, sqrt(pop1[ -N  ] * pop2[ -1 ])) / AgeInt
#			Tx             <- sqrt(pop1cum * pop2cum)
#			cumgrowth      <- log(pop2cum / pop1cum) / dif
#			rightterm      <- deathcum / Tx
#			leftterm       <- (birthdays / Tx) - cumgrowth
#			exclude        <-  Tx != 0 & birthdays != 0 & age >= minA & age <= maxA
#		         })
	
	codi$pop1cum        <- rev(cumsum(rev(codi$pop1)))
	codi$pop2cum        <- rev(cumsum(rev(codi$pop2))) 
	codi$deathcum       <- rev(cumsum(rev(codi$deathsAvg)))
	codi$birthdays      <- c(0, sqrt(codi$pop1[ -N  ] * codi$pop2[ -1 ])) / AgeInt
	codi$Tx             <- sqrt(codi$pop1cum * codi$pop2cum)
	codi$cumgrowth      <- log(codi$pop2cum / codi$pop1cum) / dif
	codi$rightterm      <- codi$deathcum / codi$Tx
	codi$leftterm       <- (codi$birthdays / codi$Tx) - codi$cumgrowth
	codi$exclude        <- codi$Tx != 0 & codi$birthdays != 0 & codi$age >= minA & codi$age <= maxA
	
	codi
}



#' @title determine the age range that minimizes the mean squared error
#' @description Called by \code{ggbcoverageFromYear()} whenever \code{exact.ages} are not given. This automates what one typically does visually.
#' @seealso code{\link{ggbChooseAges}}
#' 
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{ggb()}
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' 
#' @return a vector of ages that minimizes the RMSE
#' 
#' @export

ggbgetAgesFit <- function(codi, minA = 15, maxA = 75, minAges = 8, deaths.summed = FALSE){
		
	if (!"leftterm" %in% colnames(codi)){
		codi <- ggbMakeColumns(codi = codi, 
				               minA = minA, 
							   maxA = maxA, 
							   deaths.summed = deaths.summed)
	}
	
	maxAges   <- sum(codi$exclude)
	agesUniv  <- codi$age[codi$exclude]
	
	FirstAges <- agesUniv[agesUniv < 30]
	
	ind       <- 0
	agesL     <- list()
	# determine ages to test
	for (Nr in maxAges:minAges){ #
		its <- length(agesUniv) - Nr + 1
		for (set in 1:its){ # 
			ind <- ind + 1
			agesL[[ind]] <- agesUniv[set:(set+Nr-1)]
		}
	}
	
	# these are the ages that give the best r2 or RMS
	# this would be much better using pipes, but hey, one less dependency...
	agesfit     <- agesL[[which.min(unlist(lapply(agesL, ggbgetRMS, codi = codi)))]]
	agesfit
}



#' @title estimate death registration coverage using the GGB method
#' 
#' @description Given two censuses and an average annual number of deaths in each age class between censuses, we can use stable population assumptions to estimate the degree of underregistration of deaths. The method is based on finding a best-fitting linear relationship between two modeled parameters (right term and left term), but the fit, and resulting coverage estimate, depend on exactly which age range is taken. This function either finds a nice age range for you automatically, or you can specify an exact vector of ages. 
#' 
#' @details Census dates can be given in a variety of ways: 1) using Date classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing integers. If only \code{year1} and \code{year2} are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month. If you want coverage estimates for a variety of intercensal periods/regions/by sex, then stack them, and use a variable called \code{$cod} with unique values for each data chunk. Different values of \code{$cod} could indicate sexes, regions, intercensal periods, etc. The \code{$deaths} column should refer to the average annual deaths for each age class in the intercensal period. Sometimes one uses the arithmetic average of recorded deaths in each age, or simply the average of the deaths around the time of census 1 and census 2. To identify an age-range in the traditional visual way, see \code{ggbChooseAges()}, when working with a single year/sex/region of data. The automatic age-range determination feature of this function tries to implement an intuitive way of picking ages that follows the advice typically given for doing so visually. We minimize the square of the average squared residual between the fitted line and right term.
#' 
#' @param X \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$age}, and \code{$cod} (if there are more than 1 region/sex/intercensal period).
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param exact.ages optional. A user-specified vector of exact ages to use for coverage estimation
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#'
#' @return a \code{data.frame} with columns for the coverage coefficient \code{$coverage}, the minimum \code{$lower} and maximum \code{$upper} of the age range on which it is based. \code{$a} and \code{$b} give the intercept and slope of the line on which the coverage estimate is based. \code{$delta}, \code{$k1}, and \code{$k2}  are further derived quantities that may be interesting for advanced users. Rows indicate data partitions, as indicated by the optional \code{$cod} variable.
#' 
#' @export
#' @references 
#' Hill K. Estimating census and death registration completeness. Asian and Pacific Population Forum. 1987; 1:1-13.
#' 
#' Brass, William, 1975.  Methods for Estimating Fertility and Mortality from Limited and Defective Data, Carolina Population Center, Laboratory for Population Studies, University of North Carolina, Chapel Hill.
#' 
#' @examples 
#' # The Mozambique data
#' res <- ggb(Moz)
#' res
#' # The Brasil data
#' BM <- ggb(BrasilMales)
#' BF <- ggb(BrasilFemales)
#' head(BM)
#' head(BF)
#' 
ggb <- function(
		X, 
		minA = 15, 
		maxA = 75, 
		minAges = 8, 
		exact.ages = NULL, 
		deaths.summed = FALSE){         
	
	# TR: modularized Apr 2, 2017
	tab1        <- headerPrep(X)
	# iterate over whatever it happens to be: regions, years
	coverages   <- as.data.frame(
			      do.call(
					rbind,
					lapply(
					    tab1, 
					    ggbcoverageFromYear, 
					    exact.ages = exact.ages,
						minA = minA, 
						maxA= maxA,
						minAges = minAges,
						deaths.summed = deaths.summed
					)))
	
	# this has cod as a column, but no year, sex. 
	return(coverages)
}

############################################################
# Note: GGB is used to determine ages for all methods here #
# ergo, these plotting functions are only implemented for  #
# GGB method                                               #
############################################################

# functions related to ggb plotting.


#' @title does a given pairlist of x and y coordinates fall within the plot region?
#' 
#' @description Check to see if a point clicked falls in the plot or outside it. This function is used by \code{ggbChooseAges()}.
#' 
#' @param USR, as given by \code{par("usr")}
#' @param click a pairlist with elements \code{$x} and \code{$y}, as returned by \code{locator(1)}
#' 
#' @return logical. \code{TRUE} if in the plot region.
#' 
#' @export
inUSR <- function(USR, click){
	x <- click$x
	y <- click$y
	if (x < USR[1] | x > USR[2] | y < USR[3] | y > USR[4]){
		return(FALSE)
	}
	TRUE
}


#' @title which age is closest to the point clicked?
#' @description a utility function called by \code{ggbChooseAges()}.
#' 
#' @param xvec \code{$rightterm}, as given by \code{ggbMakeColumns()}
#' @param yvec \code{$lefttterm}, as given by \code{ggbMakeColumns()}
#' @param click a point given by \code{locator(1)}
#' @param age ages present in dataset
#' 
#' @return the age corresponding to the x,y pair of \code{$rightterm}, \code{$lefttterm} closest to the point clicked.
#' 
#' @export
guessage <- function(xvec,yvec,click,age){
	age[which.min(sqrt((xvec - click$x) ^ 2 + (yvec - click$y) ^ 2))]
}


#' @title adjust the range of ages used by \code{ggbChooseAges()}
#' @description a utility function called by \code{ggbChooseAges()}. After clicking a point, this function readjusts the age range
#' 
#' @param a an age specified by the user, as returned by \code{guessage()}
#' @param age ages present in dataset
#' @param agesfit the former age range used for calculating the coverage coefficient
#' 
#' @return the adjusted set of ages used for calculating the coverage coefficient
#' 
#' @export
adjustages <- function(a, age, agesfit){
	maxa <- max(agesfit)
	mina <- min(agesfit)
	
	isitamin <- abs(a - mina) < abs(a - maxa)
	if (isitamin){
		mina <- a
	} else {
		maxa <- a
	}
	age[age >= mina & age <= maxa]
}

#' @title get the slope the slope and intercept implied by a set of ages
#' @description Called by \code{ggbFittedFromAges()} and \code{ggbChooseAges()}
#' @param codi \code{data.frame} as produced by \code{ggbMakeColumns()}
#' @param agesfit a set of ages to estimate coverage from 
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' @return a pairlist with elements \code{$a} for the intercept and \code{$b} for the slope
#' @export
slopeint <- function(codi, agesfit, deaths.summed = FALSE){
	# a robustness measure
	if (! "leftterm" %in% colnames(codi)){
		codi <- ggbMakeColumns(codi, minA = min(agesfit), maxA = max(agesfit), deaths.summed = deaths.summed)
	}
	#age <- codi$age
	# TODO: find eq numbers to cite here
	slope       <- 	with(codi,sd(leftterm[age %in% agesfit]) /  
						sd(rightterm[age %in% agesfit]))
#	
	intercept   <- 	with(codi,mean(leftterm[age %in% agesfit]) * (1/slope) - 
			            mean(rightterm[age %in% agesfit]))
#	
	#coefs <- with(codi,lm(leftterm[age %in% agesfit]~rightterm[age %in% agesfit]))$coef
	
	list(a = intercept, b = slope)
}



#' @title interactively determine ages to use for estimating coverage
#' @description In a spreadsheet one would typically set up the GGB method to produce a plot that updates as the user changes the age range. This function implements that kind of work flow. This will be intuitive for spreadsheet users, but it does not scale well. Imagine you have 200 territorial units, then you would not want to repeat this task. \code{ggb()} does the same thing automatically. You can compare the age range you select manually with the one given back by \code{ggb()} as a diagnostic, for instance. To set up the plot device, just give a single year/region/sex of data. By default it will give the RMSE-optimized age range to start with, but you can specify a  vector of exact ages to use as well. All points are plotted, with a fitted line that has been set to a subset of the points, which is plotted in a different color. You can click any point to change the age range, and the plot updates accordingly, up to a maximum of 15 clicks so you don't waste your time. You can stop the plot by either clicking on the graphics device outside the plot area or clicking out the 15 tries (or more if you increase \code{maxit}).
#' @details If you want to send the results of this into \code{ggb()}, you can do so by setting \code{Exact.ages} to \code{seq(lower,upper,by=5)}, where \code{$lower}, and \code{$upper} are the results returned from \code{ggbChooseAges()} after you're done manually determining the age range.
#' 
#' @param codi \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, and \code{$age}.
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param exact.ages optional. A user-specified vector of exact ages to use for coverage estimation. 
#' @param maxit the maximum number of clicks you can take. Default 15.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' 
#' @return \code{data.frame} containing elements \code{$coverage}, \code{$lower}, \code{$upper}, and \code{ages}.
#' 
#' @importFrom grDevices gray
#' @importFrom graphics abline legend locator mtext par plot points rect segments text
#' @export
#' 
#' @examples
#' \dontrun{
#' # for interactive sessions only
#' # *click points to adjus age range used (yellow)
#' # *click in margin to stop and return coverage results
#' ggbChooseAges(Moz)
#' }

ggbChooseAges <- function(codi, 
		                  minA = 15, 
						  maxA = 75, 
						  minAges = 8, 
						  exact.ages = NULL, 
						  maxit = 15, 
						  deaths.summed = FALSE){
	# this is the automatic age selection.
	
	# only run if in anteractive r session...
    stopifnot(interactive())
	
	# reset ages if necessary
	if (!is.null(exact.ages) & length(exact.ages) >= 3){
		if (min(exact.ages) < minA){
			minA <- min(exact.ages)
		} 
		if (max(exact.ages) > maxA){
			maxA <- max(exact.ages)
		}
		if (minAges < length(exact.ages)){
			minAges <- length(exact.ages)
		}
	}
	codi    <- data.frame(codi)           
	colnames(codi) <- tolower(colnames(codi))

	# guess which column is the deaths column, rename it deaths
	codi    <- guessDeathsColumn(codi)
	
	# start GGB stuff
	codi    <- ggbMakeColumns(codi, minA, maxA, deaths.summed = deaths.summed)
	
	# some potential starting ages. either auto or self-supplied
	if (!is.null(exact.ages) & length(exact.ages) >= 3){
		agesfit <- exact.ages
	} else {
		agesfit <- ggbgetAgesFit(codi, minAges, deaths.summed = deaths.summed)
	}
	codi     <- ggbFittedFromAges(codi=codi, agesfit=agesfit, deaths.summed = deaths.summed)
	# starting values for abline
	si       <- slopeint(codi, agesfit)
	
	# this is the basic formula
	#coverage <- ggbcoverageFromAges(codi, agesfit)
	coverage <- 1/si$b
	# some objects used throughout
	age      <- codi$age
	leftt    <- codi$leftterm
	rightt   <- codi$rightterm
	
	# age ranges used for fitting
	amin    <- min(agesfit); amax <- max(agesfit)
	plot(rightt, leftt, asp = 1, pch = 19, col = "#00000050", cex = 1.6,
			xlab = "right term",
			ylab = "left term",
			main = paste0("Age range [",amin,
					",",amax,"], est. coverage = ",round(coverage*100,1)),
			sub = "(optimized age range)")
	# automatically fit line (RMS of ggb)
	abline(a = si$a, b = si$b, col = "blue")
	# shows points used to fit line
	points(rightt[age %in% agesfit], 
			leftt[age %in% agesfit], col = "#FFFF00", pch = 19, cex = 1.6)
	text(rightt, leftt, age, cex = .6)
	legend("bottomright",lty=1,col="blue",legend="fitted line",bty="n")
	# message to user
	mtext("*Adjusting ages*\nClick any age to change bounds for fitting\nClick in margin to quit",
			side = 3, line = -3, outer = FALSE)
	
	
	USR    <- par()$usr
	agesfit <- agesfit # keep old one...
	for (i in 1:maxit){
		# now enter into interactive loop
		clicki <- locator(1)
		# was the click inside the plot or outside the plot?
		IN     <- inUSR(USR, clicki)
		if (IN){
			
			# if it's inside the plot, then which is the closest age?
			a        <- guessage(rightt,leftt,clicki,age)
			# guess how to adjust ages based on which age was clicked
			agesfit  <- adjustages(a, age, agesfit)
			# new range of ages used for fitting
			amin     <- min(agesfit)
			amax     <- max(agesfit)
			
			# an estimate of the resulting coverage
			coverage <- 1/si$b
			# get params for abline..
			si       <- slopeint(codi, agesfit)
			
			# redraw plot
			plot(   rightt, 
					leftt, 
					asp = 1, 
					pch = 19, 
					col = "#00000050", 
					cex = 1.6,
					xlab = "right term",
					ylab = "left term",
					main = paste0("Age range [", amin,
							",", amax, "], est. coverage = %",round(coverage * 100, 1)),
					sub = "(optimized age range)")
			# new fitted slope, intercept
		    abline(a=0,b=1,col=gray(.8)) # line of perfection
			#
			abline(a = si$a, b = si$b, col = "blue")
			# indicate which points used with color
			points(rightt[age %in% agesfit], 
					leftt[age %in% agesfit], col = "#FFFF00", pch = 19, cex = 1.6)
			text(rightt, leftt, age, cex = .6)
			legend("bottomright", lty = 1, col = "blue", legend = "fitted line", bty = "n")
		} else {
			break
		}
	}
	
	# click outside the margin to return results
    out <- list(coverage = coverage, lower = min(agesfit), upper = max(agesfit), ages = agesfit)
	out
}

# end




