
# Author: tim
###############################################################################
# contains functions related to Bennett-Horiuchi methods


#' @title estimate remaining life expectancy in the open age group
#' @description This calculation is based on an indirect method to reference the Coale-Demeny West model life table. First one makes a pseudo life table deaths column using some stable pop assumptions (different in SEG vs GGB-SEG). Then take the ratio of the sum of ages 10-39 to 40-59. These ratios have been worked out for each model life table level, so we can pick the level based on the ratio we produce from the data. From there, we just pick out the remaining life expectancy that corresponds to the top age in our data, which for now hopefully is not higher than 95. The model life tables do not go higher than 95 for now, but that's well beyond the range for this method family. If your data go beyond 85 or so, then just group down to 85, say, and estimate using that instead of keeping a high open age. Called by \code{segMakeColumns()} and \code{ggbsegMakeColumns()}, and not intended for direct user interface, because you need to produce the \code{$deathsLT} column. You can skip calling this function by specifying \code{eOpen} in the top call to \code{seg()} or \code{ggbseg()}.
#' 
#' @param codiaugmented the standard codi object being passed through the chain, but having been preprocessed in the course of \code{segMakeColumns()} or \code{ggbsegMakeColumns()}
#' 
#' @return numeric an estimate of remaining life expectancy in the open age group
#' 
#' @importFrom stats splinefun
#' @export

eOpenCD <- function(codiaugmented){
	# this needs to have the column deathsLT, as passed in 
	# by either segMakeColumns() or ggbsegMakeColumns(). These
	# are slightly different, but this piece of code is identical and therefore 
	# modularized.
	# this throws an error if sex isn't coded as expected
	sex        <- detectSex(Dat = codiaugmented, 
			sexColumn = "sex")
	ages       <- codiaugmented$age
	
	ratio      <- with(codiaugmented,
			sum(deathLT[ages %in% c(10:39)]) / 
					sum(deathLT[ages %in% c(40:59)]))
	
	if (sex == "f"){
		standardratios <- c(1.376,	1.3,	1.233,	1.171,
				1.115,	1.062,	1.012,	0.964,
				0.918,	0.872,	0.827,	0.787,
				0.729,	0.673,	0.617,	0.56,
				0.501,	0.438,	0.365,	0.298,
				0.235,	0.175,	0.117)
		ex <- cdmltw("F")$ex
	}
	if (sex == "m"){
		# need to change this:
		standardratios <- c(1.161,	1.094,	1.034,	0.98,	
				0.93,	0.885,	0.842,	0.802,	
				0.763,	0.725,	0.689,	0.648,
				0.609,	0.57,	0.53,	0.49,	
				0.447,	0.401,	0.352,	0.305,	
				0.255,	0.202,	0.147)
		
		ex <- cdmltw("M")$ex
	}
	
	# this spits back a C-D level that is a decimal estimate
	AllLevels <- 3:25
	CDlevel   <- splinefun(AllLevels~standardratios)(ratio)
	
	# which is the open age?
	OA        <- max(ages)
	availages <- as.integer(colnames(ex))
	
	stopifnot(OA %in% as.integer(colnames(ex)))
	
	eOpen     <- splinefun(ex[,as.character(OA)]~1:25)(CDlevel)
	eOpen
}


#' @title given a set of ages, what is the implied death registration coverage?
#' 
#' @description For a single year/sex/region of data (formatted as required by \code{seg()}, \code{ggbseg()}), what is the registration coverage implied by a given age range? Called by \code{segCoverageFromYear()} and  \code{ggbsegCoverageFromYear()}. Here, the function simply takes the arithmetic mean of a given age range of \code{$Cx}, as returned by \code{segMakeColumns()} or \code{ggbsegMakeColumns()}. Not intended for top-level use.
#' 
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{ggb()}
#' @param agesFit an integer vector of ages, either returned from \code{ggbgetAgesFit} or user-supplied.
#'
#' @return numeric. the estimated level of coverage.
#' @export

segCoverageFromAges <- function(codi, agesFit){
	stopifnot("Cx" %in% colnames(codi))
	inds    <- codi$age %in% agesFit
	sum(codi$Cx[inds]) / length(agesFit)
}


#' @title make the Bennett-Horiuchi quasi life table columns required by the estimation method
#' 
#' @description Called by \code{segCoverageFromYear()}. This simply modulates some code that would otherwise be repeated. Users probably don't need to call this function directly. 
#' 
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{seg()}
#' @param minA the minimum of the age range searched. Default 15
#' @param maxA the maximum of the age range searched. Default 75
#' @param eOpen optional. A value for remaining life expectancy in the open age group.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#'
#' @return codi, with many columns added, most importantly \code{$Cx}.
#' 
#' @export

segMakeColumns <- function(codi, minA = 15, maxA = 75, eOpen = NULL, deaths.summed = FALSE){
	codi                   <- avgDeaths(codi = codi, deaths.summed = deaths.summed)
	
	# attempt to detect AgeInterval, should be obvious. And really, we only consider 5-yr intervals.
	# if this were done w single ages minAges would need to increase to at least 35 I guess.
	AgeInt                 <- detectAgeInterval(Dat = codi, MinAge =  minA, MaxAge = maxA, ageColumn = "age")
	
	# reduce open age to desired range
	codi                   <- reduceOpen(codi, maxA = 95, group = TRUE)
	# group inf if necessary
	codi                   <- group01(codi)
	# codi can use date columns, or year, month, day columns...
	dif                    <- yint2(X = codi)
	N                      <- nrow(codi)
	ages                   <- codi$age
	# ages better be unique! I expect this will work unless $cod is mis-specified. 
	# This is a good catch for that
	stopifnot(length(ages) == length(unique(ages)))
	
#	codi <- within(codi,{
#				# birthdays, as in GGB
#				birthdays <- c(0, sqrt(pop1[ -N  ] * pop2[ -1 ])) / AgeInt
#				# age-specific growth
#	        	growth    <- log(pop2 / pop1) / dif
#				growth[is.infinite(growth) | is.nan(growth)] <- 0
#				# cumulative growth
#				cumgrowth <- AgeInt * c(0,cumsum(growth[ -N ])) + AgeInt / 2 * growth
#				deathLT   <- deathsAvg * exp(cumgrowth)
#			})
	codi$birthdays <- c(0, sqrt(codi$pop1[ -N  ] * codi$pop2[ -1 ])) / AgeInt
				# age-specific growth
	codi$growth    <- log(codi$pop2 / codi$pop1) / dif
	codi$growth[is.infinite(codi$growth) | is.nan(codi$growth)] <- 0
				# cumulative growth
	codi$cumgrowth <- AgeInt * c(0,cumsum(codi$growth[ -N ])) + AgeInt / 2 * codi$growth
	codi$deathLT   <- codi$deathsAvg * exp(codi$cumgrowth)
			
	if (is.null(eOpen)){
		eOpen <- eOpenCD(codiaugmented = codi)
	} else {
		eOpen <- eOpen
	}
	
	eON   <- eOpen * codi$growth[N]

#	codi <- within(codi, {
#				pop_a <- 0                         # TR: test this to see if more robust
#				pop_a[N] <- deathsAvg[N] * exp(eON) - ((eON ^ 2) ^ (1 / 6))
#				for(j in N:2){
#					pop_a[j - 1] <- pop_a[j] * exp(AgeInt * growth[j - 1]) + 
#							deathsAvg[j - 1] * exp(AgeInt / 2 * growth[j - 1])
#				}
#				rm(j)
#				Cx <- pop_a / birthdays
#			})
	codi$pop_a <- 0                         # TR: test this to see if more robust
	codi$pop_a[N] <- codi$deathsAvg[N] * exp(eON) - ((eON ^ 2) ^ (1 / 6))
		for(j in N:2){
			codi$pop_a[j - 1] <- codi$pop_a[j] * exp(AgeInt * codi$growth[j - 1]) + 
					codi$deathsAvg[j - 1] * exp(AgeInt / 2 * codi$growth[j - 1])
		}
	rm(j)
	codi$Cx <- codi$pop_a / codi$birthdays
			
	################
	codi
}


#' @title estimate death registration coverage for a single year/sex/region using the Bennett-Horiuchi method
#' 
#' @description Given two censuses and an average annual number of deaths in each age class between censuses, we can use stable population assumptions to estimate the degree of underregistration of deaths. The method estimates age-specific degrees of coverage. The age pattern of these is assumed to be noisy, so we take the arithmetic mean over some range of ages. One may either specify a particular age-range, or let the age range be determined automatically. If the age-range is found automatically, this is done using the method developed for the generalized growth-balance method. Part of this method relies on a prior value for remaining life expectancy in the open age group. By default, this is estimated using a standard reference to the Coale-Demeny West model life table, although the user may also supply a value. Called by \code{seg()}. Users probably do not need to use this function directly.
#' 
#' @details Census dates can be given in a variety of ways: 1) using Date classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing integers. If only \code{year1} and \code{year2} are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month. 
#' 
#' @param codi \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$sex}, \code{$age}, and \code{$cod} (to indicate regions, periods, sexes).
#' @param exact.ages optional. use an exact set of ages to estimate coverage.
#' @param minA the minimum of the age range searched. Default 15
#' @param maxA the maximum of the age range searched. Default 75
#' @param minAges the minimum number of adjacent ages needed as points for fitting. Default 8
#' @param eOpen optional. A user-specified value for remaining life-expectancy in the open age group.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' @return a \code{data.frame} with columns for the coverage coefficient, and the min and max of the age range on which it is based. 
#' 
#' @importFrom stats quantile
#' 
#' @export


segCoverageFromYear <-  function(codi, 
								 minA = 15, 
								 maxA = 75, 
								 minAges = 8, 
								 exact.ages = NULL, 
								 eOpen = NULL, 
								 deaths.summed = FALSE){       
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
	
	# outsource automatic age selection to GGB method.
	if (is.null(exact.ages)){
		agesFit <- ggbgetAgesFit(codi = codi, 
								 minA = minA, 
								 maxA = maxA, 
								 minAges = minAges, 
								 deaths.summed = deaths.summed)
	} else {
		agesFit <- exact.ages
	}
	
	codi     <- segMakeColumns(	codi = codi, 
								minA = minA, 
								maxA = maxA,
								eOpen = eOpen,
								deaths.summed = deaths.summed)
	
	coverage <- segCoverageFromAges(codi = codi, agesFit = agesFit)

	# TR: 4-3-2017 get IQR of coverage as well
	# this could go out into a utility function too...
	inds     <- codi$age %in% agesFit
	IQR      <- quantile(codi$Cx[inds], c(.25,.75))

	meta <- 	data.frame(cod = unique(codi$cod), 
			   coverage = coverage, 
			   lower = min(agesFit), 
			   upper = max(agesFit),
			   # TR: added 3-4-2017
			   l25 = IQR[1],
	           u25 = IQR[2]
			   )
	out <- list(coverages = meta, codi = codi)
	out
}


#' @title estimate death registration coverage using the synthetic extinct generation method 
#' 
#' @description Given two censuses and an average annual number of deaths in each age class between censuses, we can use stable population assumptions to estimate the degree of underregistration of deaths. The method estimates age-specific degrees of coverage. The age pattern of these is assumed to be noisy, so we take the arithmetic mean over some range of ages. One may either specify a particular age-range, or let the age range be determined automatically. If the age-range is found automatically, this is done using the method developed for the generalized growth-balance method. Part of this method relies on a prior value for remaining life expectancy in the open age group. By default, this is estimated using a standard reference to the Coale-Demeny West model life table, although the user may also supply a value.
#' 
#' @details Census dates can be given in a variety of ways: 1) using Date classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing integers. If only \code{year1} and \code{year2} columns are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month. If you want coverage estimates for a variety of intercensal periods/regions/by sex, then stack them, and use a variable called \code{$cod} with a unique values for each data chunk. Different values of \code{$cod} could indicate sexes, regions, intercensal periods, etc. The \code{$deaths} column should refer to the average annual deaths in each age class in the intercensal period. Sometimes one uses the arithmetic average of recorded deaths in each age, or simply the average of the deaths around the time of census 1 and census 2. To identify an age-range in the traditional visual way, see \code{plot.ggb()}, when working with a single year/sex/region of data. The automatic age-range determination feature of this function tries to implement an intuitive way of picking ages that follows the advice typically given for doing so visually. We minimize the square of the average squared residual between the fitted line and right term. Finally, only specify \code{eOpen} when working with a single region/sex/period of data, otherwise the same value will be passed in irrespective of mortality and sex.
#' 
#' @param X \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$age}, \code{$sex}, and \code{$cod} (if there are more than 1 region/sex/intercensal period).
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param exact.ages optional. A user-specified vector of exact ages to use for coverage estimation
#' @param eOpen optional. A user-specified value for remaining life-expectancy in the open age group.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#'
#' @return a \code{data.frame} with columns for the coverage coefficient \code{$coverage}, and the minimum \code{$lower} and maximum \code{$upper} of the age range on which it is based. Rows indicate data partitions, as indicated by the optional \code{$cod} variable. \code{$l25} (\code{$u25}) give the mean of the lower (upper) quartile of the distribution of age-specific coverage estimates.
#' 
#' @export
#' 
#' @references Bennett Neil G, Shiro Horiuchi. Estimating the completeness of death registration in a closed population. Population Index. 1981; 1:207-221.
#' 
#' Preston, S. H., Coale, A. J., Trussel, J. & Maxine, W. Estimating the completeness of reporting of adult deaths in populations that are approximately stable.  Population Studies, 1980; v.4: 179-202
#' 
#' @examples 
#' # The Mozambique data
#' res <- seg(Moz)
#' res
#' # The Brasil data
#' BM <- seg(BrasilMales)
#' BF <- seg(BrasilFemales)
#' head(BM)
#' head(BF)

seg <- function(X, 
				minA = 15, 
				maxA = 75, 
				minAges = 8, 
				exact.ages = NULL, 
				eOpen = NULL, 
				deaths.summed = FALSE){
	
    # TR: modularized Apr 2, 2017
	tab1        <- headerPrep(X)
	coverages <- as.data.frame(
					do.call(
						rbind,
						lapply(
							tab1,
							function(X){
								segCoverageFromYear(X,minA = minA, 
										maxA = maxA,
										minAges = minAges,  
										exact.ages = exact.ages,
										eOpen = eOpen,
										deaths.summed = deaths.summed)$coverages	
							}
            )))
	#return(data.frame(Coverage = coverages,correctionFactor = 1/coverages))
	
	coverages
}

#' plot the age-pattern of coverage estimates
#' 
#' @description the SEG method works by averaging the coverage estimates over a range of ages. 
#' Users may wish to see the age pattern for diagnostic purposes.
#' 
#' @details All arguments are essentially the same as those given to \code{seg()}
#' 
#' @param X \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$age}, \code{$sex}, and \code{$cod} (if there are more than 1 region/sex/intercensal period).
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param exact.ages optional. A user-specified vector of exact ages to use for coverage estimation
#' @param eOpen optional. A user-specified value for remaining life-expectancy in the open age group.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' @param log logical. should we log the y axis?
#' 
#' @return Function called for its graphical side effects
#' 
#' @importFrom grDevices gray
#' @importFrom graphics abline legend locator mtext par plot points rect segments text
#' @export 
#' 
#' @examples 
#' \dontrun{
#' segplot(Moz)
#' }


segplot <- function(
		X, 
		minA = 15, 
		maxA = 75, 
		minAges = 8, 
		exact.ages = NULL, 
		eOpen = NULL, 
		deaths.summed = FALSE,
		log = FALSE){
	tab1        <- headerPrep(X)
	
	if (length(tab1) > 1){
		warning("codi was not unique, taking first element")
	}
	tab1 <- tab1[[1]]
	goods <- segCoverageFromYear(tab1,
			minA = minA, 
			maxA = maxA,
			minAges = minAges,  
			exact.ages = exact.ages,
			eOpen = eOpen,
			deaths.summed = deaths.summed)
	codi     <- goods$codi
	coverage <- goods$coverages
	keep 	 <- codi$age >= minA &  codi$age <= maxA
	ages 	 <- codi$age[keep]
	yvals 	 <- codi$Cx[keep]
	keep2 	 <- codi$age >= coverage$lower & codi$age <= coverage$upper
	ages2 	 <- codi$age[keep2]
	yvals2 	 <- codi$Cx[keep2]
	
	# TR: 29 May ylim changes
	if (!log){
		ymin <- min(c(0, min(yvals)))
		ymax <- max(1, max(yvals))
		ylim <- c(ymin, ymax)
	} else {
		ylim <- range(yvals)
	}

	
	plot(ages, yvals, pch = 16, ylim = ylim, col = gray(.5), ylab="Cx", log = ifelse(log,"y",""))
	rect(ages2[1]-2.5,coverage$l25,ages2[length(ages2)]+2.5,coverage$u25, 
			border = NA, col = "#00000040")
	points(ages2, yvals2, pch = 16, col = "black")
	segments(ages2[1]-2.5,coverage$coverage,ages2[length(ages2)]+2.5,coverage$coverage,lwd=2)
	
	# TODO let user interactively select points similar to ggbplot, for purposes of
	# refitting, etc. This would be a second age-range diagnostic.
}


###################################################################################
# End seg functions
###################################################################################
# Start ggbseg functions
###################################################################################


#' @title make the Bennett-Horiuchi quasi life table columns required by the second estimation method
#' 
#' @description Called by \code{ggbsegCoverageFromYear()}. This simply modulates some code that would otherwise be repeated. Users probably don't need to call this function directly. 
#' @details \code{agesFit} is a vector passed in from \code{ggbsegMakeColumns()}, and it was either estimated using the GGB automatic method there, or simply came from the argument \code{exact.ages} specified in \code{ggbseg()}. By default we just automatically estimate these. \code{eOpen} can be either user-specified, or it will be estimated automatically using \code{eOpenCD()}.
#' 
#' @param codi a chunk of data (single sex, year, region, etc) with all columns required by \code{ggbseg()}
#' @param minA the minimum of the age range searched. Default 15
#' @param maxA the maximum of the age range searched. Default 75
#' @param eOpen optional. A value for remaining life expectancy in the open age group.
#' @param agesFit vector of ages as passed in by \code{ggbsegCoverageFromYear)} 
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#'
#' @return codi, with many columns added, most importantly \code{$Cx}.
#' 
#' @export

ggbsegMakeColumns <- function(
		codi, 
		minA = 15, 
		maxA = 75,  
		agesFit, 
		eOpen = NULL,
		deaths.summed = FALSE){
	codi         <- avgDeaths(codi = codi, deaths.summed = deaths.summed)
	
	AgeInt       <- detectAgeInterval(
							Dat = codi, 
							MinAge =  minA, 
							MaxAge = maxA, 
							ageColumn = "age")
	
	# reduce open age to desired range
	codi         <- reduceOpen(codi, maxA = 95, group = TRUE)
	# group inf if necessary
	codi         <- group01(codi)
	ages         <- codi$age
	N            <- nrow(codi)
	# ages better be unique! I expect this will work unless $cod is mis-specified. 
	# This is a good catch for that
	stopifnot(length(ages) == length(unique(ages)))
	
	dif          <- yint2(X = codi)
	
	# just get left term / right term
	# slopeint() is in the ggb() family
	ab           <- slopeint(codi = codi, agesfit = agesFit)
	
	# the only difference between this method and seg is that
	# in the next couple lines we use pop1adj instead of pop1...
	relcomp      <- exp(ab$a * dif)
   	
#	codi <- within(codi,{
#				pop1adj   <- pop1 / relcomp
#				# birthdays, as in GGB
#				birthdays <- c(0, sqrt(pop1adj[ -N  ] * pop2[ -1 ])) / AgeInt
#				# age-specific growth
#				growth    <- log(pop2 / pop1adj) / dif
#				growth[is.infinite(growth) | is.nan(growth)] <- 0
#				# cumulative growth
#				cumgrowth <- AgeInt * c(0,cumsum(growth[ -N ])) + AgeInt / 2 * growth
#				deathLT   <- deathsAvg * exp(cumgrowth)
#			})
	
	codi$pop1adj   <- codi$pop1 / relcomp
	# birthdays, as in GGB
	codi$birthdays <- c(0, sqrt(codi$pop1adj[ -N  ] * codi$pop2[ -1 ])) / AgeInt
	# age-specific growth
	codi$growth    <- log(codi$pop2 / codi$pop1adj) / dif
	codi$growth[is.infinite(codi$growth) | is.nan(codi$growth)] <- 0
	# cumulative growth
	codi$cumgrowth <- AgeInt * c(0,cumsum(codi$growth[ -N ])) + AgeInt / 2 * codi$growth
	codi$deathLT   <- codi$deathsAvg * exp(codi$cumgrowth)
#	codi      <- cbind(codi, 
#				data.frame(pop1adj = pop1adj,
#					   birthdays = birthdays,
#					   growth = growth,
#					   cumgrowth = cumgrowth,
#					   deathLT = deathLT))		
#	
	if (is.null(eOpen)){
		eOpen <- eOpenCD(codiaugmented = codi)
	} else {
		eOpen <- eOpen
	}
	
	eON   <- eOpen * codi$growth[N]
	
#	codi <- within(codi, {
#				pop_a    <- 0
#				pop_a[N] <- deathsAvg[N] * exp(eON) - ((eON ^ 2)^(1/6))
#				for(j in N:2){
#					pop_a[j - 1] <- pop_a[j] * exp(AgeInt * growth[j - 1]) + 
#							deathsAvg[j - 1] * exp(AgeInt / 2 * growth[j - 1])
#				}
#				rm(j)
#				Cx <- pop_a / birthdays
#			})
	codi$pop_a    	<- 0
	codi$pop_a[N] 	<- codi$deathsAvg[N] * exp(eON) - ((eON ^ 2)^(1/6))
		for(j in N:2){
			codi$pop_a[j - 1] <- codi$pop_a[j] * exp(AgeInt * codi$growth[j - 1]) + 
				codi$deathsAvg[j - 1] * exp(AgeInt / 2 * codi$growth[j - 1])
		}
	rm(j)
	codi$Cx 		<- codi$pop_a / codi$birthdays
			
	
	codi
}


#' @title estimate death registration coverage for a single year/sex/region using the modified Bennett-Horiuchi method
#' 
#' @description Given two censuses and an average annual number of deaths in each age class between censuses, we can use stable population assumptions to estimate the degree of underregistration of deaths. The method estimates age-specific degrees of coverage. The age pattern of these is assumed to be noisy, so we take the arithmetic mean over some range of ages. One may either specify a particular age-range, or let the age range be determined automatically. If the age-range is found automatically, this is done using the method developed for the generalized growth-balance method. Part of this method relies on a prior value for remaining life expectancy in the open age group. By default, this is estimated using a standard reference to the Coale-Demeny West model life table, although the user may also supply a value. The difference between this method and \code{seg()} is that here we adjust census 1 part way through processing, based on some calculations similar to GGB. Called by \code{ggbseg()}. Users probably do not need to use this function directly.
#' 
#' @details Census dates can be given in a variety of ways: 1) using Date classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing integers. If only \code{year1} and \code{year2} are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month. 
#' 
#' @param codi \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$sex}, \code{$age}, and \code{$cod} (to indicate regions, periods, sexes).
#' @param exact.ages optional. use an exact set of ages to estimate coverage.
#' @param minA the minimum of the age range searched. Default 15
#' @param maxA the maximum of the age range searched. Default 75
#' @param minAges the minimum number of adjacent ages needed as points for fitting. Default 8
#' @param eOpen optional. A user-specified value for remaining life-expectancy in the open age group.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#'
#' 
#' @return a \code{data.frame} with columns for the coverage coefficient, and the min and max of the age range on which it is based. 
#' 
#' @export


ggbsegCoverageFromYear <- function(codi, 
								minA = 15, 
								maxA = 75, 
								minAges = 8, 
								exact.ages = NULL, 
								eOpen = NULL, 
								deaths.summed = FALSE){
	codiggb      <- ggbMakeColumns(codi = codi, 
								   minA = minA, 
								   maxA = maxA, 
								   deaths.summed = deaths.summed)
	# Get age range using the GGB auto fitting
	if (is.null(exact.ages)){
		agesFit <- ggbgetAgesFit(codi = codiggb, 
								minA = minA, 
								maxA = maxA, 
								minAges = minAges,
								deaths.summed = deaths.summed)
	} else {
		agesFit <- exact.ages
	}
	
	codi         <- ggbsegMakeColumns(
								codi = codiggb, 
								minA = minA, 
								maxA = maxA, 	
								agesFit = agesFit,
								eOpen = eOpen,
								deaths.summed = deaths.summed)
	coverage     <- segCoverageFromAges(codi = codi, agesFit = agesFit )
	data.frame(cod = unique(codi$cod), coverage = coverage, lower = min(agesFit), upper = max(agesFit))
}


#' @title estimate death registration coverage using the hybrid generalized growth balance and synthetic extinct generation
#' 
#' @description Given two censuses and an average annual number of deaths in each age class between censuses, we can use stable population assumptions to estimate the degree of underregistration of deaths. The method estimates age-specific degrees of coverage. The age pattern of these is assumed to be noisy, so we take the arithmetic mean over some range of ages. One may either specify a particular age-range, or let the age range be determined automatically. If the age-range is found automatically, this is done using the method developed for the generalized growth-balance method. Part of this method relies on a prior value for remaining life expectancy in the open age group. By default, this is estimated using a standard reference to the Coale-Demeny West model life table, although the user may also supply a value. The difference between this method and \code{seg()} is that here we adjust census 1 part way through processing, based on some calculations similar to GGB.
#' 
#' @details Census dates can be given in a variety of ways: 1) using Date classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing integers. If only \code{year1} and \code{year2} columns are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month. If you want coverage estimates for a variety of intercensal periods/regions/by sex, then stack them, and use a variable called \code{$cod} with a unique values for each data chunk. Different values of \code{$cod} could indicate sexes, regions, intercensal periods, etc. The \code{$deaths} column should refer to the average annual deaths in each age class in the intercensal period. Sometimes one uses the arithmetic average of recorded deaths in each age, or simply the average of the deaths around the time of census 1 and census 2. To identify an age-range in the traditional visual way, see \code{plot.ggb()}, when working with a single year/sex/region of data. The automatic age-range determination feature of this function tries to implement an intuitive way of picking ages that follows the advice typically given for doing so visually. We minimize the square of the average squared residual between the fitted line and right term. Finally, only specify \code{eOpen} when working with a single region/sex/period of data, otherwise the same value will be passed in irrespective of mortality and sex.
#' 
#' @param X \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$age}, \code{$sex}, and \code{$cod} (if there are more than 1 region/sex/intercensal period).
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param exact.ages optional. A user-specified vector of exact ages to use for coverage estimation
#' @param eOpen optional. A user-specified value for remaining life-expectancy in the open age group.
#' @param deaths.summed logical. Is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#'
#' 
#' @return a \code{data.frame} with columns for the coverage coefficient \code{$coverage}, and the minimum \code{$lower} and maximum \code{$upper} of the age range on which it is based. Rows indicate data partitions, as indicated by the optional \code{$cod} variable.
#' 
#' @export
#' @references Hill K. Methods for measuring adult mortality in developing countries: a comparative review. The global burden of disease 2000 in aging populations. Research paper; No. 01.13; 2001.
#' 
#' Hill K, You D, Choi Y.  Death distribution methods for estimating adult mortality: sensitivity analysis with simulated data errors. Demographic Research. 2009; 21:235-254.
#' 
#' Preston, S. H., Coale, A. J., Trussel, J. & Maxine, W. Estimating the completeness of reporting of adult deaths in populations that are approximately stable.  Population Studies, 1980; v.4: 179-202
#' 
#' 
#' @examples 
#' # The Mozambique data
#' res <- ggbseg(Moz)
#' res
#' # The Brasil data
#' BM <- ggbseg(BrasilMales)
#' BF <- ggbseg(BrasilFemales)
#' head(BM)
#' head(BF)
ggbseg <- function(X, 
				minA = 15, 
				maxA = 75, 
				minAges = 8, 
				exact.ages = NULL, 
				eOpen = NULL, 
				deaths.summed = FALSE){
	# TR: modularized Apr 2, 2017
    tab1        <- headerPrep(X)
	
	coverages <- as.data.frame(
			do.call(
					rbind,
					lapply(
						tab1, 
						ggbsegCoverageFromYear, 
						minA = minA,  
						maxA = maxA,
						minAges = minAges,  	
						exact.ages = exact.ages,
						eOpen = eOpen,
						deaths.summed = deaths.summed
                  )))
	coverages
}
