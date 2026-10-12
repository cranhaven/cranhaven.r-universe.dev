
# Author: tim
###############################################################################

#' @title run all three deaths registration coverage estimation methods
#' 
#' @description Estimate the generalized growth balance method, and the two Bennett-Horiuchi methods of estimating death registration coverage. This requires two censuses and an estimate of the deaths in each 5-year age group between censuses. This might be the arithmetic average of deaths in each age class, or simply the average of deaths around the time of the two censuses. All methods use some stable population assumptions. 

#' @details All methods require some specification about which age range to base results on. If not given, an optimal age range will be estimated automatically, and this information is returned to the user. To identify an age-range in the visually, see \code{plot.ggb()}, when working with a single year/sex/region of data. The automatic age-range determination feature of this function tries to implement an intuitive way of picking ages that follows the advice typically given for doing so visually. We minimize the square of the average squared residual between the fitted line and right term.If you want coverage estimates for a variety of partitions (intercensal periods/regions/by sex), then stack them, and use a variable called \code{$cod} with unique values for each data partition. If data is partitioned using the variable \code{$cod}, then the age range automatically determined might not be the same for each partition. If user-specified, (using a vector of \code{exact.ages}) the age ranges will be the same for all partitions. If you want to specify particular age ranges for each data partition, then you'll need to loop it somehow. 
#' 
#' All three methods require time points of the two censuses. Census dates can be given in a variety of ways: 1) (preferred) using \code{Date} classes, and column names \code{$date1} and \code{$date2} (or an unambiguous character string of the date, like, \code{"1981-05-13"}) or 2) by giving column names \code{"day1","month1","year1","day2","month2","year2"} containing respective integers. If only \code{year1} and \code{year2} are given, then we assume January 1 dates. If year and month are given, then we assume dates on the first of the month.  Different values of \code{$cod} could indicate sexes, regions, intercensal periods, etc. The \code{$deaths} column should refer to the average annual deaths for each age class in the intercensal period. Sometimes one uses the arithmetic average of recorded deaths in each age, or simply the average of the deaths around the time of census 1 and census 2. 
#' 
#' The synthetic extinct generation methods require an estimate of remaining life expectancy in the open age group of the data provided. This is produced using a standard reference to the Coale-Demeny West model life tables. That is a place where things can be improved.
#' @param X \code{data.frame} with columns, \code{$pop1}, \code{$pop2}, \code{$deaths}, \code{$date1}, \code{$date2}, \code{$age}, \code{$sex}, and \code{$cod} (if there are more than 1 region/sex/intercensal period).
#' @param minA the lowest age to be included in search
#' @param maxA the highest age to be included in search (the lower bound thereof)
#' @param minAges the minimum number of adjacent ages to be used in estimating
#' @param exact.ages optional. A user-specified vector of exact ages to use for coverage estimation
#' @param eOpen optional. A user-specified value for remaining life-expectancy in the open age group.
#' @param deaths.summed logical. is the deaths column given as the total per age in the intercensal period (\code{TRUE}). By default we assume \code{FALSE}, i.e. that the average annual was given.
#' 
#' @return data.frame with columns \code{$cod}, \code{$ggb}, \code{$bh1}, \code{$bh2}, \code{$lower}, and \code{$upper}. 
#' @references 
#' Bennett Neil G, Shiro Horiuchi. Estimating the completeness of death registration in a closed population. Population Index. 1981; 1:207-221.
#' 
#' Hill K. Estimating census and death registration completeness. Asian and Pacific Population Forum. 1987; 1:1-13.
#' 
#' Hill K, You D, Choi Y. Death distribution methods for estimating adult mortality: sensitivity analysis with simulated data errors. Demographic Research. 2009; 21:235-254.
#' 
#' Brass, William, 1975.  Methods for Estimating Fertility and Mortality from Limited and Defective Data, Carolina Population Center, Laboratory for Population Studies, University of North Carolina, Chapel Hill.
#' 
#' Preston, S. H., Coale, A. J., Trussel, J. & Maxine, W. Estimating the completeness of reporting of adult deaths in populations that are approximately stable.  Population Studies, 1980; v.4: 179-202
#' 
#' 
#' @export
#' 
#' @examples 
#' # The Mozambique data
#' res <- ddm(Moz)
#' head(res)
#' # The Brasil data
#' BM <- ddm(BrasilMales)
#' BF <- ddm(BrasilFemales)
#' head(BM)
#' head(BF)
ddm <- function(
		X, 
		minA = 15, 
		maxA = 75, 
		minAges = 8, 
		exact.ages = NULL, 
		eOpen = NULL, 
		deaths.summed = FALSE){
	ggbres <- ggb(X = X, 
					minA = minA, 
					maxA = maxA, 
					minAges = minAges, 
					exact.ages = exact.ages,
					deaths.summed = deaths.summed)
	segres <- seg(X = X, 
					minA = minA, 
					maxA = maxA, 
					minAges = minAges, 
					exact.ages = exact.ages, 
					eOpen = eOpen,
					deaths.summed = deaths.summed)
	ggbsegres <- ggbseg(
			        X = X, 
					minA = minA, 
					maxA = maxA, 
					minAges = minAges, 
					exact.ages = exact.ages, 
					eOpen = eOpen,
					deaths.summed = deaths.summed)
	# return all results
	results <- data.frame(	
			        cod = ggbres$cod,
					ggb = ggbres$coverage,
					seg = segres$coverage,
					ggbseg = ggbsegres$coverage,
					lower = ggbres$lower,
					upper = ggbres$upper,
					delta = ggbres$delta)
#	if (delta){
#		results$delta <- ggbres$delta
#		#results <- cbind(results, delta = ggbres$delta)
#	}

	results
}

#' @title get a quick overview of the different estimates produced
#' @description produce a dot plot, where each x position is a unique value of \code{$cod}, and points indicate the GGB, SEG, GGB-SEG, and harmonic mean of these. Feed this function the output of \code{ddm()}.
#' 
#' @param X output of \code{ddm()}.
#' @param ... other arguments passed to \code{plot()}
#' 
#' @return called for its graphical device side-effects.
#' @importFrom grDevices gray 
#' @importFrom graphics abline legend locator mtext par plot points rect segments text
#' @export
#' 
#' @examples 
#' # just a rough sketch of the results!
#' res <- ddm(Moz)
#' ddmplot(res)

ddmplot <- function(X,...){
#	if (class(X) == "data.frame"){
#		X <- ddm(X, minA = minA, 
#				maxA = maxA, 
#				minAges = minAges, 
#				exact.ages = exact.ages, 
#				eOpen = eOpen)
#	} 
	
	X$x <- 1:nrow(X)
	
	# these can move to utils.R if we decide it's worth keeping as a summary method
	h.mean <- function(x){
		1/mean(1/x)
	}
	#g.mean <- function(x){
	#	n <- length(x)
	#	prod(x)^(1/n)
	#}
	Range <- range(as.matrix(X[,c("ggb","seg","ggbseg")]))
	plot(X$x,X$ggb, pch = 19,col="#FFA155", ylim=Range, cex=.6, 
			xlab = "data row", ylab = "coverage estimate",
			panel.first = list(
					segments(X$x,
							apply(X[,c("ggb","seg","ggbseg")],1,min),
							X$x,
							apply(X[,c("ggb","seg","ggbseg")],1,max), 
							lwd=.5,
							col=gray(.5))),
			...)
	points(X$x,X$seg,pch=19,col = "royalblue", cex=.6)
	points(X$x,X$ggbseg,pch=19,col = "forestgreen", cex=.6)
	#points(X$x, apply(X[,1:3],1,g.mean), col = "gray", cex = .6, pch = 19)
	points(X$x, apply(X[,c("ggb","seg","ggbseg")],1,h.mean), col = "magenta", cex = .6, pch = 19)
	legend("topright", col = c("#FFA155","royalblue","forestgreen","magenta"), 
			pch = 19, cex=.6, legend = c("GGB","SEG","GGBSEG","Hmean"),bty="n")
	
	
}

