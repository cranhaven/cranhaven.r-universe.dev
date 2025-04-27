
#' @import  stats
#' @importFrom  methods  show
#' @importFrom  utils  head






# convenient helper function
verb <- function(...) cat(sprintf(...), sep='', file=stderr())











# ' Get Minimal Median from sorted values
# '
# ' Given sorted values, will efficiently return the minimal median.
# '
# ' @param  pop		matrix of features.  rows = population members; columns = features.
# ' @param  sortpop	sorted matrix of features.  can be in column-major vector format. 
# '				Each column is a column of pop but sorted.
# '				Each column is sorted independently, hence rownames are meaningless.
# '				Columns = features.  In same order as in pop.
# ' @param  ord		When sorting each column of pop (separately), you get an "order matrix"
# '				where each column of the order matrix is the ordering of the row indeces
# '				for sorting that column.  Then you convert this order matrix
# '				to column-major linear indexing.
# ' @param  sample	character vector of population member names.
# '
# ' @return vector of minimal median values.
minMedian_sample_sorted  <-  function( pop , sortpop , ord , sample ) {

	# get sub sample indeces
	logi.samp  <-  rownames(pop) %in% sample

	# repeat to get linear matrix column-major format
	logi.samp  <-  rep( logi.samp , ncol(pop) )

	# apply order, where ord is in linear indexing format
	logi.samp  <-  logi.samp[ord]

	# get subset of sorted
	sub.vec  <-  sortpop[logi.samp]

	# median index
	medx  <-  ceiling(length(sample)/2)
	medx  <-  seq( from = ceiling(length(sample)/2) , by = length(sample) , to = ceiling(length(sample)/2) + length(sample)*(ncol(pop)-1) )

	medval  <-  sub.vec[medx]
	names(medval)  <-  names(sortpop)

	return(medval)	
	
} # minMedian_sample_sorted






#' Known Population Median Test
#'
#' Performs the known population median test.
#'
#' @param  pop		[data frame, matrix, vector] numeric values for the whole population.
#'			If a data frame or matrix is given, it should have format:
#'			\itemize{
#'				\item{rownames}  {= population member names (e.g. gene names)}
#'				\item{colnames}  {= features to test (e.g. relative codon usage, UTR length, MFE, etc.)}
#'			}
#'			For a data frame or matrix, the test will be performed on each column, separately.
#'			If a named vector is given, it should have format:
#'			\itemize{
#'				\item{names} {= population member names.}
#'				\item{values} {= numeric values of the feature.}
#'			}
#' @param  obs		[character vector or named list of character vectors]
#'				 a character vector of population member names, or a named list of character vectors of population member names.
#'			\enumerate{
#'				\item  If obs is a list, then each list element name should correspond to a feature name of pop.
#'				\item  If obs is a vector, then it is considered to be the same sample for each population feature.
#'				\item  if size = NULL, then obs is considered to be a sample of population member names
#'				\item  if size is non-NULL, then obs is considered to be the observed
#'					median values per column of pop. (length obs must be equal to number of features in pop)
#'			}
#' @param  med		[number or vector] pre-computed minimal medians of pop.
#' @param  size		[integer]  size of the set which generated the observed median.  If obs is a sample, i.e. contains
#'			population member names, then size must be NULL.
#' @param  tail		["two-sided", "lower", "upper"]  if NULL, then the minimum of lower and upper will be reported.
#' @param  verbose	display extra messages for tracking execution.
#'
#' @return		data frame with columns:
#'			\itemize{ 
#'				\item{"name"}  			{a column from pop}
#'				\item{"median.sample"}		{min median of the sample}
#'				\item{"median.all"}		{min median of the whole population}
#'				\item{"median.background"}	{min median of the non-sampled members.}
#'				\item{"logp"}			{log of p.value if sample median is different from all}
#'				\item{"p.value"}		{p.value if sample median is different from all}
#'				\item{"FDR"}			{only if > 30 features, i.e columns of pop}
#'			}
#'			each row is a different population feature, i.e. column of pop.
#'
#' @examples
#' data(genefeat)
#' data(GO0007186)
#' res  <-  kpmt( pop = genefeat , obs = GO0007186 )
#' @export
kpmt  <-  function(	pop ,
			obs = NULL , med = NULL ,
			size = NULL ,
			tail = "two-sided" ,
			verbose = FALSE ) {

	if (is.vector(pop)) {
		np  <-  names(pop)
		pop  <- data.frame( feature1 = pop )
		rownames(pop)  <-  np
	} # pop to df

	#### prepare
	if (verbose) { verb("\nprepare input...\n") }
	prep  <-  prepareInput( pop = pop , obs = obs , med = med , size = size , tail = tail , verbose = verbose )


	#### global medians
	if (verbose) { verb("\nglobal medians...\n") }
	globalmed  <-  minMedian_sorted(prep$pop)


	### sample medians
	if (verbose) { verb("\nsample medians...\n") }

	samp.meds  <-  NULL
	other.meds  <-  NULL

	if (!is.null(prep$obs)) {

		popvec  <-  as.vector(as.matrix(as.data.frame(prep$pop)))

		samp.meds  <-  list()
		other.meds  <-  list()
		for (obsx  in  names(prep$obs)) {
			if (verbose) {
				verb("\t\t%s\n", obsx)
				show(head(prep$obs[[obsx]]))
				show(class(prep$obs[[obsx]]))
				show(head(pop))
			}

			samp.meds[[obsx]]  <-  minMedian_sample_sorted( pop = pop , sortpop = popvec , ord = prep$ord , sample = prep$obs[[obsx]] )

			if (verbose) { verb("\t\t\tother\n") }

			nonsamp  <-  setdiff( names(prep$pop[[1]]) , prep$obs[[obsx]] )
			other.meds[[obsx]]  <-  minMedian_sample_sorted( pop = pop , sortpop = popvec , ord = prep$ord , sample = nonsamp )
		} # obsx

		samp.meds  <-  do.call( c , samp.meds )
		samp.meds  <-  t(matrix( samp.meds , nrow = length(prep$pop)))
		samp.meds  <-  as.data.frame(samp.meds)
		rownames(samp.meds)  <-  names(prep$obs)
		colnames(samp.meds)  <-  colnames(pop)

		other.meds  <-  do.call( c , other.meds )
		other.meds  <-  t(matrix( other.meds , nrow = length(prep$pop)))
		other.meds  <-  as.data.frame(other.meds)
		rownames(other.meds)  <-  names(prep$obs)
		colnames(other.meds)  <-  colnames(pop)
			
	} else {
		samp.meds  <-  prep$med
	} # samp.meds

	### samp.meds = data frame of sample medians.
	#		cols = features of pop
	#		rows = separate samples for testing.

	### dist
	global.mat  <-  rep(globalmed , nrow(samp.meds))
	global.mat  <-  t(matrix( global.mat , nrow = length(prep$pop)))

	dist.samp.global  <-  abs( as.matrix(samp.meds) - global.mat )
	dist.samp.global  <-  as.data.frame(dist.samp.global)
	colnames(dist.samp.global)  <-  colnames(samp.meds)
	rownames(dist.samp.global)  <-  rownames(samp.meds)


	######################## compute
	if(verbose) {
		verb("computing...\n")
		show(head(prep$pop[[1]]))
		show(head(prep$size))
	} # verbose

	precomp  <-  NULL
	outdf  <-  list()

	# each sample
	for (namex  in  rownames(samp.meds)) {
		if(verbose) { verb("\t%s\n", namex) }

		######## precompute
		if (is.null(precomp)  ||  precomp$s != prep$size[namex]) {
			if (verbose) { verb("\ndoing precompute...\n") }
			precomp  <-  precomputeConstants( N = length(prep$pop[[1]]) , s = prep$size[namex] )
		}

		allLogP  <-  list()

		# each feature
		for (colx  in  colnames(samp.meds)) {
			if(verbose) { verb("\t\t%s\n", colx ) }

			if (prep$tail[namex] == "lower"  ||  prep$tail[namex] == "upper") {
				isLower  <-  prep$tail[namex] == "lower"
				allLogP[[colx]]  <-  kpmt.vector( pop.vec = prep$pop[[colx]] , sample.median = samp.meds[namex,colx] , sample.size = prep$size[namex] , lower.tail = isLower , precomp = precomp )

			} else if (prep$tail[namex] == "two-sided-by-value") {

				med.below  <-  globalmed[colx] - dist.samp.global[namex,colx]
				med.above  <-  globalmed[colx] + dist.samp.global[namex,colx]

				logp.below  <-  kpmt.vector( pop.vec = prep$pop[[colx]] , sample.median = med.below , sample.size = prep$size[namex] , lower.tail = TRUE , precomp = precomp )
				logp.above  <-  kpmt.vector( pop.vec = prep$pop[[colx]] , sample.median = med.above , sample.size = prep$size[namex] , lower.tail = FALSE , precomp = precomp )

				allLogP[[colx]]  <-  matrixStats::logSumExp( c(logp.below,logp.above) )

			} else if (prep$tail[namex] == "two-sided"  ||  prep$tail[namex] == "two-sided-by-quantile") {
						
				loc.obs  <-  max(which(prep$pop[[colx]] <= samp.meds[namex,colx]))
				loc.obs  <-  min( loc.obs , length(prep$pop[[colx]]) + 1 - loc.obs )

				med.below  <-  prep$pop[[colx]][loc.obs]
				med.above  <-  prep$pop[[colx]][length(prep$pop[[colx]]) + 1 - loc.obs]
		
				logp.below  <-  kpmt.vector( pop.vec = prep$pop[[colx]] , sample.median = med.below , sample.size = prep$size[namex] , lower.tail = TRUE , precomp = precomp )
				logp.above  <-  kpmt.vector( pop.vec = prep$pop[[colx]] , sample.median = med.above , sample.size = prep$size[namex] , lower.tail = FALSE , precomp = precomp )

				allLogP[[colx]]  <-  matrixStats::logSumExp( c(logp.below,logp.above) )
			} # tail
		} # colx

		allLogP  <-  unlist(allLogP)
	
		cn  <-  colnames(samp.meds)
		outdf[[namex]]  <-  data.frame( feature = cn , sample = namex ,  logp = allLogP , p.value = exp(allLogP) ,  median.sample = unlist(samp.meds[namex,cn]) , median.all = globalmed[cn] , median.other = NA )
		if (!is.null(other.meds)) {
			outdf[[namex]]$median.other  <-  unlist(other.meds[namex,cn])
		} 

	} # namex

	outdf  <-  do.call( rbind , outdf )

	outdf  <-  outdf[ order(outdf$logp) , , drop=FALSE]

	return(outdf)

} # kpmt




















# ' Prepare population
# '
# ' Prepares a population vector/matrix for efficient computation by the fixed population median test.
# '
# ' @param	pop	A data frame or matrix of population values.  rows are population members (e.g. transcripts),
# '			and each column is a different feature (e.g. 5' UTR length, codon usage of GAA, etc).
# '			Or, a vector of population values (e.g. 5' UTR length).  Each element represents a different population member
# '			(e.g. transcripts).
# '
# ' @return	A list.  Each element of the list is a sorted named vector.  Names correponds to rownames of the input df/matrix.
# '		Each list element is sorted independently of the others.  The name of each list element is the name
# '		of one of the columns of the input df/matrix.
preparePopulation  <-  function( pop ) {

	if (is.data.frame(pop)  ||  is.matrix(pop)) {

		### make sure has variable names
		if (is.null(colnames(pop))) {
			colnames(pop)  <-  paste("feature" , 1:ncol(pop) , sep="")
		} # colnames

		### make sure has rownames
		if (is.null(rownames(pop))) {
			rownames(pop)  <-  paste("entity" , 1:nrow(pop) , sep="" )
		} # rownames
		
		### assign rownames in list format
		rn  <-  rownames(pop)
		pop  <-  as.list(as.data.frame(pop))
		for (colx  in  1:length(pop)) {
			names(pop[[colx]])  <-  rn
		} # colx

	} else if (is.vector(pop)) {
		### make sure has names
		if (is.null(names(pop))) {
			names(pop)  <-  paste("entity" , 1:length(pop) , sep="")
		} # names

		pop  <-  list( feature1 = pop )
	} # df

	### now pop is a list of named vectors, though unordered by value.
		

	### sort
	ordlist  <-  sapply( pop , order , simplify = FALSE , USE.NAMES = TRUE )
	ordmat  <-  as.data.frame(ordlist)

	# to linear indexing
	linord  <-  (col(ordmat)-1) * nrow(ordmat)  +  as.matrix(ordmat)
	linord  <-  as.vector(linord)

	### actually sort pop
	pop  <-  mapply( FUN = function(x,y) x[y] , pop , ordlist , SIMPLIFY = FALSE , USE.NAMES = TRUE )

	prep  <-  list( pop = pop , ord = linord )
	return(prep)

} # preparePopulation












# ' Prepare input data.
# '
# ' This function prepares input data for efficient computation of finite population tests.
# '
# ' @param  pop	matrix,df,vector
# ' @param  obs	character vector, or list of char vec, or NULL
# ' @param  med	NULL or vector/df/matrix of medians
# ' @param  size	NULL or the sizes of the med in med (nrow(med))
# ' @param  tail	which side of tail to test, per row of med or element of obs
# ' @param  verbose	display extra messages for debugging.
# '
# ' @return   list with elements:
# '		pop	list of sorted pop vector.
prepareInput  <-  function(	pop ,
				obs  ,
				med , 
				size ,
				tail ,
				verbose ) { 


	#################### prepare
	if (verbose) { verb("\nprepare...\n") }
	prep  <-  preparePopulation(pop)
	pop  <-  prep$pop
	



	### obs & meds
	if (!is.null(obs)  &&  !is.null(med)) {
		stop("\n\n\n\n\nERROR!  can only give one of obs and med.\n\n\n\n")

	} else if (!is.null(obs)) {
		#### prepare observations

		if (!is.list(obs)) {
			obs  <-  list( sample1 = obs )
		}

		### check member names
		all.obs  <-  Reduce( union , obs )
		pop.rn  <-  names(pop[[1]])

		if (!all(all.obs %in% pop.rn)) {
			verb("\n\n\n\nERROR!  Not all observation members in the population!\n\n\n")
			verb("Observations not found in population:\n\n%s\n", paste(setdiff(all.obs , pop.rn ) , sep=" , ") )
			stop()
		}
	
		### sizes
		size  <-  sapply( obs , length , simplify = TRUE , USE.NAMES = TRUE )

	} else if (!is.null(med)) {

		# as data frame
		if (is.vector(med)) {
			med  <-  as.data.frame(matrix(med, nrow=1)) 
			colnames(med)  <-  names(pop)
			rownames(med)  <-  paste("entity", 1:nrow(med),sep="")
		} else if (is.data.frame(med)  ||  is.matrix(med)) {
			med  <-  as.data.frame(med)
		}

		# col & row names
		if (is.null(colnames(med))) {
			colnames(med)  <-  names(pop)
		}

		if (is.null(rownames(med))) {
			rownames(med)  <-  paste("entity", 1:nrow(med),sep="")
		}
				

		if (!all(colnames(med) %in% names(pop))) {
			verb("\n\n\n\nERROR!  not all med names in population features!\n")
			verb("\n\nmed names not in poulation features:\n%s\n", paste(setdiff(colnames(med),names(pop)) , sep=" , "))
			stop()
		} # med


		### size
		if (is.null(size)) { 
			stop("\n\n\n\nERROR!  if med is given, then must give  size too!\n\n")
		} # given

		if (is.null(names(size))) {
			names(size)  <-  rownames(med)
		} # names

	} else { # both obs & med are NULL
		stop("\n\n\n\nERROR! at least one of obs or med needs to be given!!!\n\n\n\n")
	} # obs, med


	### tail character manipulation
	tail  <-  tolower(tail)

	if (length(tail) == 1) {
		tail <- rep(tail , length(size))
	} # tail

	if (is.null(names(tail))) {
		names(tail)  <-  names(size)
	} # names tail



	#### order by sample size for pre-compute efficiency
	ordering  <-  order(size)
	size  <-  size[ordering]
	ordnam  <-  names(size)

	tail  <-  tail[ordnam]	

	if (!is.null(obs)) {	
		obs  <-  obs[ordnam]
	}
	if (!is.null(med)) {
		med  <-  med[ordnam, ,drop=FALSE]
	} 


	prepData  <-  list( pop = pop , obs = obs , med = med , size = size , tail = tail , ord = prep$ord )

	return(prepData)

} # prepareInput





















kpmt.vector  <-  function( pop.vec , sample.median , sample.size , lower.tail = TRUE , precomp = NULL ) {
######  Internal function.   This is not meant to be called by the user.
# This function will perform the finite median test on a single population vector, given an observed median number.

# Assumes that pop.vec is already sorted!

	################ definitions
	v  <-  pop.vec
	w  <-  sample.median
	s  <-  sample.size
	m  <-  ceiling(s/2)
	N  <-  length(v)


	################# prepare
	if (!lower.tail) {
		v  <-  -v
		w  <-  -w
	} # upper tail

	logi.U  <-  v <= w  &  1:N >= m  &  N-(1:N) >= s-m
	#	v <= w		necessary by definition of "median less than observed median"
	#	1:N >= m	there must be enough points BELOW the proposed median that half the sample can fit.
	#	N-(1:N) >= s-m	there must be enough points ABOVE  the proposed median that half the sample can fit.

	# If the population has duplicate values, these are conveniently automatically controlled for by this method, since 
	# this method is exhaustive and enumerative.


	if (!is.null(precomp)  &&  precomp$N == N  &&  precomp$s == s) {
		################### precompute
		logp  <-  precomp$const + matrixStats::logSumExp(precomp$summands[logi.U])
	} else {
		stop("\n\n\n\n\nERROR!  no/bad precompute provided!\n")
	} # precomp
	
	return(logp)

} # kpmt.vector













precomputeConstants  <-  function(N , s) {
#  OUTPUT:   list with fields "const", "summands", "N", "s"
#			const		= the log of the constant outside the sum.
#			summands	= a vector of log values of the summands for the whole population (NA where appropriate).
#			N		= population size for which const and summands were computed.
#			s		= sample size for which const and summands were computed.

	##################################### precompute
	# The working formula is:
	#	P( minMedian(Y) <= w )
	#		= sum_{x in U} frac{ binomial(x-1,m-1) * binomial(N-x,s-m) }{ binomila(N,s) }
	#		= frac{s! (N-s)!}{N! (m-1)! (s-m)!}    *   sum_{x in U} frac{(x-1)! (N-x)! }{ (x-m)! (N-x-s+m)! }
	#
	# (definitions in code below)
	#
	# Thus, the only thing that is data-specific is the set U.   With N and s known, we can pre-compute almost all values.
	#


	################ definitions
	m  <-  ceiling(s/2)

	# general U.  We do not impose the sample median constraint	
	U  <-  1:N

	################### compute
	const  <-  logStirling(s) + logStirling(N-s) - logStirling(N) - logStirling(m-1) - logStirling(s-m)

	summands  <-  logStirling(U-1) + logStirling(N-U) - logStirling(U-m) - logStirling(N-U-s+m)
	#summands  <-  sapply( U , FUN = function(x)  logStirling(x-1) + logStirling(N-x) - logStirling(x-m) - logStirling(N-x-s+m) )

	precomp  <-  list( const = const , summands = summands , N = N , s = s )

	return(precomp)

} # precomputeConstants


























#' Log Stirling
#'
#' Computes the log of the Stirling approximation of n!.
#'
#' @param n	integer or vector of integers.
#'
#' @return  Stirling approximation of log(n!).  If n <= 14, then computes log(n!) directly, i.e. no Stirling approximation.
#'
#' @export
logStirling  <-  function(n) {

	# Note:  we use more terms than the simple conventional "stirling approximation".

	logi.big  <-  n > 14
	retval  <-  rep(NA , length(n))
	retval[logi.big]  <-  n[logi.big] * log(n[logi.big]) - n[logi.big] + 0.5*log(2*pi*n[logi.big]) + 1/(12*n[logi.big]) - 1/(360*n[logi.big]*n[logi.big]*n[logi.big])
	suppressWarnings(retval[!logi.big]  <-  log(factorial(n[!logi.big])))

	return(retval)

} # logStirling










# ' Minimal Median of sorted vector
# ' 
# ' Given a sorted vector, efficiently identifies the minimal median
# '
# ' @param  x  a sorted vector of real values.
# '
# ' @return the minimal median of x.
minMedian_sorted  <-  function(x) {
	# assume already sorted

	if (is.list(x)) {
		medval  <-  sapply( x , FUN = function(y) y[ ceiling(length(y)/2) ] , simplify = TRUE , USE.NAMES = FALSE )
		names(medval)  <-  names(x)
	} else {
		medval  <- x[ ceiling(length(x)/2) ] 
	}

	return(medval)

} # minMedian_sorted





#' Minimal median
#'
#' Computes the minimal median of a vector or matrix.
#'
#' @param  x   a vector or matrix of real numbers.  if a matrix, then the minimal median will be computed for each column.
#'
#' @return the minimal median of x.
#'
#' @examples
#' minMedian(1:6) # returns 3
#'
#' @export
minMedian  <-  function(x) {
	# Definitions of "median" vary widely and often are not precise.
	# To make the computations and theory of this test feasible, we adopt the following
	# definition of "median".
	# 	Def:	Let X = { x_1 , x_2 , ... , x_N } be an ordered set of real numbers such that
	#			x_1 <= x_2 <= ...  <= x_N
	#		We define the "minimal median index" to be:
	#			i_mmi = ceiling(N/2)
	#		Thus:
	#			i_mmi =		m    if N = 2m 		(i.e. N is even)
	#				 	m+1  if N = 2m+1	(i.e. N is odd)
	#		And we now define the "median" to be:
	#			x_{i_mmi}
	#
	#   Note that by this definition of the median, we are guaranteed that >= 50% of the
	# population has value <= x_{i_mmi}.
	#   To distinguish this definition from "conventional" definitions, I also
	# refer to this definition as the "minimal median".

	if (is.data.frame(x)  ||  is.matrix(x)) {
		medx  <-  ceiling(nrow(x)/2)
		medval  <-  as.vector(apply(x,2,sort)[medx ,])
		names(medval)  <-  colnames(x)
	} else {
		medx  <-  ceiling(length(x)/2)
		x  <- x[order(x)]
		medval  <-  x[medx]
	} 

	return(medval)

} # minMedian





































#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
######################  VALIDATE  #######################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################


#' Validate computational accuracy of Known Population Median Test algorithm
#'
#' The implementation of the exact analytical solution of the Known Population Median Test
#' involves approximations using the Stirling series and is therefore suspect for computational error.
#' This function creates a population and empirically computes p-values via resampling.  It then
#' compares these empirical p-values to those calculated by the Known Population Median Test, and returns
#' the error in log(p).  WARNING!  Takes a long time.
#'
#' @param N  population size
#' @param n  sample size
#' @param nrep  number of resampling samples for empirically estimating p-values.
#'
#' @return  data frame with log(p) error and other information.
#'
#' @export
validate_accuracy  <-  function( N = 50 , n = 10 , nrep = 1e8  ) {

	####### create population
	verb("create population.\n")

	pop.vec  <-  rnorm(N , mean = 0 , sd = 1)
	pop.vec  <-  pop.vec[order(pop.vec)]


	
	######## sub-sample
	verb("sub-sample (nrep = %d)\n", nrep )

	ceil.half.samp  <-  ceiling(n/2)
	samp.hms  <-  replicate( n = nrep , { s = sample(pop.vec, size = n) ; s[order(s)][ceil.half.samp] } )


	########## estimate p-value
	verb("estimate p-values.\n")

	xran.ub  <-  min( N , n + 100 )
	xran  <-  n:min(N,n+100)

	pts  <-  pop.vec[xran]
	sampcts  <-  sapply( pts , FUN = function(x)  sum(samp.hms <= x) )


	####### my test
	verb("perform test.\n")

	my.logp  <-  sapply( pts , FUN = function(x)  kpmt( pop = pop.vec , size = n , med = x , tail = "lower" )$logp  )

	tdf  <-  data.frame( N = N , n = n , nrep = nrep , population.value = pts , num.sample.below = sampcts , empirical.logp.value = log(sampcts) - log(nrep) , test.logp.value = my.logp  )
	tdf$error.log.p  <-  tdf$test.logp.value - tdf$empirical.logp.value

	return(tdf)

} # validate_accuracy









