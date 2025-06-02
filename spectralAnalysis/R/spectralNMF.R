# Project: spectralAnalysis_git
# 
# Author: nsauwen <nicolas.sauwen@openanalytics.eu>
#
# Description: Run NMF analyses on spectral data
#
# Changes:
#  * version 0.3.2 
#      + change of extractor (S4)
#      + object replaces by object for consistency 
#
###############################################################################




#' Perform Non-Negative Matrix factorization on spectral data
#' 
#' @param object \code{\link{SpectraInTime-class}}
#' @param rank number of NMF components to be found
#' @param method name of the NMF method to be used. "PGNMF" (default), "HALSacc" 
#' and "semiNMF" are methods derived from the hNMF package. All methods from the NMF package are also available.
#' @param initSpectralData this can be a list of spectralData objects, containing 
#' the pure component spectra. It can also be either of the NMF factor matrices with initial values 
#' @param nruns number of NMF runs. It is recommended to run the NMF analyses multiple
#' times when random seeding is used, to avoid a suboptimal solution
#' @param subsamplingFactor subsampling factor used during NMF analysis
#' @param checkDivergence Boolean indicating whether divergence checking should be performed
#' @param maxIter maximum number of iterations per NMF run 
#' @param includeRefs boolean, indicating whether references should be included in the input matrix for the NMF analysis
#' @return \code{\link{SpectraInTimeComp-class}} which includeds a scaled NMF model (in accordance with the NMF package definition)
#' @examples
#' \donttest{
#'  spectralExample    <-  getSpectraInTimeExample()
#'  nmfResult          <-  spectralNMF( spectralExample , rank = 2 , subsamplingFactor = 5 )
#'   nmfObject         <-  getDimensionReduction( nmfResult , type = "NMF")$NMF
#'   nmfTrends         <-  t( NMF::coef( nmfObject ) )
#'   matplot( nmfTrends , type = "l" , x = getTimePoints( spectralExample , timeUnit = "hours"  ),
#' 		 xlab = "time in hours"  )
#' }
#' @author Nicolas Sauwen
#' @return \code{\link{SpectraInTimeComp-class}}
#' @export
spectralNMF        <- function ( object , rank, method = "PGNMF", initSpectralData = NULL,
		nruns = 10, subsamplingFactor = 1, checkDivergence = TRUE, maxIter = 1000, includeRefs = FALSE) {
  
  spectra          <- getNMFInputMatrix( object , method)

  if(includeRefs) { # added for THz Raman
	  spectra <- cbind(spectra, initSpectralData) 
	  timePointsList <- NULL
  } else{
	  timePointsList <- list()
	  timePointsList[[1]] <- getTimePoints( object )
  }
  seed             <- initializeNMFModel( initSpectralData, spectra = spectra, spectralAxis = getSpectralAxis( object ) )
  # Check if NMF initialization is consistent with specified rank
  if(!is.null(seed)){
	  W0               <- NMF::basis(seed)
	  H0			   <- NMF::coef(seed)
	  if(ncol(W0) > rank) {
		  stop("Number of provided pure component spectra is larger than specified NMF rank.")    
	  }
	  else if(ncol(W0) < rank){
		  # In that case we complete the initial matrix with random sources
		  warning("Number of specified source components is lower than specified rank. Source matrix will be completed with random vectors.")
#		  nruns <- 10
#		  checkDivergence <- FALSE
	  }
	  if(method != "semiNMF" & (length(which(W0<0)) != 0 | length(which(H0<0)) != 0)){
		  stop("Improper NMF initialization. Provided initialization contains negative values.") 
	  }
  }

  NMFResult         <- runNMF(spectra, rank, method, seed, nruns, checkDivergence, timePointsList, subsamplingFactor, maxIter)
  NMFResult         <- NMFResult[[1]]
  NMFResult         <- scaleNMFResult( NMFResult )
  nmfSettings       <-  list(  rank = rank , method = method , initSpectralData = initSpectralData , nruns = nruns, subsamplingFactor = subsamplingFactor , checkDivergence = checkDivergence) # remove function name and spectral object from the list
  nmfSlot           <-  list( NMF = NMFResult , settings = nmfSettings , experimentsUsed =  getExperimentName( object ) )
  SpectraInTimeComp( object , dimensionReduction = list( NMF = nmfSlot )  )
}


#' Perform Non-Negative Matrix factorization on list of SPC files
#' 
#' @param objectList list of SPC files
#' @param rank number of NMF components to be found
#' @param method name of the NMF method to be used, consult the help
#' of the 'nmf' function from the NMF package for the methods available by default
#' @param initSpectralData list of SPC files containing pure component spectra
#' @param nruns number of NMF runs.
#' @param subsamplingFactor subsampling factor used during NMF analysis
#' @param checkDivergence Boolean indicating whether divergence checking should be performed
#' @param maxIter maximum number of iterations per NMF run
#' @examples 
#' \donttest{
#' 
#'    spectralData    <-  getListOfSpectraExample()
#'    spectraWithNmf  <-  spectralNMFList( spectralData , rank = 2 )
#' }
#' @return list of \code{\link{SpectraInTimeComp-class}}
#' @importFrom NMF basis coef nmfModel
#' @author Nicolas Sauwen
#' @export
spectralNMFList             <-  function( objectList ,  rank , method = "PGNMF" , initSpectralData = NULL, nruns = 10, subsamplingFactor = 3, checkDivergence = TRUE, maxIter = 1000) {
  timePointsList            <- list()
  nObservations 			      <- 0
  nWavelengths 				      <- ncol(   getSpectra( objectList[[1]] ) )
  
  for( iFile in 1:length(objectList)) {
    nObservations            <-  nObservations + nrow(  getSpectra( objectList[[iFile]] ) )
  }
  
  spectra <- matrix(0, nWavelengths, nObservations) # initialize object
  count <- 1 # Counter keeps track of where to put the data matrices per experiment in the global spectra matrix
  
  for( iFile in seq_along (objectList)) {
    spectra_temp            <-  getNMFInputMatrix(objectList[[iFile]], method)
    count_temp 				<- ncol(spectra_temp)
    if(nrow(spectra_temp) != nrow(spectra)) {
      stop("NMF analysis cannot be executed. Please make sure all spectral files cover the same wavelength range.")    
    }
    spectra[ , count:(count+count_temp-1)]	<-  spectra_temp
    timePointsList[[iFile]] 				<- getTimePoints( objectList[[iFile]] )
    count <- count + count_temp
  }
  
  seed             <- initializeNMFModel(initSpectralData, spectra = spectra, spectralAxis = getSpectralAxis( objectList[[1]] ) )
  # Check if NMF initialization is consistent with specified rank
  W0               <- NMF::basis( seed )
  if( !is.null( W0 ) ){ # check dimension when there is initialization matrix
    if( ncol(W0) > rank ) { 
      stop("Number of provided pure component spectra is larger than specified NMF rank.")    
    }
    else if( ncol(W0) < rank ){
      # In that case we complete the initial matrix with random sources
      warning( "Number of specified source components is lower than specified rank. Source matrix will be completed with random vectors." )
      nruns <- 10
      checkDivergence <- FALSE
    }
  }

  NMFResultList        <- runNMF(spectra, rank, method, seed, nruns, checkDivergence, timePointsList, subsamplingFactor, maxIter)
  NMFResultList        <- lapply(NMFResultList, scaleNMFResult)
  
  ## return list of SpectraInTimeComp with appropriate settings
  nmfSettings       <-  list(  rank = rank , method = method , initSpectralData = initSpectralData , nruns = nruns, subsamplingFactor = subsamplingFactor , checkDivergence = checkDivergence) 
  experimentsUsed    <-  unname( sapply( objectList , getExperimentName ) )
      
  spectraInTimeCompList   <-  mapply( spec = objectList, nmf = NMFResultList , FUN = function( spec , nmf ){
        nmfSlot           <-  list( NMF = nmf , settings = nmfSettings , experimentsUsed = experimentsUsed )
        SpectraInTimeComp( spec , dimensionReduction = list( NMF = nmfSlot )  ) 
      } 
  )
  
spectraInTimeCompList
}

#' Actual NMF analysis
#' 
#' @param spectra spectral input matrix, with wavelengths as its rows and time points as its columns
#' @param rank number of NMF components to be found
#' @param method name of the NMF method to be used, consult the help
#' of the 'nmf' function from the NMF package for the methods available by default
#' @param seed nmfModel object containing initialization of the factor matrices
#' @param nruns number of NMF runs. It is recommended to run the NMF analyses multiple
#' times when random seeding is used, to avoid a suboptimal solution
#' @param checkDivergence Boolean indicating whether divergence checking should be performed, defaults to \code{TRUE}
#' @param timePointsList list of time point vectors of the individual experiments
#' @param subsamplingFactor subsampling factor used during NMF analysis
#' @param maxIter maximum number of iterations per NMF run
#' @return Resulting NMF model (in accordance with the NMF package definition)
#' @importFrom NMF basis nmf nmfAlgorithm setNMFMethod
#' @importFrom hNMF HALSacc PGNMF semiNMF
#' @importFrom stats runif
#' @importFrom utils combn
#' @author Nicolas Sauwen
#' @export
runNMF                      <-  function (spectra, rank, method = "PGNMF", seed = NULL, nruns = 10, checkDivergence = TRUE, timePointsList = NULL, subsamplingFactor = 3, maxIter = 1000) {
  
#  # Check if NMF method is already available in the NMF package
#  strComp                  <- match( nmfAlgorithm() , method )
#  if( sum( is.na( strComp ) ) == length(strComp)) {
#    setNMFMethod( method , get( method ) ) # necessary to set method if you provide in in nmf function?
#  }
  nTimePoints <- ncol(spectra)
  
  sampleInds <- seq(1,ncol(spectra),subsamplingFactor)
  if(subsamplingFactor != 1) {
    spectra        <- spectra[, sampleInds]
    if( !is.null( seed ) ) {
      W0           <- NMF::basis(seed) 
      H0           <- NMF::coef(seed)
      H0           <- H0[, seq(1,ncol(H0),subsamplingFactor), drop = FALSE] 
      seed         <- NMF::nmfModel( W = W0 , H = H0 )
    }
  }
  
  if( is.null( seed ) ) {
#    NMFResult               <- nmf( spectra , rank = rank, method = get(method), nrun = nruns, checkDivergence = F, .options="-p")
	
	NMFResult <- NULL
	residu <- Inf
	
	for(iRun in 1:nruns) {	
		W0 <- matrix(runif(rank*nrow(spectra)), nrow = nrow(spectra), ncol = rank)	
		H0 <- matrix(runif(rank*ncol(spectra)), nrow = rank, ncol = ncol(spectra))	
		seed <- NMF::nmfModel( W = W0 , H = H0 )
		if(method == "PGNMF"){
			NMFResult_temp <- PGNMF(spectra, nmfMod = seed, checkDivergence = F, maxIter = maxIter)
		}
		else if(method == "HALSacc"){
			NMFResult_temp <- HALSacc(spectra, nmfMod = seed, checkDivergence = F, maxiter = maxIter)
		}
		else if(method == "semiNMF"){
			NMFResult_temp <- semiNMF(spectra, nmfMod = seed, checkDivergence = F, maxiter = maxIter)
		}
		W_temp <- NMF::basis(NMFResult_temp)
		H_temp <- NMF::coef(NMFResult_temp)
		residu_temp <- norm(spectra - W_temp%*%H_temp,'f')
		if(residu_temp < residu) {
			NMFResult <- NMFResult_temp
			residu <- residu_temp
		}
	}
  }
  else{
#      NMFResult             <-  nmf( spectra , rank = rank , method = get(method) , nrun = 1 , seed = seed , checkDivergence = checkDivergence, .options = "-p" )
	  W0 <- NMF::basis(seed)
	  if(ncol(W0) == rank) {
		  if(method == "PGNMF"){
			  NMFResult <- PGNMF(spectra, nmfMod = seed, checkDivergence = checkDivergence, maxIter = maxIter)
		  }
		  else if(method == "HALSacc"){
			  NMFResult <- HALSacc(spectra, nmfMod = seed, checkDivergence = checkDivergence, maxiter = maxIter)
		  }
		  else if(method == "semiNMF"){
			  NMFResult <- semiNMF(spectra, nmfMod = seed, checkDivergence = checkDivergence, maxiter = maxIter)
		  }
	  }
	  else { # This means that the source matrix has to be completed with random vector(s)
		  
		  NMFResult <- NULL
		  residu <- Inf
		  W0_orig <- W0
		  
		  for(iRun in 1:nruns) {	
			  W0 <- completeSourceMatrix(W0_orig, rank, method)
			  seed <- initializeNMFModel( initSpectralData = W0, spectra = spectra)
			  iter <- 1
			  maxIter2 <- 20
			  overlap <- FALSE # Boolean to indicate when random source vectors start overlapping too much with initialized sources
			  NMFResult_prev <- NULL
			  while(iter <= maxIter2 & !overlap) { #This repetition is done to be able to keep using checkDivergence = T (although with start initialization with 1 random vector)
				  if(method == "PGNMF"){
					  NMFResult_temp <- PGNMF(spectra, nmfMod = seed, checkDivergence = T, maxIter = maxIter)
				  }
				  else if(method == "HALSacc"){
					  NMFResult_temp <- HALSacc(spectra, nmfMod = seed, checkDivergence = T, maxiter = maxIter)			  
				  }
				  else if(method == "semiNMF"){
					  NMFResult_temp <- semiNMF(spectra, nmfMod = seed, checkDivergence = T, maxiter = maxIter)
				  }
				  NMFResult_temp <- scaleNMFResult(NMFResult_temp)
				  W_temp <- NMF::basis(NMFResult_temp)
				  checkOverlapMat <- t(W_temp)%*%W_temp
				  checkOverlapInds <- upper.tri(checkOverlapMat)
				  if(ncol(W0_orig) > 1){
					  skipInds <- t(combn(1:ncol(W0_orig),2))
					  checkOverlapInds[skipInds] <- FALSE
				  }
				  checkOverlap <- checkOverlapMat[checkOverlapInds]
				  # Here we are checking the correlation coefficients between the randomly initialized sources and all sources
				  if(sum(checkOverlap > 0.9) > 0){
					  NMFResult_temp <- NMFResult_prev
					  overlap <- TRUE
				  }
				  W_temp <- cbind(W0_orig, W_temp[,(ncol(W0_orig)+1):ncol(W_temp)])
				  NMFResult_prev <- NMFResult_temp
				  seed <- initializeNMFModel( initSpectralData = W_temp, spectra = spectra)				  
				  iter <- iter + 1
			  }
			  W_temp <- NMF::basis(NMFResult_temp)
			  H_temp <- NMF::coef(NMFResult_temp)
			  residu_temp <- norm(spectra - W_temp%*%H_temp,'f')
			  if(residu_temp < residu) {
				  NMFResult <- NMFResult_temp
				  residu <- residu_temp
			  }
		  }
	  }
  }
  
  # Split abundances per file and save in a list of NMF model objects 
  W <- NMF::basis(NMFResult)
  H <- NMF::coef(NMFResult)
  
  if(is.null(timePointsList)) {
    timePointsList <- list(1:nTimePoints)
  }
  
  NMFResultList  <- list()
  ind1           <- 1
  ind2           <- floor((length(timePointsList[[1]])-1)/subsamplingFactor)+1
  currentInd <- 1
  shift1 <- 0
  shift2 <- 0
  
  for(iFile in 1:length(timePointsList)) {
    W_temp       <- W
    H_temp       <- H[, (ind1:ind2), drop = FALSE]
    if(iFile != length(timePointsList)) {
      currentInd <- currentInd + length(timePointsList[[iFile]])
      shiftInd <- sampleInds-currentInd
      shiftInd <- which(shiftInd >= 0)[1]
      shift2 <- sampleInds[shiftInd] - currentInd
      
      ind1       <- ind1 + floor((length(timePointsList[[iFile]])-shift1-1)/subsamplingFactor) + 1
      ind2       <- ind2 + floor((length(timePointsList[[iFile+1]])-shift2-1)/subsamplingFactor) + 1
    }
	dimnames(W_temp) <- NULL
	dimnames(H_temp) <- NULL
	NMFResultList[[iFile]] <- NMF::nmfModel(W = W_temp, H = H_temp)
	colnames(NMF::basis(NMFResultList[[iFile]])) <- colnames(W)
    if(subsamplingFactor != 1) {
      NMFResultList[[iFile]]      <- upsampleNMFResult(NMFResultList[[iFile]], timePointsList[[iFile]], subsamplingFactor, shift1)
      NMFResultList[[iFile]]      <- scaleNMFResult(NMFResultList[[iFile]])
    }
    shift1 <- shift2
  }    
  gc()
  return( NMFResultList )
}



#' Get spectralData as input NMF model
#' 
#' Extract spectral input matrix from \code{\link{SpectraInTime-class}} and condition properly for NMF modeling
#' 
#' @param object object of the 'spectralData' class, such as a raw SPC file
#' @param method name of the NMF method to be used.
#' @return spectral matrix, with wavelengths as its rows and time points as its columns
#' @author Nicolas Sauwen
#' @export
getNMFInputMatrix          <- function(object, method = "") {
  
  spectra                  <-  getSpectra( object )
  spectraPreprocessed      <-  nonNegativePreprocessing( spectra, method )
  return( spectraPreprocessed )
}


# seperate from getNMFInputMatrix because needed in spectralApps

#'condition datamatrix to input in and condition properly for NMF
#' @details put negative values to zero, transpose, an add small value zero row (wavelength with only zeros) 
#' 
#' @param spectra matrix of spectra 
#' @param method name of the NMF method to be used.
#' @return matrix, with wavelengths as its rows and time points as its columns
#' @export
nonNegativePreprocessing    <-  function( spectra, method = "") {
  zeroInds <- which(spectra < 0)
  if(length(zeroInds) > 0 & method != "semiNMF"){
    spectra[ spectra < 0 ]    <-  0
    warning("Input data contain negative values. These were reset to zero for NMF analysis")
  }
  
  spectraT                  <-  t( spectra )
  
  zeroRow                   <-  which( ( apply( spectraT , 1 , sum ) ) == 0 )
  spectraT[ zeroRow , ]     <- 1e-16
  spectraT[ spectraT == 0 ] <- 1e-16
  return(spectraT)
}


if( 0 == 1 ) {
  
# debug not working in app 
  initSpectralData           <-  NULL
  W0Init                     <-  nonNegativePreprocessing( initialization )
  W0 = W0Init  ; spectra = dataForNMF ; spectralAxis = dataInput$spectralAxis
  initNmfModel               <-  initializeNMFModel( W0 = W0Init  , spectra = dataForNMF ,  spectralAxis = dataInput$spectralAxis )   
  
}

#' Initialize NMF model with initial spectral data
#' 
#' @param initSpectralData this can be a list of spectralData objects, containing 
#' the pure component spectra. It can also be either of the NMF factor matrices with initial values 
#' @param spectra spectral matrix, with wavelengths as its rows and time points as its columns
#' @param spectralAxis vector of wavelength/spectralAxis values
#' @importFrom nnls nnls
#' @importFrom stats coefficients approx
#' @importFrom NMF nmfModel
#' @return an object that inherents from the class \code{\link[NMF]{NMF}}
#' @export
initializeNMFModel     <- function(initSpectralData, spectra, spectralAxis = NULL) {
  if( !is.list( initSpectralData ) & !is.matrix( initSpectralData ) ){
    NMFInit            <- NULL
    return(NMFInit)
  }  
  else if( is.list( initSpectralData ) ) {
	rank               <-  length(initSpectralData)
    W0                 <-  matrix( 0 , nrow(spectra), rank)
    
    for(iList in 1:rank) {
      spectralAxis_0    <-  getSpectralAxis( initSpectralData[[iList]] )
      spectra_0        <- getSpectra( initSpectralData )[[iList]][1,]
      if(abs(spectralAxis[1] - spectralAxis_0[1]) > 1e-3 | abs(spectralAxis[length(spectralAxis)] - spectralAxis_0[length(spectralAxis_0)]) > 1e-3) {
        stop("Please make sure the pure component SPC files cover the same wavelength range as the input SPC file(s).")    
      }
      if(length(spectralAxis) != length(spectralAxis_0)) {
        spectra_0      <- approx(spectralAxis_0, spectra_0, xout = spectralAxis) 
        spectra_0      <- spectra_0$y
      }    
      W0[,iList]       <-  spectra_0
    }
  } 
  else if(is.matrix( initSpectralData ) & nrow(initSpectralData) == nrow(spectra)) {
    W0                 <-  initSpectralData
    rank               <-  ncol( W0 )
  }
  
  else if(is.matrix( initSpectralData ) & ncol(initSpectralData) == ncol(spectra)) {
	  H0                 <-  initSpectralData
	  rank               <-  nrow( H0 )
  }
  else {
	  W0                 <-  initSpectralData
	  rank               <-  ncol( W0 )
  }
  
  if(is.na(match("H0", ls()))){
	  nSignals           <-  ncol(spectra)
	  H0                 <-  matrix( 0 , rank , nSignals)
	  for( iSignal in 1:nSignals ) {
		  nlsFit           <-  nnls::nnls( W0 , spectra[,iSignal] )   
		  H0[,iSignal]     <-  coefficients(nlsFit)
	  }
  }
  
  if(is.na(match("W0", ls()))){
	  nFeatures           <-  nrow(spectra)
	  W0                  <-  matrix( 0 , rank, nFeatures)
	  for( iFeature in 1:nFeatures ) {
		  nlsFit           <-  nnls::nnls( t(H0) , spectra[iFeature, ] )   
		  W0[,iFeature]     <-  coefficients(nlsFit)
	  }
	  W0 <- t(W0)
  }
  
  # Proper scaling of the factor matrices
  N0                 <- sqrt(diag(t(W0)%*%W0))
  N_W0               <- matrix(N0,nrow = nrow(W0),ncol = ncol(W0),byrow = T)
  W0                 <- W0/N_W0
  N_H0               <- matrix(N0,nrow = nrow(H0),ncol = ncol(H0))
  H0                 <- H0*N_H0
  
  # Avoid H0 having zero rowSum
  scoresSums <- rowSums(H0)
  eps <- 1e-12
  zeroInd <- which(scoresSums < eps)
  if(length(zeroInd) > 0) H0[zeroInd, ] <- eps
  
  NMFInit            <-  NMF::nmfModel( W = W0 , H = H0 )
  NMFInit            <-  scaleNMFResult(NMFInit)
  
  return(NMFInit)
}


#' Apply fixed scaling to NMF model
#' 
#' Apply fixed scaling to NMF model matrices by normalizing the basis vectors
#' 
#' @param NMFResult Fitted NMF model
#' @return NMFResult Rescaled NMF model
#' @importFrom NMF basis coef
#' @author Nicolas Sauwen
#' @export
scaleNMFResult      <- function(NMFResult) {
  
  W                 <- NMF::basis(NMFResult)
  N                 <- sqrt(diag(t(W)%*%W))
  N_W               <- matrix(N,nrow = nrow(W),ncol = ncol(W),byrow = T)
  W                 <- W/N_W
  H                 <- NMF::coef(NMFResult)
  N_H               <- matrix(N,nrow = nrow(H),ncol = ncol(H))
  H                 <- H*N_H
  NMF::basis(NMFResult)  <- W
  NMF::coef(NMFResult)   <- H
  return(NMFResult)
  
}


#' Check for redunt NMF source vectors
#' 
#' Check if any of the source vectors in the initialized NMF model are redundant
#'  and should be omitted from the actual NMF analysis
#' 
#' @param seed nmfModel object containing initialization of the factor matrices
#' @return boolean vector, indicating which source vector(s) are redundant
#' @importFrom NMF coef
#' @importFrom stats median
#' @author Nicolas Sauwen
checkForRedundantSources <- function(seed) {
  H0                   <- coef(seed)
  redundantSourceVect  <- rep(FALSE, nrow(H0))
  H0_sum               <- apply(H0,1,sum)
  redundantSourceVect[which(H0_sum < 0.005*median(H0_sum))] <- TRUE
  return(redundantSourceVect)
}

#' Remove redundant sources from the initial NMF model
#' 
#' @param seed nmfModel object containing initialization of the factor matrices
#' @param redundantSources boolean vector, obtained from \code{\link{checkForRedundantSources}}
#' @return nmfModel object with redundant sources removed from initial factor matrices
#' @importFrom NMF basis coef nmfModel
#' @author Nicolas Sauwen
removeRedundantSources <- function(seed, redundantSources) {
  W0 <- basis(seed)
  H0 <- coef(seed)
  redundantInds <- which(redundantSources == TRUE)
  W0 <- W0[,-redundantInds]
  H0 <- H0[-redundantInds,]
  seedNew <- nmfModel(W = W0 , H = H0)
  return(seedNew)
}

#' Re-introduce redundant sources in NMF-model
#' 
#' Re-introduce redundant source vectors and corresponding zero abundances 
#' into final NMF result
#' 
#' @param NMFResult Fitted NMF model
#' @param seed_orig Initial NMF model
#' @param redundantSources boolean vector, obtained from \code{\link{checkForRedundantSources}}
#' @return Final NMF model with redundant sources re-introduced
#' @importFrom NMF basis coef
#' @author Nicolas Sauwen
includeRedundantSources <- function(NMFResult, seed_orig, redundantSources) {
  W <- basis(NMFResult)
  H <- coef(NMFResult)
  W0 <- basis(seed_orig)
  H0 <- coef(seed_orig)
  nRows <- nrow(W0)
  nCols <- ncol(H0)
  rank <- ncol(W0)
  Wnew <- matrix(0, nrow = nRows, ncol = rank)
  Hnew <- matrix(0, nrow = rank, ncol = nCols)
  count <- 1
  for(i in 1:rank) {
    if(redundantSources[i] == TRUE){
      Wnew[,i] <- W0[,i]
      Hnew[i,] <- 0
    }
    else {
      Wnew[,i] <- W[,count]
      Hnew[i,] <- H[count,]
      count <- count + 1
    }
  }
  NMF::basis(NMFResult) <- Wnew
  NMF::coef(NMFResult) <- Hnew
  return(NMFResult)
}

#' Upsample NMF result to original temporal resolution
#' 
#' @param NMFResult Fitted NMF model
#' @param timePoints Original time points
#' @param subsamplingFactor Subsampling factor
#' @param shift Integer that correctly shifts subsampling index when 
#' applying NMF to multiple experiments
#' @return Upsampled NMF model
#' @importFrom stats approx
#' @author Nicolas Sauwen
upsampleNMFResult <- function(NMFResult, timePoints, subsamplingFactor, shift = 0) {
  H_subsamp <- NMF::coef(NMFResult)
  timePoints_subsamp <- timePoints[seq(1+shift,length(timePoints),subsamplingFactor)]
#  H <- apply(H_subsamp, 1, approx, x = timePoints_subsamp, xout = timePoints)
  H <- matrix(0, nrow(H_subsamp), length(timePoints))
  for(i in 1:nrow(H)) {
    H_interp <- approx(x = timePoints_subsamp, y = H_subsamp[i,], xout = timePoints, rule = 2)
    H[i,] <- H_interp$y
  }
  NMF::coef(NMFResult) <- H
  return(NMFResult)
}

#' complete source matrix
#' @importFrom stats runif
#' @keywords internal
completeSourceMatrix <- function(W0, rank, method) {
	
	extraComponents <- rank - ncol(W0)
	if(method == "semiNMF"){
		for(i in 1:extraComponents) {
			W0 <- cbind(W0, runif(nrow(W0), -1, 1))
		}
	} else{
		for(i in 1:extraComponents) {
			W0 <- cbind(W0, runif(nrow(W0), 0, 1))
		}
	}	
	return(W0)
}

#' Based on previously obtained NMF result \code{NMFResult}, estimate coefficients for a new 
#' spectralData object \code{object} using non-negative least squares fitting. The result is 
#' returned as as an NMF model.
#' 
#' @param object \code{\link{SpectraInTime-class}}
#' @param NMFResult Fitted NMF model
#' @return Fitted non-negative least squares result in the form of an NMF model
#' @author nsauwen
#' @importFrom NMF basis
#' @importFrom nnls nnls
#' @export
predictNNLS <- function(object, NMFResult) {
	W <- NMF::basis(NMFResult)
	rank <- ncol(W)
	spectra <- getNMFInputMatrix( object )
	H <- matrix(0, rank, ncol(spectra))
	for(iSignal in 1:ncol(spectra)) {
		nlsFit <- nnls::nnls(W,spectra[,iSignal]) 
		H[,iSignal] <- stats::coefficients(nlsFit)
	}
	predictResult <- NMF::nmfModel(W = W, H = H)
}


#' NMF relative residual per observation
#' 
#' Compute relative residual per observation of an NMF fit to a spectral data set
#' 
#' @param object \code{\link{SpectraInTime-class}}
#' @param NMFResult Fitted NMF model
#' @return Dataframe, containing time (observation) vector and residual vector
#' @author nsauwen
#' @importFrom hNMF residualNMF
#' @export
computeNMFResidu <- function(object, NMFResult) {
	
	timePts <- getTimePoints(object)
	spectra <- t(getSpectra(object))
	residu <- residualNMF(spectra, NMFResult)
	
	return(data.frame(time=timePts, residu=residu))
	
}
