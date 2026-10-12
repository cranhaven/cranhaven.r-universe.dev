initialiseTree <-
function(){
	
	##### the private data ####
	private <- list()
	private$Naff <- NULL
	private$K <- NULL
	private$IDs <- c() #indiviual IDs
	private$ps <- c() # index of parent of each individual
	private$os <- list() # incidies of offspring of each indivdual
	private$data <- list() # genotype data associated with each indiviudal
	private$Ls <- list() # likelihoods of genotype data given true genotypes
	private$alpha <- list() #inside probabilities
	private$beta <- list() #outside probabilities
	private$betap <- list() #partial outside probabilies
	private$simVars <- NULL #sampled variants
	private$simCases <- NULL #sampled cases
	  
	mendT <- array(NA,c(3,3,3))
	mendT[c(3,6,7,8,9,10,18,19,20,21,22,25)] <- 0
	mendT[c(5,23)] <- 0.25
	mendT[c(2,4,11,13,14,15,17,24,26)] <- 0.5
	mendT[c(1,12,16,27)] <- 1
	
        private$mendT <- mendT
        
	##### the public functions ####
	public <- list()
	
	### add functions - these add nodes ###
	public$addRoot <- .addRoot	
	public$addNode <- .addNode	
	public$addPed <- .addPed

	### calc functions - these calculate internal data, but do not produce output ###
	public$calcLikelihoods <- .calcLikelihoods
	public$calcAlpha <- .calcAlpha
	public$calcBeta <- .calcBeta
	public$calcPost <- .calcPost
	public$calcParams <- .calcParams
	public$sampleVariant <- .sampleVariant
	public$sampleVariants <- .sampleVariants
	public$sampleCases <- .sampleCases

	### get functions: these return data about the tree ###
	public$getRoot <- .getRoot
	public$getOffspring <- .getOffspring
	public$getMaxDepth <- .getMaxDepth	
	public$getNNodes <- .getNNodes
	public$getNinds <- .getNinds
	public$getData <- .getData
	public$getPrevs <- .getPrevs
	
	### print functions: these print data  ###
	public$printTree <- .printTree
	public$printSummary <- .printSummary
	
	# set the environment of public functions to the environment of the data
	for (i in 1:length(public)){
		environment(public[[i]]) <- environment() 
		}
		
	# set the class
	class(public) <- "MangroveTree"
	return(public)
}

