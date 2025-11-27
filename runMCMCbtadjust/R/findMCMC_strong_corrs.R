
#' @title findMCMC_strong_corrs
#'
#' @description  finds the couples of parameters of a MCMC.list object that have at least a minCorr level of (absolute) correlation
#'
#' @param mcmcList R object of type mcmc.list that contains the MCMC output
#' @param corrMethod  character: designates the kind of correlation calculated among "pearson" (the default, for linear relationships), "spearman" (for monotone relationships) or "hoeffd" (for general associations - i.e. dependencies - between parameters)
#' @param minCorr  double, between 0 and 1: minimum level of (absolute) correlation to report.
#' @param namesToRemove  R object (can be a vector, matrix, array, list...) all components of which must be of character type: will remove parameters whose names partially match one of tghese components.
#'
#'@details
#' In case corrMethod equals "hoeffd", the hoeffd function in the Hmisc package will be used. This can be very slow. Therefore a warning message is printed in this case.
#'
#' @examples
#' \dontrun{
#' #generating data
#' set.seed(1)
#' y1000<-rnorm(n=1000,mean=600,sd=30)
#' ModelData <-list(mass = y1000,nobs = length(y1000))
#'
#' #writing the Jags code as a character chain in R
#' modeltotransfer<-"model {
#'
#' # Priors
#' population.mean ~ dunif(0,5000)
#' population.sd ~ dunif(0,100)
#'
#' # Precision = 1/variance: Normal distribution parameterized by precision in Jags
#' population.variance <- population.sd * population.sd
#' precision <- 1 / population.variance
#'
#' # Likelihood
#' for(i in 1:nobs){
#'   mass[i] ~ dnorm(population.mean, precision)
#'  }
#'  }"
#'
#' #specifying the initial values
#' ModelInits <- function()
#' {list (population.mean = rnorm(1,600,90), population.sd = runif(1, 1, 30))}
#' params <- c("population.mean", "population.sd", "population.variance")
#' K<-3
#' set.seed(1)
#' Inits<-lapply(1:K,function(x){ModelInits()})
#'
#' # running runMCMC_btadjust with MCMC_language="Jags":
#' set.seed(1)
#' out.mcmc.Coda<-runMCMC_btadjust(MCMC_language="Jags", code=modeltotransfer,
#' data=ModelData,
#' Nchains=K, params=params, inits=Inits,
#' niter.min=1000, niter.max=300000,
#' nburnin.min=100, nburnin.max=200000,
#' thin.min=1, thin.max=1000,
#' neff.min=1000, conv.max=1.05,
#' control=list(print.diagnostics=TRUE, neff.method="Coda"))
#'
#' findMCMC_strong_corrs(out.mcmc.Coda)
#' }
#' @export
#'
#'
#'
#'
findMCMC_strong_corrs<-function(mcmcList, corrMethod="pearson",minCorr=0.3,namesToRemove=NULL)
{#finds the couples of parameters of a MCMC.list object that have at least a minCorr level of (absolute) correlation

  ##0-checkings

  if (!coda::is.mcmc.list(mcmcList)&!is.matrix(mcmcList))
  {stop("The mcmcList argument is not a mcmc.list or a matrix")}

  if (length(corrMethod)!=1)
  {stop(paste0("The corrMethod argument should be of length 1. Is of length ",length(corrMethod)))}

  if (!is.character(corrMethod))
  {stop(paste0("The corrMethod argument should of character type. Is of type: ",typeof(corrMethod)))}

  if (!is.element(corrMethod,c("pearson","spearman","hoeffd")))
  {stop("The corrMethod argument should take its values among \"pearson\", \"spearman\", \"hoeffd\"")}


  if (length(minCorr)!=1)
  {stop(paste0("The minCorr argument should be of length 1. Is of length: ",length(minCorr)))}

  if (!is.double(minCorr))
  {stop(paste0("The minCorr argument should of double type. Is of type: ",typeof(minCorr)))}

  if (minCorr>1|minCorr<0)
  {stop("The minCorr argument should be between 0 and 1")}


  if (length(namesToRemove)>0)
    {if (sum(sapply(as.vector(unlist(namesToRemove,typeof)))!="character")>0)
             {stop("The namesToRemove argument should have only components of type \"character\"")}
    }
  #
  ##1-transforming mcmcList to a matrix
  if (!is.matrix(mcmcList))
    {Tablein<-as.matrix(mcmcList)}
  else
    {Tablein<-mcmcList}

  ##2-removing parameters if namesToRemove specified
  if (!is.null(namesToRemove))
  {dims.to.remove<-unique(unlist(sapply(as.vector(unlist(namesToRemove,typeof)),function(x){grep(x,dimnames(Tablein)[[2]])})))
   Tablein<-Tablein[,setdiff(1:dim(Tablein)[2],dims.to.remove)]
  }

  ##3- calculating correlations
  if (corrMethod!="hoeffd") {
    Table<-Hmisc::rcorr(Tablein,type=corrMethod)$r
  } else {
    warning("findMCMC_strong_corrs can be cvery slow when corrMethod is \"hoeffd\". If so consider restricting the analyzed parameters with argument namesToRemove, thinning the mcmc.list or using another corrMethod.")
    Table<-Hmisc::hoeffd(Tablein)$D
  }

  ##4- keeping only correlations above minCorr
  Temp<-which(abs(Table)>minCorr&Table!=1,arr.ind=T)

  ##5- formatting the result and ending function
  Res<-cbind.data.frame(dimnames(Table)[[1]][Temp[,1]],dimnames(Table)[[1]][Temp[,2]],Table[Temp])

  return(Res[order(abs(Res[,3]),decreasing=TRUE),])
  }

