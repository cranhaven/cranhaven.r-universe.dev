#' Incremental Automated Mixtures of Factor Analyzers
#'
#' @description An alternative implementation of AMFA algorithm \insertCite{WangWan-Lun2020Alom}{autoMFA}. The number of factors, \emph{q}, is estimated during the fitting process of each MFA model.
#' Instead of employing a grid search over \emph{g} like the \code{AMFA} method, this method starts with a \emph{1} component MFA model and splits components according to their multivariate kurtosis. This uses the same approach as \code{amofa} \insertCite{kaya2015adaptive}{autoMFA}.
#' Once a component has been selected for splitting, the new components are initialised in the same manner as \code{vbmfa} \insertCite{ghahramani2000variational}{autoMFA}.
#' It keeps trying to split components until all components have had \code{numTries} splits attempted with no decrease in BIC, after which the current model is returned.
#' @param Y An \emph{n} by \emph{p} data matrix, where \emph{n} is the number of observations and \emph{p} is the number of dimensions of the data.
#' @param numTries The number of attempts that should be made to split each component. 
#' @param eta The smallest possible entry in any of the error matrices \emph{D_i} \insertCite{Jian-HuaZhao2008FMEf}{autoMFA}.
#' @param itmax The maximum number of ECM iterations allowed for the estimation of each MFA model.
#' @param tol The ECM algorithm terminates if the measure of convergence falls below this value.
#' @param conv_measure The convergence criterion of the ECM algorithm. The default \code{'diff'} stops the ECM iterations if |l^{(k+1)} - l^{(k)}| < \code{tol} where l^{(k)} is the log-likelihood at the \emph{k}th ECM iteration. If \code{'ratio'}, then the convergence of the ECM iterations is measured using |(l^{(k+1)} - l^{(k)})/l^{(k+1)}|.
#' @param nkmeans The number of times the \emph{k}-means algorithm will be used to initialise the (single component) starting models.
#' @param nrandom The number of randomly initialised (single component) starting models.
#' @param varimax Boolean indicating whether the output factor loading matrices should be constrained
#' using varimax rotation or not. 
#' 
#' @return A list containing the following elements:
#' \itemize{
#' \item{\code{model}:}{ A list specifying the final MFA model. This contains: \itemize{
#'            \item{\code{B}:}{ A \emph{p} by \emph{p} by \emph{q} array containing the factor loading matrices for each component.}
#'             \item{\code{D}:}{ A \emph{p} by \emph{p} by \emph{g} array of error variance matrices.}
#'            \item{\code{mu}:}{  A \emph{p} by \emph{g} array containing the mean of each cluster.}
#'           \item{\code{pivec}:}{ A 1 by \emph{g} vector containing the mixing
#'             proportions for each FA in the mixture.}
#'           \item{\code{numFactors}:}{ A 1 by \emph{g} vector containing the number of factors for each FA.}}
#'           }
#'\item{\code{clustering}:}{ A list specifying the clustering produced by the final model. This contains: \itemize{
#'             \item{\code{responsibilities}:}{ A \emph{n} by \emph{g} matrix containing the probability
#'               that each point belongs to each FA in the mixture.}
#'             \item{\code{allocations}:}{ A \emph{n} by 1 matrix containing which
#'               FA in the mixture each point is assigned to based on the responsibilities.}}}
#'\item{\code{diagnostics}:}{ A list containing various pieces of information related to the fitting process of the algorithm. This contains: \itemize{
#'             \item{\code{bic}:}{ The BIC of the final model.}
#'             \item{\code{logL}:}{ The log-likelihood of the final model.}
#'             \item{\code{totalTime}:}{ The total time taken to fit the final model.}}}
#'}
#'
#'@seealso \code{\link{amofa}} \code{\link{vbmfa}}  
#'
#' @references 
#' \insertRef{WangWan-Lun2020Alom}{autoMFA}
#' 
#' \insertRef{kaya2015adaptive}{autoMFA}
#' 
#' \insertRef{ghahramani2000variational}{autoMFA}
#' 
#' \insertRef{Jian-HuaZhao2008FMEf}{autoMFA}
#' @export
#'
#' @examples
#' RNGversion('4.0.3'); set.seed(3) 
#' MFA.fit <- AMFA.inc(autoMFA::MFA_testdata, itmax = 1, numTries = 0)

AMFA.inc <- function(Y,numTries = 2, eta = 0.5e-2 ,itmax = 500,
                    tol = 1e-05, conv_measure = "diff", 
                    nkmeans = 1, nrandom = 1, varimax = FALSE){
  
  stopifnot("varimax should be either TRUE or FALSE"={varimax %in% c(TRUE,FALSE)})
  stopifnot("Y should be an n by p numeric matrix"={is.numeric(Y)&is.matrix(Y)})
  stopifnot("numTries should be numeric"={is.numeric(numTries)})
  stopifnot("numTries should be an integer greater than or equal to 0"={(numTries%%1==0) & (numTries >=0)})
  stopifnot("itmax should be numeric"={is.numeric(itmax)})
  stopifnot("itmax should be an integer greater than or equal to 1"={(itmax%%1==0) & (itmax >=1)})
  stopifnot("nkmeans should be numeric"={is.numeric(nkmeans)})
  stopifnot("nkmeans should be an integer greater than or equal to 0"={(nkmeans%%1==0) & (nkmeans >=0)})
  stopifnot("nrandom should be numeric"={is.numeric(nrandom)})
  stopifnot("nrandom should be an integer greater than or equal to 0"={(nrandom%%1==0) & (nrandom >=0)})
  stopifnot("eta should be a numeric" = {is.numeric(eta)})
  stopifnot("eta should be greater than zero" = {eta > 0})
  stopifnot("tol should be numeric" = {is.numeric(tol)})
  stopifnot("tol should be greater than zero" = {tol > 0})
  stopifnot("conv_measure should be either `diff` or `ratio`"={conv_measure %in% c("diff","ratio")})
  
  
  start.time <- Sys.time()
  p <- ncol(Y); n <- nrow(Y); 
  
  cat(sprintf('\rFitting an initial 1 component model using AMFA\n'))
  #Fitting a model so that we can try to add a component.
  model.init <- AMFA(Y, gmin = 1, gmax = 1, eta = eta ,itmax = itmax,
                     nkmeans = nkmeans, nrandom = nrandom, tol = tol,
                    conv_measure = conv_measure)
  cat(sprintf('\rInitial model done. Now trying to add components.\n'))
  
  bestmodel <- model.init #Start with this single component model.
  
  accept = 1
  while(accept==1){
    if(numTries==0){break}
    #Before we try to split an existing component, store the existing responsibility
    #matrix
    tau_before_split <- bestmodel$clustering$responsibilities 
    split.index <- 1:ncol(tau_before_split)
    
    #Check whether any of the components should be allowed to split in two.
    #Using the same criterion as in AMoFA
    best.q <- dim(bestmodel$model$B)[2]
    split.allowed <- (bestmodel$model$pivec*n > (p*(best.q+2)+2))  
    #Calculating the Mardia kurtosis metric for each component
    kurt <- abs(kurtosis(Y,bestmodel$model$mu,bestmodel$model$B,bestmodel$model$D,tau_before_split))
    #Try to split components in order of decreasing Mardia metric
    kurt <- kurt[split.allowed]
    split.index <- split.index[split.allowed]
    split.order.temp <- order(kurt,decreasing = T)
    splitOrder <- split.index[split.order.temp]
    if(length(splitOrder) == 0){
      #No component can be split because they all have insufficient support
      break
    }
    
    for (k in 1:numTries){
      accept = 0
      #Otherwise attempt a split
      for(i in splitOrder){
        
        parent = i
        cluster = Rfast::rowMaxs(tau_before_split)
        alloc <- (cluster == parent)
        
        #Add a component using dobirth
        init.split <- dobirth.amfa(Y,bestmodel$model$mu,bestmodel$model$B,
                                   bestmodel$model$D,
                                   bestmodel$clustering$responsibilities,parent)
        B <- init.split$B; D <- init.split$D; mu <- init.split$mu; tau <- init.split$tau
        child <- dim(B)[3]; g <- dim(B)[3]
        localB <- abind::abind(matrix(B[,,parent],nrow = p), matrix(B[,,child],nrow = p),along = 3)
        localD <- abind::abind(D[,,parent], D[,,child],along = 3)
        localmu <- abind::abind(mu[,parent], mu[,child],along = 2)
        localtau <- tau[( cluster == parent), c(parent,child)]
        localpi <- colSums(localtau)/nrow(localtau)
        localinit <- list(g = 2, q = best.q, pivec = localpi, B = localB, mu = localmu, 
                          D = localD, sigma_type = "unique", D_type = "unique"); class(localinit) = "mfa"
        localdata <- Y[alloc,]
 
        failed = FALSE
        model.split <- MFA.ecm.withInit(localdata, g = 2, q = best.q, eta = eta ,itmax = itmax, tol = tol,
                                        init_para = localinit, conv_measure = conv_measure, 
                                        warn_messages = TRUE)
        if(class(model.split)[1] != "list"){
          failed = TRUE
          model.split.full <- vector(mode = "list")
          model.split.full$diagnostics$bic <- Inf
        }
        
        if(!failed){
          tempB <- B;  tempB[,,parent] <- model.split$model$B[,,1];  tempB[,,child] <- model.split$model$B[,,2]; 
          tempD <- D; tempD[,,parent] <- model.split$model$D[,,1]; tempD[,,child] <- model.split$model$D[,,2]; 
          tau[alloc, parent] <- tau_before_split[alloc,parent]*model.split$clustering$responsibilities[,1]
          tau[alloc, child] <- tau_before_split[alloc,parent]*model.split$clustering$responsibilities[,2]
          tempmu <- mu; tempmu[,parent] <- model.split$model$mu[,1]; tempmu[,child] <- model.split$model$mu[,2]
          tempg <- dim(tempB)[3];
          
          #Run the ECM alg here to check if overall BIC increases --- 
          init.split.full <- list(g = tempg, q = best.q, pivec = colSums(tau)/n, B = tempB, mu = tempmu, 
                                  D = tempD, sigma_type = "unique", D_type = "unique"); class(init.split.full) = "mfa"
          model.split.full <- MFA.ecm.withInit(Y, g = tempg, q = dim(bestmodel$model$B)[2], q_update = TRUE,
                                               eta = 0.5e-2 ,itmax = 500, tol = 1e-05,
                                               init_para  = init.split.full, conv_measure = "diff", 
                                               warn_messages = TRUE)}
        if(model.split.full$diagnostics$bic < bestmodel$diagnostics$bic){
          bestmodel <- model.split.full; accept = 1; break
        }
        
      }
      if(accept==1){break}
    }
    progress(bestmodel)
  }
  cat(sprintf("\n"))
  cat(sprintf("Model fitted."))
  end.time <- Sys.time()
  bestmodel$diagnostics$totalTime <- as.numeric(difftime(end.time, start.time, units = 'secs'))
  
  if(varimax){
    p <- dim(bestmodel$model$B)[1]
    no.comp <- dim(bestmodel$model$B)[3]
    for(i in 1:no.comp){
      bestmodel$model$B[,,i] <- matrix(bestmodel$model$B[,,i], nrow = p) %*% stats::varimax(matrix(bestmodel$model$B[,,i], nrow = p))$rotmat
    }
  }
  
  
  return(bestmodel)
}