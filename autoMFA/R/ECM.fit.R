#' ECM-Based MFA Estimation
#'
#' @description An implementation of an ECM algorithm for the MFA model which does not condition on the factors being known \insertCite{Jian-HuaZhao2008FMEf}{autoMFA}. 
#' Performs a grid search from \code{gmin} to \code{gmax}, and \code{qmin} to \code{qmax}, respectively. The best combination of \emph{g} and \emph{q} is chosen to be the model with the minimum BIC.
#' 
#' @param Y An \emph{n} by \emph{p} data matrix, where \emph{n} is the number of observations and \emph{p} is the number of dimensions of the data.
#' @param gmin The smallest number of components for which an MFA model will be fitted.
#' @param gmax The largest number of components for which an MFA model will be fitted.
#' @param qmin The smallest number of factors with which an MFA model will be fitted.
#' @param qmax The largest number of factors with which an MFA model will be fitted. Must obey the Ledermann bound.
#' @param eta The smallest possible entry in any of the error matrices \emph{D_i} \insertCite{Jian-HuaZhao2008FMEf}{autoMFA}.
#' @param itmax The maximum number of ECM iterations allowed for the estimation of each MFA model.
#' @param nkmeans The number of times the \emph{k}-means algorithm will be used to initialise models for each combination of \emph{g} and \emph{q}.
#' @param nrandom The number of randomly initialised models that will be used for each combination of g and q.
#' @param tol The ECM algorithm terminates if the measure of convergence falls below this value.
#' @param conv_measure The convergence criterion of the ECM algorithm. The default \code{'diff'} stops the ECM iterations if |l^{(k+1)} - l^{(k)}| < \code{tol} where l^{(k)} is the log-likelihood at the \emph{k}th ECM iteration. If \code{'ratio'}, then the convergence of the ECM iterations is measured using |(l^{(k+1)} - l^{(k)})/l^{(k+1)}|.
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
#'             \item{\code{times}:}{ A data frame containing the amount of time taken to fit each MFA model.}
#'             \item{\code{totalTime}:}{ The total time taken to fit the final model.}}}
#'}
#'
#' @export
#' @references \insertRef{Jian-HuaZhao2008FMEf}{autoMFA}
#' @examples
#' RNGversion('4.0.3'); set.seed(3)
#' MFA.fit <- MFA_ECM(autoMFA::MFA_testdata,3,3)

MFA_ECM <- function(Y, gmin = 1, gmax =10, qmin = 1, qmax = NULL, eta = 0.5e-2 ,itmax = 500, nkmeans = 5, nrandom = 5, tol = 1e-05,
                 conv_measure = "diff", varimax = FALSE){
  #The structure of this function is based on the function mfa from package EMMIXmfa. 
  #See package EMMIXmfa for more details. 
  start.time <- Sys.time()
  
  #Checking inputs 
  stopifnot("varimax should be either TRUE or FALSE"={varimax %in% c(TRUE,FALSE)})
  stopifnot("Y should be an n by p numeric matrix"={is.numeric(Y)&is.matrix(Y)})
  n <- nrow(Y); p <- ncol(Y); ledermann <- floor(p + (1-sqrt(1+8*p))/2)
  if(is.null(qmax)){
    qmax <- ledermann}
  stopifnot("gmin should be numeric"={is.numeric(gmin)})
  stopifnot("gmin should be an integer greater than or equal to 1"={(gmin%%1==0) & (gmin >=1)})
  stopifnot("gmax should be numeric"={is.numeric(gmax)})
  stopifnot("gmax should be an integer greater than or equal to 1"={(gmax%%1==0) & (gmax >=1)})
  stopifnot("gmin should be less than or equal to gmax"={gmin <= gmax})
  stopifnot("qmin should be numeric"={is.numeric(qmin)})
  stopifnot("qmin should be an integer greater than or equal to 1"={(qmin%%1==0) & (qmin >=1)})
  stopifnot("qmax should be numeric"={is.numeric(qmax)})
  stopifnot("qmax should be an integer greater than or equal to 1"={(qmax%%1==0) & (qmax >=1)})
  # Using stop here because I want it to print the ledermann bound in the error message, couldn't get this
  #to work with stopifnot.
  if(!((qmin <= qmax) & (qmax <= ledermann))){
    led_error <- paste("qmin and qmax should satisfy 1<=qmin<=qmax<=ledermann, where ledermann is the maximum number of factors given by the ledermann bound for p-dimensional data. In this case it is ",ledermann,sep="")
    stop(led_error)
  }
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
  
  
  
  
  init_clust = NULL; init_para = NULL
  
  if(qmax < qmin){stop(paste("Invalid choice of q. Need qmin <=",qmax," for identifiability."))}

  warn_msg <- NULL
  init_clust <- NULL;

  BIC.best <- Inf
  model <- NULL

  pb <- txtProgressBar(style = 3, char = ".")
  prog <- 0

  times <- data.frame(g = numeric(), time = numeric(), status = character()) #Time taken for each model
  ind = 1 #For tracking the number of models which have been fitted

  for(g in gmin:gmax){
    for(q in qmin:qmax){
      initial_partitions <- start_clust(Y, g, init_clust, nkmeans,
                                      nrandom)
      maxinit <- ncol(initial_partitions)
      if (is.null(maxinit))
        maxinit <- 1

      if (g == gmin){
        tinit <- maxinit*(gmax-gmin+1)*(qmax-qmin+1)
      }

      for (ii in 1:maxinit) {
        if (min(table(initial_partitions[, ii])) == 1) {
          when <- paste("At start", ii)
          what <- "Initial partitioning has a single sample cluster."
          warn_msg <- rbind(warn_msg, cbind(when, what))
          next
        }
        init_model_para <- try(init_est_para.mfa(Y, g, q,
                                               initial_partitions[, ii], "unique", "unique"),
                             silent = TRUE)
        if (class(init_model_para) == "try-error") {
          when <- paste("At start", ii)
          what <- "Failed to estimate initial parameters"
          warn_msg <- rbind(warn_msg, cbind(when, what))
          next
        }
        loop.start <- Sys.time()

        estd_model <- est.mfa.ecm(init_para = init_model_para,
                            Y = Y, g = g, q , itmax = itmax, tol = tol,
                            conv_measure = conv_measure, eta = eta)
        loop.end <- Sys.time()

        if ((class(estd_model) == "mfa")) {
          estd_q <- q
          d_model <- (g - 1) + 2*g * p + g * (p * estd_q - estd_q * (estd_q - 1)/2)
          estd_model_BIC <- -2 * estd_model$logL + d_model * log(n)
          if(estd_model_BIC < BIC.best){model <- estd_model; BIC.best <- estd_model_BIC}
          times[ind,] <- c(g,as.numeric(difftime(loop.end,loop.start,units = "secs")),"OK")
          ind = ind + 1
        }
        if(class(estd_model) == "warn" || class(estd_model) == "error") {
          when <- paste("At start", ii)
          what <- estd_model
          warn_msg <- rbind(warn_msg, cbind(when, what))
          times[ind,] <- c(g,as.numeric(difftime(loop.end,loop.start,units = "secs")),class(estd_model))
          ind = ind + 1
        }
        prog <- prog + 1/tinit
        setTxtProgressBar(pb, prog)
      }
    }
  }
  setTxtProgressBar(pb, 1)
  writeLines('\n')
  if(gmax != gmin){
    writeLines("\n")
    if(model$g == gmax){warning("The final model uses the maximum number of components. Consider increasing gmax.")}
    if((model$g == gmin) && (gmin != 1)){warning("The final model uses the minimum number of components. Consider lowering gmin.")}
  }

  end.time <- Sys.time()
  out <- vector(mode = 'list')
  model$numFactors <- matrix(rep(model$q,model$g),nrow = 1)
  out$model <- model[-c(1,2,7,8,9,10)]
  out$diagnostics$logL <- model$logL
  out$diagnostics$bic <- BIC.best
  out$diagnostics$totalTime <- as.numeric(difftime(end.time, start.time, units = 'secs'))
  out$diagnostics$times <- times
  out$diagnostics$warnings <- warn_msg
  out$clustering$responsibilities <- model$tau
  out$clustering$allocations <- matrix(Rfast::rowMaxs(model$tau),nrow = 1)
  
  if(varimax){
    p <- dim(out$model$B)[1]
    no.comp <- dim(out$model$B)[3]
    for(i in 1:no.comp){
      out$model$B[,,i] <- matrix(out$model$B[,,i], nrow = p) %*% stats::varimax(matrix(out$model$B[,,i], nrow = p))$rotmat
    }
  }
  
  return(out)

}
