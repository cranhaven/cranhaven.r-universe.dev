#'@title
#'est.changepoints

#'@description
#'This function estimates multiple change points using marginal likelihood method
#'proposed by Du, Kao and Kou (2015), which we would denoted as DKK2015 afterward.

#'@details
#'See Manual.pdf in "data" folder.

#'@param
#'data.x   Observed data in vector or matrix form. When the data is in matrix form, each column should represent a single observation.
#'@param
#'model    The specified distributional assumption. Currently we have implemented two arguments: "normal" (data follows one dimensional Normal distribution with unknown mean and variance) and "poisson" (data follows Poisson distribution with unknown intensity). A third argument "user" is also accepted, given that the prior and the log marginal likelihood function are specified in the parameter prior and logMD.
#'@param
#'prior    The pre-specified prior parameters in consistent with the form used in \code{logMD}.\cr For the proposed priors in DKK2015, use the corresponding \code{prior} function provided.
#'@param
#'max.segs (Opt.) The maximum number of segments allowed, which is the value M in DKK2015. Must be a positive integer greater then 1. If missing, the function would process using the algorihtm by Jackson et al.(2005).
#'@param
#'logH     (Opt.) A Boolean algebra determine whether to report the log H matrix in DKK2015. Default is FALSE.
#'@param
#'logMD    (Opt.) The log marginal likelihood function (which is the log of D function in DKK2015). This parameter is only used when the model="user". The function must be in the form of \code{logMD(data.x, prior)}. \cr


#'@return
#'If \code{logH} is FALSE, the function returns the set of estimated change-points by the index of the data, where each index is the end point of a segment. If the result is no change-points, the function returns \code{NULL}.
#'If \code{logH} is TRUE, then the function returns a list with three components: \code{changePTs} is the set of estimated change-points, \code{log.H} is the log value for the H matrix used in the algorithm, where \eqn{log.H(m,i) = log H(x1, x2, ..., xi | m)}, and \code{max.j} records the \eqn{j} that maximizes the marginal likelihood in each step. See the manual in data folder for more details.

#'@references
#'Chao Du, Chu-Lan Michael Kao and S. C. Kou (2015), "Stepwise Signal Extraction via Marginal Likelihood." Forthcoming in Journal of American Statistical Association.

#'@examples
#'library(StepSignalMargiLike)
#'
#'n <- 5
#'data.x <- rnorm(n, 1, 1)
#'data.x <- c(data.x, rnorm(n, 10,1))
#'data.x <- c(data.x, rnorm(n, 2,1))
#'data.x <- c(data.x, rnorm(n, 10,1))
#'data.x <- c(data.x, rnorm(n, 1,1))
#'
#'prior <- prior.norm.A(data.x)
#'max.segs <- 10
#'
#'est.changepoints(data.x=data.x, model="normal", prior=prior)
#'est.changepoints(data.x=data.x, model="normal", prior=prior, max.segs=max.segs)
#'est.changepoints(data.x=data.x, model="normal", prior=prior, max.segs=max.segs,logH=TRUE)

#'@export
est.changepoints <- function(data.x, model, prior, max.segs,logH,logMD)
{
  # The Notations are in consistent with Du, Kao and Kou (2013)

  if(is.vector(data.x)){
     Dim<-1
     n<-length(data.x)
  }else{
     Dim<-nrow(data.x)
     n <- ncol(data.x)
  }



  if (model=="normal" && Dim==1){

		if (missing(prior)){
			prior=prior.norm.A(data.x)
		}

		if (missing(max.segs)){

		  RCpp.output <- ChangePointAnalyzeNormUnRes(data.x, n, prior)
		  log.H<-RCpp.output[1,]
		  max.j<-RCpp.output[2,]

		}else{

    RCpp.output <- ChangePointAnalyzeNorm(data.x, n, max.segs, prior)
		log.H<-RCpp.output[1:max.segs,]
		max.j<-RCpp.output[(max.segs+1):(2*max.segs),]

		}

  } else if (model=="poisson" && Dim==1){

		if (missing(prior)){
			prior=prior.pois(data.x)
		}

		if (missing(max.segs)){
		RCpp.output <- ChangePointAnalyzePoissUnRes(data.x, n, prior)
		log.H<-RCpp.output[1,]
		max.j<-RCpp.output[2,]

		}else{
		RCpp.output <- ChangePointAnalyzePoiss(data.x, n, max.segs, prior)
		log.H<-RCpp.output[1:max.segs,]
		max.j<-RCpp.output[(max.segs+1):(2*max.segs),]
        }

  } else if (model=="user" && missing(prior)==0 && missing(logMD)==0){

		if(is.vector(data.x)){
		   data.x<-matrix(data.x,1)
		}

        if(missing(max.segs)==0){

          # log.H[m,i] := logH(data.x[1:i] | m)
          log.H <- matrix(rep(0, n*max.segs), max.segs, n)
          # max.j[m,i] = m <=> H(data.x[1:i]|m) = H(data.x[1:j]|m-1)*D(data.x[j+1:i])
          max.j <- matrix(rep(0, n*max.segs), max.segs, n)

			  # H(data[1]|1) = D(data.x[1])
			  log.H[1,1] <- logMD(data.x[,1], prior)
			  # function use to compute the maximum
			  m.loop.func <- function(A,B,k,l)
			  {
				temp.HD <- A[(k-1), (k-1):(l-1)] + B[k:l]
				max.index <- which.max(temp.HD)
				c(temp.HD[max.index], max.index+k-2)
			  }
			  # Recursively compute H for i>2
			  for (i in 2:n)
			  {
				# log.D[j] := logD(xj, ..., xi)
				log.D <- do.call("c",lapply(1:i,function(j){ logMD(data.x[,j:i],prior) }))
				# H(x1,...,xi|1) = D(data[1:i])
				log.H[1,i] <- log.D[1]
				# Recursively compute H for m>2
				max.m.given.i <- min(i, max.segs)
				max.temp <- matrix(do.call("c",lapply(2:max.m.given.i,function(m){m.loop.func(log.H, log.D, m, i) })),2)
				log.H[2:max.m.given.i,i] <- max.temp[1,]
				max.j[2:max.m.given.i,i] <- max.temp[2,]
		      }

        }else{
        return("ERROR: This method has not been implemented or the key arguments are missing.")
        }
   } else {
        return("ERROR: This method has not been implemented or the key arguments are missing.")
   }


  # Extract Change Points

 if(missing(max.segs)!=1){
	  max.m <- which.max(log.H[,n])
	  temp.j <- max.j[max.m,n]
	  changePTs <- c()
	  while(temp.j > 0)
	  {
		changePTs <- c(temp.j, changePTs)
		max.m <- max.m - 1
		temp.j <- max.j[max.m, temp.j]
	  }
  }else{
      max.m<-n
  	  temp.j <- max.j[n]
	  changePTs <- c()
	  while(temp.j > 0)
	  {
		changePTs <- c(temp.j, changePTs)
		max.m <- temp.j
		temp.j <- max.j[max.m]
	  }
  }



  if(missing(logH))
  {
    return(changePTs)
  }
  else
  {
    temp.dataframe <- list(changePTs=changePTs, log.H=log.H, max.j=max.j)
    return(temp.dataframe)
  }
  # Clear Up
  variable.list <- c("log.H","max.j")
  rm(variable.list)
}
