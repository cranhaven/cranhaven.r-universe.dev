#' Relaxed Guided Random Search
#'
#'\code{rgrs} Produce a point configuration given the edge lengths of the desired minimum spanning tree
#'
#'@details 
#'
#'In 2-dimensions, when a new point is proposed, the position for the new point is determined by:
#'
#'x <- x0 + r*sin(theta)
#'y <- y0 + r*cos(theta)
#'
#'where (x0,y0) is the base point, and r is the minimum spanning tree distance. 
#'theta is generated from a uniform distribution on (-pi,pi). By specifying the theta argument, the
#'proposed theta is restricted, and is then generated from Uniform(theta1,theta2) or Uniform(-theta2,-theta1) with equal probability.
#'This restriction allows the user to introduce striation into their point configuration.
#'
#'@param edges A numeric vector containing the desired edge lengths of the minimum spanning tree. If n is specified, must be NULL.
#'@param d the dimension of the resulting configuration.
#'@param n the desired number of edge lengths to simulate. If edges is specified, must be set to NULL.
#'@param theta Angle restriction during point proposal of the form (theta1,theta2,p), where p represents the probability of confining the proposal to [theta1,theta2]. Only used for d=2, otherwise NULL. See details for more in depth explanation.
#'@param outlying One of "L", "M", or "H", specifying if the simulated edge lengths should have a Low, Medium, or High outlying scagnostic value.
#'@param skew One of "L", "M", or "H", specifying if the simulated edge lengths should have a Low, Medium, or High skew scagnostic value.
#'@param stringy One of "L", "M", or "H", specifying if the simulated edge lengths should have a Low, Medium, or High stringy scagnostic value. A numeric scalar specifying a value of stringy is also accepted.
#'
#'@return An nxd matrix containing the d-dimensional locations of the points.
#'
#'@examples 
#' 
#'\donttest{
#' # An example where edge lengths are supplied
#' EL <- runif(100,0,1)
#' rgrs(edges = EL, d = 2)
#' rgrs(edges = EL, d = 3) 
#'  
#' # An Example where edge lengths are simulated internally
#' rgrs(d=2, n=100)
#' rgrs(d=3, n=100)
#' rgrs(d=2, n=100, outlying="H")
#' rgrs(d=2, n=100, skew = "M")
#' rgrs(d=2, n=100, stringy = "H")
#'  
#' # An Example making use of theta
#' rgrs(d=2, n=100, theta=c(pi/4,pi/3,.5))
#'
#'}
#'@export
#'@importFrom stats cmdscale dist optim rexp rgamma rnorm runif

rgrs <- function(edges = NULL,
                  d,           
                  n = NULL,    
                  theta=NULL, 
                  outlying="N", 
                  skew="N", 
                  stringy = "N"){
  
  #minimum spanning tree tolerance
  tol <- 0.0001
  
  ################# INPUT CHECKING ####################
  
  #Check that d is numeric and >0
  
  if(!is.numeric(d)){
    stop("d must be a numeric integer")
  }else if(length(d) != 1){
    stop("d must be a numeric integer")
  }else if(!(d %% 2 == 1 | d %% 2 == 0)){
    stop("d must an integer")
  } 
  
  #If n is provided, do some error checking
  if(!is.null(n)){
    if(!is.numeric(n)){
      stop("n must be a numeric integer")
    }else if(length(n) != 1){
      stop("n must be a numeric integer")
    }else if(!(n %% 2 == 1 | n %% 2 == 0)){
      stop("n must an integer")
    }
  }
  
  #Check that Edge Lengths are numeric and all positive
  
  #edges must be numeric or NULL
  if(!(is.null(edges) | is.numeric(edges))){
    stop("edges must be NULL or a numeric vector")
  }
  
  #Edge Lengths and n cannot both be specified, they also both can't be NULL
  if(is.null(edges) && is.null(n)){
    stop("You must supply a vector of edges, or the number of edges desired")
  }else if(!is.null(edges) && !is.null(n)){
    stop("You may not supply both edges and n")
  }
  
  #Check if d > 2, theta = NULL
  if(d > 2 && !is.null(theta)){
    stop("theta input is only usable for d=2")
  }
  
  #Check theta Input if it's not NULL
  if(!is.null(theta)){
    if(!is.numeric(theta)){
      stop("theta input must be numeric in the form (min angle, max angle, probability)")
    }else if(length(theta) != 3){
      stop("theta input must be numeric in the form (min angle, max angle, probability)")
    }else  if(theta[1] >= theta[2]){
      stop("you must provide angles such that theta[1] < theta[2]")
    }else if(theta[1] < 0 | theta[2] > pi){
      stop("theta input must be in [0,pi]")
    }else if(theta[3] < 0 | theta[3] > 1){
      stop("theta[3] must represent a probability in [0,1]")
    }
  }
  
  #Check outlying, skew, stringy
  
  #Only Stringy can be provided if edges are provided
  if(!is.null(edges) & (outlying != "N" | skew != "N")){
    stop("outlying/skew cannot be provided with edges")
  }
  
  #They must be equal to N,L,M,H
  if(outlying != "N" && outlying != "L" && outlying != "M" && outlying != "H"){
    stop("outlying must be one of N, L, M, or H")
  }
  
  if(skew != "N" && skew != "L" && skew != "M" && skew != "H"){
    stop("skew must be one of N, L, M, or H")
  }
  
  if(stringy != "N" && stringy != "L" && stringy != "M" && stringy != "H"){
    stop("stringy must be one of N, L, M, or H")
  }
  
  #Can only have one not equal to N
  if(sum(c(stringy != "N",  skew != "N", outlying != "N")) > 1){
    stop("Only one of outlying, skew, and stringy may be used. The others must be set to N")
  }
  
  #####################################################
  ################ MAIN FUNCTION START ################
  #####################################################
  
  # Generate Edge Lengths to give an approximate value of skew
  if(skew == "L"){         #Generate a distribution with LOW skew
    Roll <- runif(1,0,.5)
    
    if(Roll < .5){
      lambda <- runif(1,.5,2)
      edges <- -rexp(n,lambda)
      edges <- edges - min(edges) + .1
    }else{
      lambda <- runif(1,.5,2)
      edges <- -rexp(n,lambda)^2
      edges <- edges - min(edges) + .1
    }
  }else if(skew == "M"){   #Generate a distribution with MODERATE skew
    Roll <- runif(1,0,1)
    if(Roll < .5){
      edges <- runif(n,0,1)
    }else{
      alpha <- runif(1,2,4)
      beta <- runif(1,2,4)
      edges <- rgamma(n,alpha,beta)
    }
  }else if(skew == "H"){   # Generate a distribution with HIGH skew
    Roll <- runif(1,0,1)
    if(Roll < .5){
      lambda <- runif(1,1,4)
      edges <- rexp(n,lambda)
    }else{
      Power <- runif(1,1,2)
      lambda <- runif(1,1,4)
      edges <- rexp(n,lambda)^Power
      edges <- edges/max(edges)
    }
  }
  
  #Generate Edge lengths with an approximate value of outlying
  
  if(outlying == "L"){         #Generate a sitribution with LOW outlying
    edges <- runif(n,0,1)
  }else if(outlying == "M"){   #Generate a distribution with MODERATE outlying
    sigma <- runif(1,1,3)
    edges <- (rnorm(n,0,sigma))^2
    edges <- edges/max(edges)
  }else if(outlying == "H"){   # Generate a distribution with HIGH outlying
    lambda <- runif(1,.5,2)
    Power <- runif(1,1,3)
    edges <- rexp(n,lambda)^3
    edges <- edges/max(edges)
  }
  
  if(outlying == "N" & skew == "N" & is.null(edges)){
    edges <- runif(n,0,1)
  }
  
  if(stringy == "N"){
    stringy <- NULL
  }else if(stringy == "L"){
    stringy <- runif(1,0,.33)
  }else if(stringy == "M"){
    stringy <- runif(1,.33,.66)
  }else if(stringy == "H"){
    stringy <- runif(1,.66,1)
  }
  
  #Check that Edge Lengths are all positive
  
  if(any(edges < 0)){
    stop('Edge Lengths must be strictly non-negative')
  }
  
  #Initialize
  
  IndexMatrix <- matrix(rep(0, (length(edges)+1)^2), nrow=(length(edges)+1))
  
  IndexMatrix[1,2] <- 1
  IndexMatrix[2,1] <- 1
  
  #Initialize with Point
  
  Xn <- matrix(runif(d,0,1),nrow=1)
  
  #Add the First point
  Xn <- rbind(Xn,rgrsNewPoint(Xn[1,],edges[1],d,theta))
  ReducedEdgeLengths <- edges[-1]
  
  for(i in 1:length(ReducedEdgeLengths)){
    
    Accept <- FALSE
    
    while(Accept == FALSE){
    
      if(is.null(stringy)){
        RandomPosition <- sample(c(1:length(Xn[,1])),1,replace=FALSE)
        XnBase <- Xn[RandomPosition,]
      }else{
        
        NewStringy <- getStringy(IndexMatrix)
        CurrentStringy <- NewStringy[[1]]
        RS <- NewStringy[[2]]
        
        if(CurrentStringy < stringy){
          Samplers <- which(RS == 1)
          RandomPosition <- sample(Samplers,1,replace=FALSE)
          
          XnBase <- Xn[RandomPosition,]
        }else{
          RandomPosition <- sample(c(1:length(Xn[,1])),1,replace=FALSE)
          XnBase <- Xn[RandomPosition,]
        }  
      }
      
      #Propose a new point in d-dimensions
      NewXn <- rgrsNewPoint(XnBase,edges[i+1],d,theta)
      
      #Check
      XnCheck <- rbind(Xn,NewXn)
      Accept <- rgrsCheckMST(XnCheck,edges,i,tol)
      
      if(Accept){
        Xn <- rbind(Xn,NewXn)
        IndexMatrix[RandomPosition,i+2] <- 1
        IndexMatrix[i+2,RandomPosition] <- 1
      }
    }
  }
  return(Xn)
}