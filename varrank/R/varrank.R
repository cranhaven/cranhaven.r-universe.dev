###############################################################################
## varrank.R ---
## Author          : Gilles Kratzer
## Last modified   : 07/09/2017
##                 : 27/11/2017 greedy search removed
##                 : 15/03/2018 code cleaning
###############################################################################

varrank <- function(data.df = NULL, variable.important = NULL, method = c("battiti", "kwak", "peng", "estevez"), algorithm=c("forward","backward"), scheme=c("mid", "miq"), discretization.method = NULL, ratio=NULL,n.var=NULL, verbose=TRUE){

  ##Tests of common sense

  if(!exists("method"))stop("A method has to be provided")
  if(!exists("algorithm"))stop("An algorithm has to be provided")
  #if(length(scheme)!=1)stop("A scheme have to be provided")
  if(is.null(discretization.method))stop("A discretization method has to be provided")

  method <- tolower(method)
  algorithm <- tolower(algorithm)
  scheme <- tolower(scheme)
  # Validation of discretization.method is deferred to discretization
  #  if(is.character(discretization.method)) {
  #    discretization.method <- tolower(discretization.method)


  method <- c("battiti", "kwak", "peng", "estevez")[pmatch(method,c("battiti", "kwak", "peng", "estevez"))]
  if ( (is.na(method)) || (length(method)>1) ){  method <- "peng" ; warning("'method' not recognised; set to 'peng'")}

  algorithm <- c("forward","backward")[pmatch(algorithm,c("forward","backward"))]

  if ( (is.na(algorithm)) || (length(algorithm)>1) ){ algorithm <- "forward"; warning("'algorithm' not recognised; set to 'forward'")}
  scheme <- c("mid", "miq")[pmatch( tolower(scheme), c("mid", "miq"))]
  if ( (is.na(scheme)) || (length(scheme)>1) ){ scheme <- "mid"; warning("'scheme' not recognised; set to 'mid'")}

  #  discretization.method <- c("fd","doane","cencov","sturges","rice","scott","kmeans","terrell-scott")[pmatch(discretization.method,c("fd","doane","cencov","sturges","rice","scott","kmeans","terrell-scott"))]

  if(length(variable.important)<1)stop("A least one variable of importance should be given")
  if(dim(data.df)[2]<length(variable.important))stop("Misspecification of number of the data.frame and the variables of importance")
  if(dim(data.df)[1]<2)stop("Not enough observations to compute information theory metrics")
  #if(!(method %in% c("battiti", "kwak", "peng", "estevez")) & algorithm %in% c("forward")){method <- "peng" ; warning("Method not recognised; method assigned to peng")}
  #if(is.null(algorithm) & length(n.var)==1){algorithm <- "forward"; warning("Algorithm not recognised; algorithm assigned to forward")}

  #if(!(algorithm %in% c("forward","backward"))){stop("Algorithm not recognised")}

  if(is.numeric(n.var)) if(n.var>(dim(data.df)[2]-length(variable.important))) warning("n.var too large; assigned to max possible value")

if(!is.null(n.var)){
  if(length(n.var)>1){stop("Wrong format of n.var. It should be c(number of steps forward, number of steps backward, optionally number of variable to rank)")}
}
  ##end of tests

  ##forward & backward scheme

  if(algorithm=="forward"){tmp <- varrank.forward(data.df = data.df ,variable.important = variable.important,method = method,discretization.method = discretization.method,scheme=scheme,ratio = ratio,n.var=n.var, verbose=verbose)}
  if(algorithm=="backward"){tmp <- varrank.backward(data.df = data.df ,variable.important = variable.important,method = method,discretization.method = discretization.method,scheme=scheme,ratio = ratio,n.var=n.var, verbose=verbose)}


    out <- list(ordered.var=tmp[[1]], distance.m=tmp[[2]], algorithm=algorithm, method=method, scheme=scheme)
    class(out) <- "varrank"
    return(out)

}#EOF
