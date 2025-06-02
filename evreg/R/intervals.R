#' Computation of prediction intervals from a trained ENNreg model
#'
#' \code{intervals} computes probabilistic and belief prediction intervals from a prediction object
#' returned by function \code{\link{predict.ENNreg}}.
#'
#' @param pred Prediction object returned by function \code{\link{predict.ENNreg}}.
#' @param level Level of the prediction interval (between 0 and 1).
#' @param yt Optional vector of test response values.
#'
#' @return A list with four elements:
#' \describe{
#' \item{INTP}{Matrix (n,2) of probabilistic prediction intervals.}
#' \item{INTBel}{Matrix (n,2) of belief prediction intervals.}
#' \item{coverage.P}{Estimated coverage rate of the probabilistic intervals (if yt is provided).}
#' \item{coverage.Bel}{Estimated coverage rate of the belief intervals (if yt is provided).}
#' \item{Pl.Bel}{Mean plausibility of the belief intervals.}
#' }
#' @export
#' @importFrom stats qnorm uniroot
#' @seealso \code{\link{predict.ENNreg}}, \code{\link{ENNreg}}
#'
#' @examples
#' library(MASS)
#' \donttest{
#' X<-as.matrix(scale(Boston[,1:13]))
#' y<-Boston[,14]
#' set.seed(220322)
#' n<-nrow(Boston)
#' ntrain<-round(0.7*n)
#' train <-sample(n,ntrain)
#' fit <- ENNreg(X[train,],y[train],K=30)
#' pred<-predict(fit,newdata=X[-train,],yt=y[-train])
#' int<- intervals(pred,level=0.95,y[-train])
#' print(c(int$coverage.P,int$coverage.Bel))
#' }
#'
intervals<-function(pred,level=0.9,yt=NULL){

  coverage<-function(INT,y) mean((y>=INT[,1])&(y<=INT[,2]))

  p<-level
  mut<-pred$mux
  sigt<-sqrt(pred$sig2x)
  ht<-pred$hx
  nt<-length(mut)
  L<-mut+sigt*qnorm((1-p)/2)
  U<-mut-sigt*qnorm((1-p)/2)
  INTP <- cbind(L,U)
  INTBel <- matrix(0,nt,2)
  Pl.Bel <- 0
  for(i in 1:nt){
    GRFN <- list(mu=mut[i],sig=sigt[i],h=ht[i])
    INTBel[i,]<-Belint(p, GRFN)
    Pl.Bel <- Pl.Bel + Pl(INTBel[i,1],INTBel[i,2],GRFN)
  }
  Pl.Bel <- Pl.Bel/nt
  if(!is.null(yt)){
    coverage.P<-coverage(INTP,yt)
    coverage.Bel<-coverage(INTBel,yt)
  } else{
    coverage.P<-NULL
    coverage.Bel<-NULL
  }
  return(list(INTP=INTP,INTBel=INTBel,coverage.P=coverage.P,coverage.Bel=coverage.Bel,
              Pl.Bel=Pl.Bel))
}
