#' Prediction method for the ENNreg model
#'
#' Predicted values based on a trained ENNreg model (object of class "ENNreg").
#'
#' @param object An object of type "ENNreg"
#' @param newdata Input matrix of attributes for test data
#' @param yt Optional test response vector
#' @param ... Further arguments passed to or from other methods
#'
#' @return Predictions for the new data, coded as a list with the following components:
#' \describe{
#' \item{mux}{Predicted means}
#' \item{sigx}{Predicted standard deviations.}
#' \item{hx}{Prediction precisions.}
#' \item{Einf}{Lower expectation.}
#' \item{Esup}{Upper expectations}
#' \item{NLL}{Negative log likelihood (computed only if yt is provided).}
#' \item{RMS}{Root mean squared error (computed only if yt is provided).}
#' }
#' @export
#'
#' @seealso \code{\link{ENNreg}}, \code{\link{ENNreg_init}}
#'
#' @examples # Boston dataset
#' \donttest{
#' library(MASS)
#' X<-as.matrix(scale(Boston[,1:13]))
#' y<-Boston[,14]
#' set.seed(220322)
#' n<-nrow(Boston)
#' ntrain<-round(0.7*n)
#' train <-sample(n,ntrain)
#' fit <- ENNreg(X[train,],y[train],K=30)
#' pred<-predict(fit,newdata=X[-train,],yt=y[-train])
#' plot(y[-train],pred$mux,xlab="observed response",ylab="predicted response")
#' }
#'
predict.ENNreg<-function(object,newdata,yt=NULL,...){
  p<-ncol(object$param$W)
  if(is.vector(newdata) & (p>1)) Xt<-matrix(newdata,ncol=p) else Xt<-as.matrix(newdata)
  nt<-nrow(Xt)
  K<-object$K
  d<-matrix(0,nt,K)
  a<-matrix(0,nt,K)
  for(k in 1:K){
    d[,k] <- rowSums((Xt - matrix(object$param$W[k,],nt,p,byrow=TRUE))^2)
    a[,k] <- exp(- object$param$gam[k]^2 * d[,k])
  }
  H<-matrix(object$param$h,nt,K,byrow=TRUE)
  hx<-rowSums(a*H)
  mu<-Xt%*%t(object$param$Beta)+matrix(object$param$alpha,nt,K,byrow=TRUE) # size (nt,K)
  mux<-rowSums(mu*a*H)/hx
  sig2x<-rowSums(matrix(object$param$sig^2,nt,K,byrow=TRUE)*a^2*H^2)/hx^2
  if(!is.null(yt)){
    NLL <- mean(0.5*log(2*pi*sig2x)+(yt-mux)^2/(2*sig2x))
    RMS <- sqrt(mean((yt-mux)^2))
  }else{
    NLL<-NULL
    RMS<-NULL
  }
  return(list(mux=mux,sig2x=sig2x,hx=hx,Einf=mux-sqrt(pi/(2*hx)),Esup=mux+sqrt(pi/(2*hx)),
              NLL=NLL,RMS=RMS))
}

