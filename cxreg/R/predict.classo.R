#' make predictions from a "classo" object.
#'
#' Similar to other predict methods, this functions predicts fitted values,
#' coefficients and more from a fitted \code{"classo"} object.
#'
#' This function actually calls \code{NextMethod()}.
#' \code{coef(...)} is equivalent to \code{predict(type="coefficients",...)}
#'
#' @aliases coef.classo, predict.classo
#' @param object Fitted \code{"classo"} model object.
#' @param newx Matrix of new values for \code{x} at which predictions are to be
#' made. Must be a matrix. This #' argument is not used for \code{type=c("coefficients","nonzero")}
#' @param s Value(s) of the penalty parameter \code{lambda} at which
#' predictions are required. Default is the entire sequence used to create the
#' model.
#' @param type Type of prediction required. Type \code{"link"} givee the fitted
#' values for \code{"gaussian"}. Type \code{"response"} is equivalent to type
#' \code{"link"}. Type \code{"coefficients"} computes the coefficients at the
#' requested values for \code{s}. Type \code{"nonzero"} returns a list of the indices
#' of the nonzero coefficients for each value of \code{s}.
#' @param exact This argument is relevant only when predictions are made at
#' values of \code{s} (lambda) \emph{different} from those used in the fitting
#' of the original model. If \code{exact=FALSE} (default), then the predict function
#' uses linear interpolation to make predictions for values of \code{s} (lambda) that do
#' not coincide with those used in the fitting algorithm. While this is often a
#' good approximation, it can sometimes be a bit coarse.  With
#' \code{exact=TRUE}, these different values of \code{s} are merged (and
#' sorted) with \code{object$lambda}, and the model is refit before predictions
#' are made. In this case, it is required to supply the original data \code{x=}
#' and \code{y=} as additional named arguments to \code{predict()} or
#' \code{coef()}.  The workhorse \code{predict.classo()} needs to \code{update}
#' the model, and so needs the data used to create it. The same is true of
#' \code{weights} if these were used in the original call. Failure to do
#' so will result in an error.
#' \code{type="nonzero"})
#' @param \dots This is the mechanism for passing arguments like \code{x=} when
#' \code{exact=TRUE}; see\code{exact} argument.
#' @return The object returned depends on type.
#' @author Younghoon Kim, Navonil Deb, and Sumanta Basu \cr Maintainer:
#' Younghoon Kim <yk748@cornell.edu>
#' @seealso \code{classo}, and \code{print}, and \code{coef} methods, and
#' \code{cv.classo}.
#' @references Deb, N., Kuceyeski, A. and Basu, S. (2024)
#' \emph{Regularized estimation of sparse spectral precision matrices (2024), Preprint},
#' \url{https://arxiv.org/abs/2401.11128}.
#' @keywords models regression
#' @method predict classo
#' @export
#' @export predict.classo
#'
predict.classo <- function(object,newx,s=NULL,
                          type=c("response","coefficients","nonzero"),
                          exact=FALSE,...){

  type <- match.arg(type)

  if(missing(newx)){
    if(!match(type,c("coefficients","nonzero"),FALSE)){
      stop("You need to supply a value for 'newx'")
    }
  }
  if(exact&&(!is.null(s))){
    lambda <- object$lambda
    which <- match(s,lambda,FALSE)

    if(!all(which>0)){
      lambda <- unique(rev(sort(c(s,lambda))))
      object <- update(object,lambda=lambda,...)
    }
  }

  nbeta <- object$beta
  
  if(!is.null(s)){
    vnames <- dimnames(nbeta)[[1]]
    dimnames(nbeta) <- list(NULL,NULL)
    lambda <- object$lambda
    lamlist <- lambda.interp(lambda,s)
    nbeta <- nbeta[,lamlist$left,drop=FALSE]%*%diag(lamlist$frac,length(lamlist$frac)) + nbeta[,lamlist$right,drop=FALSE]%*%diag(1-lamlist$frac,length(lamlist$frac))
    namess <- names(s)

    if(is.null(namess)){
      namess <- paste0("s",seq(along=s))
    }
    dimnames(nbeta) <- list(vnames,namess)
  }
  
  
  if(type=="coefficients"){
    return(nbeta)
  }
  if(type=="nonzero"){
    return(nonzeroCoef(nbeta[-1,,drop=FALSE],bystep=TRUE))
  }


  if(inherits(newx, "sparseMatrix")){
    newx <- as(newx,"dMatrix")
  }
  dx <- dim(newx)
  p <- object$dim[1]
  if(is.null(dx)){
    newx <- matrix(newx,1,byrow=TRUE)
  }
  if(ncol(newx) != p){
    stop(paste0("The number of variables in newx must be ",p))
  }
  
  nfit <- as.matrix(cbind2(newx)%*%nbeta)
  nfit
}
