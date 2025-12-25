#' print method for a "bsrr" object
#'
#' Print the primary elements of the "\code{bsrr}" object.
#'
#' prints the fitted model and returns it invisibly.
#'
#' @param x A "\code{bsrr}" object.
#' @param digits Minimum number of significant digits to be used.
#' @param nonzero Whether the output should only contain the non-zero coefficients.
#' @param \dots additional print arguments
#' @seealso \code{\link{bsrr}}, \code{\link{coef.bsrr}}.
#' @inherit bsrr return author
#' @return
#' No return value, called for side effect
#' @examples
#'
#' # Generate simulated data
#' n = 200
#' p = 20
#' k = 5
#' rho = 0.4
#' seed = 10
#' Tbeta <- rep(0, p)
#' Tbeta[1:k*floor(p/k):floor(p/k)] <- rep(1, k)
#' Data = gen.data(n, p, k, rho, family = "gaussian", beta = Tbeta, seed=seed)
#' lambda.list = exp(seq(log(5), log(0.1), length.out = 10))
#' lm.bsrr = bsrr(Data$x, Data$y, lambda.list = lambda.list, method = "sequential")
#'
#' print(lm.bsrr)
#'
#'@method print bsrr
#'@export
#'@export print.bsrr
#'
print.bsrr<-function(x, digits = max(5, getOption("digits") - 5), nonzero = FALSE,...)
{
  # if(x$method != "sequential"){
  #   if(x$algorithm_type != "L0L2"){
  #     df <- apply(matrix(unlist(x$beta_all), nrow = length(x$beta), byrow = F), 2, function(x){sum(ifelse(abs(x) < 1e-8, 0, 1))})
  #     if(x$ic_type == "cv") {
  #       print(cbind(Df=df, deviance = unlist(deviance_all(x, x$train_loss_all)),  cvm = as.vector(x$cvm_all)))
  #     }else{
  #       ic <- x$ic_type
  #       print(cbind(Df=df, deviance =unlist(deviance_all(x, x$train_loss_all)),ic = as.vector(x$ic_all)))
  #     }
  #   } else{
  #     df <- apply(x$beta_all, 2, function(x){sum(ifelse(abs(x) < 1e-8, 0, 1))})
  #     if(x$ic_type == "cv"){
  #       print(cbind(DF = df, lambda = x$lambda_all, deviance = as.vector(deviance_all(x, x$train_loss_all)), cvm = as.vector(x$cvm_all)))
  #     } else{
  #       ic <- x$ic_type
  #       print(cbind(DF = df, lambda = x$lambda_all, deviance = as.vector(deviance_all(x, x$train_loss_all)), ic = as.vector(x$ic_all)))
  #     }
  #   }
  # } else{
  #   if(x$algorithm_type != "L0L2"){
  #     if(x$ic_type == "cv") {
  #       print(cbind(Df=x$s.list,deviance=unlist(deviance_all(x, x$train_loss_all)), cvm = as.vector(x$cvm_all)))
  #     }else{
  #       ic = x$ic_type
  #       print(cbind(Df=x$s.list, deviance=unlist(deviance_all(x, x$train_loss_all)),ic = as.vector(x$ic_all)))
  #     }
  #   } else{
  #     train_loss_all <- matrix(unlist(x$train_loss_all), nrow = length(x$s.list), byrow = F)
  #     deviance <- deviance_all(x, train_loss_all)
  #     # rownames(train_loss_all) = x$s.list
  #     # colnames(train_loss_all) = x$lambda_list
  #     if(x$ic_type == "cv") {
  #       cv_all <- x$cvm_all
  #       #rownames(cv_all) <- x$s.list
  #       #colnames(cv_all) <- x$lambda_list
  #       print(list(deviance = deviance, cvm = cv_all))
  #     }else{
  #       ic_all <- x$ic_all
  #       # rownames(ic_all) = x$s.list
  #       #colnames(ic_all) = x$lambda_list
  #       ic <- x$ic_type
  #       print(list(deviance = deviance, ic = ic_all))
  #     }
  #   }
  # }

    cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
          "")
  if(!nonzero){
    print(round(coef(x),  digits), ...)
  } else{
    coefx <- round(coef(x, sparse = FALSE)[which(coef(x, sparse = FALSE)!=0)], digits)
    print(coefx, ...)
  }
    cat("\n")
    invisible(x)


}
