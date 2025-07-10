#' Diversity Boosting Algorithm
#'
#' Train a set of initial learners by promoting diversity among them. For this, a gradient descent strategy is adopted where a specialized loss function induces diversity which yields on a reduction of the mean-square-error of the aggregated learner.
#'
#' @param target name of the target variable 
#' @param cov the model equation, a character string provided in the formula syntax. For example, for a linear model including covariates $X_1$ and $X_2$ it will be "X1+X2" and for a GAM with smooth effects it will be "s(X1)+s(X2)"
#' @param data0 the learning set
#' @param data1 the test set
#' @param sample_size the size of the bootstrap sample as a proportion of the learning set size. sample_size=0.5 means that the resamples are of size n/2 where n is the number of rows of data0.
#' @param grad_step step of the gradient descent
#' @param diversity_weight the weight of the diversity encouraging penalty (kappa in the paper)
#' @param Nstep the number of iterations of the diversity boosting algorithm ($N$ in the paper)
#' @param model the type of base learner used in the algorithm if using a single base learner (model_list=NULL). Currently it could be either
#' "gam" for an additive model, "rf" for a random forest, ""gbm" for gradient boosting machines, "rpart" for single CART trees.
#' @param sampling the type of sampling procedure used in the resampling step. Could be either \code{"random"} for uniform random sampling with
#' replacement or \code{"blocks"} for uniform sampling with replacement of blocks of consecutive data points. Default is "random".
#' @param Nblock number of blocks for the block sampling. Equal to 10 by default.
#' @param aggregation_type type of aggregation used for the ensemble method, default is uniform weights but it could be also "MLpol" an aggregation algorithm
#' from the opera package
#' @param param a list containing the parameters of the model chosen. It could be e.g. the number of trees for "rf", the depth of the tree for "rpart"...
#' @param theorical_dw set to TRUE if one want to use the theoretical upper bound of the diversity weight kappa
#' @param model_list a list of model among the possible ones (see the description of model argument). In that case the week learner is sample at each step in the list.
#' "Still "experimental", be careful.
#' @param w_list the prior weights of each model in the model_list
#' @param param_list list of parameters of each model in the model_list
#' @param cov_list list of covariates of each model in the model_list
#' @return a list including the boosted models, the ensemble forecast
#'   \item{fitted_ensemble}{Fitted values (in-sample predictions) for the ensemble method (matrix).}
#'   \item{forecast_ensemble}{Forecast (out-sample predictions) for the ensemble method (matrix).}
#'   \item{fitted}{Fitted values of the last boosting iteration (vector).}
#'   \item{forecast}{Forecast of the last boosting iteration (vector).}
#'   \item{err_oob}{Estimated out-of-bag errors by iteration (vector).}
#'   \item{diversiy_oob}{Estimated out-of-bag diversity (vector).}
#'
#' @importFrom stats as.formula predict
#' @examples
#' all <- na.omit(airquality)
#' smp <- sample(nrow(all), floor(.8 * nrow(all)))
#' boosting_diversity("Ozone", "Solar.R+Wind+Temp+Month+Day", 
#'                    data0 = all[smp, ], data1 = all[-smp, ])
#' @author Yannig Goude <yannig.goude@edf.fr>
#' @export


boosting_diversity <- function(target, cov, data0, data1, sample_size = 0.5, 
                               grad_step = 1, diversity_weight = 1, Nstep = 10,
                               model = "gam", sampling = "random", Nblock = 10, 
                               aggregation_type = "uniform", param = list(), 
                               theorical_dw = FALSE, model_list = NULL, w_list = NULL,
                               param_list = NULL, cov_list = NULL)
{
  n <- nrow(data0)
  ####here the hypothesis about the relationship between y and covariates has to be specified in cov
  equation <- paste0(target, "~", cov)

  
  stopifnot(sampling %in% c('random', 'blocks'))
  if(sampling=="random")
  {
    subset1 <- sample(c(1:n), floor(n*sample_size), replace=TRUE)
    #subset2 <- sample(c(1:n), floor(n*sample_size), replace=T) if we don't want that I2=I-I1
    subset2 <- c(1:n)[-subset1]
  } else {#  if(sampling=="blocks")
      blocks <- buildBlock(Nblock, data0)
      s <- sample(c(1:Nblock), Nblock, replace=TRUE)
      ind <- unlist(blocks[s])
      subset1 <- ind
      subset2 <- c(1:n)[-ind]
  }

  if(!is.null(model_list))
  {
    model <- sample(model_list, 1, prob=w_list)
    param <- param_list[[model]]
    cov <- cov_list[[model]]
    equation <- paste0(target, "~", cov)
  }

  mod_out <- model

  if(model == "gam")
  {
    param$data <- data0[subset1,]
    param$formula <- as.formula(equation)
    g <- do.call(mgcv::gam, args=param)
    #g <- mgcv::gam(equation%>%as.formula, data=data0[subset1,])
    forecast0 <- matrix(predict(g, newdata=data0), ncol=1, nrow=n)
    forecast1 <- matrix(predict(g, newdata=data1), ncol=1, nrow=nrow(data1))
  }

  if(model=="rf")
  {
    param$data <- data0[subset1,]
    param$formula <- as.formula(equation)
    rf <- do.call(ranger::ranger, args=param)
    #rf <- ranger::ranger(equation%>%as.formula, data=data0[subset1,])
    forecast0 <- matrix(predict(rf, data=data0)$predictions, ncol=1, nrow=n)
    forecast1 <- matrix(predict(rf, data=data1)$predictions, ncol=1, nrow=nrow(data1))
  }

  if(model=="gbm")
  {
    param$data <- data0[subset1,]
    param$formula <- as.formula(equation)
    gb <- do.call(gbm::gbm, args=param)
    best.iter <- gbm::gbm.perf(gb,method="OOB", plot.it = FALSE)
    forecast0 <- matrix(predict(gb, newdata=data0, n.trees=best.iter), ncol=1, nrow=n)
    forecast1 <- matrix(predict(gb, newdata=data1, n.trees=best.iter), ncol=1, nrow=nrow(data1))
  }

  if(model=="rpart")
  {
    param$data <- data0[subset1,]
    param$formula <- as.formula(equation)
    rp <- do.call(rpart::rpart, args=param)
    forecast0 <- matrix(predict(rp, newdata=data0), ncol=1, nrow=n)
    forecast1 <- matrix(predict(rp, newdata=data1), ncol=1, nrow=nrow(data1))
  }

  err_oob <- mean((forecast0[subset2]-data0[subset2, target])^2)
  diversiy_oob <- 0


  if(Nstep>1)
  {



  for(i in c(2:Nstep))
  {
    last_forecast0 <- forecast0[, ncol(forecast0)]
    last_forecast1 <- forecast1[, ncol(forecast1)]

    if(ncol(forecast0)<=1)
    {
      gradient <- (last_forecast0-data0[, target]) #####initialize with only a classical gradient boosting step
    }

    else
    {
      if(aggregation_type=="uniform")
      {
        div <- last_forecast0- rowMeans(forecast0)
      }

      if(aggregation_type=="MLpol")
      {
        agg <- opera::mixture(Y = data0[, target] , experts = forecast0 , model = aggregation_type, loss.type = "square", loss.gradient = TRUE)
        div <- last_forecast0 - agg$prediction
      }

      if(theorical_dw==FALSE)
      {
        gradient <- (last_forecast0-data0[, target]) - diversity_weight*div
      }
      if(theorical_dw==TRUE)
      {
        gradient <- (last_forecast0-data0[, target]) - (i/(i-1))*div
      }
    }

    data0$gradient <- gradient
    equation <- paste0("gradient", "~", cov)

    if(!is.null(model_list))
    {
      model <- sample(model_list, 1, prob=w_list)
      param <- param_list[[model]]
      cov <- cov_list[[model]]
      # print(model)
      # print(cov)
      equation <- paste0("gradient", "~", cov)
    }

    if(model=="gam")
    {
      param$data <- data0[subset2,]
      param$formula <- as.formula(equation)
      g <- do.call(mgcv::gam, args=param)
      boost_forecast0 <- last_forecast0 -grad_step*mgcv::predict.gam(g, newdata=data0)
      boost_forecast1 <- last_forecast1 -grad_step*mgcv::predict.gam(g, newdata=data1)
    }

    if(model=="rf")
    {
      param$data <- data0[subset2,]
      param$formula <- as.formula(equation)
      rf <- do.call(ranger::ranger, args=param)
      boost_forecast0 <- last_forecast0 -grad_step*predict(rf, data=data0)$predictions
      boost_forecast1 <- last_forecast1 -grad_step*predict(rf, data=data1)$predictions
    }


    if(model=="gbm")
    {
      param$data <- data0[subset2,]
      param$formula <- as.formula(equation) 
      gb <- do.call(gbm::gbm, args=param)
      best.iter <- gbm::gbm.perf(gb,method="OOB", plot.it = FALSE)
      boost_forecast0 <- last_forecast0 -grad_step*predict(gb, newdata=data0, n.trees=best.iter)
      boost_forecast1 <- last_forecast1 -grad_step*predict(gb, newdata=data1, n.trees=best.iter)
    }

    if(model=="rpart")
    {
      param$data <- data0[subset2,]
      param$formula <- as.formula(equation)
      rp <- do.call(rpart::rpart, args=param)
      boost_forecast0 <- last_forecast0 -grad_step*predict(rp, newdata=data0)
      boost_forecast1 <- last_forecast1 -grad_step*predict(rp, newdata=data1)
    }

    forecast0 <- cbind(forecast0, boost_forecast0)
    forecast1 <- cbind(forecast1, boost_forecast1)

    err_oob <- c(err_oob, mean((boost_forecast0[subset1]-data0[subset1, target])^2))
    diversiy_oob <- c(diversiy_oob, 
                      mean(rowMeans((forecast0[subset1,]-rowMeans(forecast0[subset1,]))^2)))


    if(sampling=="random")
    {
      subset1 <- sample(c(1:n), floor(n*sample_size), replace=TRUE)
      subset2 <- c(1:n)[-subset1]
    }


    if(sampling=="blocks")
    {
      blocks <- buildBlock(Nblock, data0)
      s <- sample(c(1:Nblock), Nblock, replace=TRUE)
      ind <- unlist(blocks[s])
      subset1 <- ind
      subset2 <- c(1:n)[-ind]
    }
    mod_out <- c( mod_out, model)
  }

  }

  res <- list()
  res$fitted_ensemble <- forecast0
  res$forecast_ensemble <- forecast1
  colnames(res$fitted_ensemble) <- paste0("boosting", c(1:ncol(res$fitted_ensemble)))
  colnames(res$forecast_ensemble) <- paste0("boosting", c(1:ncol(res$forecast_ensemble)))
  res$fitted <- forecast0[, ncol(forecast0)]
  res$forecast<- forecast1[, ncol(forecast1)]
  res$err_oob <- err_oob
  res$diversiy_oob <- diversiy_oob
#  res$mod_out <- mod_out

  return(res)
}



