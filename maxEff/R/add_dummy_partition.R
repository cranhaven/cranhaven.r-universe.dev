

#' @title Additional \link[base]{logical} Predictor via Repeated Partitioning
#' 
#' @param start.model a regression model, e.g., 
#' \link[stats]{lm}, \link[stats]{glm}, or \link[survival]{coxph}, etc.
#' 
#' @param x one-sided \link[stats]{formula},
#' \link[base]{numeric} predictors \eqn{x}'s as the columns of one \link[base]{matrix} column in `data`
#' 
#' @param data (optional) \link[base]{data.frame} in the model \link[base]{call} of `start.model`
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param times,... additional parameters of function \link[caret]{createDataPartition} or [statusPartition()]
#' 
#' @returns 
#' The function [add_dummy_partition()] returns an object of \link[base]{class} `'add_dummy'`.
#' 
#' @keywords internal
#' @importFrom caret createDataPartition
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel mclapply makeCluster stopCluster
#' @export
add_dummy_partition <- function(
    start.model, 
    x,
    data = eval(start.model$call$data),
    times, 
    mc.cores = getOption('cores'), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  #data <- tmp$data # not here!
  x_ <- tmp$x_
  xval <- tmp$xval
  
  ids <- if (inherits(y, what = 'Surv')) {
    statusPartition(y = y, times = times, ...)
  } else createDataPartition(y = y, times = times, groups = 2L, ...)
  # using same split for all predictors
  
  fn <- \(i) { 
    tmp_ <- ids |>
      lapply(FUN = splitd, start.model = start.model, x_ = x_[[i]], x = xval[[i]], data = data)
    tmp <- tmp_[lengths(tmp_, use.names = FALSE) > 0L]
    
    effsize <- tmp |> 
      vapply(FUN = attr, which = 'effsize', exact = TRUE, FUN.VALUE = NA_real_)
    id <- tmp |> 
      seq_along() |> 
      quantile(probs = .5, type = 3L, na.rm = TRUE) # median *location*
    return(tmp[[order(effsize)[id]]])  
  }
  
  sq <- x_ |>
    seq_along()
  switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = {
      out <- sq |>
        mclapply(mc.cores = mc.cores, FUN = fn)
        #lapply(FUN = fn) # debugging
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
      out <- foreach(i = sq, .options.multicore = list(cores = mc.cores)) %dopar% fn(i)
      stopCluster(cl)
    })
  
  class(out) <- c('add_dummy', 'add_', 'listof', class(out))
  return(invisible(out))
  
}






#' @title Split-Dichotomized Regression Model
#' 
#' @description
#' Split-dichotomized regression model.
#' 
#' @param start.model a regression model
#' 
#' @param x_ \link[base]{language}
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param data \link[spatstat.geom]{hyperframe}
#' 
#' @param id \link[base]{logical} \link[base]{vector}, indices of training (`TRUE`) and test (`FALSE`) subjects 
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @section Split-Dichotomized Regression Model:
#' 
#' The function [splitd()] performs a univariable regression model on the test set with a dichotomized predictor, using a dichotomizing rule determined by a recursive partitioning of the training set. 
#' Specifically, given a training-test sample split,
#' \enumerate{
#' \item find the *dichotomizing rule* \eqn{\mathcal{D}} of the predictor \eqn{x_0} given the response \eqn{y_0} in the training set (via function [node1()]);
#' \item fit a univariable regression model of the response \eqn{y_1} with the dichotomized predictor \eqn{\mathcal{D}(x_1)} in the test set.
#' }
#' Currently the Cox proportional hazards (\link[survival]{coxph}) regression for \link[survival]{Surv} response, logistic (\link[stats]{glm}) regression for \link[base]{logical} response and linear (\link[stats]{lm}) regression for \link[stats]{gaussian} response are supported.
#' 
#' @returns
#' 
#' The function [splitd()] returns a \link[base]{function}, 
#' the dichotomizing rule \eqn{\mathcal{D}} based on the training set \eqn{(y_0, x_0)}, 
#' with additional attributes
#' \describe{
#' \item{`attr(,'p1')`}{\link[base]{double} scalar, \eqn{p_1 = \text{Pr}(\mathcal{D}(x_1)=1)}}
#' \item{`attr(,'effsize')`}{\link[base]{double} scalar, univariable regression coefficient estimate of \eqn{y_1\sim\mathcal{D}(x_1)}}
#' }
#' 
#' @keywords internal
#' @importFrom rpart rpart
#' @export
splitd <- function(start.model, x_, x, data, id, ...) {
  
  y <- start.model$y
  
  data_df <- unclass(data)$df
  
  # `id`: training set
  rule <- rpart(formula = y[id] ~ x[id], cp = .Machine$double.eps, maxdepth = 2L) |>
    node1(nm = x_)
  
  # `-id`: test set (`id` is `integer`)
  y_ <- y[-id]
  data_ <- data_df[-id, , drop = FALSE]
  dx_ <- tryCatch(rule(x[-id]), warning = identity)
  if (inherits(dx_, what = 'warning')) return(invisible()) # exception
  if ('x.' %in% names(data_df)) stop('do not allow `x.` as an original column in `data`')
  data_$x. <- dx_
  
  m_ <- update(start.model, formula. = . ~ . + x., data = data_)

  cf <- m_$coefficients
  cf_ <- cf[length(cf)]
  
  attr(rule, which = 'p1') <- mean.default(dx_, na.rm = TRUE)
  attr(rule, which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
  attr(rule, which = 'model') <- m_ # only model formula needed for [predict.add_dummy_]!!!
  
  class(rule) <- c('add_dummy_', class(rule))
  return(rule)
}



