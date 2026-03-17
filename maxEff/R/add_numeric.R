

#' @title Additional Predictor as \link[base]{numeric}
#' 
#' @description
#' Additional predictor as \link[base]{numeric}.
#' 
#' @param start.model a regression model (e.g., \link[stats]{lm}, \link[stats]{glm}, or \link[survival]{coxph}, etc.)
#' 
#' @param x one-sided \link[stats]{formula} to specify 
#' the \link[base]{numeric} predictors \eqn{x}'s as the columns of one \link[base]{matrix} column in `data`
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details 
#' The function [add_numeric()] treats each additional predictor as a \link[base]{numeric} variable, 
#' and \link[stats]{update}s the starting model with each additional predictor.  
#' 
#' @returns 
#' The function [add_numeric()] returns an [add_numeric] object, 
#' which is a \link[stats]{listof} objects with an internal class `'add_numeric_'`.
#' 
#' @keywords internal
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel mclapply makeCluster stopCluster
#' @export
add_numeric <- function(
    start.model, 
    x,
    data = eval(start.model$call$data),
    mc.cores = getOption('cores'), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data_ <- tmp$data # 'data.frame'
  x_ <- tmp$x_
  xval <- tmp$xval

  fn <- \(i) {
    x. <- x_[[i]]
    data_$x. <- xval[[i]]
    m_ <- update(start.model, formula. = . ~ . + x., data = data_)
    cf <- m_$coefficients
    cf_ <- cf[length(cf)]
    attr(x., which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
    attr(x., which = 'model') <- m_ # needed for [predict.*]
    class(x.) <- c('add_numeric_', class(x.))
    return(x.)
  }
  
  sq <- x_ |>
    seq_along()
  switch(
    EXPR = .Platform$OS.type, # as of R 4.5, only two responses, 'windows' or 'unix'
    unix = {
      out <- sq |>  
        mclapply(mc.cores = mc.cores, FUN = fn) # 'list'
        #lapply(FUN = fn) # debugging
    }, windows = {
      i <- NULL # just to suppress devtools::check NOTE
      registerDoParallel(cl = (cl <- makeCluster(spec = mc.cores)))
      out <- foreach(i = sq, .options.multicore = list(cores = mc.cores)) %dopar% fn(i)
      stopCluster(cl)
    })
    
  class(out) <- c('add_numeric', 'add_', 'listof', class(out))
  return(invisible(out))
  
}



# tzh's [predict.*] only needs model call, not the full model!!


#' @title [labels.add_numeric]
#' 
#' @param object a [add_numeric] object
#' 
#' @param ... ..
#' 
#' @returns
#' The `S3` method [labels.add_numeric()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export labels.add_numeric
#' @export
labels.add_numeric <- function(object, ...) {
  object |>
    vapply(FUN = deparse1, FUN.VALUE = '')
}




#' @title [print.add_numeric]
#' 
#' @param x a [add_numeric] object
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @export print.add_numeric
#' @export
print.add_numeric <- function(x, ...) {
  x |>
    labels.add_numeric() |>
    cat(sep = '\n')
}





#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [add_numeric] object
#' 
#' @param ... additional parameters of function `predict.add_numeric_`, e.g., `newdata`
#' 
#' @returns
#' The `S3` method [predict.add_numeric()] returns a \link[stats]{listof} regression models.
#' 
#' @keywords internal
#' @name predict_add_numeric
#' @export predict.add_numeric
#' @export
predict.add_numeric <- function(object, ...) {
  ret <- object |> 
    lapply(FUN = predict.add_numeric_, ...)
  names(ret) <- object |>
    labels.add_numeric()
  class(ret) <- 'listof'
  return(ret)
}

#' @rdname predict_add_numeric
#' @export predict.add_numeric_
#' @export
predict.add_numeric_ <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  newd <- unclass(newdata)$df
  newd$x. <- with(data = newdata, ee = object) # ?spatstat.geom::with.hyperframe
  
  object |>
    attr(which = 'model', exact = TRUE) |>
    update(data = newd)
  
}

