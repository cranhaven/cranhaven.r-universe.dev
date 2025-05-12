

# for a given call, check its type. Possible types can be
# 1. distribution (i.e. qnorm). In this case a quantile function is created accordingly
# 2. data (i.e. data frame). In this case, a quantile function is created using data provided

# call: a call specified in ...
# env: environment of the call
#
#' @importFrom stats qnorm qt qgamma qbinom qbeta qcauchy qchisq qexp qf
#' @importFrom stats qgeom qlnorm qlogis qnbinom qpois qunif qweibull
#' @importFrom stats ppoints sd

parseCallIntoQuantileFunction <- function(call, env){

  if(isFunctionQuantile(call, env)){
    func <- eval(call, envir = env)
    par_list <- formals(func)
    par_list$x <- NULL
    attributes(func) <-
      c(
        list(type = 'user_defined'),
        par_list
      )

    return(func)
  }

  if(isDataQuantile(call, env)){

    func_list <- local(
      {
        data_in_call <- eval(call, env)
        quantile_functions_from_data(data = data_in_call)
      }
    )

    attributes(func_list) <-
      list(
        type = 'data',
        var_names = names(func_list)
      )

    return(func_list)
  }

  if(isCommonQuantile(call, 'norm', c('mean', 'sd'))){

    func <- local(
      {
        mean_in_call <- eval(call$mean, env)
        sd_in_call <- eval(call$sd, env)
        function(x, mean = mean_in_call, sd = sd_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qnorm(x, mean, sd)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'normal',
        mean = eval(call$mean, env),
        sd = eval(call$sd, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 't', c('df'))){

    func <- local(
      {
        df_in_call <- eval(call$df, env)
        function(x, df = df_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qt(x, df)
        }
      }
    )

    attributes(func) <-
      list(
        type = "Student's t",
        df = eval(call$df, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'gamma', c('shape', 'rate'))){

    func <- local(
      {
        shape_in_call <- eval(call$shape, env)
        rate_in_call <- eval(call$rate, env)

        function(x, shape = shape_in_call, rate = rate_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qgamma(x, shape, rate)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'gamma',
        shape = eval(call$shape, env),
        rate = eval(call$rate, env)
      )

    return(func)

  }

  if(isCommonQuantile(call, 'binom', c('size', 'prob'))){

    func <- local(
      {
        size_in_call <- eval(call$size, env)
        prob_in_call <- eval(call$prob, env)

        function(x, size = size_in_call, prob = prob_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qbinom(x, size, prob)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'binomial',
        size = eval(call$size, env),
        prob = eval(call$prob, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'unif', c('min', 'max'))){

    func <- local(
      {
        min_in_call <- eval(call$min, env)
        max_in_call <- eval(call$max, env)

        function(x, min = min_in_call, max = max_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qunif(x, min, max)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'uniform',
        min = eval(call$min, env),
        max = eval(call$max, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'beta', c('shape1', 'shape2'))){

    func <- local(
      {
        shape1_in_call <- eval(call$shape1, env)
        shape2_in_call <- eval(call$shape2, env)

        function(x, shape1 = shape1_in_call, shape2 = shape2_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qbeta(x, shape1, shape2)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'beta',
        shape1 = eval(call$shape1, env),
        shape2 = eval(call$shape2, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'exp', c('rate'))){

    func <- local(
      {
        rate_in_call <- eval(call$rate, env)

        function(x, rate = rate_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qexp(x, rate)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'exponential',
        rate = eval(call$rate, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'geom', c('prob'))){

    func <- local(
      {
        prob_in_call <- eval(call$prob, env)

        function(x, prob = prob_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qgeom(x, prob)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'geometric',
        prob = eval(call$prob, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'chisq', c('df'))){

    func <- local(
      {
        df_in_call <- eval(call$df, env)

        function(x, df = df_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qchisq(x, df)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'chi-squared',
        df = eval(call$df, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'f', c('df1', 'df2'))){

    func <- local(
      {
        df1_in_call <- eval(call$df1, env)
        df2_in_call <- eval(call$df2, env)

        function(x, df1 = df1_in_call, df2 = df2_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qf(x, df1, df2)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'F',
        df1 = eval(call$df1, env),
        df2 = eval(call$df2, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'lnorm', c('meanlog', 'sdlog'))){

    func <- local(
      {
        meanlog_in_call <- eval(call$meanlog, env)
        sdlog_in_call <- eval(call$sdlog, env)

        function(x, meanlog = meanlog_in_call, sdlog = sdlog_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qlnorm(x, meanlog, sdlog)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'log normal',
        meanlog = eval(call$meanlog, env),
        sdlog = eval(call$sdlog, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'nbinom', c('size', 'prob'))){

    func <- local(
      {
        size_in_call <- eval(call$size, env)
        prob_in_call <- eval(call$prob, env)

        function(x, size = size_in_call, prob = prob_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qnbinom(x, size, prob)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'negative binomial',
        size = eval(call$size, env),
        prob = eval(call$prob, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'pois', c('lambda'))){

    func <- local(
      {
        lambda_in_call <- eval(call$lambda, env)

        function(x, lambda = lambda_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qpois(x, lambda)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'Poisson',
        lambda = eval(call$lambda, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'cauchy', c('location', 'scale'))){

    func <- local(
      {
        location_in_call <- eval(call$location, env)
        scale_in_call <- eval(call$scale, env)

        function(x, location = location_in_call, scale = scale_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qcauchy(x, location, scale)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'Cauchy',
        location = eval(call$location, env),
        scale = eval(call$scale, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'logis', c('location', 'scale'))){

    func <- local(
      {
        location_in_call <- eval(call$location, env)
        scale_in_call <- eval(call$scale, env)

        function(x, location = location_in_call, scale = scale_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qlogis(x, location, scale)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'logistic',
        location = eval(call$location, env),
        scale = eval(call$scale, env)
      )

    return(func)
  }

  if(isCommonQuantile(call, 'weibull', c('shape', 'scale'))){

    func <- local(
      {
        shape_in_call <- eval(call$shape, env)
        scale_in_call <- eval(call$scale, env)

        function(x, shape = shape_in_call, scale = scale_in_call){
          if(any(x > 1) || any(x < 0)){
            stop('x should be a probability between 0 and 1')
          }
          qweibull(x, shape, scale)
        }
      }
    )

    attributes(func) <-
      list(
        type = 'Weibull',
        shape = eval(call$shape, env),
        scale = eval(call$scale, env)
      )

    return(func)

  }

  invisible(NULL)

}

