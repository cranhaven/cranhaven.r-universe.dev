#' Function to Randomly Generate Data (with Constraints)
#'
#' This function is primarily used for reproducibility. It will generate a data set of
#' a given size with a given number of constraints for testing function code.
#' @param n number of rows in randomly-generated data set (default is 1000)
#' @param  p number of variables in randomly-generated data set (default is 10)
#' @param  m number of constraints in randomly-generated constraint matrix (default is 5)
#' @param  s number of true non-zero elements in coefficient vector beta1 (default is 5)
#' @param  sigma standard deviation of noise in response (default is 1, indicating standard normal)
#' @param  cov.mat a covariance matrix applied in the generation of data to impose a correlation structure. Default is NULL (no correlation)
#' @param  err error to be introduced in random generation of coefficient values. Default is no error (err = 0)
#' @param  glasso should the generalized Lasso be used (TRUE) or standard Lasso (FALSE). Default is FALSE

#' @return \code{x} generated \code{x} data
#' @return \code{y} generated response \code{y} vector
#' @return \code{C.full} generated full constraint matrix (with constraints of the form \code{C.full}*\code{beta}=\code{b})
#' @return \code{b} generated constraint vector \code{b}
#' @return \code{b.run} if error was included, the error-adjusted value of \code{b}
#' @return \code{beta} the complete beta vector, including generated \code{beta1} and \code{beta2}
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' dim(random_data$x)
#' head(random_data$y)
#' dim(random_data$C.full)
#' random_data$beta

generate.data <-
  function(n = 1000, p = 10, m = 5, cov.mat = NULL, s = 5, sigma = 1,
           glasso = F, err = 0){

    if(!is.null(cov.mat)){
      x<-MASS::mvrnorm(n = n, rep(0, p), cov.mat)
    }
    else {x <- matrix(stats::rnorm(n*p), n, p)}
    C.full = matrix(stats::rnorm(m*p), m, p)
    b = stats::rnorm(m)
    b.run = b*(1+err)
    if (glasso)
      b = rep(0,m)
    beta1 = c(stats::runif(s)+1,rep(0,p-m-s))
    index = (1:(p-m))
    C1 = C.full[,index]
    C2 = C.full[,-index]
    beta2 = as.vector(solve(C2)%*%(b.run-C1%*%beta1))
    s.beta = stats::sd(beta2)
    beta2 = beta2/s.beta
    C.full[,-index] = C.full[,-index]*s.beta
    beta = c(beta1,beta2)
    y = as.vector(x%*%beta+sigma*stats::rnorm(n))
    list(x = x, y = y, C.full = C.full, b = b, b.run = b.run, beta = beta)}

#' Constrained LARS Coefficient Function (Equality Constraints)
#'
#' This function computes the PaC constrained LASSO
#'   coefficient paths following the methodology laid out in the PaC
#'   paper. This function could be called directly as a standalone
#'   function, but the authors recommend using \code{lasso.c} for any
#'   implementation. This is because \code{lasso.c} has additional checks for
#'   errors across the coefficient paths and allows for users to go
#'   forwards and backwards through the paths if the paths are unable
#'   to compute in a particular direction for a particular run.

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param C.full complete constraint matrix C (with constraints of the form \code{C.full}*\code{beta}=\code{b})
#' @param b constraint vector b
#' @param l.min lowest value of lambda to consider (used as 10^\code{l.min}). Default is -2
#' @param l.max largest value of lambda to consider (used as 10^\code{l.max}). Default is 6
#' @param step step size increase in lambda attempted at each iteration (by a factor of 10^\code{step}). Default is 0.2
#' @param beta0 initial guess for beta coefficient vector. Default is NULL (indicating
#'         initial vector should be calculated by algorithm)
#' @param verbose should function print output at each iteration (TRUE) or not (FALSE). Default is FALSE
#' @param max.it maximum number of times step size is halved before the algorithm terminates and gives a warning. Default is 12
#' @param intercept should intercept be included in modeling (TRUE) or not (FALSE). Default is TRUE.
#' @param normalize should \code{x} data be normalized. Default is TRUE
#' @param forwards if \code{forwards} = F, then the algorithm starts at 10^\code{l.max} and
#'             moves backwards (without the forward step). If \code{forwards} = T,
#'             algorithm starts at 10^\code{l.min} and works forward. Default is FALSE

#' @return \code{coefs} A \code{p} by length(\code{lambda}) matrix with each column corresponding to the beta estimate for that lambda
#' @return \code{lambda} the grid of lambdas used to calculate the coefficients on the coefficient path
#' @return \code{intercept} vector with each element corresponding to intercept for corresponding lambda
#' @return \code{error} did the algorithm terminate due to too many iterations (TRUE or FALSE)
#' @return \code{b2index} the index of the \code{beta2} values identified by the algorithm at each lambda
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' lars_fit = lars.c(random_data$x, random_data$y, random_data$C.full, random_data$b)
#' lars_fit$lambda
#' lars_fit$error
#' ### The coefficients for the first lambda value
#' lars_fit$coefs[1,]
#' ### Example of code where path is unable
#' ### to be finished (only one iteration)
#' lars_err = lars.c(random_data$x, random_data$y, random_data$C.full,
#' random_data$b, max.it = 1)
#' lars_err$error
#' lars_err$lambda

lars.c <-
  function(x, y, C.full, b, l.min = -2, l.max = 6, step = 0.2,
           beta0 = NULL, verbose = F, max.it = 12, intercept = T,
           normalize = T, forwards = T){
    p = ncol(x)
    n = nrow(x)
    m = nrow(C.full)
    beta.new = rep(0,p)
    one <- rep(1, n)
    if (intercept) {
      meanx <- drop(one %*% x)/n
      x <- scale(x, meanx, FALSE)
      mu <- mean(y)
      y <- drop(y - mu)
    }
    else {
      meanx <- rep(0, p)
      mu <- 0
      y <- drop(y)
    }
    normx <- rep(1, p)
    if (normalize) {
      normx <- sqrt(n)*apply(x,2,stats::sd,na.rm=T)
      x <- scale(x, FALSE, normx)}
    C.full = t(t(C.full)/normx)
    if (!forwards){
      lambda = lambda.old = 10^l.max
      step = -step
      if (is.null(beta0))
        beta0 = lin.int(C.full,b)
    }
    else{
      lambda = lambda.old = 10^l.min
      if (is.null(beta0))
        beta0 = quad.int(x,y,C.full,b,lambda)
    }
    step.orig = step
    coefs = grid = b2index = NULL
    t.data=transformed(x, y, C.full, b, lambda, beta0)
    beta1.old = t.data$beta1
    beta2.old = t.data$beta2
    iterations = 1
    end.loop = F
    while (!end.loop & (iterations <= max.it)){
      iterations = 1
      loop = T

      while (loop & (iterations <= max.it)){

        t.data$y = t.data$y+(lambda-lambda.old)*t.data$C
        beta1.new = rep(0,length(beta1.old))
        fit = lars::lars(t.data$x[,t.data$active], t.data$y, normalize = F,
                   intercept = F)
        beta1.new[t.data$active] = stats::predict(fit, s = lambda,
                                           type = "coefficients",
                                           mode = "lambda")$coef
        beta2.new = beta2.old + t.data$a2 %*% (beta1.old - beta1.new)
        bad.beta2 = (sum(abs(sign(t.data$beta2)-sign(beta2.new))) != 0)
        X_star = t.data$x
        derivs = abs(as.vector(t(X_star)%*%(X_star%*%beta1.new))-t(X_star)%*%t.data$Y_star-lambda*t.data$C2)
        bad.active = F
        if (n<(p-m))
          bad.active = (max(derivs[-t.data$active])>lambda)
        if (bad.beta2 | bad.active){
          t.data$y = t.data$y-(lambda-lambda.old)*t.data$C
          step=step/2
          lambda = lambda.old*10^step
          iterations = iterations+1}
        else
          loop = F
      }
      if (iterations <= max.it){
        if (verbose==T){
          print(paste("Lambda =",round(lambda,3)))
          if (abs(step) < abs(step.orig))
            print(paste("Step size reduced to ",step))
        }
        step = step.orig
        beta.new[t.data$beta2.index] = beta2.new
        beta.new[-t.data$beta2.index] = beta1.new
        coefs = cbind(coefs,beta.new)
        b2index = cbind(b2index,t.data$beta2.index)
        change.beta = (min(abs(beta2.new))<max(abs(beta1.new)))
        change.active = F
        if (n<(p-m))
          change.active = (min(derivs[t.data$active]) < max(derivs[-t.data$active]))
        if (change.beta | change.active){
          t.data = transformed(x, y, C.full, b, lambda, beta.new)
          beta1.new = t.data$beta1
          beta2.new = t.data$beta2}
        beta1.old = beta1.new
        beta2.old = beta2.new
        grid = c(grid,lambda)
        lambda.old = lambda
        lambda = lambda*10^step}
      if ((forwards & (lambda>10^l.max)) | (!forwards & (lambda<10^l.min)))
        end.loop = T
    }
    if (iterations > max.it)
      print(paste("Warning: Algorithm terminated at lambda =",round(lambda.old,1),": Maximum iterations exceeded."))
    colnames(coefs) = intercept = NULL
    if (!is.null(grid)){
      coefs = coefs/normx
      coefs = coefs[,order(grid)]
      b2index = b2index[,order(grid)]
      grid = sort(grid)
      intercept = mu-drop(t(coefs)%*%meanx)}
    list(coefs = coefs, lambda = grid, intercept = intercept,
         error = (iterations>max.it), b2index = b2index)
  }

#' Complete Run of Constrained LASSO Path Function (Equality Constraints)
#'
#' This is a wrapper function for the \code{lars.c} PaC
#'   constrained Lasso function. \code{lasso.c} controls the overall path,
#'   providing checks for the path and allowing the user to control
#'   how the path is computed (and what to do in the case of a stopped path).

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param  C.full complete constraint matrix C (with constraints of the form \code{C.full}*\code{beta}=\code{b})
#' @param  b constraint vector b
#' @param  l.min lowest value of lambda to consider (used as 10^\code{l.min}). Default is -2
#' @param  l.max largest value of lambda to consider (used as 10^\code{l.max}). Default is 6
#' @param  step step size increase in lambda attempted at each iteration (by a factor of 10^\code{step}). Default is 0.2
#' @param  beta0 initial guess for beta coefficient vector. Default is NULL (indicating
#'         initial vector should be calculated by algorithm)
#' @param  verbose should function print output at each iteration (TRUE) or not (FALSE). Default is FALSE
#' @param  max.it maximum number of times step size is halved before the algorithm terminates and gives a warning. Default is 12
#' @param  intercept should intercept be included in modeling (TRUE) or not (FALSE). Default is TRUE.
#' @param  normalize should X data be normalized. Default is TRUE
#' @param  backwards which direction should algorithm go, backwards from lambda = 10^\code{l.max} (TRUE)
#'             or forwards from 10^\code{l.max} and then backwards if algorithm gets stuck (FALSE).
#'             Default is FALSE.

#' @return \code{coefs} A \code{p} by length(\code{lambda}) matrix with each column corresponding to the beta estimate for that lambda
#' @return \code{lambda} vector of values of lambda that were fit
#' @return \code{intercept} vector with each element corresponding to intercept for corresponding lambda
#' @return \code{error} Indicator of whether the algorithm terminated early because max.it was reached
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' lasso_fit = lasso.c(random_data$x, random_data$y, random_data$C.full, random_data$b)
#' lasso_fit$lambda
#' lasso_fit$error
#' ### The coefficients for the first lambda value
#' lasso_fit$coefs[1,]
#' ### Example of code where path is unable to be finished
#' ### (only one iteration), so both directions will be tried
#' lasso_err = lasso.c(random_data$x, random_data$y, random_data$C.full,
#' random_data$b, max.it = 1)
#' lasso_err$error
#' lasso_err$lambda

lasso.c <-
  function(x, y, C.full, b, l.min = -2, l.max = 6, step = 0.2,
           beta0 = NULL, verbose = F, max.it = 12, intercept = T,
           normalize = T, backwards = F){

    if (!backwards){
      fit = lars.c(x, y, C.full, b, l.min = l.min, l.max = l.max,
                   step = step, beta0 = beta0, verbose = verbose,
                   max.it = max.it, intercept = intercept,
                   normalize = normalize, forwards = T)
      if (fit$error | is.null(fit$coefs)){
        if (is.null(fit$lambda))
          fit$lambda = 10^l.min
        fit2 = lars.c(x, y, C.full, b, l.min = log10(max(fit$lambda)),
                      l.max = l.max, step = step, beta0 = beta0,
                      verbose = verbose, max.it = max.it,
                      intercept = intercept, normalize = normalize,
                      forwards = F)
        if (is.null(fit$coefs))
          fit$lambda = NULL
        fit$coefs = cbind(fit$coefs,fit2$coefs)
        fit$lambda = c(fit$lambda,fit2$lambda)
        fit$intercept = c(fit$intercept,fit2$intercept)
        fit$b2index = cbind(fit$b2index,fit2$b2index)
      }}
    else
      fit = lars.c(x, y, C.full, b, l.min = l.min, l.max = l.max,
                   step = step, beta0 = beta0, verbose = verbose,
                   max.it = max.it, intercept = intercept,
                   normalize = normalize, forwards = F)
    fit}

#' Initialize Linear Programming Fit (Equality Constraints)
#'
#' This function is called internally by \code{lars.c}
#'   to get the linear programming initial fit if the user requests
#'   implementation of the algorithm starting at the largest lambda
#'   value and proceeding backwards.

#' @param C.full complete constraint matrix C (with constraints of the form \code{C.full}*\code{beta}=\code{b})
#' @param b constraint vector b

#' @return \code{beta} the initial beta vector of coefficients to use for the PaC algorithm
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' lin_start = lin.int(random_data$C.full, random_data$b)
#' lin_start

lin.int <-
  function(C.full, b){

    p = ncol(C.full)
    Cmat = cbind(C.full,-C.full)
    cvec = rep(1,2*p)
    temp = limSolve::linp(E = Cmat,F = b,Cost = cvec)
    beta = temp$X[1:p]-temp$X[(p+1):(2*p)]
    beta

  }

#' Initialize Quadratic Programming Fit (Equality Constraints)
#'
#' This function is called internally by \code{lars.c}
#'   to get the quadratic programming fit if the user requests
#'   implementation of the algorithm starting at the smallest lambda
#'   value and proceeding forwards.

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param C.full complete constraint matrix C (with constraints of the form \code{C.full}*\code{beta}=\code{b})
#' @param b constraint vector b
#' @param lambda value of lambda
#' @param d very small diagonal term to allow for SVD (default 10^-7)

#' @return \code{beta} the initial beta vector of coefficients to use for the PaC algorithm
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' quad_start = quad.int(random_data$x, random_data$y, random_data$C.full,
#' random_data$b, lambda = 0.01)
#' quad_start

quad.int <-
  function(x, y, C.full, b, lambda, d=10^-7){

    Xstar = cbind(x,-x)
    p = ncol(x)
    Dmat = t(Xstar)%*%Xstar+d*diag(2*p)
    dvec = as.vector(t(Xstar)%*%y-lambda*rep(1,2*p))
    Cmat = cbind(t(cbind(C.full,-C.full)),diag(2*p))
    bvec = c(b,rep(0,2*p))
    temp = quadprog::solve.QP(Dmat, dvec, Cmat, bvec, meq = length(b))
    beta = temp$sol[1:p]-temp$sol[(p+1):(2*p)]
    beta

  }

#' Transform Data to Fit PaC Implementation (Equality Constraints)
#'
#' This function is called internally by \code{lars.c}
#'   to compute the transformed versions of the X, Y, and constraint
#'   matrix data, as shown in the PaC paper.

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param C.full complete constraint matrix C (with constraints of the form \code{C.full}*\code{beta}=\code{b})
#' @param b constraint vector b
#' @param lambda value of lambda
#' @param beta0 initial guess for beta coefficient vector
#' @param eps value close to zero used to verify SVD decomposition. Default is 10^-8

#' @return \code{x} transformed x data to be used in the PaC algorithm
#' @return \code{y} transformed y data to be used in the PaC algorithm
#' @return \code{Y_star} transformed Y* value to be used in the PaC algorithm
#' @return \code{a2} index of A used in the calculation of beta2 (the non-zero coefficients)
#' @return \code{beta1} beta1 values
#' @return \code{beta2} beta2 values
#' @return \code{C} constraint matrix
#' @return \code{C2} subset of constraint matrix corresponding to non-zero coefficients
#' @return \code{active.beta} index of non-zero coefficient values
#' @return \code{beta2.index} index of non-zero coefficient values
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' transform_fit = transformed(random_data$x, random_data$y, random_data$C.full,
#' random_data$b, lambda = 0.01, beta0 = rep(0,20))
#' dim(transform_fit$x)
#' head(transform_fit$y)
#' dim(transform_fit$C)
#' transform_fit$active.beta

transformed <-
  function(x, y, C.full, b, lambda, beta0, eps = 10^-8){

    p = ncol(x)
    m = nrow(C.full)
    n = nrow(x)
    beta.order = order(-abs(beta0))
    s = svd(C.full[,beta.order[1:m]])
    if (min(s$d)<eps){
      i = 1
      while (i <= m){
        s = svd(C.full[,beta.order[1:i]])
        if (min(s$d)<eps)
          beta.order = beta.order[-i]
        else
          i = i+1
      }
    }
    beta2.index = sort(beta.order[1:m])
    beta2 = beta0[beta2.index]
    beta1 = beta0[-beta2.index]
    s2 = sign(beta2)
    # Find C (split by A) and X split matrices

    A2 = C.full[,beta2.index]
    A1 = C.full[,-beta2.index]
    if(length(beta2.index)==1){A1 = t(A1)}
    A2.inv = solve(A2)
    a2 = A2.inv %*% A1
    X2 = x[,beta2.index]
    X1 = x[,-beta2.index]

    # Compute X_star, X_dash, Y_star, and Y_tilde as referenced in writeup

    X_star = X1 - (X2 %*% A2.inv %*% A1)
    Y_star = as.vector(y - (X2 %*% (A2.inv %*% b)))
    C2 = (t(A1)%*%(t(A2.inv)%*%s2))
    active.beta = sort(order(-abs(as.vector(t(X_star)%*%(X_star%*%beta1))-t(X_star)%*%Y_star-lambda*C2))[1:min(c(p-m,n))])
    s = svd(X_star[,active.beta])
    X_dash = s$u %*% diag(1/s$d) %*% t(s$v)
    C = X_dash %*% C2[active.beta]
    Y_til = Y_star + lambda*C

    list(x = X_star, y = Y_til, Y_star = Y_star, a2 = a2, beta1 = beta1,
         beta2 = beta2, C = C, C2 = C2, active.beta = active.beta,
         beta2.index = beta2.index)
  }

#' Constrained LARS Coefficient Function with Inequality Constraints
#'
#' This function computes the PaC constrained LASSO
#'   coefficient paths following the methodology laid out in the PaC
#'   paper but with inequality constraints. This function could be called directly as a standalone
#'   function, but the authors recommend using \code{lasso.ineq} for any
#'   implementation. This is because \code{lasso.ineq} has additional checks for
#'   errors across the coefficient paths and allows for users to go
#'   forwards and backwards through the paths if the paths are unable
#'   to compute in a particular direction for a particular run.

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param C.full complete inequality constraint matrix C (with inequality constraints of the form \code{C.full}*\code{beta} >= \code{b}))
#' @param b constraint vector b
#' @param l.min lowest value of lambda to consider (used as 10^\code{l.min}). Default is -2
#' @param l.max largest value of lambda to consider (used as 10^\code{l.max}). Default is 6
#' @param step step size increase in lambda attempted at each iteration (by a factor of 10^\code{step}). Default is 0.2
#' @param beta0 initial guess for \code{beta} coefficient vector. Default is NULL (indicating
#'         initial vector should be calculated by algorithm)
#' @param verbose should function print output at each iteration (TRUE) or not (FALSE). Default is FALSE
#' @param max.it maximum number of times step size is halved before the algorithm terminates and gives a warning. Default is 12
#' @param intercept should intercept be included in modeling (TRUE) or not (FALSE). Default is TRUE.
#' @param normalize should \code{x} data be normalized. Default is TRUE
#' @param forwards if \code{forwards} = F, then the algorithm starts at 10^\code{l.max} and
#'             moves backwards (without the forward step). If \code{forwards} = T,
#'             algorithm starts at 10^\code{l.min} and works forward. Default is FALSE

#' @return \code{coefs} A \code{p} by length(\code{lambda}) matrix with each column corresponding to the beta estimate for that lambda
#' @return \code{lambda} the grid of lambdas used to calculate the coefficients on the coefficient path
#' @return \code{intercept} vector with each element corresponding to intercept for corresponding lambda
#' @return \code{error} did the algorithm terminate due to too many iterations (TRUE or FALSE)
#' @return \code{b2index} the index of the \code{beta2} values identified by the algorithm at each lambda
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @import methods
#' @import penalized
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' lars_fit = lars.ineq(random_data$x, random_data$y, random_data$C.full, random_data$b)
#' lars_fit$lambda
#' lars_fit$error
#' ### The coefficients for the first lambda value
#' lars_fit$coefs[1,]
#' ### Example of code where path is unable to be finished
#' ### (only one iteration)
#' lars_err = lars.ineq(random_data$x, random_data$y, random_data$C.full,
#' random_data$b, max.it = 1)
#' lars_err$error
#' lars_err$lambda

lars.ineq <-
  function(x, y, C.full, b, l.min = -2, l.max = 6, step = 0.2,
           beta0 = NULL, verbose = F, max.it = 12, intercept = T,
           normalize = T, forwards = T){
    p=ncol(x)
    n=nrow(x)
    M=nrow(C.full)
    one <- rep(1, n)
    if (intercept) {
      meanx <- drop(one %*% x)/n
      x <- scale(x, meanx, FALSE)
      mu <- mean(y)
      y <- drop(y - mu)
    }
    else {
      meanx <- rep(0, p)
      mu <- 0
      y <- drop(y)
    }
    normx <- rep(1, p)
    if (normalize) {
      normx <- sqrt(n)*apply(x,2,stats::sd,na.rm=T)
      x <- scale(x, FALSE, normx)}
    C.full=t(t(C.full)/normx)
    if (!forwards){
      lambda=lambda.old=10^l.max
      step=-step
      if (is.null(beta0))
        beta0=lin.int.ineq(C.full,b)
    }
    else{
      lambda=lambda.old=10^l.min
      if (is.null(beta0))
        beta0=quad.int.ineq(x,y,C.full,b,lambda)
    }
    A.old=C.full
    x.old=x
    C.full=cbind(C.full,-diag(M))
    x=cbind(x,matrix(0,n,M))
    p=p+M
    beta.new=rep(0,p)
    step.orig=step
    coefs=grid=b2index=NULL
    t.data=transformed.ineq(x,y,C.full,b,lambda,beta0)
    beta1.old=t.data$beta1
    beta2.old=t.data$beta2
    iterations=1
    end.loop=F
    while (!end.loop & (iterations<=max.it)){
      iterations=1
      loop=T
      while (loop & (iterations<=max.it)){
        t.data$y=t.data$y+(lambda-lambda.old)*t.data$C
        beta1.new=rep(0,length(beta1.old))
        act=length(t.data$active)
        lambda.pen=rep(lambda,act)
        lambda.pen[t.data$delta1.index]=0
        positive.pen=rep(F,act)

        if(act>p){positive.pen[t.data$delta1.index]=T}
        fit.pen=penalized::penalized(t.data$y,t.data$x[,t.data$active],~0,lambda.pen,trace=F,positive=positive.pen,maxiter=10000,startbeta=beta1.old,epsilon=10^-10)
        beta1.new[t.data$active]=coef(fit.pen,"all")
        if (!attr(fit.pen,"converged"))
          print(paste("Warning: Maximum iterations exceeded in penalized fit."))
        beta2.new=beta2.old + t.data$a2 %*% (beta1.old - beta1.new)
        bad.beta2=(sum(abs(sign(t.data$beta2)-sign(beta2.new))) != 0)
        X_star=t.data$x
        derivs=abs(as.vector(t(X_star)%*%(X_star%*%beta1.new))-t(X_star)%*%t.data$Y_star-lambda*t.data$C2)
        bad.active=F
        if (n<(p-M))
          bad.active=(max(derivs[-t.data$active])>lambda)
        if (bad.beta2 | bad.active){
          t.data$y=t.data$y-(lambda-lambda.old)*t.data$C
          step=step/2
          lambda=lambda.old*10^step
          iterations=iterations+1}
        else
          loop=F
      }
      if (iterations<=max.it){
        if (verbose==T){
          print(paste("Lambda =",round(lambda,3)))
          if (abs(step)<abs(step.orig))
            print(paste("Step size reduced to ",step))
        }
        step=step.orig
        beta.new[t.data$beta2.index]=beta2.new
        beta.new[-t.data$beta2.index]=beta1.new
        coefs=cbind(coefs,beta.new)
        change.beta=(min(abs(beta2.new))<max(abs(beta1.new)))
        change.active=F
        if (n<(p-M))
          change.active=(min(derivs[t.data$active]) < max(derivs[-t.data$active]))
        if (change.beta | change.active){
          t.data=transformed.ineq(x,y,C.full,b,lambda,beta.new)
          beta1.new=t.data$beta1
          beta2.new=t.data$beta2}
        beta1.old=beta1.new
        beta2.old=beta2.new
        grid=c(grid,lambda)
        lambda.old=lambda
        lambda=lambda*10^step}
      if ((forwards & (lambda>10^l.max)) | (!forwards & (lambda<10^l.min)))
        end.loop=T
    }
    if (iterations>max.it)
      print(paste("Warning: Algorithm terminated at lambda =",round(lambda.old,1),": Maximum iterations exceeded."))
    colnames(coefs)=intercept=NULL
    if (!is.null(grid)){
      normx=c(normx,rep(1,M))
      coefs=coefs/normx
      coefs=coefs[,order(grid)]
      grid=sort(grid)
      meanx=c(meanx,rep(0,M))
      intercept=mu-drop(t(coefs)%*%meanx)}
    coefs=coefs[1:(p-M),]
    list(coefs=coefs,lambda=grid,intercept=intercept,error=(iterations>max.it))
  }

#' Initialize Quadratic Programming Fit with Inequality Constraints
#'
#' This function is called internally by \code{lars.ineq}
#'   to get the quadratic programming fit if the user requests
#'   implementation of the algorithm starting at the smallest lambda
#'   value and proceeding forwards.

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param C.full complete constraint matrix C (with inequality constraints of the form \code{C.full}*\code{beta} >= \code{b}))
#' @param b constraint vector b
#' @param lambda value of lambda
#' @param d very small diagonal term to allow for SVD (default 10^-7)

#' @return \code{beta} the initial beta vector of coefficients to use for the PaC algorithm
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' quad_start = quad.int.ineq(random_data$x, random_data$y,
#' random_data$C.full, random_data$b, lambda = 0.01)
#' quad_start

quad.int.ineq <-
  function(x,y,C.full,b,lambda,d=10^-5){
    Xstar=cbind(x,-x)
    p=ncol(x)
    Dmat=t(Xstar)%*%Xstar+d*diag(2*p)
    dvec=as.vector(t(Xstar)%*%y-lambda*rep(1,2*p))
    Amat=cbind(t(cbind(C.full,-C.full)),diag(2*p))
    bvec=c(b,rep(0,2*p))
    temp=quadprog::solve.QP(Dmat,dvec,Amat,bvec, meq=0)
    beta=temp$sol[1:p]-temp$sol[(p+1):(2*p)]
    beta=c(beta,as.vector(C.full%*%beta-b))
    beta
  }

#' Initialize Linear Programming Fit with Inequality Constraints
#'
#' This function is called internally by \code{lars.ineq}
#'   to get the linear programming initial fit if the user requests
#'   implementation of the algorithm starting at the largest lambda
#'   value and proceeding backwards.

#' @param C.full complete constraint matrix C (with inequality constraints of the form \code{C.full}*\code{beta} >= \code{b}))
#' @param b constraint vector b

#' @return \code{beta} the initial beta vector of coefficients to use for the PaC algorithm
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' lin_start = lin.int.ineq(random_data$C.full, random_data$b)
#' lin_start
#'

lin.int.ineq <-
  function(C.full,b){
    M=nrow(C.full)
    p=ncol(C.full)
    Amat=cbind(C.full,-C.full,-diag(M))
    cvec=c(rep(1,2*p),rep(0,M))
    temp=limSolve::linp(E=Amat,F=b,Cost=cvec)
    beta=c(temp$X[1:p]-temp$X[(p+1):(2*p)],temp$X[-(1:(2*p))])
    beta
  }

#' Complete Run of Constrained LASSO Path Function with Inequality Constraints
#'
#' This is a wrapper function for the \code{lars.c} PaC
#'   constrained Lasso function. \code{lasso.c} controls the overall path,
#'   providing checks for the path and allowing the user to control
#'   how the path is computed (and what to do in the case of a stopped path).

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param  C.full complete constraint matrix C (with inequality constraints of the form \code{C.full}*\code{beta} >= \code{b}))
#' @param  b constraint vector b
#' @param  l.min lowest value of lambda to consider (used as 10^\code{l.min}). Default is -2
#' @param  l.max largest value of lambda to consider (used as 10^\code{l.max}). Default is 6
#' @param  step step size increase in lambda attempted at each iteration (by a factor of 10^\code{step}). Default is 0.2
#' @param  beta0 initial guess for beta coefficient vector. Default is NULL (indicating
#'         initial vector should be calculated by algorithm)
#' @param  verbose should function print output at each iteration (TRUE) or not (FALSE). Default is FALSE
#' @param  max.it maximum number of times step size is halved before the algorithm terminates and gives a warning. Default is 12
#' @param  intercept should intercept be included in modeling (TRUE) or not (FALSE). Default is TRUE.
#' @param  normalize should X data be normalized. Default is TRUE
#' @param  backwards which direction should algorithm go, backwards from lambda = 10^\code{l.max} (TRUE)
#'             or forwards from 10^\code{l.max} and then backwards if algorithm gets stuck (FALSE).
#'             Default is FALSE.

#' @return \code{coefs} A \code{p} by length(\code{lambda}) matrix with each column corresponding to the beta estimate for that lambda
#' @return \code{lambda} vector of values of lambda that were fit
#' @return \code{intercept} vector with each element corresponding to intercept for corresponding lambda
#' @return \code{error} Indicator of whether the algorithm terminated early because max.it was reached
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' lasso_fit = lasso.ineq(random_data$x, random_data$y, random_data$C.full, random_data$b)
#' lasso_fit$lambda
#' lasso_fit$error
#' ### The coefficients for the first lambda value
#' lasso_fit$coefs[1,]
#' ### Example of code where path is unable to be finished
#' ### (only one iteration), so both directions will be tried
#' lasso_err = lasso.ineq(random_data$x, random_data$y, random_data$C.full,
#' random_data$b, max.it = 1)
#' lasso_err$error
#' lasso_err$lambda

lasso.ineq <-
  function(x,y,C.full,b,l.min=-2,l.max=6,step=.2,beta0=NULL,verbose=F,max.it=12,intercept=T,normalize=T,backwards=F){
    if (!backwards){
      fit=lars.ineq(x,y,C.full,b,l.min=l.min,l.max=l.max,step=step,beta0=beta0,verbose=verbose,max.it=max.it,intercept=intercept,normalize=normalize,forwards=T)
      if (fit$error | is.null(fit$coefs)){
        if (is.null(fit$lambda))
          fit$lambda=10^l.min
        fit2=lars.ineq(x,y,C.full,b,l.min=log10(max(fit$lambda)),l.max=l.max,step=step,beta0=beta0,verbose=verbose,max.it=max.it,intercept=intercept,normalize=normalize,forwards=F)
        if (is.null(fit$coefs))
          fit$lambda=NULL
        fit$coefs=cbind(fit$coefs,fit2$coefs)
        fit$lambda=c(fit$lambda,fit2$lambda)
        fit$intercept=c(fit$intercept,fit$intercept)
      }}
    else
      fit=lars.ineq(x,y,C.full,b,l.min=l.min,l.max=l.max,step=step,beta0=beta0,verbose=verbose,max.it=max.it,intercept=intercept,normalize=normalize,forwards=F)
    fit}

#' Transform Data to Fit PaC Implementation for Inequality Constraints
#'
#' This function is called internally by \code{lars.c}
#'   to compute the transformed versions of the X, Y, and constraint
#'   matrix data, as shown in the PaC paper.

#' @param x independent variable matrix of data to be used in calculating PaC coefficient paths
#' @param y response vector of data to be used in calculating PaC coefficient paths
#' @param C.full complete constraint matrix C (with inequality constraints of the form \code{C.full}*\code{beta} >= \code{b}))
#' @param b constraint vector b
#' @param lambda value of lambda
#' @param beta0 initial guess for beta coefficient vector
#' @param eps value close to zero used to verify SVD decomposition. Default is 10^-8

#' @return \code{x} transformed x data to be used in the PaC algorithm
#' @return \code{y} transformed y data to be used in the PaC algorithm
#' @return \code{Y_star} transformed Y* value to be used in the PaC algorithm
#' @return \code{a2} index of A used in the calculation of beta2 (the non-zero coefficients)
#' @return \code{beta1} beta1 values
#' @return \code{beta2} beta2 values
#' @return \code{C} constraint matrix
#' @return \code{C2} subset of constraint matrix corresponding to non-zero coefficients
#' @return \code{active.beta} index of non-zero coefficient values
#' @return \code{beta2.index} index of non-zero coefficient values
#' @references Gareth M. James, Courtney Paulson, and Paat Rusmevichientong (JASA, 2019) "Penalized and Constrained Optimization."
#' (Full text available at http://www-bcf.usc.edu/~gareth/research/PAC.pdf)
#' @export
#' @examples
#' random_data = generate.data(n = 500, p = 20, m = 10)
#' transform_fit = transformed.ineq(random_data$x, random_data$y,
#' random_data$C.full, random_data$b, lambda = 0.01, beta0 = rep(0,20))
#' dim(transform_fit$x)
#' head(transform_fit$y)
#' dim(transform_fit$C)
#' transform_fit$active.beta
#'

transformed.ineq <-
  function(x, y, C.full, b, lambda, beta0, eps=10^-8){
    p=ncol(x)
    m=nrow(C.full)
    n=nrow(x)
    delta=NULL
    beta.order=order(-abs(beta0))
    s=svd(C.full[,beta.order[1:m]])
    if (min(s$d)<eps){
      i=1
      while (i<=m){
        s=svd(C.full[,beta.order[1:i]])
        if (min(s$d)<eps)
          beta.order=beta.order[-i]
        else
          i=i+1
      }
    }
    beta2.index=sort(beta.order[1:m])
    beta1.index=(1:p)[-beta2.index]
    beta2=beta0[beta2.index]
    beta1=beta0[beta1.index]
    s2=sign(beta2)
    delta2.index=rep(F,m)
    delta2.index[beta2.index>(p-m)]=T
    delta1.index=rep(F,p-m)
    delta1.index[beta1.index>(p-m)]=T
    s2[delta2.index]=0

    # Find C and X split matrices

    A2=C.full[,beta2.index]
    A1=C.full[,-beta2.index]
    A2.inv=solve(A2)
    a2=A2.inv %*% A1
    X2=x[,beta2.index]
    X1=x[,-beta2.index]

    # Compute X_star, X_dash, Y_star, and Y_tilde as referenced in writeup

    X_star = X1 - (X2 %*% A2.inv %*% A1)
    Y_star = as.vector(y - (X2 %*% (A2.inv %*% b)))
    C2=(t(A1)%*%(t(A2.inv)%*%s2))
    active.beta=sort(order(-abs(as.vector(t(X_star)%*%(X_star%*%beta1))-t(X_star)%*%Y_star-lambda*C2))[1:min(c(p-m,n))])
    s=svd(X_star[,active.beta])
    X_dash = s$u %*% diag(1/s$d) %*% t(s$v)
    C=X_dash %*% C2[active.beta]
    Y_til = Y_star + lambda*C

    list(x=X_star,y=Y_til,Y_star=Y_star,a2=a2,beta1=beta1,beta2=beta2,C=C,C2=C2,active.beta=active.beta,beta2.index=beta2.index,delta1.index=delta1.index)
  }
