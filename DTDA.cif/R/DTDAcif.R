#' @title Doubly Truncated Data Analysis, Cumulative Incidence Functions
#' @aliases  DTDAcif
#' @description This function computes a nonparametric estimator of the cumulative incidences of competing risks under double truncation. The estimator generalizes the Efron-Petrosian NPMLE (Non-Parametric Maximun Likelihood Estimator) to the competing risks setting.
#'
#' @param x Numeric vector corresponding to the variable of ultimate interest.
#' @param u Numeric vector corresponding to the left truncation variable.
#' @param v Numeric vector corresponding to the right truncation variable.
#' @param comp.event Competing risk indicator.
#' @param method The method used to compute the nonparametric estimator. Use ‘indep’ for independent truncation variables and ``dep`` for truncation variables possibly depending on the competing risk.
#' @param boot Logical. If TRUE the bootstrap standard deviation of the cumulative incidences is calculated.
#' @param B Number of bootstrap replicates.
#' @param N.iter Maximum number of iterations.
#' @param error Error criterion for convergence.
#'
#' @details The nonparametric estimator is based on the Efron-Petrosian NPMLE (Efron and Petrosian, 1999).
#'  Actually, each pair (Xi,Zi) -where Xi stands for the variable of interest and Zi is the competing event
#'  indicator- is weighted by the jump of the Efron-Petrosian NPMLE at Xi (method=``indep"), or by a normalized
#'  version of the Efron-Petrosian NPMLE computed from the subset of (Xs,Zs)'s such that Zs=Zi (method=``dep'').
#'  The former is suitable when the truncating couple (U,V) is independent of (X,Z), while the latter is recommended
#'  when (U,V) and X are only conditionally independent given Z; see de Uña-Álvarez (2019) for a full description of
#'  the estimators and of their properties. When the competing event indicator is missing, the function simply computes
#'  the Efron-Petrosian NPMLE and the argument method has no role.
#
#' @return  A list containing:
#' \itemize{
#'  \item{method: }{The method used to compute the estimator.}
#'  \item{biasf: }{The biasing function which reports the sampling probability for each Xi.}
#'  \item{cif.mas: }{The mass attached to each (Xi,Zi). The cumsum of cif.mas for Zi=j is the estimator of the j-th cumulative incidence function.}
#'  \item{data: }{The data corresponding to (X,Z) ordered with respect to X within each Z-value.}
#'  \item{sd.boot: }{The bootstrap standard deviation.}
#' }
#'
#' @section Acknowledgements:
#' \itemize{
#' \item{Jacobo de Uña-Álvarez was supported by Grant MTM2017-89422-P (MINECO/AEI/FEDER, UE).}
#' \item{José Carlos Soage was supported by Grupos de Referencia Competitiva,
#'  Consolidación y Estructuración de Unidades de Investigación Competitivas del SUG,
#'  Cons. de Cultura, Educación e OU, Xunta de Galicia (GRC ED431C 2016/040).}
#' }
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#' @references
#' \itemize{
#' \item{de Uña-Álvarez, J. (2019). Nonparametric estimation of the cumulative incidences of competing risks under double truncation. Preprint.}
#' \item{Efron, B. and Petrosian, V. (1999). Nonparametric methods for doubly truncated data. Journal of the American Statistical Association 94, 824-834.}
#' }
#'
#' @useDynLib DTDA.cif
#' @examples
#'
#' \dontshow{
#'
#' set.seed(1234)
#' n <- 25  # sample size
#'
#' x <- runif(n, 0, 1)  # time variable of interest
#' z <- rbinom(n, 1, 1 / 4)   # competing event indicator
#'
#' # truncation variables
#'
#' u <- runif(n, -.25, .5)  # left truncation variable
#' v <- u + .75   # right truncation variable
#'
#' # note: (u,v) is independent of (x,z) so both estimation methods are consistent
#'
#' # truncating the sample:
#'
#' for (i in 1:n) {
#'   while (u[i] > x[i] | v[i] < x[i]) {
#'     x[i] <- runif(1, 0, 1)
#'     z[i] <- rbinom(1, 1, 1 / 4)
#'     u[i] <- runif(1, -.25, .5)
#'     v[i] <- u[i] + .75
#'   }
#' }
#'
#' # note: (u,v) since is independent of (x,z)
#' # both estimation methods are consistent:
#'
#' res.i <- DTDAcif(x, u, v, z, boot = TRUE)
#' }
#'
#' \donttest{
#' set.seed(1234)
#' n <- 50  # sample size
#'
#' x <- runif(n, 0, 1)  # time variable of interest
#' z <- rbinom(n, 1, 1 / 4)   # competing event indicator
#'
#' # truncation variables
#'
#' u <- runif(n, -.25, .5)  # left truncation variable
#' v <- u + .75   # right truncation variable
#'
#' # note: (u,v) is independent of (x,z) so both estimation methods are consistent
#'
#' # truncating the sample:
#'
#' for (i in 1:n) {
#'   while (u[i] > x[i] | v[i] < x[i]) {
#'     x[i] <- runif(1, 0, 1)
#'     z[i] <- rbinom(1, 1, 1 / 4)
#'     u[i] <- runif(1, -.25, .5)
#'     v[i] <- u[i] + .75
#'   }
#' }
#'
#' # note: (u,v) since is independent of (x,z)
#' # both estimation methods are consistent:
#'
#' res.i <- DTDAcif(x, u, v, z, method = "indep", boot = TRUE)
#' res.d <- DTDAcif(x, u, v, z, method = "dep", boot = TRUE)
#'
#' oldpar <- par(mfrow=c(1,2))
#' plot(res.i, main = "Indep trunc", intervals = TRUE)
#' plot(res.d, main = "Cond indep trunc", intervals = TRUE)
#'
#' summary(res.i)
#' summary(res.d)
#'
#' plot(res.i$data$x, res.i$biasf, type = "s")  # the observational bias
#' # the observational bias, event 1
#' plot(res.d$data$x[res.d$data$z == 1], res.d$biasf$biasf_1, type = "s")
#' # the observational bias, event 2
#' lines(res.d$data$x[res.d$data$z == 2], res.d$biasf$biasf_2, type = "s", col = 2)
#' par(oldpar)
#' }
#'
#' @import foreach Rcpp
#' @export
DTDAcif <- function(x, u, v, comp.event, method = c("indep", "dep"), boot = F, B = 300, N.iter = 100, error = 1e-06) {

  # cat("Call:", "\n")
  # match.call()

  if (any(!is.numeric(c(x, u, v)))) {
    stop("Arguments 'x', 'u' and 'v' must be numeric")
  }

  if (missing(x) && missing(u) && missing(v)){
    stop("Arguments 'x', 'u' and 'v' are missing, with no default")
  }

  if (missing(x) && missing(v)){
    stop("Arguments 'x' and 'v' are missing")
  }

  if(missing(method) && !missing(comp.event) && length(unique(comp.event)) != 1){
    method <- "dep"
    cat("'dep' method used by default", "\n")
  }

  n <- length(x)

  if(!missing(comp.event)){
    nz <- length(unique(comp.event))
  }

  len_un_z = FALSE

  if(!missing(comp.event)){
    if(length(unique(comp.event)) == 1){
      len_un_z = TRUE #stop("Length of comp.event must be greater than 1")
    }
  }


  if(missing(comp.event) || len_un_z == TRUE) {

    I <- matrix(sapply(1:n, function(i) as.numeric(u[i] <= x & x <= v[i])),
                nrow = n, ncol = n, byrow = TRUE)


    J <- matrix(sapply(1:n, function(i) as.numeric(u <= x[i] & x[i] <= v)),
                nrow = n, ncol = n, byrow = TRUE)


    Gn <- matrix(nrow = 2, ncol = n)
    Gn[1, ] <- rep(1 / n, n)

    res <- algoritmo(Gn = Gn, I = I, J = J, Niter = N.iter, error = error)
    w  <- 1 / res[order(x)]
    w <- w / sum(w)

    r <- list(biasf = res, cif.mas = w, data = data.frame(x = x[order(x)]))

    if(!missing(method)){
      warning("'method' is not necessary")
    }

  } else {

    z <- comp.event

    if(any(sort(unique(z)) != seq(1, length(unique(z)))) == TRUE){

      z <- as.numeric(factor(z))
    }

    if (is.character(z)) {
      z <- factor(z)
      z <- as.integer(z)
    }

    z <- z[order(x)]

  }

  # Order w.r.t. x:
  u <- u[order(x)]
  v <- v[order(x)]
  x <- x[order(x)]


  if(!missing(comp.event) && method == "dep") {

    x1   <- vector("list", length(unique(z)))
    u1   <- vector("list", length(unique(z)))
    v1   <- vector("list", length(unique(z)))
    res1 <- vector("list", length(unique(z)))
    w1   <- vector("list", length(unique(z)))

    for (i in  length(unique(z)):1) {

      x1[i]   <- list(x[z == i])
      u1[i]   <- list(u[z == i])
      v1[i]   <- list(v[z == i])
      res1[i] <- list(shen(unlist(x1[[i]]), unlist(u1[[i]]), unlist(v1[[i]])))
      w1[i]   <- list(1 / res1[[i]]$biasf)
    }

    ww1 <- vector("list", length(unique(z)))

    for(i in 1:length(unique(z))){
      ww1[i] <- list(unlist(w1[i]) / sum(sum(unlist(w1))))
    }

    biasf <- vector("list", length = length(unique(z)))
    biasf <- lapply(1:length(unique(z)), function (i) res1[[i]]$biasf)

    r <- list(method = method, biasf = biasf, cif.mas = ww1, data = data.frame(x = x, z = z))

    # List names
    # names(r$time) <- paste0("time_", 1:length(x1))
    names(r$biasf) <- paste0("biasf_", 1:length(ww1))
    names(r$cif.mas) <- paste0("cif.mas_", 1:length(ww1))
  }

  if(!missing(comp.event) && method == "indep") {
    I <- matrix(sapply(1:n, function(i) as.numeric(u[i] <= x & x <= v[i])),
                nrow = n, ncol = n, byrow = TRUE)


    J <- matrix(sapply(1:n, function(i) as.numeric(u <= x[i] & x[i] <= v)),
                nrow = n, ncol = n, byrow = TRUE)


    Gn <- matrix(nrow = 2, ncol = n)
    Gn[1, ] <- rep(1 / n, n)

    res <- algoritmo(Gn = Gn, I = I, J = J, Niter = N.iter, error = error)
    w <- 1 / res[order(x)]
    w <- w / sum(w)   # Weights for independence (biased estimator if (U,V) depends on Z)

    w0 <- vector("list", length(unique(z)))

    for(i in 1:length(unique(z))){
      w0[i] <- list(w * ifelse(z == i, 1, 0))
      w0[i] <- lapply(w0[i], function(x) {x[x!=0]})
    }

    r <- list(method = method, biasf = res,  cif.mas = w0, data = data.frame(x = x, z = z))

    # List names
    names(r$cif.mas) <- paste0("cif.mas_", 1:length(w0))
  }


  if(boot == TRUE) {

    ties <- ifelse(length(x) == length(unique(x)), FALSE, TRUE)

    if(missing(comp.event) || len_un_z == TRUE){

      data <- cbind(x, u , v)

      if(ties == FALSE){
        cifb <- matrix(0, B, length(x))
      } else {
        cifb <- matrix(0, B, length(unique(x)))
      }


      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)

      pointsb <- foreach::foreach(b = 1:B, .combine = rbind, .packages = c("doParallel", "foreach"), .noexport = "algoritmo") %dopar% {
        rem <- sample(1:length(x), length(x), replace = TRUE)
        rem <- data.frame(data[rem, ])

        I <- matrix(sapply(1:n, function(i) as.numeric(rem$u[i] <= rem$x & rem$x <= rem$v[i])),
                    nrow = n, ncol = n, byrow = TRUE)


        J <- matrix(sapply(1:n, function(i) as.numeric(rem$u <= rem$x[i] & rem$x[i] <= rem$v)),
                    nrow = n, ncol = n, byrow = TRUE)


        Gn <- matrix(nrow = 2, ncol = n)
        Gn[1, ] <- rep(1 / n, n)

        res <- algoritmo(Gn = Gn, I = I, J = J, Niter = N.iter, error = error)
        w <- 1 / res[order(x)]
        w <- w / sum(w)

        rb <- list(biasf = res, cif.mas = w)

        if(length(unique(rem[, 2])) == 1){
          rep <- which(data[, 1] == unique(rem$x))

          pointsb <- c(rep(min(cumsum(rb$cif)), rep - 1), rep(max(cumsum(rb$cif)), length(data[,1]) - length(rep(min(cumsum(rb$cif)), rep - 1))))

        } else {

          pointsb <- unlist(lapply(1:length(unique(x)),
                                   function(i) stats::approx(rem$x[order(rem$x)], cumsum(rb$cif[order(rem$x)]),
                                                             xout = unique(x[order(x)])[i],
                                                             ties = max, yleft = stats::approx(rem$x[order(rem$x)], cumsum(rb$cif[order(rem$x)]),
                                                                                               xout = min(rem$x[order(rem$x)]), ties = min)$y,
                                                             method = "constant", rule = 2)$y))

        }

        return (pointsb)
      }
      parallel::stopCluster(cl)

      sd.boot <- apply(pointsb, 2, stats::sd)
      r <- c(r, sd.boot = list(sd.boot))

    } else {

      data <- cbind(x, z, u , v)

      if(method == "indep"){

        cifb <- vector("list", length = nz)

        cifb <- lapply(1:nz, function(i) cifb[[i]] <- matrix(0, B, length(which(unlist(r$cif.mas[i]) != 0))))

        if(ties == TRUE){
          cifb  <- lapply(1:nz, function(i) cifb[[i]] <- matrix(0, B, length(unique(x[z == i]))))
        }

        pb <- utils::txtProgressBar(min = 0, max = B , style = 3)

        for(b in 1:B) {
          utils::setTxtProgressBar(pb, b)
          rem <- sample(1:n, n, replace = TRUE)
          rem <- data.frame(data[rem, ])

          while(stats::var(rem$z) == 0){
            rem <- sample(1:n, n, replace = TRUE)
            rem <- data.frame(data[rem, ])
          }

          I <- matrix(sapply(1:n, function(i) as.numeric(rem$u[i] <= rem$x & rem$x <= rem$v[i])),
                      nrow = n, ncol = n, byrow = TRUE)


          J <- matrix(sapply(1:n, function(i) as.numeric(rem$u <= rem$x[i] & rem$x[i] <= rem$v)),
                      nrow = n, ncol = n, byrow = TRUE)


          Gn <- matrix(nrow = 2, ncol = n)
          Gn[1, ] <- rep(1 / n, n)

          res <- algoritmo(Gn = Gn, I = I, J = J, Niter = N.iter, error = error)
          w <- 1 / res[order(x)]
          w <- w / sum(w)   # Weights for independence (biased estimator if (U, V) depends on Z)

          w0 <- vector("list", nz)

          for(i in 1:nz) {
            w0[i] <- list(w * ifelse(rem$z == i, 1, 0))
          }

          rb <- list(method = "indep", time = rem$x,  cif.mas = w0)

          # plot(rb$time, rb$cif)

          # List names
          names(rb$cif.mas) <- paste0("cif_", 1:length(w0))

          pointsb <- vector("list", nz)



            if(length(unique(rem[, 1])) == 1) {
              pointsb <- max(cumsum(unlist(rb$cif.mas)))
            }else{
              # pointsb <- lapply(1:nz, function(j) unlist(lapply(1:length(unique(subset(data.frame(data),
              #                                                                          data.frame(data)$z == j))$x),
              #                                                   function(i) stats::approx(unlist(unlist(rb$time))[order(unlist(unlist(rb$time)))],
              #                                                                             cumsum(unlist(rb$cif[[j]][order(rb$time)])),
              #                                                                             xout = unique(subset(data.frame(data),
              #                                                                                                  data.frame(data)$z == j)$x[order(subset(data.frame(data), data.frame(data)$z == j)$x)])[i],
              #                                                                             ties = max,
              #                                                                             yleft = stats::approx(rem$x[order(rem$x)],
              #                                                                                                   cumsum(unlist(rb$cif[[j]][order(rb$time)])),
              #                                                                                                   xout = min(rem$x[order(rem$x)]),
              #                                                                                                   ties = min)$y,
              #                                                                             method = "constant", rule = 2)$y)))

              pointsb <- lapply(1:nz, function(j) sapply(1:length(unique(subset(data.frame(data),
                                                                                data.frame(data)$z == j))$x),
                                                         function(i) stats::approx(rem$x[order(rem$x)],
                                                                                   cumsum(unlist(rb$cif.mas[[j]])),
                                                                                   xout = unique(r$data$x[r$data$z == j])[i],
                                                                                   ties = max,
                                                                                   yleft = min(cumsum(unlist(rb$cif.mas[[j]]))),
                                                                                   yright = max(cumsum(unlist(rb$cif.mas[[j]]))),
                                                                                   method = "constant", rule = 2)$y))
            }


            # for(w in 1:nz){
            #   pointsb[[w]] <- pointsb[[w]]
            # }

            for(w in 1:nz){

              cifb[[w]][b,] <- stats::na.omit(pointsb[[w]])

            }
          }




        #####################################


        sd.boot <- vector("list", nz)


        for(k in 1:nz){
          sd.boot[[k]] <- apply(cifb[[k]], 2, stats::sd)  # Esto lo devuelve la función en res$sd.boot
        }

        r <- c(r, sd.boot = list(sd.boot))
      }


      if(method == "dep"){

        x   <- vector("list", length(unique(r$data$z)))

        for (i in  length(unique(r$data$z)):1) {

          x[i] <- list(unique(r$data$x[r$data$z == i]))

        }

        cifb <- vector("list", length = nz)

        for(i in 1:nz){
          cifb[[i]] <- matrix(NA, B, length(unlist(r$cif.mas[i])))
        }

        if(ties == TRUE){
          for(i in 1:nz){

            cifb[[i]] <- matrix(NA, B, length(unique(unlist(unlist(x[i])))))

          }
        }

        pb <- utils::txtProgressBar(min = 0, max = B , style = 3)

        for (b in 1:B) {
          utils::setTxtProgressBar(pb, b)
          rem <- sample(1:n, n, replace = TRUE)
          rem <- data.frame(data[rem, ])

          while(stats::var(rem$z) == 0){
            rem <- sample(1:n, n, replace = TRUE)
            rem <- data.frame(data[rem, ])
          }

          lurz <- length(unique(rem$z))
          x1   <- vector("list", lurz)
          u1   <- vector("list", lurz)
          v1   <- vector("list", lurz)
          res1 <- vector("list", lurz)
          w1   <- vector("list", lurz)

          for (i in  nz:1) {
            x1[i] <- list(rem$x[rem$z == i])
            u1[i] <- list(rem$u[rem$z == i])
            v1[i] <- list(rem$v[rem$z == i])
            res1[i] <- list(shen(unlist(x1[[i]]), unlist(u1[[i]]), unlist(v1[[i]])))
            w1[i] <- list(1 / res1[[i]]$biasf)
          }

          ww1 <- vector("list", nz)

          for(i in 1:nz){

            ww1[i] <- list(unlist(w1[i]) / sum(sum(unlist(w1))))
          }


          rb <- list(method = "dep", time = x1, cif = ww1)

          # List names
          names(rb$time) <- paste0("time_", 1:length(x1))
          names(rb$cif) <- paste0("cif_", 1:length(ww1))

          pointsb <- vector("list", nz)

          #if(length(unique(rem[, 1])) == 1 | any(lapply(1:nz, function(i) length(unique(unlist(rb$time[[i]])))) == 1)) {
          # num <- which(lapply(1:nz, function(i) length(unique(unlist(rb$time[[i]])))) == 1)

          #  pointsb[[num]] <- rep(max(cumsum(unlist(rb$cif))), ncol(cifb[[num]]))
          #
          #for(i in 1:length(pointsb)){
              #   if(i != num){
                pointsb[[i]] <- rep(0, ncol(cifb[[i]]))
                #  }
                # }

                # pointsb[[i]] <- rep(max(cumsum(unlist(rb$cif))), ncol(cifb[[num]]))
            #} else{

            pointsb <-  lapply(1:nz, function(j) unlist(lapply(1:length(unique(unlist(x[j]))),
                                                               function(i) stats::approx(unlist(rb$time[j])[order(unlist(rb$time[j]))],
                                                                                         cumsum(unlist(rb$cif[[j]])),
                                                                                         xout = unique(unlist(x[j])[order(unlist(x[j]))])[i],
                                                                                         ties = max,
                                                                                         yleft = min(cumsum(unlist(rb$cif[[j]]))),
                                                                                         yright = max(cumsum(unlist(rb$cif[[j]]))),
                                                                                         method = "constant", rule = 2)$y)))

          #}

          for(w in 1:nz){

            cifb[[w]][b, c(1:length(pointsb[[w]]))] <- pointsb[[w]]
          }
        }

        sd.boot <- vector("list", nz)

        for(k in 1:nz) {
          sd.boot[[k]] <- apply(cifb[[k]], 2, stats::sd, na.rm = TRUE)
          sd.boot[[k]] <- stats::na.omit(sd.boot[[k]])

        }

        r <- c(r, sd.boot = list(sd.boot))

      }
    }
  }

  class(r) <- c('list', 'DTDAcif')
  return(r)
}

