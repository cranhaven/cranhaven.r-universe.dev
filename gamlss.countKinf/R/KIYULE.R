#___________________________________________________________________________________________________________________
dKIYULE=
  function (x, mu = 1, sigma = 0.1, kinf=0, log = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0", "\n", ""))
    if (any(sigma <= 0) | any(sigma >= 1))
      stop(paste("sigma must be between 0 and 1", "\n", ""))
    if (any(x < 0))
      stop(paste("x must be 0 or greater than 0", "\n", ""))
    ly <- max(length(x), length(mu))
    x <- rep(x, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    inf<- rep(kinf, length = ly)
    fy <- dYULE(x, mu = mu, log=T)
    logfy <- rep(kinf, ly)
    logfy <- ifelse((x == inf), log(sigma + (1 - sigma) * exp(fy)), (log(1 -
                                                                           sigma) + fy))
    if (log == FALSE)
      fy <- exp(logfy)
    else fy <- logfy
    fy
  }
#___________________________________________________________________________________________________________________

pKIYULE=
  function (q, mu = 1, sigma = 0.1, kinf=0, lower.tail = TRUE, log.p = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0", "\n", ""))
    if (any(sigma <= 0) | any(sigma >= 1))
      stop(paste("sigma must be between 0 and 1", "\n", ""))
    if (any(q < 0))
      stop(paste("y must be 0 or greater than 0", "\n", ""))
    ly <- max(length(q), length(mu), length(sigma))
    q <- ifelse((q< 100), q, 100)
    q <- rep(q, length = ly)
    mu <- rep(mu, length = ly)
    sigma <- rep(sigma, length = ly)
    inf<- rep(kinf, length = ly)
    cdf <- rep(kinf, ly)
    cdf <- pYULE(q, mu = mu, lower.tail = TRUE, log.p = FALSE)
    cdf <- ifelse((q< inf), (1 - sigma) * cdf, sigma + (1 - sigma) * cdf)
    if (lower.tail == TRUE)
      cdf <- cdf
    else cdf <- 1 - cdf
    if (log.p == FALSE)
      cdf <- cdf
    else cdf <- log(cdf)
    cdf
  }

#___________________________________________________________________________________________________________________
qKIYULE=
  function (p, mu = 1, sigma = 0.1, kinf=0, lower.tail = TRUE, log.p = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0", "\n", ""))
    if (any(p <= 0) | any(p >= 1))
      stop(paste("p must be between 0 and 1", "\n", ""))
    if (log.p == TRUE)
      p <- exp(p)
    else p <- p
    if (lower.tail == TRUE)
      p <- p
    else p <- 1 - p
    ly <- max(length(p), length(mu), length(sigma))
    p <- rep(p, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    inf<- rep(kinf, length = ly)
    pnew <-ifelse(p>pKIYULE(max(inf-1,0), mu=mu, sigma=sigma, kinf=inf),
                  (p - sigma)/(1 - sigma)-(1e-07), p /(1 - sigma)-(1e-07))
    pnew <-ifelse( inf==0,(p - sigma)/(1 - sigma)-(1e-07), pnew)
    pnew <- ifelse((pnew > 0), pnew, 0)
    q <- qYULE(pnew, mu = mu)
    q
  }
#___________________________________________________________________________________________________________________

rKIYULE=
  function (n, mu = 1, sigma = 0.1, kinf=0)
  {
    if (any(mu <= 0))
      stop(paste("mu must greated than 0", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must greated than 0", "\n", ""))
    if (any(n <= 0))
      stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    r=c()
    for(i in 1:n){
      if(p[i]<sigma){r[i]=kinf}
      else{r[i] = rYULE(1, mu = mu)}
    }
    r
  }

#___________________________________________________________________________________________________________________
KIYULE=
  function (mu.link = "log", sigma.link = "logit", kinf="K")
  {
    mstats <- checklink(which.link = "mu.link", which.dist = "KIYULE",
                        link = substitute(mu.link), link.List = "log")
    dstats <- checklink("sigma.link", "KIYULE", substitute(sigma.link),
                        c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    structure(list(family = c(paste("inf",kinf,"YULE", sep = ""), paste(kinf,"-inflated Yule", sep = "") ),
                   parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
                   type = "Discrete", mu.link = as.character(substitute(mu.link)),
                   sigma.link = as.character(substitute(sigma.link)), mu.linkfun = mstats$linkfun,
                   sigma.linkfun = dstats$linkfun, mu.linkinv = mstats$linkinv,
                   sigma.linkinv = dstats$linkinv, mu.dr = mstats$mu.eta,
                   sigma.dr = dstats$mu.eta, dldm = function(y, mu, sigma) {
                     dldm0 <- (1 - sigma) * ((sigma + (1 - sigma) * dYULE(kinf, mu))
                                             ^(-1)) * dYULE(kinf, mu) * YULE()$dldm(kinf, mu)
                     dldm <- ifelse(y == kinf, dldm0, YULE()$dldm(y, mu))
                     dldm
                   }, d2ldm2 = function(y, mu, sigma) {
                     dldm0 <- (1 - sigma) * ((sigma + (1 - sigma) * dYULE(kinf, mu))
                                             ^(-1)) * dYULE(kinf, mu) * YULE()$dldm(kinf, mu)
                     dldm <- ifelse(y == kinf, dldm0, YULE()$dldm(y, mu))
                     d2ldm2 <- -dldm * dldm
                     d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2, -1e-15)
                     d2ldm2
                   }, dldd = function(y, mu, sigma) {
                     dldd0 <- ((sigma + (1 - sigma) * dYULE(kinf, mu))^(-1)) *
                       (1 - dYULE(kinf, mu))
                     dldd <- ifelse(y == kinf, dldd0, -1/(1 - sigma))
                     dldd
                   }, d2ldd2 = function(y, mu, sigma) {
                     dldd0 <- ((sigma + (1 - sigma) * dYULE(kinf, mu))^(-1)) *
                       (1 - dYULE(kinf, mu))
                     dldd <- ifelse(y == kinf, dldd0, -1/(1 - sigma))
                     d2ldd2 <- -dldd * dldd
                     d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2, -1e-15)
                     d2ldd2
                   }, d2ldmdd = function(y, mu, sigma) {
                     dldm0 <- (1 - sigma) * ((sigma + (1 - sigma) * dYULE(kinf, mu))
                                             ^(-1)) * dYULE(kinf, mu) * YULE()$dldm(kinf, mu)
                     dldm <- ifelse(y == kinf, dldm0, YULE()$dldm(y, mu))
                     dldd0 <- ((sigma + (1 - sigma) * dYULE(kinf, mu))^(-1)) *
                       (1 - dYULE(kinf, mu))
                     dldd <- ifelse(y == kinf, dldd0, -1/(1 - sigma))
                     d2ldmdd <- -dldm * dldd
                     d2ldmdd
                   }, G.dev.incr = function(y, mu, sigma, kinf, ...) -2 * dKIYULE(y,
                      mu, sigma, kinf=kinf, log = TRUE), rqres = expression(rqres(pfun = "pKIYULE",
                     type = "Discrete", ymin = 0, y = y, mu = mu, sigma = sigma, kinf=kinf)),
                     mu.initial = expression(mu <- rep(mean(y),length(y))),
                     sigma.initial = expression(sigma <- rep(0.5,length(y))), mu.valid = function(mu) all(mu > 0),
                     sigma.valid = function(sigma) all(sigma > 0 & sigma <1),
                     y.valid = function(y) all(y >= 0)),
              class = c("gamlss.family","family"))
  }
#___________________________________________________________________________________________________________________


