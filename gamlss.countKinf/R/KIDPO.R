
#___________________________________________________________________________________________________________________
dKIDPO=
  function (x, mu = 1, sigma = 1, nu = 0.3, kinf=0 ,log = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(nu <= 0) | any(nu >= 1))
      stop(paste("nu must be between 0 and 1 ", "\n", ""))
    if (any(x < 0))
      stop(paste("x must be >=0", "\n", ""))
    ly <- max(length(x), length(mu), length(sigma), length(nu))
    x <- rep(x, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    nu <- rep(nu, length = ly)
    inf<- rep(kinf, length = ly)
    fy <- dDPO(x, mu = mu, sigma = sigma, log = T)
    logfy <- rep(inf, length(x))
    logfy <- ifelse((x == inf), log(nu + (1 - nu) * exp(fy)), (log(1 -
                                                                     nu) + fy))
    if (log == FALSE)
      fy2 <- exp(logfy)
    else fy2 <- logfy
    fy2
  }
#___________________________________________________________________________________________________________________

pKIDPO=
  function (q, mu = 1, sigma = 1, nu = 0.3, kinf=0, lower.tail = TRUE,
            log.p = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(nu <= 0) | any(nu >= 1))
      stop(paste("nu must be between 0 and 1 ", "\n", ""))
    if (any(q < 0))
      stop(paste("y must be >=0", "\n", ""))
    ly <- max(length(q), length(mu), length(sigma), length(nu))
    q <- rep(q, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    nu <- rep(nu, length = ly)
    inf<- rep(kinf, length = ly)
    cdf <- pDPO(q, mu = mu, sigma = sigma)
    cdf <- ifelse((q< inf), (1 - nu) * cdf, nu + (1 - nu) * cdf)
    if (lower.tail == TRUE)
      cdf <- cdf
    else cdf <- 1 - cdf
    if (log.p == FALSE)
      cdf <- cdf
    else cdf <- log(cdf)
    cdf
  }
#___________________________________________________________________________________________________________________

qKIDPO=
  function (p, mu = 1, sigma = 1, nu = 0.3, kinf=0, lower.tail = TRUE,
            log.p = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(nu <= 0) | any(nu >= 1))
      stop(paste("nu must be between 0 and 1 ", "\n", ""))
    if (any(p < 0) | any(p > 1))
      stop(paste("p must be between 0 and 1", "\n", ""))
    if (log.p == TRUE)
      p <- exp(p)
    else p <- p
    if (lower.tail == TRUE)
      p <- p
    else p <- 1 - p
    ly <- max(length(p), length(mu), length(sigma), length(nu))
    p <- rep(p, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    nu <- rep(nu, length = ly)
    inf<- rep(kinf, length = ly)
    pnew <-ifelse(p>pKIDPO(max(inf-1,0), mu=mu, sigma=sigma, nu=nu, kinf=inf),
                  (p - nu)/(1 - nu)-(1e-07), p /(1 - nu)-(1e-07))
    pnew <-ifelse( inf==0,(p - nu)/(1 - nu)-(1e-07), pnew)
    pnew <- ifelse((pnew > 0), pnew, 0)
    q <- qDPO(pnew, mu = mu, sigma = sigma)
    q
  }
#___________________________________________________________________________________________________________________

rKIDPO=
  function (n, mu = 1, sigma = 1, nu = 0.3, kinf=0)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(nu <= 0) | any(nu >= 1))
      stop(paste("nu must be between 0 and 1 ", "\n", ""))
    if (any(n <= 0))
      stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    r=c()
    for(i in 1:n){
      if(p[i]<nu){r[i]=kinf}
      else{r[i] = rDPO(1, mu = mu, sigma = sigma)}
    }
    r
  }

#___________________________________________________________________________________________________________________

KIDPO=
  function (mu.link = "log", sigma.link = "log", nu.link = "logit", kinf="K")
  {
    mstats <- checklink("mu.link", "kIDPO", substitute(mu.link),
                        c("inverse", "log", "identity"))
    dstats <- checklink("sigma.link", "kIDPO", substitute(sigma.link),
                        c("inverse", "log", "identity"))
    vstats <- checklink("nu.link", "kIDPO", substitute(nu.link),
                        c("logit", "probit", "cloglog", "log", "own"))
    structure(list(family = c(paste("inf",kinf,"DPO", sep = ""),paste(kinf,"-inflated Double Poisson", sep = "") ),
                   parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE),
                   nopar = 3, type = "Discrete", mu.link = as.character(substitute(mu.link)),
                   sigma.link = as.character(substitute(sigma.link)), nu.link = as.character(substitute(nu.link)),
                   mu.linkfun = mstats$linkfun, sigma.linkfun = dstats$linkfun,
                   nu.linkfun = vstats$linkfun, mu.linkinv = mstats$linkinv,
                   sigma.linkinv = dstats$linkinv, nu.linkinv = vstats$linkinv,
                   mu.dr = mstats$mu.eta, sigma.dr = dstats$mu.eta, nu.dr = vstats$mu.eta,
                   dldm = function(y, mu, sigma, nu) {
                     dldm0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) * DPO()$dldm(kinf,
                                                                                                                   mu, sigma)
                     dldm <- ifelse(y == kinf, dldm0, DPO()$dldm(y, mu, sigma))
                     dldm
                   }, d2ldm2 = function(y, mu, sigma, nu) {
                     dldm0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) *DPO()$dldm(kinf,
                                                                                                                  mu, sigma)
                     dldm <- ifelse(y == kinf, dldm0, DPO()$dldm(y, mu, sigma))
                     d2ldm2 <- -dldm * dldm
                     d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2, -1e-15)
                     d2ldm2
                   }, dldd = function(y, mu, sigma, nu) {
                     dldd0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) *DPO()$dldd(kinf,
                                                                                                                  mu, sigma)
                     dldd <- ifelse(y == kinf, dldd0, DPO()$dldd(y, mu, sigma))
                     dldd
                   }, d2ldd2 = function(y, mu, sigma, nu) {
                     dldd0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) * DPO()$dldd(kinf,
                                                                                                                   mu, sigma)
                     dldd <- ifelse(y == kinf, dldd0, DPO()$dldd(y, mu, sigma))
                     d2ldd2 <- -dldd^2
                     d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2, -1e-15)
                     d2ldd2
                   }, dldv = function(y, mu, sigma, nu) {
                     dldv0 <- ((nu + (1 - nu) * dDPO(kinf, mu, sigma))^(-1)) *
                       (1 - dDPO(kinf, mu, sigma))
                     dldv <- ifelse(y == kinf, dldv0, -1/(1 - nu))
                     dldv
                   }, d2ldv2 = function(y, mu, sigma, nu) {
                     dldv0 <- ((nu + (1 - nu) * dDPO(kinf, mu, sigma))^(-1)) *
                       (1 - dDPO(kinf, mu, sigma))
                     dldv <- ifelse(y == kinf, dldv0, -1/(1 - nu))
                     d2ldv2 <- -dldv^2
                     d2ldv2 <- ifelse(d2ldv2 < -1e-15, d2ldv2, -1e-15)
                     d2ldv2
                   }, d2ldmdd = function(y, mu, sigma, nu) {
                     dldm0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) * DPO()$dldm(kinf,
                                                                                                                   mu, sigma)
                     dldm <- ifelse(y == kinf, dldm0, DPO()$dldm(y, mu, sigma))
                     dldd0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) * DPO()$dldd(kinf,
                                                                                                                   mu, sigma)
                     dldd <- ifelse(y == kinf, dldd0, DPO()$dldd(y, mu, sigma))
                     d2ldm2 <- -dldm * dldd
                     d2ldm2
                   }, d2ldmdv = function(y, mu, sigma, nu) {
                     dldm0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) * DPO()$dldm(kinf,
                                                                                                                   mu, sigma)
                     dldm <- ifelse(y == kinf, dldm0, DPO()$dldm(y, mu, sigma))
                     dldv0 <- ((nu + (1 - nu) * dDPO(kinf, mu, sigma))^(-1)) *
                       (1 - dDPO(kinf, mu, sigma))
                     dldv <- ifelse(y == kinf, dldv0, -1/(1 - nu))
                     d2ldmdv <- -dldm * dldv
                     d2ldmdv
                   }, d2ldddv = function(y, mu, sigma, nu) {
                     dldd0 <- (1 - nu) * ((nu + (1 - nu) * dDPO(kinf, mu,
                                                                sigma))^(-1)) * dDPO(kinf, mu, sigma) * DPO()$dldd(kinf,
                                                                                                                   mu, sigma)
                     dldd <- ifelse(y == kinf, dldd0, DPO()$dldd(y, mu, sigma))
                     dldv0 <- ((nu + (1 - nu) * dDPO(kinf, mu, sigma))^(-1)) *
                       (1 - dDPO(kinf, mu, sigma))
                     dldv <- ifelse(y == kinf, dldv0, -1/(1 - nu))
                     d2ldddv <- -dldd * dldv
                     d2ldddv
                   }, G.dev.incr = function(y, mu, sigma, nu, kinf, ...) -2 *
                     dKIDPO(y, mu = mu, sigma = sigma, nu = nu, kinf=kinf, log = TRUE),
                   rqres = expression(rqres(pfun = "pKIDPO", type = "Discrete",
                                            ymin = 0, y = y, mu = mu, sigma = sigma, nu = nu)),
                   mu.initial = expression(mu <- (y + mean(y))/2), sigma.initial = expression(sigma <- rep(1,
                           length(y))), nu.initial = expression(nu <- rep(((sum(y ==
                           kinf)/length(y)) + 0.01)/2, length(y))), mu.valid = function(mu) all(mu >0),
                           sigma.valid = function(sigma) all(sigma > 0), nu.valid = function(nu) all(nu > 0 & nu < 1),
                           y.valid = function(y) all(y >= 0)),
                  class = c("gamlss.family", "family"))
  }
#___________________________________________________________________________________________________________________
