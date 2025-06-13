#___________________________________________________________________________________________________________________
dKIBNB=
  function (x, mu = 1, sigma = 1, nu = 1, tau = 0.1, kinf=0, log = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(tau <= 0) | any(tau >= 1))
      stop(paste("tau must be between 0 and 1 ", "\n", ""))
    if (any(x < 0))
      stop(paste("x must be >=0", "\n", ""))
    ly <- max(length(x), length(mu), length(sigma), length(nu),
              length(tau))
    x <- rep(x, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    nu <- rep(nu, length = ly)
    inf<- rep(kinf, length = ly)
    fy <- dBNB(x, mu = mu, sigma = sigma, nu = nu, log = T)
    logfy <- rep(kinf, length(x))
    logfy <- ifelse((x == inf), log(tau + (1 - tau) * exp(fy)),
                    (log(1 - tau) + fy))
    if (log == FALSE)
      fy2 <- exp(logfy)
    else fy2 <- logfy
    fy2
  }
#___________________________________________________________________________________________________________________


pKIBNB=
  function (q, mu = 1, sigma = 1, nu = 1, tau = 0.1, kinf=0, lower.tail = TRUE,
            log.p = FALSE)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(tau <= 0) | any(tau >= 1))
      stop(paste("tau must be between 0 and 1 ", "\n", ""))
    if (any(q < 0))
      stop(paste("y must be >=0", "\n", ""))
    ly <- max(length(q), length(mu), length(sigma), length(nu),
              length(tau))
    q <- rep(q, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    nu <- rep(nu, length = ly)
    inf<- rep(kinf, length = ly)
    cdf <- pBNB(q, mu = mu, sigma = sigma, nu = nu)
    cdf <- ifelse((q< inf), (1 - tau) * cdf, tau + (1 - tau) * cdf)
    if (lower.tail == TRUE)
      cdf <- cdf
    else cdf <- 1 - cdf
    if (log.p == FALSE)
      cdf <- cdf
    else cdf <- log(cdf)
    cdf
  }

#___________________________________________________________________________________________________________________

qKIBNB=
  function (p, mu = 1, sigma = 1, nu = 1, tau = 0.1, kinf=0, lower.tail = TRUE,
            log.p = FALSE, max.value = 10000)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(tau <= 0) | any(tau >= 1))
      stop(paste("tau must be between 0 and 1 ", "\n", ""))
    if (any(p < 0) | any(p > 1.0001))
      stop(paste("p must be between 0 and 1", "\n", ""))
    if (log.p == TRUE)
      p <- exp(p)
    else p <- p
    if (lower.tail == TRUE)
      p <- p
    else p <- 1 - p
    ly <- max(length(p), length(mu), length(sigma), length(nu),
              length(tau))
    p <- rep(p, length = ly)
    sigma <- rep(sigma, length = ly)
    mu <- rep(mu, length = ly)
    nu <- rep(nu, length = ly)
    tau <- rep(tau, length = ly)
    inf <- rep(kinf, length = ly)
    pnew <-ifelse(p>pKIBNB(max(inf-1,0), mu=mu, sigma=sigma, nu=nu, tau=tau, kinf=inf),
                  (p - tau)/(1 - tau)-(1e-07), p /(1 - tau)-(1e-07))
    pnew <-ifelse( inf==0,(p - tau)/(1 - tau)-(1e-07), pnew)
    pnew <- ifelse((pnew > 0), pnew, 0)
    q <- qBNB(pnew, mu = mu, sigma = sigma, nu, , max.value = max.value)
    q
  }
#___________________________________________________________________________________________________________________

rKIBNB=
  function (n, mu = 1, sigma = 1, nu = 1, tau = 0.1, kinf=0, max.value = 10000)
  {
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(tau <= 0) | any(tau >= 1))
      stop(paste("tau must be between 0 and 1 ", "\n", ""))
    if (any(n <= 0))
      stop(paste("n must be a positive integer", "\n", ""))
    n <- ceiling(n)
    p <- runif(n)
    r=c()
    for(i in 1:n){
      if(p[i]<tau){r[i]=kinf}
      else{r[i] = rBNB(1, mu = mu, sigma = sigma, nu = nu, max.value = max.value)}
    }
    r
  }

#___________________________________________________________________________________________________________________
KIBNB=
  function (mu.link = "log", sigma.link = "log", nu.link = "log",
            tau.link = "logit", kinf="K")
  {
    mstats <- checklink("mu.link", "KIBNB", substitute(mu.link),
                        c("1/mu^2", "log", "identity"))
    dstats <- checklink("sigma.link", "KIBNB", substitute(sigma.link),
                        c("inverse", "log", "identity"))
    vstats <- checklink("nu.link", "KIBNB", substitute(nu.link),
                        c("1/nu^2", "log", "identity"))
    tstats <- checklink("tau.link", "KIBNB", substitute(tau.link),
                        c("logit", "probit", "cloglog", "log", "own"))
    structure(list(family = c(paste("inf", kinf,"BNB", sep = ""),paste(kinf,"-inflated beta negative binomial", sep = "") ),
                   parameters = list(mu = TRUE,
                                     sigma = TRUE, nu = TRUE, tau = TRUE), nopar = 4, type = "Discrete",
                   mu.link = as.character(substitute(mu.link)), sigma.link = as.character(substitute(sigma.link)),
                   nu.link = as.character(substitute(nu.link)), tau.link = as.character(substitute(tau.link)),
                   mu.linkfun = mstats$linkfun, sigma.linkfun = dstats$linkfun,
                   nu.linkfun = vstats$linkfun, tau.linkfun = tstats$linkfun,
                   mu.linkinv = mstats$linkinv, sigma.linkinv = dstats$linkinv,
                   nu.linkinv = vstats$linkinv, tau.linkinv = tstats$linkinv,
                   mu.dr = mstats$mu.eta, sigma.dr = dstats$mu.eta, nu.dr = vstats$mu.eta,
                   tau.dr = tstats$mu.eta, dldm = function(y, mu, sigma,
                                                           nu, tau) {
                     dldm0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldm(kinf, mu, sigma, nu)
                     dldm <- ifelse(y == kinf, dldm0, BNB()$dldm(y, mu, sigma,
                                                                 nu))
                     dldm
                   }, d2ldm2 = function(y, mu, sigma, nu, tau) {
                     dldm0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldm(kinf, mu, sigma, nu)
                     dldm <- ifelse(y == kinf, dldm0, BNB()$dldm(y, mu, sigma,
                                                                 nu))
                     d2ldm2 <- -dldm * dldm
                     d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2, -1e-15)
                     d2ldm2
                   }, dldd = function(y, mu, sigma, nu, tau) {
                     dldd0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldd(kinf, mu, sigma, nu)
                     dldd <- ifelse(y == kinf, dldd0, BNB()$dldd(y, mu, sigma,
                                                                 nu))
                     dldd
                   }, d2ldd2 = function(y, mu, sigma, nu, tau) {
                     dldd0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldd(kinf, mu, sigma, nu)
                     dldd <- ifelse(y == kinf, dldd0, BNB()$dldd(y, mu, sigma,
                                                                 nu))
                     d2ldd2 <- -dldd * dldd
                     d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2, -1e-15)
                     d2ldd2
                   }, dldv = function(y, mu, sigma, nu, tau) {
                     dldv0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldv(kinf, mu, sigma, nu)
                     dldv <- ifelse(y == kinf, dldv0, BNB()$dldv(y, mu, sigma,
                                                                 nu))
                     dldv
                   }, d2ldv2 = function(y, mu, sigma, nu, tau) {
                     dldv0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldv(kinf, mu, sigma, nu)
                     dldv <- ifelse(y == kinf, dldv0, BNB()$dldv(y, mu, sigma,
                                                                 nu))
                     d2ldv2 <- -dldv * dldv
                     d2ldv2 <- ifelse(d2ldv2 < -1e-15, d2ldv2, -1e-15)
                     d2ldv2
                   }, dldt = function(y, mu, sigma, nu, tau) {
                     dldt0 <- ((tau + (1 - tau) * dBNB(kinf, mu, sigma, nu))^(-1)) *
                       (1 - dBNB(kinf, mu, sigma, nu))
                     dldt <- ifelse(y == kinf, dldt0, -1/(1 - tau))
                     dldt
                   }, d2ldt2 = function(y, mu, sigma, nu, tau) {
                     dldt0 <- ((tau + (1 - tau) * dBNB(kinf, mu, sigma, nu))^(-1)) *
                       (1 - dBNB(kinf, mu, sigma, nu))
                     dldt <- ifelse(y == kinf, dldt0, -1/(1 - tau))
                     d2ldt2 <- -dldt^2
                     d2ldt2 <- ifelse(d2ldt2 < -1e-15, d2ldt2, -1e-15)
                     d2ldt2
                   }, d2ldmdd = function(y, mu, sigma, nu, tau) {
                     dldm0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldm(kinf, mu, sigma, nu)
                     dldm <- ifelse(y == kinf, dldm0, BNB()$dldm(y, mu, sigma,
                                                                 nu))
                     dldd0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldd(kinf, mu, sigma, nu)
                     dldd <- ifelse(y == kinf, dldd0, BNB()$dldd(y, mu, sigma,
                                                                 nu))
                     d2ldmdd <- -dldm * dldd
                     d2ldmdd <- ifelse(d2ldmdd < -1e-15, d2ldmdd, -1e-15)
                     d2ldmdd
                   }, d2ldmdv = function(y, mu, sigma, nu, tau) {
                     dldm0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldm(kinf, mu, sigma, nu)
                     dldm <- ifelse(y == kinf, dldm0, BNB()$dldm(y, mu, sigma,
                                                                 nu))
                     dldv0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldv(kinf, mu, sigma, nu)
                     dldv <- ifelse(y == kinf, dldv0, BNB()$dldv(y, mu, sigma,
                                                                 nu))
                     d2ldmdv <- -dldm * dldv
                     d2ldmdv <- ifelse(d2ldmdv < -1e-15, d2ldmdv, -1e-15)
                     d2ldmdv
                   }, d2ldmdt = function(y, mu, sigma, nu, tau) {
                     dldm0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldm(kinf, mu, sigma, nu)
                     dldm <- ifelse(y == kinf, dldm0, BNB()$dldm(y, mu, sigma,
                                                                 nu))
                     dldt0 <- ((tau + (1 - tau) * dBNB(kinf, mu, sigma, nu))^(-1)) *
                       (1 - dBNB(kinf, mu, sigma, nu))
                     dldt <- ifelse(y == kinf, dldt0, -1/(1 - tau))
                     d2ldmdt <- -dldm * dldt
                     d2ldmdt <- ifelse(d2ldmdt < -1e-15, d2ldmdt, -1e-15)
                     d2ldmdt
                   }, d2ldddv = function(y, mu, sigma, nu, tau) {
                     dldd0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldd(kinf, mu, sigma, nu)
                     dldd <- ifelse(y == kinf, dldd0, BNB()$dldd(y, mu, sigma,
                                                                 nu))
                     dldv0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldv(kinf, mu, sigma, nu)
                     dldv <- ifelse(y == kinf, dldv0, BNB()$dldv(y, mu, sigma,
                                                                 nu))
                     d2ldddv <- -dldd * dldv
                     d2ldddv <- ifelse(d2ldddv < -1e-15, d2ldddv, -1e-15)
                     d2ldddv
                   }, d2ldddt = function(y, mu, sigma, nu, tau) {
                     dldd0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldd(kinf, mu, sigma, nu)
                     dldd <- ifelse(y == kinf, dldd0, BNB()$dldd(y, mu, sigma,
                                                                 nu))
                     dldt0 <- ((tau + (1 - tau) * dBNB(kinf, mu, sigma, nu))^(-1)) *
                       (1 - dBNB(kinf, mu, sigma, nu))
                     dldt <- ifelse(y == kinf, dldt0, -1/(1 - tau))
                     d2ldddt <- -dldd * dldt
                     d2ldddt <- ifelse(d2ldddt < -1e-15, d2ldddt, -1e-15)
                     d2ldddt
                   }, d2ldvdt = function(y, mu, sigma, nu, tau) {
                     dldv0 <- (1 - tau) * ((tau + (1 - tau) * dBNB(kinf,
                                                                   mu, sigma, nu))^(-1)) * dBNB(kinf, mu, sigma, nu) *
                       BNB()$dldv(kinf, mu, sigma, nu)
                     dldv <- ifelse(y == kinf, dldv0, BNB()$dldv(y, mu, sigma,
                                                                 nu))
                     dldt0 <- ((tau + (1 - tau) * dBNB(kinf, mu, sigma, nu))^(-1)) *
                       (1 - dBNB(kinf, mu, sigma, nu))
                     dldt <- ifelse(y == kinf, dldt0, -1/(1 - tau))
                     d2ldvdt <- -dldv * dldt
                     d2ldvdt <- ifelse(d2ldvdt < -1e-15, d2ldvdt, -1e-15)
                     d2ldvdt
                   },  G.dev.incr = function(y, mu, sigma, nu, tau, kinf, ...) -2 *
                     dKIBNB(y, mu, sigma, nu, tau, kinf=kinf, log = TRUE), rqres = expression(rqres(pfun = "pKIBNB",
                     type = "Discrete", ymin = 0, y = y, mu = mu, sigma = sigma,
                     nu = nu, tau = tau)), mu.initial = expression(mu <- (y +
                     mean(y))/2), sigma.initial = expression(sigma <- rep(max(((var(y) -
                     mean(y))/(mean(y)^2)), 0.1), length(y))), nu.initial = expression({
                     nu <- rep(0.1, length(y))}), tau.initial = expression({
                      tau <- rep(0.1, length(y))}), mu.valid = function(mu) all(mu > 0),
                     sigma.valid = function(sigma) all(sigma >0), nu.valid = function(nu) all(nu > 0),
                     tau.valid = function(tau) all(tau > 0 & tau < 1), y.valid = function(y) all(y >= 0)),
              class = c("gamlss.family", "family"))
  }
#___________________________________________________________________________________________________________________

