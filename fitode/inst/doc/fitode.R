## ----opts,echo=FALSE----------------------------------------------------------
library("knitr")
## DON'T cache=TRUE or warning=FALSE globally
## DO set error=FALSE so that errors are not caught
##  (except temporarily for debugging purposes)
opts_chunk$set(fig.width=6,fig.height=4,error=FALSE)
knit_hooks$set(basefig=function(before, options, envir) {
                   if (before) {
                       oldpar <- par(bty="l",las=1)
                       on.exit(par(oldpar))
                   } else { }
               })

## ----pkgversions, echo=FALSE--------------------------------------------------
used.pkgs <- c("bbmle", "Deriv", "deSolve", "fitode", "ggplot2")
pkgver <- vapply(sort(used.pkgs),function(x) as.character(packageVersion(x)),"")
print(pkgver,quote=FALSE)

## ----SierraLeonedata, message=FALSE, warning=FALSE, basefig=TRUE--------------
library(ggplot2); theme_set(theme_bw())
library(fitode)
plot(SierraLeone2014)

## ----expmodel-----------------------------------------------------------------
exp_model <- odemodel(
    name="exponential",
    model=list(
        X ~ r * X
    ),
    observation=list(
        confirmed ~ dpois(lambda=X)
    ),
    initial=list(
        X ~ X0
    ),
    par=c("r", "X0")
)

## ----expsetup,basefig=TRUE----------------------------------------------------
start <- c(r=7, X0=30)
ss <- simulate(exp_model, parms=start, times=SierraLeone2014$times)
plot(SierraLeone2014)
lines(X ~ times, data=ss)
abline(v=2014.8, col="red", lty=2)

## ----expfit-------------------------------------------------------------------
exp_fit <- fitode(
    model=exp_model,
    data=subset(SierraLeone2014, times <= 2014.8),
    start=start
)

## ----printexp-----------------------------------------------------------------
exp_fit

## ----expfit2------------------------------------------------------------------
mlefit <- bbmle::mle2(confirmed ~ dpois(X0*exp(r*(times-times[1]))),
                      data=subset(SierraLeone2014, times <= 2014.8),
                      start=as.list(start))
coef(mlefit)

## ----ciexp--------------------------------------------------------------------
confint(exp_fit)

## ----plotexp,basefig=TRUE-----------------------------------------------------
plot(exp_fit, level=0.95)

## ----nbupdate-----------------------------------------------------------------
exp_fit_nbinom <- update(
    exp_fit,
    observation=list(
        confirmed ~ dnbinom(mu=X, size=phi)
    ),
    par=c("r", "X0", "phi"),
    start=c(start, phi=10)
)

## ----nbupdate2----------------------------------------------------------------
exp_model_nbinom <-
  update(exp_model,
         name="exponential (nbinom)",
         observation=list(
           confirmed ~ dnbinom(mu=X, size=phi)
         ),
         par=c("r", "X0", "phi")
         )

exp_fit_nbinom2 <- fitode(
    model=exp_model_nbinom,
    data=SierraLeone2014[SierraLeone2014$times <+ 2014.8,],
    start=c(start, phi=10)
)

## ----plotnb,basefig=TRUE------------------------------------------------------
plot(exp_fit_nbinom, level=0.95)

## ----cinb---------------------------------------------------------------------
confint(exp_fit_nbinom)

## ----cumsumplot,basefig=TRUE--------------------------------------------------
plot(cumsum(confirmed) ~ times, data=SierraLeone2014)

## ----logistmodel--------------------------------------------------------------
logistic_model <- update(
  exp_model_nbinom,
  name="logistic (nbinom)",
  model=list(
    X ~ r * X * (1 - X/K)
  ),
  diffnames="X",
  par=c("r", "X0", "K", "phi")
)

## ----addinit------------------------------------------------------------------
SierraLeone2014b <- rbind(
    c(times=SierraLeone2014$times[1] -
          diff(SierraLeone2014$times)[1], confirmed=NA),
    SierraLeone2014
)

## ----logistsetup,warning=FALSE,basefig=TRUE-----------------------------------
start_logistic <-
    c(coef(exp_fit_nbinom), K=sum(SierraLeone2014$confirmed))
## need to use a different value for X0
start_logistic[["X0"]] <- 300
ss_logistic <- simulate(
    logistic_model,
    parms=start_logistic,
    times=SierraLeone2014b$times
)

plot(SierraLeone2014)
lines(X~times, data=ss_logistic)

## ----logistfit,warning=FALSE--------------------------------------------------
logistic_fit <- fitode(
    logistic_model,
    data=SierraLeone2014b,
    start=start_logistic
)

## ----logistci-----------------------------------------------------------------
confint(logistic_fit)

## ----logistplot,basefig=TRUE--------------------------------------------------
plot(logistic_fit, level=0.95)

## ----mafit,warning=FALSE------------------------------------------------------
ma_begin <- 1
ma_end <- which.max(SierraLeone2014b$confirmed) + 1

logistic_fit_ma <- update(
    logistic_fit,
    data=SierraLeone2014b[ma_begin:ma_end,]
)

## ----maplot,basefig=TRUE------------------------------------------------------
plot(logistic_fit, level=0.95)
plot(logistic_fit_ma, level=0.95, add=TRUE, col.traj="red", col.conf="red")

## ----maci---------------------------------------------------------------------
confint(logistic_fit_ma)

## ----sirmodel-----------------------------------------------------------------
SIR_model <- odemodel(
    name="SIR (nbinom)",
    model=list(
        S ~ - beta * S * I/N,
        I ~ beta * S * I/N - gamma * I,
        R ~ gamma * I
    ),
    observation=list(
        confirmed ~ dnbinom(mu=R, size=phi)
    ),
    initial=list(
        S ~ N * (1 - i0),
        I ~ N * i0,
        R ~ 0
    ),
    diffnames="R",
    par=c("beta", "gamma", "N", "i0", "phi"),
    link=c(i0="logit")
)

## ----sirstart,warning=FALSE,basefig=TRUE--------------------------------------
SIR_start <- c(beta=70, gamma=60, N=40000, i0=0.0004, phi=6)

ss_SIR <- simulate(SIR_model,
    parms=SIR_start, times=SierraLeone2014b$times)

plot(SierraLeone2014)
lines(ss_SIR$times, ss_SIR$R)

## ----sirfitplot,basefig=TRUE--------------------------------------------------
plot(SIR_fit, level=0.95)

## ----plot_sirbfit,basefig=TRUE------------------------------------------------
plot(SIR_fit_b, level=0.95)

## ----cisirb-------------------------------------------------------------------
confint(SIR_fit_b, parm=list(r~beta-gamma))

## ----fitsummary,echo=FALSE,fig.cap="Comparison of growth rate estimates"------
fit_summ <- data.frame(
    fits=c("exponential\n(poisson)", "exponential\n(nbinom)",
           "logistic\n(full)",
           "logistic\n(window)", "SIR\n(full)", "SIR\n(window)"),
    estimate=c(coef(exp_fit)[1], coef(exp_fit_nbinom)[1],
               coef(logistic_fit)[1], coef(logistic_fit_ma)[1],
               -diff(coef(SIR_fit)[1:2]),
               -diff(coef(SIR_fit_b)[1:2])),
    lwr=c(confint(exp_fit)[1,2], confint(exp_fit_nbinom)[1,2],
          confint(logistic_fit)[1,2],
          confint(logistic_fit_ma)[1,2],
          confint(SIR_fit, parm=list(r~beta-gamma))[2],
          confint(SIR_fit_b, parm=list(r~beta-gamma))[2]),
    upr=c(confint(exp_fit)[1,3], confint(exp_fit_nbinom)[1,3],
          confint(logistic_fit)[1,3],
          confint(logistic_fit_ma)[1,3],
          confint(SIR_fit, parm=list(r~beta-gamma))[3],
          confint(SIR_fit_b, parm=list(r~beta-gamma))[3])
)

fit_summ$fits <- factor(fit_summ$fits, level=fit_summ$fits)

print(ggplot(fit_summ)
      + geom_pointrange(aes(fits, estimate, ymin=lwr, ymax=upr))
      + labs(y="Initial epidemic growth rate")
      + coord_flip()
      )

## ----haredata-----------------------------------------------------------------
## FIXME: store these data locally
hare <- read.csv("https://raw.githubusercontent.com/stan-dev/example-models/master/knitr/lotka-volterra/hudson-bay-lynx-hare.csv", skip=2)
plot(Hare~Year, data=hare, type="l")
lines(Lynx~Year, data=hare, type="l", col=2)

## ----lvmodel------------------------------------------------------------------
lotka_model <- odemodel(
    name="Lotka Volterra model",
    model=list(
        u ~ alpha * u - beta * u * v,
        v ~ delta * u * v - gamma * v
    ),
    observation=list(
        Hare ~ dnbinom(mu=u, size=size1),
        Lynx ~ dnbinom(mu=v, size=size2)
    ),
    initial=list(
        u ~ u0,
        v ~ v0
    ),
    par=c("alpha", "beta", "delta", "gamma", "u0", "v0", "size1", "size2")
)

## ----lvstart,warning=FALSE----------------------------------------------------
harestart <- c(alpha=0.55, beta=0.028, delta=0.026, gamma=0.84, u0=30, v0=10,
               size1=1, size2=1)
harefit <- fitode(lotka_model, data=hare,
                  start=harestart,
                  tcol="Year")
plot(harefit, level=0.95)

## ----lvcoef-------------------------------------------------------------------
coef(harefit)

## ----lvpoiss,warning=FALSE----------------------------------------------------
## FIXME: we need this (fancy stuff with filling in all
## of the links as log) now because I'm checking links more carefully
## is there a way around this?
poisson_pars <- setdiff(lotka_model@par, c("size1", "size2"))
harefit_poisson <- update(
    harefit,
    observation=list(
        Hare ~ dpois(lambda=u),
        Lynx ~ dpois(lambda=v)
    ),
    link=setNames(rep("log",length(poisson_pars)),poisson_pars),
    par=poisson_pars
)

## ----lvpoissfit,basefig=TRUE--------------------------------------------------
plot(harefit_poisson, level=0.95)

## ----lvnbci,warning=FALSE-----------------------------------------------------
confint(harefit,
        parm=c("size1","size2"),
        method="profile",
        std.err=1,
        tol.newmin=0.1)

