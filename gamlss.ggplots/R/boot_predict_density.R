################################################################################
################################################################################
################################################################################
################################################################################
# thore two function takes a fitted gamlss model and a set 
# of bootstrap simulations  and generates a predictive distribution 
# or an empirical cdf for  for observation i
# the functions are  untested and there are here for future 
# exploration 
################################################################################
################################################################################
################################################################################
################################################################################ 
boot_predict_density <- function(model, boot, 
                            obs = 1, 
                       hist.col = "black", 
                      hist.fill = "white", 
                      dens.fill = "#FF6666", 
                           title, 
                              N = 100, # this multiply the B 
                           hist = FALSE,
                            pdf = FALSE,
                            from = 0,
                            to = 100, 
                            ... )
{
  rqres <- y <-  NULL
# checking if model and boot are missing  
if (missing(model) && missing(boot)) 
     stop("A GAMLSS fitted object and a bootstrap object should be used")
# checking if model is a GAMLSS model
if (!missing(model) && !is.gamlss(model)) 
    stop("the model is not a gamlss model")
# checking if BayesianBoot() or NonParametricBoot() functions are used
if  (!(is(boot, "Bayesian.boot") || is(boot, "NonParametric.Boot")) )
   stop("A bootstrap GAMLSS object should be used")
# checking if the original call for boot is the same as the model
if (!identical(boot$orig.call , model$call))
  stop("The bootstrap call is different from GAMLSS call")
################################################################ 
# get distribution
gamlss.bi.list <- .binom
      Family <- if (is.null(model$call$family)) "NO" else as.character(model$call$family)
         fam <- as.gamlss.family(Family)  # this is created so I can get things
      dorfun <- paste("r",Family,sep="")  # say rNO
       nopar <- fam$nopar                 # or fam1$nopar
        type <- fam$type
   par.names <- names(fam$parameters)
par.bt.names <- names(boot$param)
if (!identical(par.names , par.bt.names))
  stop("The bootstrap distribution paraeter names are not maching the original fit")
#################################################################
# check the observations
lobs <- length(obs)
if (lobs>1) stop("at the moment only one observation is accepted")
#################################################################
# get the simulated parameters
PY <- switch(nopar,
   eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]])),
   eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]], sigma=boot$param$sigma[obs[1]])),
   eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]], sigma=boot$param$sigma[obs[1]], nu=boot$param$nu[obs[1]])),
   eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]], sigma=boot$param$sigma[obs[1]], nu=boot$param$nu[obs[1]], tau=boot$param$tau[obs[1]]))
       )
        d <- data.frame(y=PY)
txt.title <- if (missing(title)) 
  paste("prediction density of obs", deparse(substitute(obs)))
else title

if (pdf==TRUE)
{
  dd <- fitted_pdf_data(model,obs, from=from, to=to)
  gg <- dd +    geom_density(data=d,  aes(x = y), alpha = 0.2, fill = dens.fill)+
                ggtitle(txt.title)
  return(gg)
} else
{
 gg <- if (hist)  y_hist(PY, title=txt.title)
       else ggplot2::ggplot(d, ggplot2::aes(x = y)) + 
   ggplot2::geom_density(alpha = 0.2, fill = dens.fill) + xlab("response") + 
   ggplot2::ylab("predictive density") + 
   ggplot2::ggtitle(txt.title)
}  
 gg
}
#################################################################################
#################################################################################
#################################################################################
#################################################################################
boot_predict_ecdf <- function( model, boot, 
                               obs = 1, 
                               hist.col = "black", 
                               hist.fill = "white", 
                               dens.fill = "#FF6666", 
                               title, 
                               N = 100, # this multiply the B 
                               ... )
{
  rqres  <- NULL
  # checking if model and boot are missing  
  if (missing(model) && missing(boot)) 
    stop("A GAMLSS fitted object and a bootstrap object should be used")
  # checking if model is a GAMLSS model
  if (!missing(model) && !is.gamlss(model)) 
    stop("the model is not a gamlss model")
  # checking if BayesianBoot() or NonParametricBoot() functions are used
  if  (!(is(boot, "Bayesian.boot") || is(boot, "NonParametric.Boot")) )
    stop("A bootstrap GAMLSS object should be used")
  # checking if the original call for boot is the same as the model
  if (!identical(boot$orig.call , model$call))
    stop("The bootstrap call is different from GAMLSS call")
  ################################################################ 
  # get distribution
  gamlss.bi.list <- .binom
  Family <- if (is.null(model$call$family)) "NO" else as.character(model$call$family)
  fam <- as.gamlss.family(Family)  # this is created so I can get things
  dorfun <- paste("r",Family,sep="")  # say rNO
  nopar <- fam$nopar                 # or fam1$nopar
  type <- fam$type
  par.names <- names(fam$parameters)
  par.bt.names <- names(boot$param)
  if (!identical(par.names , par.bt.names))
    stop("The bootstrap distribution paraeter names are not maching the original fit")
  #################################################################
  # check the observations
  lobs <- length(obs)
  if (lobs>1) stop("at the moment only one observation is accepted")
  #################################################################
  # get the simulated parameters
  
  PY <- switch(nopar,
               eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]])),
               eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]], sigma=boot$param$sigma[obs[1]])),
               eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]], sigma=boot$param$sigma[obs[1]], nu=boot$param$nu[obs[1]])),
               eval(call(dorfun, boot$B*N, mu=boot$param$mu[obs[1]], sigma=boot$param$sigma[obs[1]], nu=boot$param$nu[obs[1]], tau=boot$param$tau[obs[1]]))
  )
  d <- data.frame(y=PY)
  txt.title <- if (missing(title)) 
    paste("prediction density of obs", deparse(substitute(obs)))
  else title
  y_ecdf(PY, title=txt.title)
}
################################################################################
################################################################################
################################################################################
################################################################################