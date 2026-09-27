#$Author: sinnwell $
#$Date: 2013/01/14 19:32:24 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/haplo.glm.q,v 1.22 2013/01/14 19:32:24 sinnwell Exp $
#$Locker:  $
#$Log: haplo.glm.q,v $
#Revision 1.22  2013/01/14 19:32:24  sinnwell
#add haplo.binomial, no need glm.fit.nowarn
#
#Revision 1.21  2012/02/22 14:59:44  sinnwell
#make assigning of names to var.mat allow for when haplo.elim=NA
#
#Revision 1.20  2012/02/03 21:36:59  sinnwell
#fix small bug in haplo.glm in names for var.mat
#
#Revision 1.19  2011/11/23 20:34:02  sinnwell
#release 1.4.81, updates with test scripts
#
#Revision 1.18  2011/11/10 15:29:40  sinnwell
#major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
#
#Revision 1.17  2008/04/14 19:46:26  sinnwell
#make exception if yxmiss not added in na.geno.keep b/c no NAs
#
#Revision 1.16  2008/03/24 21:22:25  sinnwell
#rm allele.lev, miss.val parameters, re-work model.frame code, yet leave old ode there.
#return missing info from na.geno.keep
#
#Revision 1.15  2007/02/01 20:25:29  sinnwell
#starting values for eta taken out of fit.null and updated correctly
#at each step of while loop
#
#Revision 1.14  2006/10/25 15:06:17  sinnwell
#rm Matrix library dependency, only done in Ginv.q
#
#Revision 1.13  2006/01/27 16:30:19  sinnwell
#depends on Matrix library
#
#Revision 1.12  2005/01/25 23:04:06  sinnwell
#use as.list(args(haplo.glm)) instead of formals when na.action not given
#formals is for R only
#
#Revision 1.11  2005/01/25 22:25:22  sinnwell
#take out print statements left from debugging
#
#Revision 1.10  2005/01/23 20:01:38  sinnwell
#use Thomas Lumley hint from R-help, use formals of haplo.glm to set
#default na.action to na.geno.keep.
#
#Revision 1.9  2005/01/19 16:14:39  sinnwell
#default na.action not set to na.geno.keep, force it if missing(na.action)
#m$na.action <- as.name("na.geno.keep")
#
#Revision 1.8  2005/01/04 17:37:22  sinnwell
#set haplo.min.freq as a function of haplo.min.count
#using a min expected count of haplotypes is better criteria
#
#Revision 1.7  2004/03/19 15:01:48  sinnwell
#consider .C(PACKAGE= as part of '...'
#
#Revision 1.6  2004/03/17 21:11:12  sinnwell
#separate calls of .C("groupsum" for R and Splus
#
#Revision 1.5  2004/03/03 22:13:16  schaid
#added allele.lev to pass allele labels, to allow haplo.glm to work in R for character alleles
#
#Revision 1.4  2004/01/12 14:46:57  sinnwell
#get around warnings for binomial trait in R
#
#Revision 1.3  2003/12/08 19:37:09  sinnwell
#changed F,T to FALSE,TRUE
#
#Revision 1.2  2003/11/17 23:27:20  schaid
#made compatible with R
#
#Revision 1.1  2003/09/16 16:01:54  schaid
#Initial revision
#

haplo.glm  <- function(formula = formula(data),
                       family = gaussian, 
                       data = parent.frame(),
                       weights,
                       na.action="na.geno.keep",
                       start=NULL,
                       locus.label=NA,
                       control = haplo.glm.control(),
                       method = "glm.fit",
                       model = TRUE,
                       x = FALSE,
                       y = TRUE,
                       contrasts = NULL,
                       ...) {
  
  call <- match.call()

  # Create a model.frame call with formula elements only 
  frame.call <- call('model.frame', formula=formula)

  # Steps to assign args to model.frame call, Terry Therneau 7/2005
  # assign desired parameters from Call to frame.call
  k=length(frame.call)

  ## because R doesn't automatically name model.frame column names
  ## let k keep track of index of last element of frame.call,
  ## and assign the name (only when using R)

  for(i in c('data', 'weights', 'subset', 'na.action', 'drop.unused.levels'))
    {
      if(!is.null(call[[i]])) {
        frame.call[[i]] <- call[[i]]
        k <- k+1
        names(frame.call)[k]=i
      }

    }

  # if no drop.unused.levels or na.action given, assign them
  if(is.null(frame.call$drop.unused.levels))
    frame.call$drop.unused.levels <- TRUE
  if(is.null(frame.call$na.action))
    frame.call$na.action=as.list(args(haplo.glm))$na.action
  
  # evaluate the call
  m <- eval(frame.call, parent.frame())

  Terms <- attr(m, "terms")

  # save missing value information from model.frame, na.action
  if(!is.null(attributes(m)$yxmiss) & !is.null(attributes(m)$gmiss)) 
     missing <- data.frame(yxmiss=attributes(m)$yxmiss, genomiss=attributes(m)$gmiss)
  
  # set design matrix contrasts
  contrasts.old <- options()$contrasts
  options(contrasts = c(factor="contr.treatment", ordered = "contr.poly"))
  on.exit(options(contrasts=contrasts.old))

  # Extract  weights per subject
  wt.subj <- model.weights(m)    

  if(!length(wt.subj))
    wt.subj <- rep(1, nrow(m))
  else if(any(wt.subj < 0))
    stop("negative weights not allowed")


  # for control, base minimum haplotype frequencies on haplo.min.count
  # as precedent over haplo.freq.min, the min count is 5
  if(is.na(control$haplo.freq.min))
    control$haplo.freq.min <- control$haplo.min.count/(2*nrow(m))

  #### Modify model.frame 
  ## Translate from unphased genotype matrix to haplotype design matrix
  ## Use haplo.model.frame to modify the model.frame, which 
  ## calls haplo.em to estimate haplotype frequencies.
  
  haplo.mf <- haplo.model.frame(m, locus.label=locus.label, control=control)

  
  # Extract the model.frame created by haplo.model.frame
  m <- haplo.mf$m.frame

  if(method == "model.frame")
    return(m)

  #### From haplo.model.frame
  ## g.dat is a dataframe with indx.subj, hap1, hap2, where hap1 and hap2 are
  ##   numeric codes for haplotypes
  ## haplo.rare.term, logical for if there is a rare haplotype term
  g.dat           <- haplo.mf$g.dat
  haplo.unique    <- haplo.mf$haplo.unique
  haplo.base      <- haplo.mf$haplo.base
  haplo.freq  <- haplo.freq.init <- haplo.mf$haplo.freq
  haplo.common    <- haplo.mf$haplo.common
  haplo.rare.term <- haplo.mf$haplo.rare.term
  haplo.rare      <- haplo.mf$haplo.rare
 
  ## Set up integer grouping variables (indices) for group sums

  haplo.group <- c(g.dat$hap1,g.dat$hap2)
  len.haplo.group <- length(haplo.group)
  n.haplo.group <- length(haplo.freq)
  subj.indx <- as.numeric(factor(g.dat$indx.subj))
  len.subj.indx <- length(subj.indx)
  n.subj <- length(unique(subj.indx))
 
  ## Setup weights after expanding each subject into pairs of haplotypes
  ## (i.e., possibly replicate weights according to replicates required
  ## for enumerating pairs of haplotypes)

  wt.expanded <- wt.subj[subj.indx]

  ## Compute priors and posteriors of pairs of haplotypes, given observed data.

  prior.coef <- ifelse(g.dat$hap1!=g.dat$hap2, 2, 1)
  prior <- prior.coef * haplo.freq[g.dat$hap1] * haplo.freq[g.dat$hap2]

  pr.pheno.init <- tapply(prior,g.dat$indx.subj,sum)
  nreps <- tapply(g.dat$indx.subj,g.dat$indx.subj,length)
  den <- rep(pr.pheno.init,nreps)
  post <- prior/den
  
 ## Keep initial post to return in haplo.post.info
  post.initial <- post
  
      
  # Keep count of true number of haplotypes for later use as denom 
  # of haplotype frequenices 

  n.hap <- 2 * sum(wt.subj)

  # First E-step: create new weights based on post. 

  # Note that these weights are based on post, which was computed assuming
  # that there are no regression terms (i.e., all regression
  # beta's = 0).

  # wt.expanded is the original input weight vector, wt.subj, 
  # (or set to 1's if not input),
  # after enumeration of all possible pairs of haplos.
  # Need to distinguish between wt.expanded and
  # working weights, which are wght.expanded * post

  w <- wt.expanded * post

  xvars <- as.character(attr(Terms, "variables"))

 
  xvars <- xvars[-1]
  if ((yvar <- attr(Terms, "response")) > 0) 
    xvars <- xvars[-yvar]
 

  if(length(xvars) > 0) {
    xlevels <- lapply(m[xvars], levels)
    xlevels <- xlevels[!sapply(xlevels, is.null)]
    if(length(xlevels) == 0)
      xlevels <- NULL
  }
  else xlevels <- NULL

  a <- attributes(m)

  Y <- model.response(m, "numeric")

  X <- model.matrix(Terms, m, contrasts)

  start <- model.extract(m, start)
  
 
  offset <- model.offset(m)
 
  if (is.character(family)) 
    family <- get(family)
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("`family' not recognized")
  }

  ## init for binomial does not allow non-integer fitted values
  ## use haplo.binomial init expression
  if(family$family =="binomial") {
    family$initialize=haplo.binomial()$initialize
  }
  

  if(missing(method))
    method <- attr(family, "method")
  if(!is.null(method)) {
    if(!existsFunction(method))
      stop(paste("unimplemented method:", method))
  }
  else method <- "glm.fit"

  glm.fitter <- get(method)
   
  # Unlike the generic glm, we compute the Null model (Intercept-only) 
  # here,  instead of within the EM loop (to avoid recomputing the null
  # within each loop iteration).

  ## switch fitter for binomial because it gives unneeded warnings
  #if(family$family=="binomial") glm.fitter=glm.fit.nowarn
  ## FIXED WITH THE INIT FIX ABOVE

  
  ## get the null fit
  fit.null <- glm.fitter(x = X[, "(Intercept)", drop = FALSE],
                y = Y, 
                weights = w, 
                etastart = start, 
                offset = offset, 
                family = family, 
                control=glm.control(maxit = control$maxit,
                  epsilon = control$epsilon))

  dfit <- dglm.fit(fit.null)

  prior <- prior*dfit
  pr.pheno <- tapply(prior, subj.indx, sum)
  lnlike.old <- lnlike.null <- sum(wt.subj*log(pr.pheno))

  
  ################
  ### EM loop
  ################
  
  ## Set up arrays for group sums within EM loop

  prior.tot <- rep(0,n.subj)
  post.tot <- rep(0,n.haplo.group)

  converge.em <- FALSE

  # need a separate control.epsilon for EM, because need more
  # stringent check for change in lnLike for EM than for 
  # glm.fit

  control.epislon.em <- min(c(control$epsilon,  0.000001))
  iter <- 0

  while(iter < control$maxit){

    iter <- iter + 1

   # M-step for regression beta's, using weights that depend on
   # earlier haplotype freqs and beta's

    fit <- glm.fitter(x = X,
                  y = Y, 
                  weights = w, 
                  etastart = start,
                  offset = offset, 
                  family = family, 
                  control=glm.control(maxit = control$maxit,
                           epsilon = control$epsilon))

    # Using new beta's, but earlier haplotype frequencies, update post, and 
    # compute lnLike

    dfit <- dglm.fit(fit)
    prior <- prior.coef * haplo.freq[g.dat$hap1] * haplo.freq[g.dat$hap2] * dfit

    ## For the following, tapply was originally used, 
    ## as pr.pheno <- tapply(prior,subj.indx, sum), but this took too much
    ## time within this EM loop, so a C function 'groupsum' is used instead.

    tmp.sum <- .C("groupsum",
                  x=as.double(prior),
                  indx=as.integer(subj.indx),
                  n=as.integer(len.subj.indx),
                  grouptot= as.double(prior.tot),
                  ngroup=as.integer(n.subj),
                  PACKAGE="haplo.stats")

    pr.pheno <- tmp.sum$grouptot
    den <- rep(pr.pheno,nreps)
    post <- prior/den
    
    lnlike.new <- sum(wt.subj*log(pr.pheno))
    
    ## Create new weights based on post. These posteriors depend
    ## on the ith iteration of beta's, but the (i-1)th iteration of
    ## haplo.freq

    w <- wt.expanded * post

    ## M-step for haplotype frequencies, based on expected counts

    tmp.sum <- .C("groupsum",
                  x=as.double(c(post*wt.expanded, post*wt.expanded)),
                  indx=as.integer(haplo.group),
                  n=as.integer(len.haplo.group),
                  grouptot= as.double(post.tot),
                  ngroup=as.integer(n.haplo.group),
                  PACKAGE="haplo.stats")

    e.hap.count <- tmp.sum$grouptot
    haplo.freq <- e.hap.count/n.hap

    ## convergence checks
    if(abs(lnlike.new - lnlike.old) < control.epislon.em){
      converge.em <- TRUE
      break
    }

    ## update starting values for eta, and update lnlike
    start <- X%*%fit$coeff
    lnlike.old <- lnlike.new
    
  }

 
  if(!converge.em) warning("Failed to converge during EM-glm loop")

  lnlike.final <- lnlike.new

  fit$iter <- iter
  fit$lnlike  <- lnlike.final
  fit$lnlike.null <-  lnlike.null
  fit$lrt <- list(lrt = 2*(lnlike.final - lnlike.null), df = fit$rank-1 )

  
  ## Re-compute deviance and aic from collapsed fitted values
  ## 9/8/11 -JPS
  dev.resids <- family$dev.resids
  aic <- family$aic

  Y.collapsed <- tapply(Y, subj.indx, FUN=function(x) x[1])
  
  mu.collapsed <- tapply(fit$fitted.values*wt.expanded*post, subj.indx,sum)

  #fit$deviance <- sum(dev.resids(Y.collapsed, mu.collapsed, wt.subj))
  fit$deviance <- sum(dev.resids(Y, fit$fitted.values, wt.expanded*post))

  fit$aic <- aic(Y.collapsed, length(Y.collapsed), mu.collapsed, wt.subj,
                  fit$deviance) + 2*fit$rank
  
  ## item added for anova method, update 9/8/11 by JPS
  fit$method <- method

  ## Assign Expanded wts from wt.subj to prior.weights 
  ## Non-expanded prior wts get from tapply(wt.subj, subj.indx, x[1])
  fit$prior.weights <- wt.expanded

  if(!is.null(xlevels))
    attr(fit, "xlevels") <- xlevels
  fit$terms <- Terms
  fit$formula <- as.vector(attr(Terms, "formula"))
  fit$call <- call
  
  fit$control <- control
  if(!is.null(attr(m, "na.action")))
    fit$na.action <- attr(m, "na.action")

  fit$haplo.unique <- haplo.unique
  fit$haplo.base <- haplo.base
  fit$haplo.freq <- haplo.freq
  fit$haplo.freq.init <- haplo.freq.init
  fit$converge.em <- converge.em
  fit$haplo.common <- haplo.common
  fit$haplo.rare <- haplo.rare
  fit$haplo.rare.term <- haplo.rare.term
  fit$haplo.names <- haplo.mf$haplo.names

  ## data.frame for info on posteriors
  haplo.post.info <- cbind(g.dat, post.initial, post)
  names(haplo.post.info) <- c("indx", "hap1", "hap2", "post.init", "post")
  fit$haplo.post.info <- haplo.post.info

  ## estimation of the information and variance matrix.
  ## requires X matrix, so attach to fit, and de-attach
  ## later if not desired to be returned
  
  fit$x <- X

  infoLouis <- louis.info(fit, epsilon=control$eps.svd)
  fit$info <- infoLouis$info
  fit$var.mat <- infoLouis$var.mat
  fit$haplo.elim <- infoLouis$haplo.elim
  fit$rank.info <- infoLouis$rank

  elim.common <- which(fit$haplo.common %in%fit$haplo.elim)
  coeffnames <- names(fit$coefficients)
  if(length(haplo.common)) {
    coeffnames <- c(coeffnames,
                    paste("hap", haplo.common, sep='.'))
  }
  
  if(length(haplo.rare)) {
    nrare <- length(haplo.rare) - sum(!is.na(fit$haplo.elim))
    coeffnames <- c(coeffnames, rep("hap.rare", nrare))
  }

  dimnames(fit$var.mat) <- list(coeffnames, coeffnames)
  
  fit$missing <- missing   

  if(model)
    fit$model <- haplo.mf$m.frame
    ## fit$model <- m  ## JPS changed to line above--9/16/11
  
  
  if(!y)
    fit$y <- NULL
  if(!x)
    fit$x <- NULL

  
  ## 9/7/11 removed exception for Splus -JPS
  ## 9/23/11 add 2nd class "glm", so it can inherit some methods
  ##         from glm, such as anova.glmlist
  class(fit) <- c("haplo.glm", "glm")
  
  
  return(fit)

}


