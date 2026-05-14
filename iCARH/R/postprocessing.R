#' @title Postprocess and plot model parameters
#'
#'@description Group of functions to postprocess and plot model parameters of interest, compute WAIC
#' (Watanabe-Akaike Information Criterion) and MADs (Mean Absolute Deviation) for posterior predictive checks
#' and check normality assumptions.
#'
#' @describeIn iCARH.plotBeta Plot boxplots of posterior densities of \eqn{\beta} coefficients.
#'
#' @param fit object returned by iCARH.model
#' @param path.names pathway names
#' @param indx vector to specify X variables to plot. Selects all variables of X by default.
#' @param indy vector to specify Y variables to plot. Selects all variables of Y by default.
#' @param indpath vector to specify pathways to plot. Selects all pathways by default.
#' @param plotx plot X data imputation?
#' @param ploty plot Y data imputation?
#' @param ...  passed to ggplot2::geom_violin
#'
#' @return the \code{iCARH.plot[*]} functions return a ggplot graph object. \code{iCARH.checkNormality} returns the normalized data.
#' \code{iCARH.waic} and \code{iCARH.mad} return corresponding waic (scalar) and mad (vector of \eqn{J*(J+1)/2}) values. 
#' \code{iCARH.checkRhats} checks model convergence.
#'
#' @examples data.sim = iCARH.simulate(4, 10, 14, 8, 2, path.probs=0.3, Zgroupeff=c(0,4),
#' beta.val=c(1,-1,0.5, -0.5))
#' XX = data.sim$XX
#' Y = data.sim$Y
#' Z = data.sim$Z
#' pathways = data.sim$pathways
#' \donttest{
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = 2)
#' fit = iCARH.model(XX, Y, Z, groups=rep(c(0,1), each=5), pathways, 
#' control = list(adapt_delta = 0.99, max_treedepth=10), iter = 2, chains = 2)
#' if(!is.null(fit$icarh))
#' gplot = iCARH.plotBeta(fit, indx=1:3, indy=1:2)}
#'
#' @export iCARH.plotBeta
#'
#' @import ggplot2
#' @importFrom rstan extract

iCARH.plotBeta = function(fit, indx=TRUE, indy=TRUE){
  X=value=NULL
  gam1 = iCARH.getBeta(fit)
  gamdf1 = reshape2::melt(gam1[,indx,indy, drop=F], varnames = c("mcmc","X","Y"))
  gamdf1$X = as.factor(gamdf1$X)
  gg=ggplot(data=gamdf1, aes(y = value, x = X)) +
    facet_wrap(~Y,nrow = 2, scales="free_y") +
    geom_boxplot(outlier.shape = NA, fill="grey") + geom_hline(yintercept = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10, face="bold"),
          axis.text.y = element_text(size = 15, face="bold"),
          axis.title = element_text(size = 20,face="bold"),
          strip.text.x = element_text(size = 20, face="bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab(expression(bold(beta)))
  return(gg)
}

#' @describeIn iCARH.plotBeta Plot boxplots of posterior densities of theta (time effect) coefficients.
#' @export iCARH.plotARCoeff

iCARH.plotARCoeff = function(fit, indx=TRUE){
  X=value=NULL
  gam1 = iCARH.getARCoeff(fit)
  gamdf1 = reshape2::melt(gam1[,indx, drop=F], varnames = c("mcmc","X"))
  gamdf1$X = as.factor(gamdf1$X)
  gg=ggplot(data=gamdf1, aes(y = value, x = X)) +
    geom_boxplot(outlier.shape = NA, fill="grey") + geom_hline(yintercept = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          axis.title=element_text(size=20,face="bold"),
          strip.text.x = element_text(size = 20, face="bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab(expression(bold(theta)))
  return(gg)
}

#' @describeIn iCARH.plotBeta Plot boxplots of posterior densities of treatment effect coefficients.
#' @export iCARH.plotTreatmentEffect

iCARH.plotTreatmentEffect = function(fit, indx=TRUE){
  X=value=NULL
  gam1 = iCARH.getTreatmentEffect(fit)
  gamdf1 = reshape2::melt(gam1[,indx, drop=F], varnames = c("mcmc","X"))
  gamdf1$X = as.factor(gamdf1$X)
  gg=ggplot(data=gamdf1, aes(y = value, x = X)) +
    geom_boxplot(outlier.shape = NA, fill="grey") + geom_hline(yintercept = 0) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          axis.title=element_text(size=20,face="bold"),
          strip.text.x = element_text(size = 20, face="bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab(expression(bold(alpha)))
  return(gg)
}

#' @describeIn iCARH.plotBeta Plot posterior densities of pathway perturbation parameters
#' @export iCARH.plotPathwayPerturbation


iCARH.plotPathwayPerturbation = function(fit, path.names, indpath=TRUE){
  loc=dens=treatment=pathway=NULL
  phi= extract(fit$icarh, inc_warmup = FALSE, pars="phi")$phi[, indpath, ,drop=F]
  P = dim(phi)[2]
  phi.diff = apply(phi[,,1, drop=F],2,function(x) density(x)$x)
  colnames(phi.diff) = c(1:P)
  phi.diff = reshape2::melt(phi.diff, varnames = c("ind","pathway"), value.name = "loc")
  phi.diff$dens = reshape2::melt(apply(phi[,,1, drop=F],2,function(x) density(x)$y))$value
  phi.diff$treatment = "Controls"
  phi.diff.u = apply(phi[,,2, drop=F],2,function(x) density(x)$x)
  colnames(phi.diff.u) = c(1:P)
  phi.diff.u = reshape2::melt(phi.diff.u, varnames = c("ind","pathway"), value.name = "loc")
  phi.diff.u$dens = reshape2::melt(apply(phi[,,2, drop=F],2,function(x) -density(x)$y))$value
  phi.diff.u$treatment = "Cases"
  for(i in 1:nrow(phi.diff)){phi.diff$dens[i] <- phi.diff$dens[i] +phi.diff$pathway[i]/10}
  for(i in 1:nrow(phi.diff.u)){phi.diff.u$dens[i] <- phi.diff.u$dens[i] +phi.diff.u$pathway[i]/10}
  pathway.names = path.names
  names(pathway.names) = 1:P
  gg=ggplot(data=rbind(phi.diff, phi.diff.u), aes(y = loc, x = dens, fill=treatment,
                                                  group=interaction(as.factor(pathway),treatment))) +
    geom_polygon(colour="black") +
    facet_grid(~pathway, switch = "both", labeller = as_labeller(pathway.names)) +
    scale_fill_manual(values=c(Controls="white",Cases="grey"))+
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size=15),
          axis.title.y = element_text(size=18),
          axis.title=element_text(size=14,face="bold"),
          strip.background = element_rect(fill="white"),
          strip.text.x = element_text(angle=90, size = 12, hjust = 1, face="bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("") + ylab(expression(bold(phi^e)))
  return(gg)
}

#' @describeIn iCARH.plotBeta Plot imputed values
#' @export iCARH.plotDataImputation
#'
iCARH.plotDataImputation = function(fit, indx=T, indy=T, plotx=T, ploty=T, ...){
  
  stopifnot(any(is.na(fit$X))|any(is.na(fit$Y)))
  
  stopifnot(!is.null(fit$Y) | !ploty)
  
  stopifnot(!any(is.na(fit$Y)) | !ploty)
  
  stopifnot(!any(is.na(fit$X)) | !plotx)
  
  impdata = iCARH.getDataImputation(fit)
  if(any(is.na(fit$X))) XX = impdata$X[,,,indx, drop=F]
  if(!is.null(fit$Y) & any(is.na(fit$Y))) YY = impdata$Y[,,,indy, drop=F]
  IT = nrow(XX)

  missplot = function(Xmis, X){
    
    convdf = function(df){
      colnames(df) = c("value", "timepoints", "obs", "variables")
      df$obs = as.factor(colnames(X)[df$obs])
      df$variables = as.factor(attr(X, "dimnames")[[3]][df$variables])
      df$timepoints = factor(rownames(X)[df$timepoints], levels=sort(unique(as.numeric(rownames(X)))))
      xdf = with(df,by(df, df$variables, data.frame, simplify = F))
    }
    
    missing = which(is.na(X), arr.ind = T)
    impx = as.data.frame(do.call(rbind, lapply(1:IT, function(i) cbind(Xmis[i,,,][missing, drop=F], missing))))
    impx = convdf(impx)
    fixed =  which(!is.na(X), arr.ind = T)
    xdf = as.data.frame(cbind(X[fixed, drop=F], fixed))
    xdf = convdf(xdf)
    pt_est = as.data.frame(cbind(colMeans(Xmis)[missing, drop=F], missing))
    pt_est = convdf(pt_est)
    gg=list()
    for(i in seq_along(impx)){
      gg[[i]]=ggplot(xdf[[i]], aes_string(x = 'timepoints', y = 'value')) +
        geom_point(colour="blue", na.rm=T) +
        facet_wrap(~obs, nrow = 3) +
        geom_violin(data=impx[[i]], fill="lightsalmon",colour="salmon", trim = T, ...) +
        geom_point(data=pt_est[[i]], colour="lightgreen" ) + geom_hline(yintercept = 0, linetype="dotted", colour="grey") +
        scale_x_discrete(drop=FALSE) +
        theme(axis.text.x = element_text(size=10, face="bold"),
              axis.text.y = element_text(size = 15, face="bold"),
              axis.title = element_text(size = 20,face="bold"),
              strip.text.x = element_text(size = 20, face="bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        ggtitle(names(impx)[i])
    }
    return(gg)
  }

  grx=NULL
  gry=NULL
  if(plotx & any(is.na(fit$X)))
    grx = missplot(XX, fit$X[,,indx, drop=F])
  if(ploty & !is.null(fit$Y) & any(is.na(fit$Y)))
    gry = missplot(YY, fit$Y[,,indy, drop=F])

  return(list(grx=grx,gry=gry))
}

#' @describeIn iCARH.plotBeta check model convergence and return Rhat coefficients
#' @export iCARH.checkRhats

iCARH.checkRhats = function(fit){
  fit_summary = summary(fit$icarh)$summary
  Rhats <- fit_summary[, "Rhat"]
  if (any(Rhats > 1.1, na.rm = TRUE) | any(is.nan(Rhats)))
    warning("Some Rhats are not < 1.1 ! The model has not converged! Results may not be valid!")
  else
    cat("Rhat values do not indicate any problems.")
  return(Rhats)
}

#' @describeIn iCARH.plotBeta Check normality assumptions. Returns normalized data
#' and performs quantile-quantile plot
#' @export iCARH.checkNormality


iCARH.checkNormality = function(fit){
  mu = colMeans(extract(fit$icarh, inc_warmup=F, pars="mu")$mu)
  Sigma = colMeans(extract(fit$icarh, inc_warmup=F, pars="Sigma")$Sigma)
  XX = ifelse(is.na(fit$X), 
              colMeans(extract(fit$icarh, inc_warmup=F, pars="XX")$XX),
              fit$X)
  bin.groups = as.numeric(as.character(factor(fit$groups, levels=sort(unique(fit$groups), decreasing=T), labels=1:2 )))
  psi = list()
  for (i in 1:2) psi[[i]] = chol(Sigma[i,,])
  err = array(dim=dim(XX))
  N = dim(XX)[2]
  for(i in 1:nrow(XX)) for(n in 1:N) err[i,n,] = psi[[bin.groups[n]]]%*%(XX[i,n,]-mu[i,n,])
  #for(i in 1:nrow(XX)) for(n in (N/2+1):N) err[i,n,] = psi2%*%(XX[i,n,]-mu[i,n,])
  qqnorm(err[,fit$groups,], cex=2.5, main="Controls")
  abline(0,1)
  qqnorm(err[,!fit$groups,], cex=2.5, main = "Cases")
  abline(0,1)
  return(err)
}

#' @describeIn iCARH.plotBeta Compute Watanabe-Akaike Information Criterion (WAIC)
#' @export iCARH.waic


iCARH.waic = function(fit){
  x = extract(fit$icarh, inc_warmup=F, pars="log_lik")$log_lik
  pwaic = sum(apply(x, c(2,3), var))
  lpd = sum(log(apply(exp(x), c(2,3), mean)))
  return(-2*(lpd-pwaic))
}

#' @describeIn iCARH.plotBeta Compute MADs (Mean Absolute Deviation) between true covariance matrix
#' and inferred covariance matrix for posterior predictive checks
#' @export iCARH.mad

iCARH.mad = function(fit){
  X = fit$X
  estimate = extract(fit$icarh, inc_warmup=F, pars="mupred")$mupred
  Tp = dim(estimate)[2]
  J = dim(estimate)[3]
  mad = array(0, dim = c(Tp, J*(J+1)/2))
  for(t in 1:Tp){
    true.cov = cov(X[t,,], use="na.or.complete")[lower.tri(cov(X[t,,]),diag=T)]
    for(i in 1:nrow(estimate)){
      cov.pred = cov(t(estimate[i,t,,]))[lower.tri(cov(t(estimate[i,t,,])), diag=T)]
      mad[t,] = mad[t,] + abs( cov.pred- true.cov)/nrow(estimate)
    }
  }
  return(mad)
}
