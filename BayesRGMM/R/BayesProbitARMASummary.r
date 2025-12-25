
BayesProbitARMA.Summary = function(object, digits = max(1L, getOption("digits") - 4L))
{
    
    post.samples = object$Posterior.Samples$PosteriorSamples

    para.names = names(post.samples)

    beta.est = matrix(unlist(apply(post.samples$beta.samples, 1, bm)), ncol = 2, byrow=TRUE)

    colnames(beta.est) = c("PostMean", "StErr")
    rownames(beta.est) = object$Fixed.Effects.Names
    beta.CI = apply(post.samples$beta.samples, 1, quantile, c(0.025, 0.975))


    beta.est.CI = as.data.frame(cbind(beta.est, t(beta.CI)))

    beta.est.CI = format(beta.est.CI, digits = digits)

    #cat("\nCoefficients:\n\n")
    #print(beta.est.CI)


    arma.order = as.numeric(gsub("\\D", "", object$call[[which(names(object$call)=="arma.order")]]))[1:2]
    ar.order = arma.order[1]
    ma.order = arma.order[2]

    arma.est = NULL
    if(sum(ar.order, ma.order)>0){
        if(ar.order>0){
            phi.est = matrix(unlist(apply(post.samples$phi.samples[1:ar.order,1 ,,drop=F], c(1,2), bm)), nrow=ar.order, byrow=T)
            phi.CI = unlist(apply(post.samples$phi.samples[1:ar.order,1 ,,drop=F], c(1,2), quantile, c(0.025, 0.975)))
            phi.CI = t(adrop(phi.CI, drop=3))
            colnames(phi.est) = c("PostMean", "StErr")
            rownames(phi.est) = paste("phi", 1:ar.order)
            phi.est.CI = cbind(phi.est, phi.CI)
            arma.est = rbind(arma.est, phi.est.CI)
            
        }
        if(ma.order>0){
            psi.est = matrix(unlist(apply(post.samples$psi.samples[1:ma.order,1 ,,drop=F], c(1,2), bm)), nrow=ma.order, byrow=T)
            psi.CI = unlist(apply(post.samples$psi.samples[1:ma.order,1 ,,drop=F], c(1,2), quantile, c(0.025, 0.975)))
            psi.CI = t(adrop(psi.CI, drop=3))
            colnames(psi.est) = c("PostMean", "StErr")
            rownames(psi.est) = paste("psi", 1:ma.order)
            psi.est.CI = as.data.frame(cbind(psi.est, psi.CI))   
            arma.est = rbind(arma.est, psi.est.CI)  
             
        }
        arma.est = as.data.frame(arma.est)
        arma.est = format(arma.est, digits = digits)
        #cat("\n\nAMRA parameters:\n\n")
        #print(arma.est)
    }
    
    info = object$Posterior.Samples$PosteriorEstimates
    model.info = data.frame(logL = info$logL, AIC = info$AIC, BIC = info$BIC, CIC = info$CIC, 
                             DIC = info$DIC, MPL =info$MPL, RJR = info$RJR, ACC = info$ACC, 
                             row.names = "")

    
    random.cov = signif(object$Posterior.Samples$PosteriorEstimates$Sigma.mean, digits = digits)
    if(length(object$Random.Effects.Names)==1)
        dimnames(random.cov) = list(object$Random.Effects.Names, "Variance") 
    if(length(object$Random.Effects.Names)>1)
        dimnames(random.cov) = list(object$Random.Effects.Names, object$Random.Effects.Names) 
    #cat("\n\nRandom effect: \n")
    #print(random.cov)



    model.info = format(model.info, digits = digits)
    #cat("\nModel Information:\n")
    #print(model.info)

    ARMA.result = list(beta.est.CI = beta.est.CI, arma.est = arma.est, model.info = model.info, random.cov = random.cov)
    ARMA.result
}
