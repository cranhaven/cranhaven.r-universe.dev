
BayesProbitHSD.Summary = function(object, digits = max(1L, getOption("digits") - 4L))
{
    
    post.samples = object$Posterior.Samples$PosteriorSamples

    para.names = names(post.samples)

    beta.est = matrix(unlist(apply(post.samples$beta.samples, 1, bm)), ncol = 2, byrow=TRUE)

    colnames(beta.est) = c("PostMean", "StErr")
    rownames(beta.est) = object$Fixed.Effects.Names
    beta.CI = apply(post.samples$beta.samples, 1, quantile, c(0.025, 0.975))


    beta.est.CI = as.data.frame(cbind(beta.est, t(beta.CI)))



    if(!is.null(post.samples$alpha.samples)){
        alpha.est = matrix(unlist(apply(post.samples$alpha.samples, 1, bm)), ncol = 2, byrow=TRUE)
        alpha.CI = t(apply(post.samples$alpha.samples, 1, quantile, c(0.025, 0.975)))
        alpha.est = alpha.est[-c(1, nrow(alpha.est)), ]
        alpha.CI = alpha.CI[-c(1, nrow(alpha.CI)), ]
        colnames(alpha.est) = c("PostMean", "StErr")
        rownames(alpha.est) = paste0("alpha", 1:nrow(alpha.est))
        alpha.est.CI = as.data.frame(cbind(alpha.est, alpha.CI))
        beta.est.CI = rbind(beta.est.CI, alpha.est.CI)
    }


    beta.est.CI = format(beta.est.CI, digits = digits)
    #cat("\nCoefficients:\n")
    #print(beta.est.CI)

 
    cl = object$call
    #print(cl)
    HDS.cov.all = all.vars.character(cl[match("HS.model", names(cl))])$m[[2]]
    HSD.cov = attr(terms.formula(as.formula(cl$HS.model)), "term.labels") #all.vars.character(cl[match("HS.model", names(cl))])$m[[2]]
    
    #cat("HSD.cov = ", HSD.cov, "\n")
    if(length(HSD.cov)>0)
    {
        if(any(HDS.cov.all %in% "1"))
            HSD.cov = c("(Intercept)", HSD.cov)

        delta.est = matrix(unlist(apply(post.samples$delta.samples, 1, bm)), ncol = 2, byrow=TRUE)
        delta.CI = apply(post.samples$delta.samples, 1, quantile, c(0.025, 0.975))
        colnames(delta.est) = c("PostMean", "StErr")
        rownames(delta.est) = HSD.cov

        delta.est.CI = as.data.frame(cbind(delta.est, t(delta.CI)))
        delta.est.CI = format(delta.est.CI, digits = digits)
        #cat("\nParameters in HSD model:\n")
        #print(delta.est.CI)
    }
    else 
        delta.est.CI = NULL

    random.cov = signif(object$Posterior.Samples$PosteriorEstimates$Sigma.mean, digits = digits)
    if(length(object$Random.Effects.Names)==1)
        dimnames(random.cov) = list(object$Random.Effects.Names, "Variance") 
    if(length(object$Random.Effects.Names)>1)
        dimnames(random.cov) = list(object$Random.Effects.Names, object$Random.Effects.Names) 
    #cat("\n\nRandom effect: \n")
    #print(random.cov)

    info = object$Posterior.Samples$PosteriorEstimates
    model.info = data.frame(logL = info$logL, AIC = info$AIC, BIC = info$BIC, CIC = info$CIC, 
                             DIC = info$DIC, MPL =info$MPL, RJR = info$RJR, ACC = info$ACC, 
                             row.names = "")
    model.info = format(model.info, , digits = digits)
    #cat("\nModel Information:\n")
    #print(model.info)

    if(!is.null(delta.est.CI)){
        if(nrow(delta.est.CI)>1)
            Ri = CorrMat.HSD(object$HS.model.Mat[,,1,], as.numeric(delta.est.CI[, 1]))
        else
            Ri = CorrMat.HSD(adrop(object$HS.model.Mat[,,1,, drop=FALSE], drop=3), as.numeric(delta.est.CI[, 1]))
        
        Ri = formatC(Ri, digits = digits, format = "f", mode = "real")

        #cat("\n\nEstimate of Ri: \n")
        #print(Ri, quote = FALSE)
        HSD.result = list(beta.est.CI = beta.est.CI, delta.est.CI = delta.est.CI, model.info = model.info, 
        random.cov = random.cov, Ri = Ri)
    }     
    else
        HSD.result = list(beta.est.CI = beta.est.CI, delta.est.CI = delta.est.CI, model.info = model.info, 
            random.cov = random.cov)
    HSD.result
    #HSD.result
}
