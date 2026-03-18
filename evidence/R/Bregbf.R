Bregbf <- function(form.list, data, l = length(form.list)) {
    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    cat("   Bayesian regression linear model comparison with Bayes factors  \n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    # if(!require(LaplacesDemon)) { stop('Required
    # package LaplacesDemon is missing; please
    # install.') } require(LaplacesDemon)
    if (!is.list(form.list)) 
        stop("form.list must be a list")
    form <- vector("list", l)
    outL <- vector("list", l)
    OUT <- list(mdl = 1:l)
    lml <- rep(0.54321, l)
    cat("Model formulae:\n")
    for (i in 1:l) {
        print(form.list[[i]])
    }
    if (!is.data.frame(data)) 
        stop("not a data.frame")
    
    ## Center and scale non-factor data
    tst <- !sapply(data, is.factor)
    for (i in 1:dim(data)[[2]]) {
        {
            if (tst[i]) {
                data[, i] <- LaplacesDemon::CenterScale(data[, 
                  i])
            }
        }
    }
    for (mdl in 1:l) {
        cat("model", mdl, "\n")
        frmla <- form.list[[mdl]]
        m <- model.frame(frmla, data)
        X <- model.matrix(frmla, data)
        Y <- model.response(m)
        pred.names <- frmla[[3]]
        n <- length(Y)
        p <- dim(X)[[2]]
        df.name <- deparse(substitute(dataf))
        dat <- list(Y = Y, X = X)
        fname <- paste(df.name, ".mod", sep = "")
        fit <- lm(frmla, data, x = TRUE)
        Initial.Values <- coef(fit)
        Xls <- fit$x
        J <- dim(Xls)[2]
        N <- dim(Xls)[1]
        Model <- NULL
        rm(Model)  ## stupid trick to appease R CMD check
        sink("tmp.mod.R")
        cat(" Model <- function(parm, Data)\n {\n beta.mu <- rep(0, Data$J)\n beta.tau <- rep(1.0E-3, Data$J)\n tau.alpha <- 1.0E-3\n tau.beta <- 1.0E-3\n beta <- parm[1:Data$J]\n tau <- exp(parm[Data$J+1])\n beta.prior <- dnorm(beta, 1/sqrt(tau), log=TRUE)\n tau.prior <- dgamma(tau, tau.alpha, tau.beta, log=TRUE)\n mu <- beta %*% t(Data$X)\n LL <- sum(dnorm(Data$y, mu, 1/sqrt(tau), log=TRUE))\n LP <- LL + sum(beta.prior) + tau.prior\n Modelout <- list(LP=LP, Dev=-2*LL, Monitor=c(LP, tau), yhat=mu,\n    parm=parm)\n return(Modelout)\n }\n", 
            fill = TRUE)
        sink()
        parm.names <- LaplacesDemon::as.parm.names(list(beta = rep(0, 
            J), log.tau = 0))
        Data <- list(N = N, J = J, y = Y, X = X, mon.names = c("LP", 
            "tau"), parm.names = parm.names)
        source("tmp.mod.R")
        
        capture.output(out <- LaplacesDemon::LaplacesDemon(Model = Model, 
            Data = Data, Initial.Values = Initial.Values, 
            Iterations = 30000, Algorithm = "AMM", 
            Thinning = 30, Status = 173, Specs = list(Adaptive = 500, 
                B = NULL, n = 0, Periodicity = 10, 
                w = 0.05)))
        
        Initial.Values <- LaplacesDemon::as.initial.values(out)
        capture.output(out <- LaplacesDemon::LaplacesDemon(Model = Model, 
            Data = Data, Initial.Values = Initial.Values, 
            Iterations = 30000, Algorithm = "DRM", 
            Covar = out$Covar, Thinning = 30, Status = 173, 
            Specs = NULL))
        OUT[[mdl]] <- out
        cat("acceptance rate =", out$Acceptance.Rate, 
            "\n")
        Converged <- !any(is.na(out$LML))
        if (!Converged) {
            cat("Convergence was not achieved for model", 
                mdl, ".\n")
            cat("You may want to change the arguments of the function\n")
            cat("LaplacesDemon.\n")
        }
        cat("out$LML:\n")
        print(out$LML)
        
        lml[mdl] <- out$LML
        cat(mdl, "logMLik =", out$LML, "MLik =", exp(lml[mdl]), 
            "\n")
    }
    modnames <- as.vector(form.list)
    bf <- matrix(NA, l, l)
    for (i in 1:l) {
        for (j in 1:l) {
            bf[i, j] <- exp(lml[i] - lml[j])
        }
    }
    dimnames(bf) <- list(modnames, modnames)
    cat("Bayes factors P(row > column):\n")
    print(bf, digits = 3)
    pmp <- round(exp(lml)/sum(exp(lml)), 3)
    df <- data.frame(model = as.character(modnames), 
        pmp = pmp)
    df$model <- factor(df$model, levels = as.character(unique(df[order(df$pmp, 
        decreasing = TRUE), ]$model)))
    cat("Marginal posterior probabilities of each of the models:\n")
    print(df)
    d <- dotplot(model ~ pmp, df, xlab = "Marginal posterior model probability", 
        ylab = "Model", main = "Model probabilities")
    print(d)
    ## save modprobs=df, BF=bf
    invisible(OUT)
}
