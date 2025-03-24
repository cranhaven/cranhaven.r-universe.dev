fh_hetop <- function(ngk, fixedcuts, p, m, gridL, gridU, Xm=NULL, Xs=NULL, seed=12345, modelfileonly = FALSE, modloc=NULL, ...){

    set.seed(seed)
    tmpdir <- tempdir()
    
    if(is.null(modloc)){
        modloc <- paste0(tmpdir,"/model.txt")
    } else {
        if(!is.character(modloc) || (length(modloc)!=1)){
            stop("invalid modloc")
        }
    }

    ## #############################################
    ## basic checks on arguments
    ## #############################################
    if(is.null(ngk)){
        stop("ngk not specified")
    }

    if(is.null(fixedcuts)){
        stop("fixedcuts not specified")
    }

    if(is.null(p)){
        stop("p not specified")
    }

    if(is.null(m)){
        stop("m not specified")
    }

    if(is.null(gridL)){
        stop("gridL not specified")
    }

    if(is.null(gridU)){
        stop("gridU not specified")
    }

    ## ##############################################
    ## additional checks on arguments
    ## ##############################################

    ## ngk
    if(!is.numeric(ngk)){
        stop("ngk must be a GxK numeric matrix of category counts")
    }
    
    if(!is.matrix(ngk)){
        stop("ngk must be a GxK numeric matrix of category counts")
    }

    if(any(is.na(ngk))){
        stop("ngk cannot contain missing values")
    }

    if(any(ngk < 0)){
        stop("ngk must contain only non-negative values")
    }

    if(any(apply(ngk, 1, sum) <= 0)){
        stop("ngk contains at least one row with insufficient data")
    }

    G <- nrow(ngk)
    K <- ncol(ngk)
    
    if(K <= 2){
        stop("Function requires K >= 3 categories")
    }

    if(G <= 10){
        warning("FH-HETOP model may not function properly with so few groups")
    }
    
    ng  <- apply(ngk, 1, sum)
    pg  <- ng/sum(ng)

    ## fixedcuts
    if(!is.numeric(fixedcuts)){
        stop("fixedcuts must be a numeric vector of length 2")
    }
    
    if(length(fixedcuts) != 2){
        stop("fixedcuts must be a numeric vector of length 2")        
    }

    if(any(is.na(fixedcuts))){
        stop("fixedcuts cannot contain missing values")
    }

    if(fixedcuts[1] >= fixedcuts[2]){
        stop("fixedcuts[1] must be strictly less than fixedcuts[2]")
    }
    
    cuts12 <- sort(fixedcuts)
    if(K == 3){
        cuts <- cuts12
    } else {
        cuts <- c(cuts12, rep(NA, K-3))
    }
    
    ## p
    if(!is.numeric(p)){
        stop("p must be a numeric vector of length 2")
    }
    
    if(length(p) != 2){
        stop("p must be a numeric vector of length 2")
    }

    if(any(is.na(p))){
        stop("p cannot contain missing values")
    }

    if(any(abs(p - floor(p)) > 1e-8)){
        stop("p must contain integer numbers of degrees of freedom")
    }
    
    if(any(p < 3)){
        stop("each element of p must be at least three since Q is based on cubic splines")
    }
        
    ## m
    if(!is.numeric(m)){
        stop("m must be a numeric vector of length 2")
    }
    
    if(length(m) != 2){
        stop("m must be a numeric vector of length 2")
    }

    if(any(is.na(m))){
        stop("m cannot contain missing values")
    }

    if(any(abs(m - floor(m)) > 1e-8)){
        stop("m must contain integer numbers of grid points")
    }
    
    if(any(m < p)){
        stop("The model is intended to have more gridpoints than degrees of freedom; either increase 'm' or decrease 'p'")
    }
    
    ## gridL & gridU
    if(!is.numeric(gridL)){
        stop("gridL must be a numeric vector of length 2")
    }
    
    if(length(gridL) != 2){
        stop("gridL must be a numeric vector of length 2")
    }

    if(any(is.na(gridL))){
        stop("gridL cannot contain missing values")
    }

    if(!is.numeric(gridU)){
        stop("gridU must be a numeric vector of length 2")
    }
    
    if(length(gridU) != 2){
        stop("gridU must be a numeric vector of length 2")
    }

    if(any(is.na(gridU))){
        stop("gridU cannot contain missing values")
    }
    
    if(any(gridL >= gridU)){
        stop("elements of gridU must be strictly greater than elements of gridL")
    }

    ## Xm and Xs
    meanX <- !is.null(Xm)
    sdX   <- !is.null(Xs)

    if(as.integer(meanX + sdX) == 1){
        stop("current implementation requires covariates be specified for neither or both of (mu, sigma)")
    }
        
    if(meanX){
        if(!is.numeric(Xm)){
            stop("Xm must be a numeric matrix of covariates for the group means")
        }
        
        if(!is.matrix(Xm)){
            stop("Xm must be a numeric matrix of covariates for the group means")
        }
        
        if(any(is.na(Xm))){
            stop("Xm cannot contain missing values")
        }

        if(any(abs(apply(Xm, 2, sum)) > 1e-8)){
            stop("each column of Xm must sum to zero")
        }

        dimXm <- ncol(Xm)
    }

    if(sdX){
        if(!is.numeric(Xs)){
            stop("Xs must be a numeric matrix of covariates for the group means")
        }
        
        if(!is.matrix(Xs)){
            stop("Xs must be a numeric matrix of covariates for the group means")
        }
        
        if(any(is.na(Xs))){
            stop("Xs cannot contain missing values")
        }

        if(any(abs(apply(Xs, 2, sum)) > 1e-8)){
            stop("each column of Xs must sum to zero")
        }

        dimXs <- ncol(Xs)
    }
    
    ## #############################################
    ## parse jags() arguments passed via ...
    ## #############################################
    
    ## ones allowed to be passed:
    valid.ja <- c("inits","parameters.to.save","n.chains","n.iter","n.burnin","n.thin",
                  "DIC","working.directory","refresh","progress.bar","digits","RNGname",
                  "jags.module")
    
    ## ones that got passed:
    passed.ja <- list(...)
    .n        <- names(passed.ja)

    if( (length(passed.ja) > 0) && any(!(.n %in% valid.ja)) ){
        stop("The ... argument list included elements not eligible to be passed to jags()")
    }

    ## check for presence of each and create arguments accordingly
    if("inits" %in% .n){
        jags.inits <- passed.ja$inits
    } else {
        jags.inits <- NULL
    }

    if("parameters.to.save" %in% .n){
        jags.parameters.to.save <- passed.ja$parameters.to.save
    } else{
        jags.parameters.to.save <- c("mu","sigma","cuts","alpha0m","alpham","alpha0s","alphas","gamma")
        if(meanX && sdX){
            jags.parameters.to.save <- c(jags.parameters.to.save,"beta_m","beta_s")
        }
    }

    if("nchains" %in% .n){
        jags.n.chains <- passed.ja$n.chains
        if(!is.null(jags.inits) && (length(jags.inits) != jags.n.chains)){
            stop("inits and n.chains arguments are inconsistent")
        }
    } else {
        jags.n.chains <- 2
    }
    
    if("n.iter" %in% .n){
        jags.n.iter <- passed.ja$n.iter
    } else {
        jags.n.iter <- 5000
    }

    if("n.burnin" %in% .n){
        jags.n.burnin <- passed.ja$n.burnin
        if(jags.n.burnin > jags.n.iter){
            stop("n.iter and n.burnin are inconsistent")
        }
    } else {
        jags.n.burnin <- jags.n.iter / 2
    }

    if("n.thin" %in% .n){
        jags.n.thin <- passed.ja$n.thin
    } else {
        jags.n.thin <- 1
    }

    if("DIC" %in% .n){
        jags.DIC <- passed.ja$DIC
    } else {
        jags.DIC <- FALSE
    }

    if("working.directory" %in% .n){
        jags.working.directory <- passed.ja$working.directory
    } else {
        jags.working.directory <- NULL
    }

    if("refresh" %in% .n){
        jags.refresh <- passed.ja$refresh
    } else {
        jags.refresh <- jags.n.iter / 20
    }

    if("progress.bar" %in% .n){
        jags.progress.bar <- passed.ja$progress.bar
    } else {
        jags.progress.bar <- "text"
    }

    if("digits" %in% .n){
        jags.digits <- passed.ja$digits
    } else {
        jags.digits <- 5
    }

    if("RNGname" %in% .n){
        jags.RNGname <- passed.ja$RNGname
    } else {
        jags.RNGname <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Mersenne-Twister")
    }

    if("jags.module" %in% .n){
        jags.jags.module <- passed.ja$jags.module
    } else {
        jags.jags.module = c("glm","dic")
    }
  
    ## ##########################
    ## set up spline bases (need to strip attributes from Qm and Qs)
    ## ##########################
    gridm  <- seq(from = gridL[1], to = gridU[1], length=m[1])
    Qm     <- bs(gridm, df = p[1], degree=3, intercept=FALSE)
    Qm     <- matrix(c(Qm), ncol=p[1], byrow=F)

    grids  <- seq(from = gridL[2], to = gridU[2], length=m[2])
    Qs     <- bs(grids, df = p[2], degree=3, intercept=FALSE)
    Qs     <- matrix(c(Qs), ncol=p[2], byrow=F)

    ## ############################
    ## create list of variable names to pass to jags()
    ###############################
    jags.data <- c("G","K","ngk","ng","cuts","m","p","Qm","Qs","gridm","grids")
    if(meanX && sdX){
        jags.data <- c(jags.data, "Xm","dimXm","Xs","dimXs")
    }

    ## ############################
    ## generate initial values (unless they have already been passed)
    ##
    ## NOTE: ran into problems with JAGS complaining about data being inconsistent
    ## with initial params - think this is due to starting SDs being small which
    ## provide nearly zero probabilities for some cells that actually have data.
    ## so fix this by starting SDs at 95th percentile and set mus near the midpoint,
    ## with the hope minimizing  the risk
    ## of an extreme mean and small sigma colliding to create a zero probability for
    ## a populated cell
    ## ############################    
    if(is.null(jags.inits)){

        jags.inits <- vector(jags.n.chains, mode="list")

        for(i in 1:jags.n.chains){
            .tmp <- list(locm      = rep(as.integer(floor(0.5*m[1])), G),
                         locs      = as.integer(rep(floor(0.95 * m[2]), G)),
                         alpha0m   = rnorm(1),
                         alphamdev = rnorm(p[1]),
                         alpha0s   = rnorm(1),
                         alphasdev = rnorm(p[2]),
                         gamma     = 0.0)

            if(meanX){
                .tmp$beta_m <- rnorm(dimXm, sd=0.1)
            }
            if(sdX){
                .tmp$beta_s <- rnorm(dimXs, sd=0.1)
            }

            if(K >= 4){
                .tmp$cuts0 <- sort(seq(from = cuts12[2] + 1, to = cuts12[2] + 3, length=K-3))
            }
  
            jags.inits[[i]] <- .tmp
        }
    }
    
    #####################
    ## create model file, forking as needed
    #####################
    cat("model
  {
    for(g in 1:G){
      ngk[g,1:K] ~ dmulti(pgk[g,1:K], ng[g])

      pgk[g,1] <- phi( (cuts[1] - mu[g]) / sigma[g])
      for(k in 2:(K-1)){
        pgk[g,k] <- phi( (cuts[k] - mu[g]) / sigma[g]) - sum(pgk[g,1:(k-1)])
      }
      pgk[g,K] <- 1.0 - phi( (cuts[K-1] - mu[g]) / sigma[g])

      mu[g]    <-     epsilon[g,1]
      sigma[g] <- exp(epsilon[g,2])

      locm[g] ~ dcat(pFm[1:m[1]])
      locs[g] ~ dcat(pFs[1:m[2]])\n", file=modloc)
    
    if( (!meanX && !sdX) ){
        cat("
      epsilon[g,1]  <- (gamma * grids[locs[g]]) + gridm[locm[g]]
      epsilon[g,2]  <- grids[locs[g]]\n", file=modloc, append=TRUE)
    }
    if( (meanX && sdX) ){
        cat("
      epsilon[g,1] <- inprod(Xm[g,1:dimXm], beta_m[1:dimXm]) + (gamma * grids[locs[g]]) + gridm[locm[g]]
      epsilon[g,2] <- inprod(Xs[g,1:dimXs], beta_s[1:dimXs]) + grids[locs[g]]\n", file=modloc, append=TRUE)
    }
    cat("    }\n", file=modloc, append=TRUE)

    cat("
    alpha0m     ~ dnorm(0.0, 0.01)
    for(i in 1:p[1]){
       alphamdev[i] ~ dnorm(0.0, 0.05)
       alpham[i] <- alpha0m + alphamdev[i]
    }
    for(i in 1:m[1]){
      pFm[i] <- exp(Qm[i,1:p[1]] %*% alpham[1:p[1]])
    }

    alpha0s     ~ dnorm(0.0, 0.01)
    for(i in 1:p[2]){
       alphasdev[i] ~ dnorm(0.0, 0.05)
       alphas[i] <- alpha0s + alphasdev[i]
    }
    for(i in 1:m[2]){
      pFs[i] <- exp(Qs[i,1:p[2]] %*% alphas[1:p[2]])
    }

    gamma ~ dnorm(0.0, 0.1)

    ", file=modloc, append=TRUE) 

    if(K >= 4){
        cat("
    for(k in 1:(K-3)){
      cuts0[k] ~ dnorm(0.0, 0.01) I(cuts[2], )
    }
    tmp[1:(K-3)] <- sort(cuts0)
    for(k in 1:(K-3)){
      cuts[k+2] <- tmp[k]
    }
    ", file=modloc, append=TRUE)
    }

    if(meanX){
        cat("
    for(i in 1:dimXm){
      beta_m[i] ~ dnorm(0.0, 0.1)
    }
    ", file=modloc, append=TRUE)
    }
    if(sdX){
        cat("
    for(i in 1:dimXs){
      beta_s[i] ~ dnorm(0.0, 0.1)
    }
    ", file=modloc, append=TRUE)
    }
    cat("}\n", file=modloc, append=TRUE)

    ## ###########################################
    ## if desired, stop here and just return model file
    ## ###########################################
    if(modelfileonly){
        return(modloc)
    }

    ## ###########################################
    ## otherwise run JAGS
    ## ###########################################
    r <- jags(
        model.file         = modloc,
        data               = jags.data,
        inits              = jags.inits,
        parameters.to.save = jags.parameters.to.save,
        n.chains           = jags.n.chains,
        n.iter             = jags.n.iter,
        n.burnin           = jags.n.burnin,
        n.thin             = jags.n.thin,
        DIC                = jags.DIC,
        working.directory  = jags.working.directory,
        refresh            = jags.refresh,
        progress.bar       = jags.progress.bar,
        digits             = jags.digits,
        RNGname            = jags.RNGname,
        jags.module        = jags.jags.module)

    ## ###########################################
    ## bunch of post-processing and additional statistics
    ## ###########################################
    fh_hetop_extras <- list()

    ## population distribution
    Finfo             <- list()
    Finfo$gridL       <- gridL
    Finfo$gridU       <- gridU
    Finfo$efron_p     <- p
    Finfo$efron_m     <- m
    Finfo$gridm       <- gridm
    Finfo$Qm          <- Qm
    Finfo$grids       <- grids
    Finfo$Qs          <- Qs
    fh_hetop_extras$Finfo <- Finfo; rm(Finfo)

    ## data stuff
    Dinfo             <- list()
    Dinfo$G           <- G
    Dinfo$K           <- K
    Dinfo$ngk         <- ngk
    Dinfo$ng          <- ng
    Dinfo$fixedcuts   <- fixedcuts
    Dinfo$Xm          <- Xm
    Dinfo$Xs          <- Xs
    fh_hetop_extras$Dinfo <- Dinfo; rm(Dinfo)

    ## WAIC
    fh_hetop_extras$waicinfo <- waic_hetop(ngk, r$BUGSoutput$sims.matrix)

    ## posterior samples of group means, group SDs and cutpoints on "star" scale.
    ## also regression coefs if we are doing regressions
    ind_m     <- grep("mu",     colnames(r$BUGSoutput$sims.matrix))
    ind_s     <- grep("sigma",  colnames(r$BUGSoutput$sims.matrix))
    ind_c     <- grep("cuts",   colnames(r$BUGSoutput$sims.matrix))
    ind_betam <- grep("beta_m", colnames(r$BUGSoutput$sims.matrix))
    ind_betas <- grep("beta_s", colnames(r$BUGSoutput$sims.matrix))
    stopifnot( (length(ind_m) == G) && (length(ind_s) == G) && length(ind_c == (K-1)) )

    tmp <- lapply(1:nrow(r$BUGSoutput$sims.matrix), function(i){
        mug    <- r$BUGSoutput$sims.matrix[i,ind_m]
        sigmag <- r$BUGSoutput$sims.matrix[i,ind_s]
        cuts   <- r$BUGSoutput$sims.matrix[i,ind_c]

        a   <- sum(pg * mug)
        b   <- sqrt(sum(pg * ( (mug - a)^2 + sigmag^2)))

        .retval <- list(mug       = (mug - a)/b,
                        sigmag    = sigmag / b,
                        cutpoints = (cuts - a)/b)

        if(meanX && sdX){
            .retval$beta_m <- r$BUGSoutput$sims.matrix[i,ind_betam] / b
            .retval$beta_s <- r$BUGSoutput$sims.matrix[i,ind_betas]
        }

        return(.retval)
    })

    fh_hetop_extras$est_star_samps <- list(mug       = do.call("rbind", lapply(tmp, function(x){ x$mug })),
                                           sigmag    = do.call("rbind", lapply(tmp, function(x){ x$sigmag })),
                                           cutpoints = do.call("rbind", lapply(tmp, function(x){ x$cutpoints })))

    if(meanX && sdX){
        fh_hetop_extras$est_star_samps$beta_m <- do.call("rbind", lapply(tmp, function(x){ x$beta_m }))
        fh_hetop_extras$est_star_samps$beta_s <- do.call("rbind", lapply(tmp, function(x){ x$beta_s }))
    }
        
    rm(tmp); gc()
    
    ## check
    stopifnot(max(abs(sapply(1:nrow(r$BUGSoutput$sims.matrix), function(i){
        sum(pg * (fh_hetop_extras$est_star_samps$mug[i,]^2 + fh_hetop_extras$est_star_samps$sigmag[i,]^2))
    }) - 1)) < 1e-12)

    ## posterior mean, constrained bayes and triple-goal estimators of group means and SDs
    fh_hetop_extras$est_star_mug    <- triple_goal(fh_hetop_extras$est_star_samps$mug)
    fh_hetop_extras$est_star_sigmag <- triple_goal(fh_hetop_extras$est_star_samps$sigmag)
    
    ## return
    r$fh_hetop_extras <- fh_hetop_extras
    rm(fh_hetop_extras); gc()
    return(r)
}
