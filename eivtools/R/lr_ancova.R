lr_ancova <- function(outcome_model, Y, W, Z, G, varfuncs, plotfile=NULL, seed=12345, modelfileonly=FALSE, scalemat=NULL, blockprior=TRUE,...){

    set.seed(seed)
    tmpdir  <- tempdir()
    modfile <- paste0(tmpdir,"/model.txt")
    if(is.null(plotfile)){
        plotfile <- paste0(tmpdir, "/lr_ancova_plots.pdf")
    }
    pdf(plotfile)

    ## #############################################
    ## basic checks on arguments
    ## #############################################
    if(is.null(outcome_model)){
        stop("outcome_model not specified")
    }
    
    if(is.null(Y)){
        stop("Y not specified")
    }

    if(is.null(W)){
        stop("W not specified")
    }

    if(is.null(Z)){
        stop("Z not specified")
    }
    
    if(is.null(G)){
        stop("G not specified")
    }

    if(is.null(varfuncs)){
        stop("varfuncs not specified")
    }

    ## #############################################
    ## parse jags() arguments passed via ... to lr_ancova()
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
        jags.parameters.to.save <- c("varXgivenZG","betaXZ","betaXG","betaYXZ","betaYG")
        if(outcome_model %in% c("normal","normalME")){
            jags.parameters.to.save <- c(jags.parameters.to.save, "sdYgivenXZG")
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

    ## #############################################
    ## checks on Y and outcome model
    ## #############################################    
    if(!(outcome_model %in% c("normal","normalME","poisson","bernoulli_probit","bernoulli_logit"))){
        stop("Invalid value of 'outcome_model' - see documentation")
    }
    
    if(!( is.numeric(Y) && is.vector(Y) )){
        stop("Y must be a numeric vector")
    }
    
    nR <- length(Y)
    
    if(outcome_model %in% c("poisson","bernoulli_probit","bernoulli_logit")){
        Y <- as.integer(Y)
    }
    
    if( (outcome_model == "poisson") && any(Y < 0) ){
        stop("With outcome_model == 'poisson', Y must consist of non-negative integers")
    }
    
    if( (outcome_model %in% c("bernoulli_probit","bernoulli_logit")) && (!all(Y %in% c(0,1))) ){
        stop("With outcome_model == 'poisson', Y must consist of non-negative integers")
    }

    if(outcome_model %in% c("normal","normalME")){
        .sdYgivenXZG <- sd(Y, na.rm=T)
    }
    
    ## #############################################
    ## checks on W
    ## #############################################
    if(is.null(dim(W))){
        nX <-  1
        
        if(length(W) != nR){
            stop("length(W) and Y are inconsistent")
        }
        
        if(all(is.na(W))){
            stop("W has all missing values")
        }
    } else{
        if(is.data.frame(W)){
            W <- as.matrix(W)
        }
        
        if(nrow(W) != nR){
            stop("nrow(W) and Y are inconsistent")
        }
        
        if(any(apply(W, 2, function(x){ sum(is.na(x))}) == nR)){
            stop("A column of W has entirely missing values")
        }
        
        nX <- ncol(W)
        
        if(nX == 1){
            W <- W[,1]
        }
    }
    
    if(!is.numeric(W)){
        stop("W must contain only numeric values")
    }

    if(nX == 1){
        .sdXgivenZG <- sd(W, na.rm=T)
    }
        
    ## ##############################################
    ## checks on Z
    ## ##############################################
    if(any(is.na(Z))){
        stop("Z cannot contain missing values")
    }
    
    if(is.null(dim(Z))){
        nZ <-  1
        
        if(length(Z) != nR){
            stop("length(Z) and Y are inconsistent")
        }
        
        if(!is.numeric(Z)){
            stop("Z must contain only numeric values")
        }
        
        if(any(abs(Z - 1) > 1e-12)){
            stop("when nZ==1, Z must be a vector of 1s (for intercept)")
        }
    } else{
        if(is.data.frame(Z)){
            Z <- as.matrix(Z)
        }
        
        if(nrow(Z) != nR){
            stop("nrow(Z) and Y are inconsistent")
        }
        
        if(!is.numeric(Z)){
            stop("Z must contain only numeric values")
        }
        
        if(any(abs(Z[,1] - 1) > 1e-12)){
            stop("First column of Z needs to be column of 1s (intercept)")
        }
        
        nZ <- ncol(Z)
        
        if(qr(Z)$rank < nZ){
            stop("Z is not of full column rank")
        }
        
        if(nZ == 1){
            Z <- Z[,1]
        }
    }

    nXZ <- nX + nZ
    
    ## #############################################
    ## checks on G
    ## #############################################
    if(!(is.vector(G) || is.factor(G))){
        stop("G must be a vector providing group memberships")
    }
    
    if(any(is.na(G))){
        stop("G cannot contain missing values")
    }
    
    if(length(G) != nR){
        stop("G and Y are inconsistent")
    }
    
    nG <- length(unique(G))
    
    if(nG == 1){
        stop("G must contain at least two distinct groups")
    }
    
    if(nG == nR){
        stop("G has too many unique values")
    }
    
    if(is.numeric(G)){
        G <- factor(G)
    }
    
    ## check that (Z,G) aren't collinear - the rank of the design matrix based on
    ## (Z,G) should be exactly one less than the number of columns because columns
    ## of G add to an intercept, which is the first column of Z
    if(qr(cbind(Z, model.matrix(~G - 1)))$rank != (nZ + nG - 1)){
        stop("columns of Z are collinear with design matrix implied by G")
    }
    
    group.map     <- data.frame(G = sort(unique(G)))
    group.map$gid <- 1:nrow(group.map)
    gid           <- merge(data.frame(G = G), group.map, by="G")$gid

    ## #############################################
    ## checks and processing on varfuncs
    ## #############################################
    if(!is.list(varfuncs)){
        stop("varfuncs must be a list")
    }

    nvf <- ifelse(outcome_model == "normalME", nX+1, nX)
    
    if(length(varfuncs) != nvf){
        stop("varfuncs must be a list with as many elements as error-prone covariates, plus one if outcome_model is 'normalME'")
    }
    
    for(i in 1:nvf){ ## loop over variance functions
        
        if(is.null(varfuncs[[i]]$type)){
            stop(paste0("varfuncs[[",i,"]] does not contain a 'type' element"))
        }
        
        if(!(varfuncs[[i]]$type %in% c("constant","piecewise_linear","log_polynomial"))){
            stop(paste0("varfuncs[[",i,"]]$type must be one of 'constant', 'piecewise_linear' or 'log_polynomial'"))
        }
        
        if(is.null(varfuncs[[i]]$vtab)){
            stop(paste0("varfuncs[[",i,"]] does not contain a 'vtab' element"))
        }
        
        if(varfuncs[[i]]$type == "constant"){ ## vtab should be a scalar variance
            
            if(!( !is.na(varfuncs[[i]]$vtab) && is.numeric(varfuncs[[i]]$vtab) && (length(varfuncs[[i]]$vtab) == 1) && (varfuncs[[i]]$vtab > 0) )){
                stop(paste0("varfuncs[[",i,"]]$vtab inconsistent with varfuncs[[",i,"]]$type"))
            }
            
        } else { ## vtab should be a table of conditional variances
            
            if( any(is.na(varfuncs[[i]]$vtab)) ){
                stop(paste0("varfuncs[[",i,"]]$vtab contains missing values"))
            }
            
            varfuncs[[i]]$vtab <- as.data.frame(varfuncs[[i]]$vtab)
            
            if(nrow(varfuncs[[i]]$vtab) <= 1){
                stop(paste0("varfuncs[[",i,"]]$vtab does not have enough rows"))
            }
            
            if(ncol(varfuncs[[i]]$vtab) != 2){
                stop(paste0("varfuncs[[",i,"]]$vtab must have two columns"))
            }
            
            names(varfuncs[[i]]$vtab) <- c("x","gx")
            varfuncs[[i]]$vtab <- varfuncs[[i]]$vtab[order(varfuncs[[i]]$vtab$x),]
            
            .K <- nrow(varfuncs[[i]]$vtab)
            if(length(unique(varfuncs[[i]]$vtab$x)) != .K){
                stop(paste0("varfuncs[[",i,"]]$vtab has duplicated values of latent variable"))
            }
            
            if(!all(varfuncs[[i]]$gx > 0)){
                stop(paste0("varfuncs[[",i,"]]$vtab has non-positive conditional variances"))
            }
            
            if(varfuncs[[i]]$type == "piecewise_linear"){
                ## build the limits for the piecewise-linear variance function
                .vtab           <- varfuncs[[i]]$vtab
                .vtabi          <- vector(.K+1, mode="list")
                .vtabi[[1]]     <- data.frame(xL = -Inf, xU = .vtab$x[1],  a = .vtab$gx[1],  b = 0.0)
                .vtabi[[.K+1]]  <- data.frame(xL = .vtab$x[.K], xU = Inf , a = .vtab$gx[.K], b = 0.0)
                for(k in 2:.K){
                    .vtabi[[k]] <- data.frame(xL = .vtab$x[k-1], xU = .vtab$x[k], a = .vtab$gx[k-1], b = (.vtab$gx[k] - .vtab$gx[k-1]) / (.vtab$x[k] - .vtab$x[k-1]))
                }
                varfuncs[[i]]$vtabi <- do.call("rbind", .vtabi)
                
                ## plot to check reasonableness
                .g <- function(x, vtabi){
                    .K    <- nrow(vtabi) - 1
                    locs  <- sapply(x, function(z){ which( (vtabi$xL <= z) & (vtabi$xU > z) )})
                    gx    <- rep(-99.9, length(x))
                    gx[which(locs == 1)]      <- vtabi$a[1]
                    gx[which(locs == (.K+1))] <- vtabi$a[.K+1]
                    wh <- which( (locs > 1) & (locs <= .K) )
                    gx[wh] <- vtabi$a[locs[wh]] + (vtabi$b[locs[wh]] * (x[wh] - vtabi$xL[locs[wh]]))
                    gx
                }
                .r <- range(varfuncs[[i]]$vtab$x)
                .x <- sort(c(seq(from=.r[1] - 0.05*diff(.r), to = .r[2] + 0.05*diff(.r), length=1000)))
                plot(.x, .g(.x, varfuncs[[i]]$vtabi), main=paste0("varfuncs[[",i,"]] approximation"), type="l", xlab="X", ylab="var(W|X)")
                points(varfuncs[[i]]$vtab$x, varfuncs[[i]]$vtab$gx, col="blue")
                
            } else {
                ## build the polynomial approximation function
                if(is.null(varfuncs[[i]]$degree)){ ## if no degree specified, default to 6
                    varfuncs[[i]]$degree <- 6
                }
                
                if(!is.numeric(varfuncs[[i]]$degree) || !(varfuncs[[i]]$degree >= 2)){
                    stop(paste0("varfuncs[[",i,"]]$degree must be a positive integer >= 2"))
                }
                
                if( (varfuncs[[i]]$degree - 1) > nrow(varfuncs[[i]]$vtab) ){
                    stop(paste0("varfuncs[[",i,"]]$degree too large for number of rows of varfuncs[[",i,"]]$vtab"))
                }
                
                varfuncs[[i]]$degree <- floor(varfuncs[[i]]$degree)
                .vtab <- varfuncs[[i]]$vtab
                for(.j in 1:varfuncs[[i]]$degree){
                    .vtab[,paste0("x",.j)] <- .vtab$x^.j
                }
                .vtab$loggx <- log(.vtab$gx)
                varfuncs[[i]]$logmod <- lm(as.formula(paste0("loggx ~ ",paste(paste0("x",1:varfuncs[[i]]$degree), collapse=" + "))), data=.vtab)
                
                ## plot to check reasonableness
                .r <- range(varfuncs[[i]]$vtab$x)
                .x <- sort(c(seq(from=.r[1] - 0.05*diff(.r), to = .r[2] + 0.05*diff(.r), length=1000)))
                tmp <- matrix(-99.9, ncol = varfuncs[[i]]$degree + 1, nrow = length(.x))
                tmp[,1] <- 1.0
                for(.j in 1:varfuncs[[i]]$degree){
                    tmp[,.j+1] <- .x^.j
                }
                tmp <- as.vector(exp(tmp %*% coef(varfuncs[[i]]$logmod)))
                tmp[which(.x < varfuncs[[i]]$vtab$x[1])] <- varfuncs[[i]]$vtab$gx[1]
                tmp[which(.x > varfuncs[[i]]$vtab$x[nrow(varfuncs[[i]]$vtab)])] <- varfuncs[[i]]$vtab$gx[nrow(varfuncs[[i]]$vtab)]
                plot(.x, tmp, main=paste0("varfuncs[[",i,"]] approximation"), type="l", xlab="X", ylab="var(W|X)")
                points(varfuncs[[i]]$vtab$x, varfuncs[[i]]$vtab$gx, col="blue")
            }
        }
    } ## end loop over variance functions
    dev.off()

    ## #############################################
    ## checks on scalemat
    ## #############################################
    if(!is.null(scalemat)){

        if(nX == 1){
            stop("scalemat applies only with multiple error-prone covariates")
        }

        if(any(is.na(scalemat))){
            stop("scalemat cannot contain missing values")
        }
        
        if(!is.matrix(scalemat)){
            stop("scalemat must be a positive-definite symmetric matrix of dimension ncol(W)")
        }

        if(!( (nrow(scalemat) == nX) && (nrow(scalemat) == nX) )){
            stop("scalemat must be a positive-definite symmetric matrix of dimension ncol(W)")
        }

        if(any(abs(scalemat - t(scalemat)) > 1e-10)){
            stop("scalemat must be a positive-definite symmetric matrix of dimension ncol(W)")
        }

        if(any(eigen(scalemat)$values <= 0.0)){
            stop("scalemat must be a positive-definite symmetric matrix of dimension ncol(W)")
        }

    }
            
    ## #############################################
    ## create list of variables names to pass to JAGS
    ## #############################################
    jags.data <- c("Y","Z","W","gid")
    
    if( (nX > 1) && !is.null(scalemat) ){
        jags.data <- c(jags.data, "scalemat")
    }

    if( (nX > 1) && is.null(scalemat) ){
        ## need to set a scalemat that is not completely inconsistent with data.
        ## use the following approximation:
        ##
        ## 1) regress components of W on (Z,G) and get residual variance/covariance
        ##    matrix - call this "Vhat" - note this includes measurement error in W,
        ##    which we ignore for the purposes here
        ##
        ## 2) the JAGS parameterization of the Wishart for the precision matrix
        ##    is such that if P ~ dwish(S,df) then E[P] = df * solve(S).
        ##    note we are setting df = (nX + 1).
        ##
        ## 3) Thus if we set S = Vhat * df, then E[P] = solve(Vhat), the empirical
        ##    precison matrix
        ##
        ## scalemat  <- (nX + 1) * cov(sapply(1:nX, function(j){ resid(lm(W.obs[,j] ~ dmat.obs - 1))}))
        ##
        ## NOTE: this seemed to create scale matrices that led to a more informative
        ##       prior than probably desirable, in part because the scale matrices are
        ##       not diagonal.  switch to diagonal scale matrix where the implied median
        ##       of the variances approximately match the observed residual variances
        dmat   <- cbind(Z, model.matrix(~G - 1))
        allobs <- which(apply(W, 1, function(x){ all(!is.na(x))}))
        
        if(length(allobs) <= ncol(dmat)){
            stop("Insufficient cases of W with complete data to set scalemat - set scalemat manually")
        }

        W.obs     <- W[allobs,]
        dmat.obs  <- dmat[allobs,]
        target    <- sapply(1:nX, function(j){ var(resid(lm(W.obs[,j] ~ dmat.obs - 1))) })
        scalemat  <- get_bugs_wishart_scalemat(target, nsim=50000, reltol = 0.01, quietly=TRUE)$bugs.scalemat
        jags.data <- c(jags.data, "scalemat")        
    }

    if(blockprior){
        betaYXZ_zeros  <- rep(0.0, nXZ)
        betaYXZ_prec   <- (1e-8)*diag(nXZ)
        jags.data      <- c(jags.data,"betaYXZ_zeros","betaYXZ_prec")
    }

    ## #############################################
    ## generate initial values (unless they have already been passed)
    ##
    ## NOTES: convergence was fussy unless we tamed the starting values a bit.
    ## so start the regression coeffs involving Y at zero plus a little noise.
    ## start the other regression coefs at values based on the data, plus noise.
    ## start X at W.
    ## #############################################
    if(is.null(jags.inits)){

        ## regress W on (Z,G) to proxy for regression of X on (Z,G)
        dmat   <- cbind(Z, model.matrix(~G, contrasts.arg=list(G = "contr.sum"))[,-1])
        if(nX == 1){
            parts <- rep(0.0, ncol(dmat))
            if(sum(!is.na(W)) > ncol(dmat)){
                parts <- as.vector(coef(lm(W ~ dmat - 1, na.action=na.omit)))
            }
        } else {
            parts <- matrix(0.0, ncol=nX, nrow = ncol(dmat))
            for(j in 1:nX){
                if(sum(!is.na(W[,j])) > ncol(dmat)){
                    parts[,j] <- as.vector(coef(lm(W[,j] ~ dmat - 1, na.action=na.omit)))
                }
            }
        }
                
        ## set lists of initial values
        for(i in 1:jags.n.chains){

            .tmp                  <- list()
            .tmp$betaYG           <- c(rep(0.0, nG-1), NA)
            .tmp$X                <- W
            .tmp$X[is.na(.tmp$X)] <- 0.0
            .tmp$betaYXZ          <- rnorm(nXZ, sd=0.05)
            
            if(outcome_model %in% c("normal","normalME")){
                .tmp$sdYgivenXZG    <- runif(1)
            }
            
            if( (nX == 1) && (nZ == 1) ){
                .tmp$betaXZ         <- parts[1] + rnorm(1, sd=0.05)
                .tmp$betaXG         <- c(parts[-1] + rnorm(nG-1, sd=0.05), NA)
                .tmp$sdXgivenZG     <- runif(1)
            } else if( (nX == 1) && (nZ > 1) ){
                .tmp$betaXZ         <- parts[1:nZ] + rnorm(nZ, sd=0.05)
                .tmp$betaXG         <- c(parts[-(1:nZ)] + rnorm(nG-1, sd=0.05), NA)
                .tmp$sdXgivenZG     <- runif(1)
            } else if( (nX > 1) && (nZ == 1) ){
                .tmp$betaXZ         <- parts[1,1:nX] + rnorm(nX, sd=0.05)
                .tmp$betaXG         <- rbind(parts[-1,1:nX] + matrix(rnorm(nX*(nG-1), sd=0.05), ncol=nX), rep(NA, nX))
                .tmp$precXgivenZG   <- diag(nX)
            } else {
                .tmp$betaXZ         <- parts[1:nZ,1:nX] + matrix(rnorm(nX*nZ, sd=0.05), ncol=nX)
                .tmp$betaXG         <- rbind(parts[-(1:nZ),1:nX] + matrix(rnorm(nX*(nG-1), sd=0.05), ncol=nX), rep(NA, nX))
                .tmp$precXgivenZG   <- diag(nX)            
            }
            
            ## initialize missing values of Y and/or W to 0
            if(any(is.na(Y))){
                .tmp$Y <- Y
                is.na(.tmp$Y) <- TRUE
                .tmp$Y[is.na(Y)] <- 0.0
            }
            
            if(any(is.na(W))){
                .tmp$W <- W
                is.na(.tmp$W) <- TRUE
                .tmp$W[is.na(W)] <- 0.0
            }

            ## if outcome_model == "normalME", need to initial Yl as well
            if(outcome_model == "normalME"){
                zz <- Y
                zz[which(is.na(zz))] <- 0.0
                .tmp$Yl <- zz + rnorm(nR, sd=0.05)
            }
            
            jags.inits[[i]] <- .tmp
            rm(.tmp)
        }
    }
    
    ## #############################################
    ## create model file
    ## #############################################
    cat(paste0("model\n{\n\nfor(i in 1:",nR,"){\n\n"), file=modfile)
    
    ## "true covariate" model
    if(nX == 1){
        cat("EXgivenZG[i] <- ", file=modfile, append=TRUE)
        
        if(nZ == 1){
            cat("(betaXZ * Z[i]) + betaXG[gid[i]]\n", file=modfile, append=TRUE)
        } else{
            cat(paste0("inprod(betaXZ[1:",nZ,"], Z[i,1:",nZ,"]) + betaXG[gid[i]]\n"), file=modfile, append=TRUE)
        }
        cat("X[i] ~ dnorm(EXgivenZG[i], precXgivenZG)\n\n\n\n", file=modfile, append=TRUE)
    } else {
        for(j in 1:nX){
            cat(paste0("EXgivenZG[i,",j,"] <- "), file=modfile, append=TRUE)
            
            if(nZ == 1){
                cat(paste0("(betaXZ[",j,"] * Z[i]) + betaXG[gid[i],",j,"]\n"), file=modfile, append=TRUE)
            } else {
                cat(paste0("inprod(betaXZ[1:",nZ,",",j,"], Z[i,1:",nZ,"]) + betaXG[gid[i],",j,"]\n"), file=modfile, append=TRUE)
            }
        }
        cat(paste0("X[i,1:",nX,"] ~ dmnorm(EXgivenZG[i,1:",nX,"], precXgivenZG[1:",nX,",1:",nX,"])\n\n\n\n"), file=modfile, append=TRUE)
    }
    
    ## "measurement model"
    if(nX == 1){
        vf <- varfuncs[[1]]
        
        if(vf$type == "constant"){
            cat(paste0("W[i] ~ dnorm(X[i], ", (1.0 / vf$vtab),")\n\n\n"), file=modfile, append=TRUE)
        }
        
        if(vf$type == "piecewise_linear"){
            .K <- nrow(vf$vtab)
            cat(paste0("precWgivenX[i] <- 1.0 / ( "), file=modfile, append=TRUE)
            cat(paste0("(ifelse(X[i] <= ",vf$vtabi$xL[2],",",vf$vtabi$a[2],",0.0)) + "), file=modfile, append=TRUE)
            for(k in 2:.K){
                cat(paste0("(ifelse( (X[i] > ",vf$vtabi$xL[k],") && (X[i] <= ",vf$vtabi$xU[k],"), ",vf$vtabi$a[k]," + (",vf$vtabi$b[k],")*(X[i] - (",vf$vtabi$xL[k],")),0.0)) + "), file=modfile, append=TRUE)
            }
            cat(paste0("(ifelse(X[i] > ",vf$vtabi$xU[.K],",",vf$vtabi$a[.K+1],",0.0)) )\n"), file=modfile, append=TRUE)
            cat("W[i] ~ dnorm(X[i], precWgivenX[i])\n\n\n", file=modfile, append=TRUE)
        }
        
        if(vf$type == "log_polynomial"){
            .K <- nrow(vf$vtab)
            ## create string for polynomial
            p1      <- paste0("(",as.vector(coef(vf$logmod)),")")
            p2      <- c("","(X[i])",paste0("(X[i]^",2:(vf$degree),")"))
            tmp     <- paste(p1, p2, sep="*")
            tmp[1]  <- gsub("*","",tmp[1],fixed=TRUE)
            pstring <- paste0("exp( ",paste(tmp, collapse=" + ")," )")
            
            cat(paste0("precWgivenX[i] <- 1.0 / ( "), file=modfile, append=TRUE)        
            cat(paste0("(ifelse(X[i] <= ",vf$vtab$x[1], ",",vf$vtab$gx[1], ",0.0)) + "), file=modfile, append=TRUE)
            cat(paste0("(ifelse(X[i] > " ,vf$vtab$x[.K],",",vf$vtab$gx[.K],",0.0)) + "), file=modfile, append=TRUE)
            cat(paste0("(ifelse( (X[i] > ",vf$vtab$x[1],") && (X[i] <= ",vf$vtab$x[.K],"), ",pstring,", 0.0)) )\n"), file=modfile, append=TRUE)
            cat("W[i] ~ dnorm(X[i], precWgivenX[i])\n\n\n", file=modfile, append=TRUE)
        }
    } else {
        for(j in 1:nX){
            vf <- varfuncs[[j]]
            
            if(vf$type == "constant"){
                cat(paste0("W[i,",j,"] ~ dnorm(X[i,",j,"], ", (1.0 / vf$vtab),")\n\n\n"), file=modfile, append=TRUE)
            }
            
            if(vf$type == "piecewise_linear"){
                .K <- nrow(vf$vtab)
                cat(paste0("precWgivenX[i,",j,"] <- 1.0 / ( "), file=modfile, append=TRUE)
                cat(paste0("(ifelse(X[i,",j,"] <= ",vf$vtabi$xL[2],",",vf$vtabi$a[2],",0.0)) + "), file=modfile, append=TRUE)
                for(k in 2:.K){
                    cat(paste0("(ifelse( (X[i,",j,"] > ",vf$vtabi$xL[k],") && (X[i,",j,"] <= ",vf$vtabi$xU[k],"), ",vf$vtabi$a[k]," + (",vf$vtabi$b[k],")*(X[i,",j,"] - (",vf$vtabi$xL[k],")),0.0)) + "), file=modfile, append=TRUE)
                }
                cat(paste0("(ifelse(X[i,",j,"] > ",vf$vtabi$xU[.K],",",vf$vtabi$a[.K+1],",0.0)) )\n"), file=modfile, append=TRUE)
                cat(paste0("W[i,",j,"] ~ dnorm(X[i,",j,"], precWgivenX[i,",j,"])\n\n\n"), file=modfile, append=TRUE)
            }
            
            if(vf$type == "log_polynomial"){
                .K <- nrow(vf$vtab)
                ## create string for polynomial
                p1      <- paste0("(",as.vector(coef(vf$logmod)),")")
                p2      <- c("",paste0("(X[i,",j,"])"),paste0("(X[i,",j,"]^",2:(vf$degree),")"))
                tmp     <- paste(p1, p2, sep="*")
                tmp[1]  <- gsub("*","",tmp[1],fixed=TRUE)
                pstring <- paste0("exp( ",paste(tmp, collapse=" + ")," )")
                
                cat(paste0("precWgivenX[i,",j,"] <- 1.0 / ( "), file=modfile, append=TRUE)
                cat(paste0("(ifelse(X[i,",j,"] <= ",vf$vtab$x[1], ",",vf$vtab$gx[1], ",0.0)) + "), file=modfile, append=TRUE)
                cat(paste0("(ifelse(X[i,",j,"] > " ,vf$vtab$x[.K],",",vf$vtab$gx[.K],",0.0)) + "), file=modfile, append=TRUE)
                cat(paste0("(ifelse( (X[i,",j,"] > ",vf$vtab$x[1],") && (X[i,",j,"] <= ",vf$vtab$x[.K],"), ",pstring,", 0.0)) )\n"), file=modfile, append=TRUE)
                cat(paste0("W[i,",j,"] ~ dnorm(X[i,",j,"], precWgivenX[i,",j,"])\n\n\n"), file=modfile, append=TRUE)
            }
        }
    }
    
    ## "outcome model"
    if( (nX == 1) && (nZ == 1) ){
        cat(paste0("eta[i] <- (betaYXZ[1] * X[i]) + (betaYXZ[2] * Z[i])  + betaYG[gid[i]]\n"), file=modfile, append=TRUE)
    } else if( (nX == 1) && (nZ > 1) ){
        cat(paste0("eta[i] <- (betaYXZ[1] * X[i]) + inprod(betaYXZ[2:",nXZ,"],Z[i,1:",nZ,"]) + betaYG[gid[i]]\n"), file=modfile, append=TRUE)
    } else if( (nX > 1) && (nZ == 1) ){
        cat(paste0("eta[i] <- inprod(betaYXZ[1:",nX,"],X[i,1:",nX,"]) + (betaYXZ[",nXZ,"] * Z[i]) + betaYG[gid[i]]\n"), file=modfile, append=TRUE)
    } else {
        cat(paste0("eta[i] <- inprod(betaYXZ[1:",nX,"],X[i,1:",nX,"]) + inprod(betaYXZ[",(nX+1),":",nXZ,"],Z[i,1:",nZ,"]) +  betaYG[gid[i]]\n"), file=modfile, append=TRUE)
    }
    
    if(outcome_model == "normal"){
        cat("Y[i] ~ dnorm(eta[i], precYgivenXZG)\n}\n\n", file=modfile, append=TRUE)
    } else if(outcome_model == "poisson"){
        cat("Y[i] ~ dpois(exp(eta[i]))\n}\n\n", file=modfile, append=TRUE)
    } else if(outcome_model == "bernoulli_probit"){
        cat("Y[i] ~ dbern(phi(eta[i]))\n}\n\n", file=modfile, append=TRUE)
    } else if(outcome_model == "bernoulli_logit"){
        cat("Y[i] ~ dbern(1.0 / (1.0 + exp(-1.0 * eta[i])))\n}\n\n", file=modfile, append=TRUE)
    }
    
    ## if outcome_model == "normalME", need to add measurement model for Y.  Introduce
    ## latent Y variable called "Yl" where Yl follows the outcome model used in "normal"
    ## but then Y measures Yl with the error structure given in the last component of varfuncs
    if(outcome_model == "normalME"){
        cat("Yl[i] ~ dnorm(eta[i], precYgivenXZG)\n\n", file=modfile, append=TRUE)

        vf <- varfuncs[[nX+1]]
        
        if(vf$type == "constant"){
            cat(paste0("Y[i] ~ dnorm(Yl[i], ", (1.0 / vf$vtab),")\n}\n\n"), file=modfile, append=TRUE)
        }
        
        if(vf$type == "piecewise_linear"){
            .K <- nrow(vf$vtab)
            cat(paste0("precYgivenYl[i] <- 1.0 / ( "), file=modfile, append=TRUE)
            cat(paste0("(ifelse(Yl[i] <= ",vf$vtabi$xL[2],",",vf$vtabi$a[2],",0.0)) + "), file=modfile, append=TRUE)
            for(k in 2:.K){
                cat(paste0("(ifelse( (Yl[i] > ",vf$vtabi$xL[k],") && (Yl[i] <= ",vf$vtabi$xU[k],"), ",vf$vtabi$a[k]," + (",vf$vtabi$b[k],")*(Yl[i] - (",vf$vtabi$xL[k],")),0.0)) + "), file=modfile, append=TRUE)
            }
            cat(paste0("(ifelse(Yl[i] > ",vf$vtabi$xU[.K],",",vf$vtabi$a[.K+1],",0.0)) )\n"), file=modfile, append=TRUE)
            cat("Y[i] ~ dnorm(Yl[i], precYgivenYl[i])\n}\n\n", file=modfile, append=TRUE)
        }
        
        if(vf$type == "log_polynomial"){
            .K <- nrow(vf$vtab)
            ## create string for polynomial
            p1      <- paste0("(",as.vector(coef(vf$logmod)),")")
            p2      <- c("","(Yl[i])",paste0("(Yl[i]^",2:(vf$degree),")"))
            tmp     <- paste(p1, p2, sep="*")
            tmp[1]  <- gsub("*","",tmp[1],fixed=TRUE)
            pstring <- paste0("exp( ",paste(tmp, collapse=" + ")," )")
            
            cat(paste0("precYgivenYl[i] <- 1.0 / ( "), file=modfile, append=TRUE)
            cat(paste0("(ifelse(Yl[i] <= ",vf$vtab$x[1], ",",vf$vtab$gx[1], ",0.0)) + "), file=modfile, append=TRUE)
            cat(paste0("(ifelse(Yl[i] > " ,vf$vtab$x[.K],",",vf$vtab$gx[.K],",0.0)) + "), file=modfile, append=TRUE)
            cat(paste0("(ifelse( (Yl[i] > ",vf$vtab$x[1],") && (Yl[i] <= ",vf$vtab$x[.K],"), ",pstring,", 0.0)) )\n"), file=modfile, append=TRUE)
            cat("Y[i] ~ dnorm(Yl[i], precYgivenYl[i])\n}\n\n", file=modfile, append=TRUE)
        }
    }
    
    ## priors: regression coefficients
    if( (nX == 1) && (nZ == 1) ){
        cat(paste0("betaXZ ~ dnorm(0.0, 1e-8)\n"), file=modfile, append=TRUE)
    } else if( (nX == 1) && (nZ > 1) ){
        cat(paste0("for(i in 1:",nZ,"){\n  betaXZ[i] ~ dnorm(0.0, 1e-8)\n}\n\n"), file=modfile, append=TRUE)
    } else if( (nX > 1) && (nZ == 1) ){
        cat(paste0("for(i in 1:",nX,"){\n  betaXZ[i] ~ dnorm(0.0, 1e-8)\n}\n\n"), file=modfile, append=TRUE)
    } else {
        cat(paste0("for(i in 1:",nZ,"){\n  for(j in 1:",nX,"){\n    betaXZ[i,j] ~ dnorm(0.0, 1e-8)\n  }\n}\n\n"), file=modfile, append=TRUE)
    }
    if(blockprior){
        cat(paste0("betaYXZ[1:",nXZ,"] ~ dmnorm(betaYXZ_zeros[1:",nXZ,"], betaYXZ_prec[1:",nXZ,",1:",nXZ,"])\n"), file=modfile, append=TRUE)
    } else { 
        cat(paste0("for(i in 1:",nXZ,"){\n  betaYXZ[i] ~ dnorm(0.0, 1e-8)\n}\n\n"), file=modfile, append=TRUE)
    }
    cat("\n\n", file=modfile, append=TRUE)
    
    ## priors: sum-to-zero group effects on X and Y
    cat(paste0("for(g in 1:",(nG-1),"){\n  betaYG[g] ~ dnorm(0.0, 1e-8)\n}\nbetaYG[",nG,"] <- -1.0 * sum(betaYG[1:",(nG-1),"])\n\n"), file=modfile, append=TRUE)
    
    if(nX == 1){
        cat(paste0("for(g in 1:",(nG-1),"){\n  betaXG[g] ~ dnorm(0.0, 1e-8)\n}\nbetaXG[",nG,"] <- -1.0 * sum(betaXG[1:",(nG-1),"])\n\n"), file=modfile, append=TRUE)
    } else {
        cat(paste0("for(j in 1:",nX,"){\n  for(g in 1:",(nG-1),"){\n    betaXG[g,j] ~ dnorm(0.0, 1e-8)\n  }\n  betaXG[",nG,",j] <- -1.0 * sum(betaXG[1:",(nG-1),",j])\n}\n\n"), file=modfile, append=TRUE)
    }
    
    ## priors: variance components
    if(nX == 1){
        cat(paste0("sdXgivenZG ~ dunif(0,",max(10, 2*.sdXgivenZG),")\nvarXgivenZG <- sdXgivenZG^2\nprecXgivenZG <- 1.0/varXgivenZG\n\n"), file=modfile, append=TRUE)
    } else {
        cat(paste0("precXgivenZG[1:",nX,",1:",nX,"] ~ dwish(scalemat[1:",nX,",1:",nX,"], ",nX+1,")\n"), file=modfile, append=TRUE)
        cat(paste0("varXgivenZG[1:",nX,",1:",nX,"] <- inverse(precXgivenZG[,])\n\n"), file=modfile, append=TRUE)
    }
    
    if(outcome_model %in% c("normal","normalME")){
        cat(paste0("sdYgivenXZG ~ dunif(0,",max(10, 2*.sdYgivenXZG),")\nvarYgivenXZG <- sdYgivenXZG^2\nprecYgivenXZG <- 1.0/varYgivenXZG\n\n"), file=modfile, append=TRUE)
    }
    
    cat("}\n", file=modfile, append=TRUE)
        
    ## ###########################################
    ## if desired, stop here and just return model file
    ## ###########################################
    if(modelfileonly){
        cat("model file location returned\n")
        return(modfile)
    }
    
    ## ###########################################
    ## run JAGS and return
    ## ###########################################
    flush.console()
    print(system.time(r <- jags(
                          model.file         = modfile,
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
                          jags.module        = jags.jags.module)))

    r$lr_ancova_extras <- list(model.location = modfile,
                               plot.location  = plotfile,
                               group.map      = group.map,
                               scalemat       = scalemat)
    return(r)
}
