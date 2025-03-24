eivreg <- function(formula, data, subset, weights, na.action, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = FALSE, contrasts = NULL, reliability = NULL, Sigma_error = NULL, cluster_varname = NULL, df_adj = FALSE, stderr = TRUE, offset, ...) {

    ###############################################################
    ## errors in variable regression.  code adapted from lm().
    ##
    ## function fits both the standard (uncorrected) linear model and the
    ## corrected version
    ###############################################################
    if(singular.ok) {
        stop("singular EIV models not currently implemented")
    }
    
    if( (is.null(reliability) && is.null(Sigma_error)) || (!is.null(reliability) && !is.null(Sigma_error)) ){
        stop("exactly one of 'reliability' or 'Sigma_error' must be specified")
    }
    
    ## check vector of reliabilities
    if( !is.null(reliability) ){
        if( !is.numeric(reliability) || !is.vector(reliability) || !is.null(dim(reliability)) ){
            stop("reliability must be numeric vector")
        }
        if(any(is.na(reliability))){
            stop("reliability cannot have missing values")
        }
        relnames  <- names(reliability)
        if(is.null(relnames)){
            stop("reliability vector must be named")
        }
        if(any(duplicated(relnames))){
            stop("reliability names not unique")
        }
        if(any(reliability <= 0) || any(reliability > 1)){
            stop("reliability must take values in (0,1]")
        }
        numep <- length(reliability) # number of error-prone covariates
    }

    ## check error covariance matrix
    if( !is.null(Sigma_error) ){
        if( !is.numeric(Sigma_error) || !is.matrix(Sigma_error) || (nrow(Sigma_error) != ncol(Sigma_error)) ){
            stop("Sigma_error must be square numeric matrix")
        }
        if(any(is.na(Sigma_error))){
            stop("Sigma_error cannot have missing values")
        }
        if(is.null(dimnames(Sigma_error))){
            stop("Sigma_error must have dimnames")
        }
        numep <- nrow(Sigma_error)
        .colnames <- colnames(Sigma_error)
        .rownames <- rownames(Sigma_error)
        if(!is.null(.colnames) && !is.null(.rownames) && any(.colnames != .rownames)){
            stop("measurement error covariance matrix has mismatched row and column names")
        }
        if(!is.null(.colnames)){
            relnames <- rownames(Sigma_error) <- .colnames
        } else {
            relnames <- colnames(Sigma_error) <- .rownames
        }
        if(any(duplicated(relnames))){
            stop("Sigma_error names not unique")
        }
        if( (max(abs(Sigma_error - t(Sigma_error))) > 1e-10) || any(eigen(Sigma_error)$values < 0) ){
            stop("Sigma_error must be symmetric and positive semi-definite")
        }
    }

    ## basic checks on cluster_varname argument (more checks on content later)
    if( !is.null(cluster_varname) ){
        if( !is.character(cluster_varname) ){
            stop("cluster_varname must be a character string providing the name of the clustering variable")
        }
        if( !is.data.frame(data) ){
            stop("cluster option requires data argument")
        }
        if( !(cluster_varname %in% names(data)) ){
            stop("cluster_varname is not in data")
        }
    }
    
    
    ###############################################################
    ## begin code taken from lm()
    ###############################################################
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m  <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame"){
        return(mf)
    } else if (method != "qr") {
        warning(gettextf("method = '%s' is not supported. Using 'qr'", method), domain = NA)
    }  
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) {
        stop("'weights' must be a numeric vector")
    }
    
    ## NOTE: added to ensure that weights are positive and sum to the number of
    ## observations, which is the only case we are considering for EIV regression.
    ## Also note that cases with missing weights are dropped prior to this, just
    ## like cases with missing covariates.
    if ( !is.null(w) ){
        if( any(w <= 0) ){
            stop("weights must be positive")
        }
        w <- length(w) * (w / sum(w))
    }
    
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(y)) {
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)", length(offset), NROW(y)), domain = NA)
        }
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
                3) else numeric(), residuals = y, fitted.values = 0 * 
              y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
                                           0) else if (is.matrix(y)) nrow(y) else length(y))
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w)) 
                 lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                        ...)
             else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                          ...)
    }
  
    class(z) <- c(if (is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- x
    if (ret.y) 
        z$y <- y
    
    ## NOTE: QR needed later, so this code moved down
    ## if (!qr) 
    ##  z$qr <- NULL
    
    ###############################################################
    ## end code taken from lm()
    ###############################################################

    class(z) <- c("eivlm")

    if(is.null(x)){
        return(z)
    }
  
    if(NCOL(y) > 1){
        stop("EIV models with matrix response not currently implemented")
    }

    ###############################################################
    ## deal with offset:
    ## define as 0s if it doesn't exist and use it throughout to simplify code
    ###############################################################
    z$N <- .N  <- nrow(x)
    if(is.null(offset)){
        offset <- rep(0, .N)
    }
    ymo <- y - offset
    
    ###############################################################
    ## deal with weights:
    ## define as 1s if it doesn't exist and use it throughout to simplify code.
    ###############################################################
    if( !is.null(w) ){
        wgt <- z$weights
    } else {
        wgt <- rep(1.0, .N)
    }
    
    ###############################################################
    ## deal with cluster:
    ## if cases with missing values were dropped during fit, need to align
    ###############################################################
    if( !is.null(cluster_varname) ){
        z$cluster_varname <- cluster_varname
        z$cluster_values  <- data[,cluster_varname]

        if( !is.null(attr(mf, "na.action")) ){
            z$cluster_values <- z$cluster_values[ setdiff(1:nrow(data), as.vector(attr(mf, "na.action"))) ]
            stopifnot(length(z$cluster_values) == .N)
        }

        if( any(is.na(z$cluster_values)) ){
            stop("clustering variable cannot have missing values among cases used in estimation")
        }

        z$cluster_num <- length(unique(z$cluster_values))
        if( z$cluster_num < 2 ){
            stop("clustering variable must take on at least two distinct values")
        }
    }
  
    ###############################################################
    ## test that all error-prone variables are numeric
    ## and show up only as main effects
    ###############################################################
    v <- attributes(mt)$term.labels
    if( !all(relnames %in% v) ){
        stop("there do not appear to be main effects for all error-prone variables")
    }
    if( !all(attributes(mt)$dataClasses[relnames] == "numeric") ){
        stop("error-prone variables must be numeric")
    }
    v <- make.names(v)
    ok <- sapply(relnames, function(n){
        length(grep(paste("^",n,"\\.+|\\.+",n,"\\.+|\\.+",n,"$",sep=""), v)) == 0
    })
    if( !all(ok) ){
        stop("at least one error-prone variable is used in a term that is not a main effect\n  Not currently implemented")
    }

    ###############################################################
    ## get cross-product matrix and correction matrix "S"
    ###############################################################
    tmp         <- cov.wt( cbind(ymo, x), wt = wgt, method = "ML")
    meanYWZ     <- tmp$center
    varYWZ      <- tmp$cov
    rm(tmp); gc()
    
    XpX         <- crossprod(qr.R(z$qr)) ## == crossprod(x * sqrt(wgt))
    S           <- matrix(0.0, ncol = ncol(XpX), nrow = nrow(XpX))
    dimnames(S) <- dimnames(XpX)
    posep       <- match(relnames, colnames(S)) ## positions of error-prone covariates
    stopifnot(all(relnames == colnames(x)[posep]))

    if( !is.null(reliability) ){
        diag(S)[posep]             <- .N * (1.0 - reliability) * diag(varYWZ)[1 + posep]
    } else {
        for(.i in 1:numep){
            for(.j in 1:numep){
                S[posep[.i],posep[.j]] <- .N * Sigma_error[.i,.j]
            }
        }
    }

    ###############################################################
    ## check that estimated covariance matrix of (Y,X,Z) is valid
    ##
    ## NOTE: Stata manual notes that reliability for each variable must be greater
    ## than or equal to the R^2 of the regression of that variable on all others
    ## plus the outcome. This condition appears to be equivalent to testing that
    ## the estimated covariance matrix of (Y,X,Z) is positive semi-definite.  Need
    ## to be careful because the presence of an intercept among (X,Z), or columns
    ## that add up to an intercept (e.g. mutually exclusive and exhaustive
    ## grouping indicators) leads varYXZ to have an eigenvalue of zero.  There
    ## should be no more than one such eigenvalue because we are not allowing
    ## singular.ok=TRUE.  Numerically, such an eigenvalue may evaluate to a number
    ## that is nearly zero, but still negative.  So we have to invoke a threshold
    ## to decide that a negative eigenvalue is sufficiently negative to indicate a
    ## problem.
    ##
    ## NOTE: adjusted regression coefficients could be computed directly from varYXZ
    ##############################################################
    varYXZ <- varYWZ - (rbind( rep(0, ncol(x) + 1), cbind( rep(0, ncol(x)), S)) /.N)
    if(any(eigen(varYXZ)$values <  -1e-8)){
        stop("reliability is too small (or measurement error variances are too large)")
    }
    gc(verbose=FALSE)

    ###############################################################
    ## get adjusted coefficients and build other pieces
    ###############################################################
    z$unadj_coefficients  <- z$coefficients
    z$coefficients        <- bhat  <- drop(solve(XpX - S, crossprod(x * wgt, ymo)))

    z$unadj_fitted.values <- z$fitted.values
    z$fitted.values       <- drop(x %*% bhat) + offset

    z$unadj_residuals     <- z$residuals
    z$residuals           <- y - z$fitted.values

    z$unadj_effects       <- z$effects
    z$effects             <- NULL
  
    if(!qr){
        z$qr <- NULL
    } else {
        z$unadj_qr <- z$qr
        z$qr       <- NULL
    }
  
    z$reliability   <- reliability
    z$Sigma_error   <- Sigma_error
    z$relnames      <- relnames
    z$XpX_adj       <- XpX - S
    z$varYXZ        <- varYXZ
    z$latent_resvar <- (sum(wgt * (ymo^2)) - as.numeric( matrix(bhat,nrow=1) %*% z$XpX_adj %*% matrix(bhat,ncol=1) )) / z$df.residual

    ###############################################################
    ##                   Sandwich SEs
    ## NOTE: loop over observations rather than use lapply to reduce
    ## storage problems with large dim models
    ##
    ###############################################################

    if(stderr){

        ## Fork on whether we are in the known reliability or known error
        ## variance case to define function that computes psi and psidot
        if( !is.null(reliability) ){
            get_psi_and_psidot <- function(i, theta){
                ## i     = observation index in 1:.N
                ## theta = (means of error-prone covariates, MLE variances of error-prone covariates, beta)
                ## NOTE: references (reliability, ymo, x, posep, numep) in parent environment
                stopifnot(length(theta) == (2*numep + ncol(x)))
                vhat              <- rep(0.0, ncol(x))
                vhat[posep]       <- (1.0 - reliability) * theta[numep + 1:numep]
                qi                <- matrix(x[i,], ncol=1)
                qqprime_adj       <- qi %*% t(qi)
                diag(qqprime_adj) <- diag(qqprime_adj) - vhat
              
                psi  <- as.vector(c( qi[posep,] - theta[1:numep],
                (qi[posep,] - theta[1:numep])^2 - theta[numep + 1:numep],
                ymo[i]*qi - (qqprime_adj %*% theta[2*numep + 1:ncol(x)])))

                row1 <- cbind(-1.0*diag(numep),
                              matrix(0.0, nrow=numep, ncol=numep),
                              matrix(0.0, nrow=numep, ncol=ncol(x)))
                
                row2 <- cbind(-2.0*diag(as.vector(qi[posep,] - theta[1:numep]),nrow=numep, ncol=numep),
                              -1.0 * diag(numep),
                              matrix(0.0, nrow=numep, ncol=ncol(x)))
                
                ## tricky piece: derivatives of beta equations w.r.t. error-prone variable latent variances
                tmp  <- matrix(0.0, nrow=ncol(x), ncol=numep)
                for(j in 1:numep){
                    tmp[posep[j],j] <- (1.0 - reliability[j]) * theta[2*numep + posep[j]]
                }
                row3   <- cbind(matrix(0.0, nrow=ncol(x), ncol=numep), tmp, -1.0 * qqprime_adj)
                psidot <- rbind(row1, row2, row3)
                stopifnot( (nrow(psidot) == ncol(psidot)) && (nrow(psidot) == (2*numep + ncol(x))) )
              
                return(list(psi = psi, psidot = psidot))
            }
        } else {
            get_psi_and_psidot <- function(i, theta){
                ## i     = observation index in 1:.N
                ## theta = beta
                ## NOTE: references (S, ymo, x, posep) in parent environment
                stopifnot(length(theta) == ncol(x))
                qi           <- matrix(x[i,], ncol=1)
                qqprime_adj  <- qi %*% t(qi)
                qqprime_adj  <- qqprime_adj - (S / .N)
                
                return(list(psi    = as.vector(ymo[i]*qi - (qqprime_adj %*% theta)),
                            psidot = -1.0 * qqprime_adj))
            }
        }

        ##############################################
        ## compute sandwich variance/covariance matrix
        ##############################################

        ## 1) define estimated parameter
        if( !is.null(reliability) ){
            thetahat  <- c(meanYWZ[1 + posep], diag(varYWZ)[1 + posep], bhat)
        } else {
            thetahat  <- bhat
        }

        ## 2) get psi and psidot at solution and check that psi satisfies appropriate constraint
        psi       <- matrix(0.0, nrow = .N,               ncol = length(thetahat))
        psidot    <- matrix(0.0, nrow = length(thetahat), ncol = length(thetahat))
        for(i in 1:.N){
            tmp     <- get_psi_and_psidot(i, theta = thetahat)
            psi[i,] <- tmp$psi
            psidot  <- psidot + (wgt[i] * tmp$psidot)
        }
        psidot <- psidot / .N
        if(max(abs(apply(psi, 2, FUN = weighted.mean, w = wgt))) > 1e-9){
            stop("EIV solution does not satisfy estimating equations - consider changing scale of data")
        }    
        gc(verbose=FALSE)

        ## 3) compute variance, restricting to regression coeffs in known reliability case
        ## NOTE: B is either the sum of individual-level outer products, or the sum across
        ## clusters of the outer products of the cluster sums.
        if( is.null(cluster_varname) ){
            B       <- crossprod(psi * wgt)
        } else {
            grpsums <- apply(psi * wgt, 2, function(x){ tapply(x, z$cluster_values, sum) })
            B       <- Reduce("+",lapply(1:nrow(grpsums), function(i){ tcrossprod(grpsums[i,]) }))
        }
        
        Ainv   <- solve(psidot)
        fudge  <- ifelse(df_adj, (.N / z$df.residual), 1.0) *
            ifelse(!is.null(z$cluster_varname), (z$cluster_num / (z$cluster_num -1)), 1.0)
        z$vcov <- fudge * (Ainv %*% (B / .N^2) %*% t(Ainv))
        if( !is.null(reliability) ){
            z$vcov <- z$vcov[2*numep + 1:ncol(x),2*numep + 1:ncol(x)]
        }
        rownames(z$vcov) <- colnames(z$vcov) <- colnames(x)

        rm(psi, psidot)
        gc(verbose=FALSE)
    }
    
    z
}
