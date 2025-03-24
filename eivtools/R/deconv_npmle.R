deconv_npmle <- function(W, csem, gridspec = list(fixed=FALSE, xmin=-5, xmax=5, numpoints=2000), lambda = 0.0005, lltol = 1e-7, psmall = 0.00005, discrete = FALSE, quietly = FALSE){
    
    if(any(is.na(W))){
        stop("missing values not allowed in W")
    }

    if(!is.function(csem)){
        stop("csem must be a function")
    }

    if(!is.list(gridspec)){
        stop("gridspec must be a list; see documentation")
    }
    
    if(is.null(gridspec$fixed) || !is.logical(gridspec$fixed)){
        stop("gridspec must contain a logical component 'fixed'; see documentation")
    }

    case1 <- !is.null(gridspec$xmin) && !is.null(gridspec$xmax) & !is.null(gridspec$numpoints)
    case2 <- !is.null(gridspec$grid)

    if( !(abs(case1 + case2 - 1) < 1e-10) ){
        stop("gridspec must specify -either- 'grid' -or- the triplet 'xmin','xmax' and 'numpoints'")
    }

    ## fork on whether range/numpoints passed, or specific grid is passed
    if(case1){
        if( !is.numeric(gridspec$xmin) || (length(gridspec$xmin) > 1) ){
            stop("'xmin' element of gridspec must be single numeric value'")
        }

        if( !is.numeric(gridspec$xmax) || (length(gridspec$xmax) > 1) ){
            stop("'xmax' element of gridspec must be single numeric value'")
        }
        
        if(gridspec$xmax <= gridspec$xmin){
            stop("invalid xmax/xmin in gridspec")
        }

        if( !is.numeric(gridspec$numpoints) || (length(gridspec$numpoints) > 1) ){
            stop("'numpoints' element of gridspec must be single integer value")
        }

        if(min(W) < gridspec$xmin){
            stop("xmin exceeds smallest data value, consider decreasing xmin")
        }
    
        if(max(W) > gridspec$xmax){
            stop("largest data value exceeds xmax; consider increasing xmax")
        }

        gridspec$grid <- seq(from   = gridspec$xmin,
                             to     = gridspec$xmax,
                             length = gridspec$numpoints)
    } else {
        if( any(is.na(gridspec$grid)) ){
            stop("missing values in specified grid")
        }

        if( length(unique(gridspec$grid)) != length(gridspec$grid) ){
            stop("specified grid has duplicate values")
        }
        
        if( any(sort(gridspec$grid) != gridspec$grid) ){
            stop("specified grid is not sorted in increasing numeric order")
        }

        gridspec$xmin      <- min(gridspec$grid)
        gridspec$xmax      <- max(gridspec$grid)
        gridspec$numpoints <- length(gridspec$grid)
    }
    gridspec <- gridspec[c("fixed","xmin","xmax","numpoints","grid")]
    
    if(lambda <= 0){
        stop("invalid lambda")
    }
    
    if(lltol <= 0){
        stop("invalid lltol")
    }

    if(psmall <= 0){
        stop("invalid psmall")
    }
    
    if(discrete){
        stop("discrete option not currently implemented")
    }
    
    ## NOTE: we collapse over repeat W values when possible, using "counts" to contribute to likelihood
    varw    <- var(W)
    .W      <- sort(unique(W))
    nW      <- length(.W)
    .counts <- sapply(.W, function(x){ sum(W == x) })
    stopifnot(sum(.counts) == length(W))
    W       <- .W
    
    ## functions to map probabilities to reduced-dimension unconstrained scale, and inverse
    theta_to_p <- function(theta){
        if(length(theta) == 0){
            return(1)
        } else {
            p <- exp(theta) / (1 + sum(exp(theta)))
            return(c(p, 1-sum(p)))
        }
    }
    
    p_to_theta <- function(p){
        stopifnot( all(p > 0) && (abs((1 - sum(p))) < 1e-9) )
        if(length(p) == 1){
            return(numeric(0))
        } else {
            last <- length(p)
            return(log(p[1:(last-1)] * (1 + ((1-p[last])/p[last])) ))
        }
    }

    ##############
    ##  CHECKS
    ##############
    ## p <- c(0.1, 0.2, 0.4, 0.3); print(max(abs(p - theta_to_p(p_to_theta(p)))))
    ## theta <- c(-3.2, 1.0, -1.0); print(max(abs(theta - p_to_theta(theta_to_p(theta)))))
    ## print(max(abs(1 - theta_to_p(p_to_theta(1)))))
    ## p_to_theta(theta_to_p(numeric(0)))
    
    ## build (nW x gridspec$numpoints) matrix of conditional densities
    grid.csem <- csem(gridspec$grid)

    if(length(grid.csem) != gridspec$numpoints){
        stop("csem() is not returning a vector of appropriate length")
    }

    if(any(grid.csem < 0)){
        stop("csem() resulted in negative values somewhere on the grid")
    }
    
    ## if(!discrete){
    fwx <- dnorm(matrix(W, ncol=gridspec$numpoints, nrow=nW),
                 mean = matrix(gridspec$grid, ncol=gridspec$numpoints, nrow=nW, byrow=TRUE),
                 sd = matrix(grid.csem, ncol=gridspec$numpoints, nrow=nW, byrow=TRUE))
    ##  } else { ## uses pxgu - fwx[i,j] = p(W=W[i] | X = grid[j])
    ## tmp <- lapply(grid, function(u){ pxgu(u, csem(u), .W)})
    ## stopifnot(all(sapply(tmp, function(x){ all(x[,1] == .W) })))
    ## fwx <- do.call("cbind", lapply(tmp, function(x){ x[,2] }))
    ## }
  
    ## negative log likelihood for a set of probabilities given ".inds" which are
    ## indices of gridspec$grid and which are continually updated
    negll <- function(theta){
        -sum(.counts * log(as.vector(fwx[,.inds,drop=FALSE] %*% theta_to_p(theta))))
    }

    ## now fork on whether the grid is fixed
    .history <- list()
    
    if( !gridspec$fixed ){ ## optimize the grid
    
        ## find initial best grid point.  NOTE this will often want the X with the
        ## biggest CSEM because a single point is inconsistent with most W unless the
        ## CSEM is large. the exception is if we pick grid to be very large, then it
        ## will find interior point.  however even if it picks an extreme starting
        ## point, that point will be dropped later if it is too far in the tails.
        ll <- apply(fwx, 2, function(x){ sum(.counts * log(x)) })
        .inds <- which.max(ll)
        .probs   <- 1
        .eligible <- setdiff(1:gridspec$numpoints, .inds)
        ll.current <- ll[.inds]
        
        .history[[1]] <- data.frame(x = gridspec$grid[.inds],
                                    csem = grid.csem[.inds],
                                    p = 1,
                                    ll = ll.current,
                                    ex = gridspec$grid[.inds],
                                    varx = 0)
        
        ## now add grid points per the algorithm until there is no improvement
        done <- FALSE  
        while(!done){
            ## evaluate each eligible point with a weight "lambda"
            part.static   <- as.vector(fwx[,.inds,drop=FALSE] %*% ((1 - lambda)*.probs))
            part.total   <- matrix(part.static, ncol=length(.eligible), nrow=nW) + (fwx[,.eligible] * lambda)
            ll.candidate <- apply(part.total, 2, function(x){ sum(.counts * log(x)) })
            if(all(ll.candidate - ll.current < lltol)){
                done <- TRUE
            } else {
                w <- which.max(ll.candidate - ll.current)
                .inds <- c(.inds, .eligible[w])
                .eligible <- setdiff(.eligible, .eligible[w])
                
                ## set starting value: mass 0.05 at new value, normalized p on existing values
                o <- optim(p_to_theta(c(0.95*(.probs/sum(.probs)), 0.05)), negll,
                           method  = "BFGS",
                           control = list(maxit=1000, trace=5*(1 - as.numeric(quietly)), REPORT = 1))
                .probs <- theta_to_p(o$par)
                ll.current <- -o$value
                
                ## sometimes we might have picked up an early grid point
                ## but now it has virtually no probability, so dump it
                w <- which(.probs < psmall)
                if(length(w) > 0){
                    .probs <- .probs[-w]
                    .probs <- .probs/sum(.probs)
                    .inds  <- .inds[-w]
                }
                
                ## reorder the grid if need be
                o <- order(.inds)
                .inds <- .inds[o]
                .probs <- .probs[o]
                
                ## summarize the state
                .history[[length(.history)+1]] <-
                    data.frame(x = gridspec$grid[.inds],
                               csem = grid.csem[.inds],
                               p = .probs,
                               ll = ll.current,
                               ex = sum(gridspec$grid[.inds] * .probs),
                               varx = sum(gridspec$grid[.inds]^2 * .probs) - (sum(gridspec$grid[.inds] * .probs))^2)
                if(!quietly){
                    print(.history[[length(.history)]])
                    cat("\n\n\n")      
                }
            }
        }
        px <- .history[[length(.history)]]
        
    } else { ## fixed grid
        .inds <- 1:gridspec$numpoints

        o <- optim(rep(0.0, gridspec$numpoints-1), negll,
                   method  = "BFGS",
                   control = list(maxit=1000, trace=5*(1 - as.numeric(quietly)), REPORT = 1))
        .probs <- theta_to_p(o$par)

        px <- data.frame(x = gridspec$grid[.inds],
                         csem = grid.csem[.inds],
                         p = .probs,
                         ll = -o$value,
                         ex = sum(gridspec$grid[.inds] * .probs),
                         varx = sum(gridspec$grid[.inds]^2 * .probs) - (sum(gridspec$grid[.inds] * .probs))^2)
    }

    ######################################################
    ## compute SIMEX variance functions using Bayes' rule
    ######################################################
    vsimex <- data.frame(W   = .W,
                         gW  = (csem(.W))^2)

    ## for each observed W, compute g(E[X|W])
    vsimex$gEXW <- sapply(vsimex$W, function(w){
        pxw <- dnorm(w, mean = px$x, sd = csem(px$x)) * px$p
        pxw <- pxw / sum(pxw)
        return( (csem( sum(px$x * pxw) ))^2 )
    })

    ## for each observed W, compute E[g(X)|W]
    vsimex$EgXW <- sapply(vsimex$W, function(w){
        pxw <- dnorm(w, mean = px$x, sd = csem(px$x)) * px$p
        pxw <- pxw / sum(pxw)
        return( sum( (csem(px$x)^2) * pxw ) )
    })
    
    return(list(gridspec       = gridspec,
                .history       = .history,
                px             = px,
                reliability    = px$varx[1] / varw,
                simex_varfuncs = vsimex))
}
