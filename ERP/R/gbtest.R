gbtest <-
function (dta, design, design0 = NULL, graphthresh = 0.05, nsamples = 1000) {
    lseq = function(x) {
        T = length(x)
        changepoints = c(x[1], diff(x))
        start = (1:T)[changepoints == 1]
        end = (1:T)[changepoints == -1] - 1
        if (length(end) < length(start)) 
            end[length(start)] = T
        list(start = start, length = end - start + 1)
    }
    signiflength = function(n, T, rho, graphthresh, nsamples = 1000) {
        sigma = matrix(rep(0:(T - 1), T), nrow = T, ncol = T, 
            byrow = TRUE)
        sigma = abs(sigma - t(sigma))
        sigma = rho^sigma
        ttest = rmt(nsamples, S = sigma, df = n - 1)
        over = abs(ttest) > qt(1 - graphthresh/2, n - 1)
        lgth = apply(over, 1, function(s) lseq(s)$length)
        maxlgth = unlist(lapply(lgth, function(x) ifelse(length(x) > 
            0, max(x), 0)))
        qtile = quantile(maxlgth[maxlgth != 0], probs = 0.95)
        check = mean(maxlgth >= qtile) <= 0.05
        if (!check) 
            qtile = qtile + 1
        return(qtile)
    }
    acfdp = function(dta) {
        acfs = apply(dta, 1, function(dp) acf(dp, lag.max = 1, 
            plot = FALSE, type = "covariance", demean = FALSE)$acf[, 
            1, 1])
        list(rho = mean(acfs[2, ]/acfs[1, ]), std = sqrt(mean(acfs[1, 
            ])))
    }
    if (is.null(design0)) 
        design0 = matrix(1, nrow = nrow(dta), ncol = 1)
    erpdta = as.matrix(dta)
    design = as.matrix(design)
    design0 = as.matrix(design0)
    if (typeof(erpdta) != "double") 
        stop("ERPs should be of type double")
    if (nrow(erpdta) != nrow(design)) 
        stop("dta and design should have the same number of rows")
    if (nrow(erpdta) != nrow(design0)) 
        stop("dta and design0 should have the same number of rows")
    if (ncol(design) <= ncol(design0)) 
        stop("design0 should have fewer columns than design")
    idsignal = NULL
    for (j in 1:ncol(design)) {
        cj = apply(design0, 2, function(x, y) all(x == y), y = design[, 
            j])
        if (all(!cj)) 
            idsignal = c(idsignal, j)
    }
    if (length(idsignal) < (ncol(design) - ncol(design0))) 
        stop("the null model design0 should be nested into the non-null model design")
    if (typeof(graphthresh) != "double") 
        stop("graphthresh should be of type double")
    if ((graphthresh <= 0) | (graphthresh >= 1)) 
        stop("graphthresh should be in ]0,1[, typically 0.05")
    n = nrow(erpdta)
    T = ncol(erpdta)

    svd.design = fast.svd(design)
    svd.design0 = fast.svd(design0)
    Proj = svd.design$u%*%t(svd.design$u)
    Proj0 = svd.design0$u%*%t(svd.design0$u)
    if (ncol(design)==1) pdesign = (svd.design$v%*%t(svd.design$u))/(svd.design$d[1])
    if (ncol(design)>1) pdesign = svd.design$v%*%diag(1/svd.design$d)%*%t(svd.design$u)    
    rdf1 = nrow(design) - length(svd.design$d)
    rdf0 = nrow(design0) - length(svd.design0$d)
    beta = (pdesign %*% erpdta)[idsignal, ]
    if (length(idsignal) == 1) 
        beta = matrix(beta, nrow = 1)
    rownames(beta) = colnames(design)[idsignal]
    lsamp = lapply(1:nsamples, function(i, p) sample(1:p), p = n)
    lres1 = lapply(lsamp, function(samp, x, Proj) Proj %*% x[samp, 
        ], x = erpdta, Proj = diag(n) - Proj)
    lres0 = lapply(lsamp, function(samp, x, Proj) Proj %*% x[samp, 
        ], x = erpdta, Proj = diag(n) - Proj0)
    lscer1 = lapply(lres1, function(res) as.vector(t(rep(1, nrow(res))) %*% 
        res^2))
    lscer1 = matrix(unlist(lscer1), nrow = nsamples, byrow = TRUE)
    lscer0 = lapply(lres0, function(res) as.vector(t(rep(1, nrow(res))) %*% 
        res^2))
    lscer0 = matrix(unlist(lscer0), nrow = nsamples, byrow = TRUE)
    lfstat = ((lscer0 - lscer1)/(rdf0 - rdf1))/(lscer1/rdf1)
    accoef = acfdp(lfstat)
    maxl = signiflength(n = rdf1, T = T, rho = accoef$rho, graphthresh = graphthresh, 
        nsamples = nsamples)
    res1 = erpdta - Proj %*% erpdta
    scer1 = as.vector(t(rep(1, n)) %*% res1^2)
    res0 = erpdta - Proj0 %*% erpdta
    scer0 = as.vector(t(rep(1, n)) %*% res0^2)
    fstat = ((scer0 - scer1)/(rdf0 - rdf1))/(scer1/rdf1)
    pval = pf(fstat, df1 = rdf0 - rdf1, df2 = rdf1, lower.tail = FALSE)
    over = pval <= graphthresh
    lgth = lseq(over)
    if (length(lgth$length) == 0) {
        intervals = integer(0)
        signifseq = logical(0)
    }
    if (length(lgth$length) > 0) {
        intervals = lapply(1:length(lgth$length), function(i, 
            l) l$start[i]:(l$start[i] + l$length[i] - 1), l = lgth)
        signifseq = lgth$length > maxl
    }
    if (length(idsignal)==1) tstat = sign(beta[1,])*sqrt(fstat)
    test = fstat
    r2 = (1 - 1/(1 + fstat * ((rdf0 - rdf1)/rdf1)))
    if (length(idsignal)==1) test = sign(beta[1,])*sqrt(fstat)
    return(list(nbsignifintervals = sum(signifseq), intervals = intervals[signifseq], 
                significant = unlist(intervals[signifseq]), signal = beta,
                test = test,rho = accoef$rho, r2 = r2))
}
