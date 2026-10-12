erpavetest <-
function (dta, design, design0 = NULL, nintervals = 10, 
    method = c("none","BH", "holm", "hochberg","hommel", "bonferroni", "BY", "fdr"),
    alpha = 0.05) {
    method = match.arg(method, choices = c("none","BH", "holm", "hochberg", 
        "hommel", "bonferroni", "BY", "fdr"))
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
    if (typeof(alpha) != "double") 
        stop("alpha should be of type double")
    if ((alpha <= 0) | (alpha >= 1)) 
        stop("alpha should be in ]0,1[, typically 0.05")
    if ((typeof(nintervals) != "double") & ((typeof(nintervals) != 
        "integer"))) 
        stop("nintervals should be of type double or integer")
    n = nrow(erpdta)
    T = ncol(erpdta)
    lgth = round(T/nintervals)
    if (((T%/%lgth) * lgth) == T) 
        brks = c(0, seq(lgth, T, by = lgth))
    if (((T%/%lgth) * lgth) < T) {
        if ((T - (T%/%lgth) * lgth) <= (0.5 * (T%/%lgth))) {
            brks = seq(lgth, T, by = lgth)
            brks = c(0, brks[-length(brks)], T)
        }
        if ((T - (T%/%lgth) * lgth) > (0.5 * (T%/%lgth))) {
            brks = seq(lgth, T, by = lgth)
            brks = c(0, brks, T)
        }
    }
    segments = cut(1:T, breaks = brks)
    ns = tapply(segments, segments, length)
    avedta = sweep(t(rowsum(t(erpdta), group = segments)), 2, 
        STATS = ns, FUN = "/")
    svd.design = fast.svd(design)
    svd.design0 = fast.svd(design0)
    Proj = svd.design$u%*%t(svd.design$u)
    Proj0 = svd.design0$u%*%t(svd.design0$u)
    rdf1 = nrow(design) - length(svd.design$d)
    rdf0 = nrow(design0) - length(svd.design0$d)
    if (ncol(design)==1) pdesign = (svd.design$v%*%t(svd.design$u))/(svd.design$d[1])
    if (ncol(design)>1) pdesign = svd.design$v%*%diag(1/svd.design$d)%*%t(svd.design$u)    
    beta = (pdesign %*% erpdta)[idsignal, ,drop=FALSE]
    avebeta = (pdesign %*% avedta)[idsignal, ,drop=FALSE]
    rownames(beta) = colnames(design)[idsignal]
    res1 = erpdta - Proj %*% erpdta
    scer1 = as.vector(t(rep(1, n)) %*% res1^2)
    res0 = erpdta - Proj0 %*% erpdta
    scer0 = as.vector(t(rep(1, n)) %*% res0^2)
    fstat = ((scer0 - scer1)/(rdf0 - rdf1))/(scer1/rdf1)
    r2 = (1 - 1/(1 + fstat * ((rdf0 - rdf1)/rdf1)))
    res1 = avedta - Proj %*% avedta
    scer1 = as.vector(t(rep(1, n)) %*% res1^2)
    res0 = avedta - Proj0 %*% avedta
    scer0 = as.vector(t(rep(1, n)) %*% res0^2)
    fstat = ((scer0 - scer1)/(rdf0 - rdf1))/(scer1/rdf1)
    pval = pf(fstat, df1 = rdf0 - rdf1, df2 = rdf1, lower.tail = FALSE)
    correctedpval = p.adjust(pval, method = method)
    avesignificant = which(correctedpval <= alpha)
    if (length(avesignificant) == 0) 
        significant = integer(0)
    if (length(avesignificant) > 0) 
        significant = which(is.element(segments, levels(segments)[avesignificant]))
    if (length(idsignal)==1) tstat = sign(avebeta[1,])*sqrt(fstat)
    return(list(pval = pval, correctedpval = correctedpval, significant = significant, 
        segments = segments, breaks = brks, test = ifelse(idsignal==1,tstat,fstat), df1 = rdf1, 
        df0 = rdf0, signal = beta, r2 = r2))
}
