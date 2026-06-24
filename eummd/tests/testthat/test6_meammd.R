library(eummd)

test_that("MEAMMD proj test", {
    seednum <- 1

    # dimensions of data and trials
    n <- 10
    m <- 10
    d <- 10
    numtrials <- 10
    K <- 20
    numperm <- 200
    alpha <- 0.05

    # Type I error
    type1_pval <- rep(0, numtrials)
    type1_pval_ts <- rep(0, numtrials)
    type1_pval_orig <- rep(0, numtrials)
    set.seed(seednum)
    for (t in seq_len(numtrials)){
        X <- matrix(rnorm(n*d, 0, 1), ncol=n, nrow=n, byrow=T)
        Y <- matrix(rnorm(m*d, 0, 1), ncol=m, nrow=m, byrow=T)

        type1_pval[t] <- meammd(X=X, Y=Y, pval=TRUE, type='proj', numproj=K, numperm=numperm, seednum=t)$pval
        type1_pval_ts[t] <- meammd(X=X, Y=Y, pval=TRUE, type='proj', numproj=K, numperm=numperm, seednum=t, alternative='two.sided')$pval
        type1_pval_orig[t] <- meammd(X=X, Y=Y, pval=TRUE, type='proj', numproj=K, numperm=numperm, seednum=t, faster=F)$pval
    }
    #cat("type1\n")
    #print(type1_pval)
    #print(type1_pval_orig)

    #count_reject <- sum(type1_pval < alpha)
    #prop_reject <- count_reject / numtrials
    type1_prop_reject <- sum(type1_pval < alpha)/numtrials
    type1_prop_reject_ts <- sum(type1_pval_ts < alpha)/numtrials
    type1_prop_reject_orig <- sum(type1_pval_orig < alpha)/numtrials

    # power
    power_pval <- rep(0, numtrials)
    power_pval_ts <- rep(0, numtrials)
    power_pval_orig <- rep(0, numtrials)
    set.seed(seednum)
    for (t in seq_len(numtrials)){
        X <- matrix(rnorm(n*d, 0, 1), ncol=n, nrow=n, byrow=T)
        Y <- matrix(rnorm(m*d, 1, 1), ncol=m, nrow=m, byrow=T)

        power_pval[t] <- meammd(X=X, Y=Y, pval=TRUE, type='proj', numproj=K, numperm=numperm, seednum=t)$pval
        power_pval_ts[t] <- meammd(X=X, Y=Y, pval=TRUE, type='proj', numproj=K, numperm=numperm, seednum=t, alternative='two.sided')$pval
        power_pval_orig[t] <- meammd(X=X, Y=Y, pval=TRUE, type='proj', numproj=K, numperm=numperm, seednum=t, faster=F)$pval
    }
    #cat("power\n")
    #print(power_pval)
    #print(power_pval_orig)
    power_prop_reject <- sum(power_pval < alpha)/numtrials
    power_prop_reject_ts <- sum(power_pval < alpha)/numtrials
    power_prop_reject_orig <- sum(power_pval_orig < alpha)/numtrials

    # cat("Type I error: ", prop_reject, "\n", sep="")
    res <- c(type1_prop_reject, 
             type1_prop_reject_ts, 
             type1_prop_reject_orig, 
             power_prop_reject, 
             power_prop_reject_ts, 
             power_prop_reject_orig)

    type1_res <- (res[1] < alpha) && (res[2] < alpha) && (res[3] < alpha)

    power_thresh <- 0.9
    power_res <- (res[4] > power_thresh) && (res[5] > power_thresh) && (res[6] > power_thresh)

    expect_true(type1_res)
    expect_true(power_res)

})

