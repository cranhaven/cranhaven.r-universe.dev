test_that( "Poisson residual no error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    e <- c(0,0,0,1,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d,"e"=e)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    pyr <- "a"
    names <- c( "d" )
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    control <- list( "ncores"=2, 'lr' = 0.95, 'maxiter' = -1, 'halfmax' = 1, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=1.0, 'verbose'=4, 'double_step'=1)
    model_control <- list( 'pearson'=F, 'deviance'=F)
    expect_no_error(RunPoissonRegression_Residual(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col='rand',model_control))
    model_control <- list( 'pearson'=T, 'deviance'=F)
    expect_no_error(RunPoissonRegression_Residual(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col='rand',model_control))
    model_control <- list( 'pearson'=F, 'deviance'=T)
    expect_no_error(RunPoissonRegression_Residual(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col='rand',model_control))
    #
    model_control <- list( 'pearson'=F, 'deviance'=F, 'strata'=T)
    expect_no_error(RunPoissonRegression_Residual(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col='e',model_control))
    sink(NULL)
    close(tfile)
})
