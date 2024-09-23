test_that( "Poisson time column missing", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a_bad"
    time2 <- "b"
    event <- "c"
    pyr <- "a_bad"
    names <- c( "d" )
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.95, 'maxiter' = -1, 'halfmax' = 1, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=1.0, 'verbose'=0, 'double_step'=1)
    expect_error(RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
})
test_that( "Poisson no events", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
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
    control <- list( "ncores"=2, 'lr' = 0.95, 'maxiter' = -1, 'halfmax' = 1, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=1.0, 'verbose'=0, 'double_step'=1)
    expect_error(RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
})
test_that( "Poisson no events", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
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
    control <- list( "ncores"=2, 'lr' = 0.95, 'maxiter' = -1, 'halfmax' = 1, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=1.0, 'verbose'=0, 'double_step'=1)
    expect_error(RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
})

test_that( "Pois loglin_M", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(0,0)
    a_n <- c(0.01,0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    e <-RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(-0.727884, -1.514085),tolerance=1e-2)
})
test_that( "Pois loglin_plin_M", {
    fname <- 'l_pl_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "plin" )
    keep_constant <- c(0,0)
    a_n <- c(0.01,0.5)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    e <-RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(-0.32, -0.6),tolerance=1e-2)
})
test_that( "Pois loglin_plin_A", {
    fname <- 'l_pl_A_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,1)
    tform <- c( "loglin", "plin" )
    keep_constant <- c(0,0)
    a_n <- c(0.01,0.5)
    modelform <- "A"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 100, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    e <-RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(-0.1585696, -1.4945955),tolerance=1e-2)
})

test_that( "Pois loglin_M Single", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(0,0)
    a_n <- c(0.01,0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    e <-RunPoissonRegression_Single(df, pyr, event, names, term_n, tform, a_n, modelform, fir, control)
    expect_equal(e$AIC,2354.055,tolerance=1e-2)
})
test_that( "Pois loglin_M Strata", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    names <- c( "dose" )
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(0.01)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    e <-RunPoissonRegression_STRATA(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,c( "fac" ))
    expect_equal(e$beta_0,c(0.05476188),tolerance=1e-1)
})