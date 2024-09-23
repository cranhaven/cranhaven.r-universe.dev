test_that( "Coxph time column missing", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a_bad"
    time2 <- "b"
    event <- "c"
    names <- c( "d" )
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
})
test_that( "Coxph no events", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d","d" )
    term_n <- c(0,1)
    tform <- c( "loglin","loglin" )
    keep_constant <- c(0,0)
    a_n <- c(-0.1,0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
})

test_that( "Coxph_strata time column missing", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    e <- c(1,1,0,0,1,0,1)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=e)
    time1 <- "a_bad"
    time2 <- "b"
    event <- "c"
    names <- c( "d" )
    strat_col <- "e"
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxRegression_STRATA(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col))
})
test_that( "Coxph_strata no events", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    d <- c(3,4,5,6,7,8,9)
    e <- c(1,1,0,0,1,0,1)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=e)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d" )
    strat_col <- "e"
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxRegression_STRATA(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col))
})
test_that( "Coxph_strata no strata", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    e <- c(1,1,0,0,1,0,1)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=e)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d" )
    strat_col <- "e_bad"
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxRegression_STRATA(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col))
})
test_that( "Coxph_strata strata with no error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,0,0,0,0,0)
    d <- c(3,4,5,6,7,8,9)
    e <- c(1,1,0,0,1,0,1)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=e)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d" )
    strat_col <- "e_bad"
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(-0.1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxRegression_STRATA(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col))
})

test_that( "Coxph null time column missing", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a_bad"
    time2 <- "b"
    event <- "c"
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxNull(df, time1, time2, event, control))
})
test_that( "Coxph null no events", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_error(RunCoxNull(df, time1, time2, event, control))
})

test_that( "Coxph loglin_M", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(-0.0996,-0.05697),tolerance=1e-2)
})
test_that( "Coxph loglin_plin_M", {
    fname <- 'l_pl_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(0.1747772,0.75),tolerance=1e-2)
})
test_that( "Coxph loglin_plin_A", {
    fname <- 'l_pl_A_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(0.11, 1.01),tolerance=1e-2)
})
#
test_that( "Coxph dose list", {
    fname <- 'dose.csv'
    colTypes <- c( "double", "double", "double", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
    event <- "lung"
    names <- c( "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose" )
    term_n <- c(0,0,0,0,0,0,0,0,0,0,0)
    tform <- c( "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope" )
    keep_constant <- c(0,0,0,0,0,0,0,0,0,0,0)
    tform <- c( "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope" )
    a_n <-   c(-0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(-0.10396225, -0.09955731,  1.00377389, -0.10537894,  1.01629097,  2.15102759, 0.29434874,  1.57836844,  0.20994531,  0.87551440,  1.00908073),tolerance=1e-2)
})

test_that( "Coxph fixed intercept", {
    fname <- 'dose.csv'
    colTypes <- c( "double", "double", "double", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
    event <- "lung"
    names <- c( "dose", "dose", "dose", "dose", "dose" )
    term_n <- c(0,0,0,0,0)
    tform <- c( "loglin", "lin_slope", "lin_int", "step_slope", "step_int" )
    keep_constant <- c(0,0,1,0,1)
    a_n <-   c(-0.1          ,0.1       ,-1,0.1,-1)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_no_error(RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
})

test_that( "Coxph loglin_M Strata", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression_STRATA(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "fac" )
    expect_equal(e$beta_0,c(-0.106),tolerance=1e-2)
})
test_that( "Coxph loglin_M Single", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression_Single(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, control)
    expect_equal(e$AIC,1056.299,tolerance=1e-2)
})
test_that( "Coxph loglin_M Null", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxNull(df, time1, time2, event, control)
    expect_equal(e$AIC,1052,tolerance=1e-2)
})

test_that( "Coxph loglin_M CENSOR", {
    fname <- 'll_cens_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    expect_equal(e$beta_0,c(-1.19,0.08),tolerance=1e-2)
})
test_that( "Coxph loglin_M CENSOR Adjusted", {
    fname <- 'll_cens_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(0,0)
    a_n <- c(-1.1169,-0.04558)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    plot_options <- list( "name"=paste(tempfile(), "run",sep="" ), "verbose"=FALSE, "studyid"="studyid", "age_unit"="years" )
    dft <- GetCensWeight(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, control, plot_options)
    #
    #
    expect_equal(sum(dft$ch),336410,tolerance=1e-2)
})
test_that( "Coxph loglin_M CENSOR Default", {
    fname <- 'll_cens_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(0,0)
    a_n <- c(0,0)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = -1, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    plot_options <- list( "name"=paste(tempfile(), "run",sep="" ), "verbose"=FALSE, "studyid"="studyid", "age_unit"="years" )
    dft <- GetCensWeight(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, control, plot_options)
    #
    #
    expect_equal(sum(dft$ch),699,tolerance=1e-2)
})

test_that( "Coxph loglin_M Basic", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
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
    e <- RunCoxRegression_Basic(df, time1, time2, event, names, keep_constant, a_n, der_iden, control)
    expect_equal(e$beta_0,c(-0.09962, -0.05698),tolerance=1e-2)
})

test_that( "Coxph censoring weight", {
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
    df$censor <- (df$lung==0)
    event <- "censor"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(1,0)
    a_n <- c(0,0)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = -1, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    plot_options <- list( "name"=paste(tempfile(), "run",sep="" ), "verbose"=TRUE, "studyid"="studyid", "age_unit"="years" )
    dft <- GetCensWeight(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, control, plot_options)
    #
    #
    t_ref <- dft$t
    surv_ref <- dft$surv
    t_c <- df$t1
    cens_weight <- approx(t_ref, surv_ref, t_c,rule=2)$y
    df$weighting <- cens_weight
    # message(sum(cens_weight))
    #
    event <- "lung"
    a_n <- c(-0.1,-0.1)
    keep_constant <- c(0,0)
    e0 <- RunCoxRegression_CR(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,"weighting")
    a_n <- c(-0.1,-0.1)
    keep_constant <- c(0,0)
    e1 <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
    #
    expect_equal(e0$LogLik - e1$LogLik,-2.909427,tolerance=1e-2)
})
