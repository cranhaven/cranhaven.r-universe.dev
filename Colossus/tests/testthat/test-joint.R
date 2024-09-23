## ------------------------------------- ##
## Verify working results
## ------------------------------------- ##
test_that( "Joint data generation, no error", {
    a <- c(0,0,0,1,1,1)
    b <- c(1,1,1,2,2,2)
    c <- c(0,1,2,2,1,0)
    d <- c(1,1,0,0,1,1)
    e <- c(0,1,1,1,0,0)
    df <- data.table( 't0'=a, 't1'=b, 'e0'=c, 'e1'=d, 'fac'=e)
    time1 <- "t0"
    time2 <- "t1"
    df$pyr <- df$t1 - df$t0
    pyr <- "pyr"
    events <- c( 'e0', 'e1' )
    names_e0 <- c( 'fac' )
    names_e1 <- c( 'fac' )
    names_shared <- c( 't0', 't0' )
    term_n_e0 <- c(0)
    term_n_e1 <- c(0)
    term_n_shared <- c(0,0)
    tform_e0 <- c( "loglin" )
    tform_e1 <- c( "loglin" )
    tform_shared <- c( "quad_slope", "loglin_top" )
    keep_constant_e0 <- c(0)
    keep_constant_e1 <- c(0)
    keep_constant_shared <- c(0,0)
    a_n_e0 <- c(-0.1)
    a_n_e1 <- c(0.1)
    a_n_shared <- c(0.001, -0.02)
    name_list <- list( 'shared'=names_shared, 'e0'=names_e0, 'e1'=names_e1)
    term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
    tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
    keep_constant_list <- list( 'shared'=keep_constant_shared, 'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
    a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=a_n_e1)
    #
    expect_no_error(Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list))
})
test_that( "Joint data generation fill defaults, no error", {
    a <- c(0,0,0,1,1,1)
    b <- c(1,1,1,2,2,2)
    c <- c(0,1,2,2,1,0)
    d <- c(1,1,0,0,1,1)
    e <- c(0,1,1,1,0,0)
    df <- data.table( 't0'=a, 't1'=b, 'e0'=c, 'e1'=d, 'fac'=e)
    time1 <- "t0"
    time2 <- "t1"
    df$pyr <- df$t1 - df$t0
    pyr <- "pyr"
    events <- c( 'e0', 'e1' )
    names_e0 <- c( 'fac' )
    names_e1 <- c( 'fac' )
    names_shared <- c( 't0', 't0' )
    term_n_e0 <- c(0)
    term_n_e1 <- c(0)
    term_n_shared <- c(0,0)
    tform_e0 <- c( "loglin" )
    tform_e1 <- c( "loglin" )
    tform_shared <- c( "quad_slope", "loglin_top" )
    keep_constant_e0 <- c(0)
    keep_constant_e1 <- c(0)
    keep_constant_shared <- c(0,0)
    a_n_e0 <- c(-0.1)
    a_n_e1 <- c(0.1)
    a_n_shared <- c(0.001, -0.02)
    name_list <- list( 'shared'=names_shared, 'e0'=names_e0, 'e1'=names_e1)
    term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
    tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
    keep_constant_list <- list( 'shared'=keep_constant_shared, 'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
    a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=a_n_e1)
    #
    expect_no_error(Joint_Multiple_Events(df, events, name_list, list(), list(), list(), list()))
    val <- Joint_Multiple_Events(df, events, name_list, list(), list(), list(), list())
    expect_equal(val$term_n, rep(0,4))
    expect_equal(val$tform, rep( 'loglin',4))
    expect_equal(val$keep_constant, rep(0,4))
    expect_equal(val$a_n, rep(0,4))
})
test_that( "Joint data generation, check results", {
    a <- c(0,0,0,1,1,1)
    b <- c(1,1,1,2,2,2)
    c <- c(0,1,2,2,1,0)
    d <- c(1,1,0,0,1,1)
    e <- c(0,1,1,1,0,0)
    df <- data.table( 't0'=a, 't1'=b, 'e0'=c, 'e1'=d, 'fac'=e)
    time1 <- "t0"
    time2 <- "t1"
    df$pyr <- df$t1 - df$t0
    pyr <- "pyr"
    events <- c( 'e0', 'e1' )
    names_e0 <- c( 'fac' )
    names_e1 <- c( 'fac' )
    names_shared <- c( 't0', 't0' )
    term_n_e0 <- c(0)
    term_n_e1 <- c(0)
    term_n_shared <- c(0,0)
    tform_e0 <- c( "loglin" )
    tform_e1 <- c( "loglin" )
    tform_shared <- c( "quad_slope", "loglin_top" )
    keep_constant_e0 <- c(0)
    keep_constant_e1 <- c(0)
    keep_constant_shared <- c(0,0)
    a_n_e0 <- c(-0.1)
    a_n_e1 <- c(0.1)
    a_n_shared <- c(0.001, -0.02)
    name_list <- list( 'shared'=names_shared, 'e0'=names_e0, 'e1'=names_e1)
    term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
    tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
    keep_constant_list <- list( 'shared'=keep_constant_shared, 'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
    a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=a_n_e1)
    #
    val <- Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list)
    expect_equal(val$names, c( "t0", "t0", "fac_e0", "fac_e1" ))
    expect_equal(names(val$df),c( 't0', 't1', 'events', 'e0', 'e1', 'fac', 'pyr', 'fac_e0', 'fac_e1' ))
})
test_that( "Joint data regression, no error", {
    a <- c(0,0,0,1,1,1)
	b <- c(1,1,1,2,2,2)
	c <- c(0,1,2,2,1,0)
	d <- c(1,1,0,0,1,1)
	e <- c(0,1,1,1,0,0)
	f <- c(0,1,0,0,1,1)
	df <- data.table( 't0'=a, 't1'=b, 'e0'=c, 'e1'=d, 'fac'=e)
	time1 <- "t0"
	time2 <- "t1"
	df$pyr <- df$t1 - df$t0
	pyr <- "pyr"
	events <- c( 'e0', 'e1' )
	names_e0 <- c( 'fac' )
	names_e1 <- c( 'fac' )
	names_shared <- c( 't0', 't0' )
	term_n_e0 <- c(0)
	term_n_e1 <- c(0)
	term_n_shared <- c(0,0)
	tform_e0 <- c( "loglin" )
	tform_e1 <- c( "loglin" )
	tform_shared <- c( "quad_slope", "loglin_top" )
	keep_constant_e0 <- c(0)
	keep_constant_e1 <- c(0)
	keep_constant_shared <- c(0,0)
	a_n_e0 <- c(-0.1)
	a_n_e1 <- c(0.1)
	a_n_shared <- c(0.001, -0.02)
	name_list <- list( 'shared'=names_shared, 'e0'=names_e0, 'e1'=names_e1)
	term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
	tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
	keep_constant_list <- list( 'shared'=keep_constant_shared,
		                       'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
	a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=a_n_e1)

	der_iden <- 0
	modelform <- "M"
	fir <- 0
	control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 2, 'halfmax' = 5, 'epsilon' = 1e-3,
	    'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE,
	   'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
	guesses_control <- list( "maxiter"=10, "guesses"=10, "lin_min"=0.001, "lin_max"=1,
		"loglin_min"=-1, "loglin_max"=1, "lin_method"="uniform", "loglin_method"="uniform",
		 strata=FALSE)
	strat_col <- 'f'
	expect_no_error(RunPoissonRegression_Joint_Omnibus(df, pyr, events, name_list, term_n_list,
		                                    tform_list, keep_constant_list, a_n_list,
		                                    modelform, fir, der_iden, control,strat_col)
    )
})
test_that( "Joint data regression, check results", {
    a <- c(0,0,0,1,1,1)
	b <- c(1,1,1,2,2,2)
	c <- c(0,1,2,2,1,0)
	d <- c(1,1,0,0,1,1)
	e <- c(0,1,1,1,0,0)
	f <- c(0,1,0,0,1,1)
	df <- data.table( 't0'=a, 't1'=b, 'e0'=c, 'e1'=d, 'fac'=e)
	time1 <- "t0"
	time2 <- "t1"
	df$pyr <- df$t1 - df$t0
	pyr <- "pyr"
	events <- c( 'e0', 'e1' )
	names_e0 <- c( 'fac' )
	names_e1 <- c( 'fac' )
	names_shared <- c( 't0', 't0' )
	term_n_e0 <- c(0)
	term_n_e1 <- c(0)
	term_n_shared <- c(0,0)
	tform_e0 <- c( "loglin" )
	tform_e1 <- c( "loglin" )
	tform_shared <- c( "quad_slope", "loglin_top" )
	keep_constant_e0 <- c(0)
	keep_constant_e1 <- c(0)
	keep_constant_shared <- c(0,0)
	a_n_e0 <- c(-0.1)
	a_n_e1 <- c(0.1)
	a_n_shared <- c(0.001, -0.02)
	name_list <- list( 'shared'=names_shared, 'e0'=names_e0, 'e1'=names_e1)
	term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
	tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
	keep_constant_list <- list( 'shared'=keep_constant_shared,
		                       'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
	a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=a_n_e1)

	der_iden <- 0
	modelform <- "M"
	fir <- 0
	control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 10, 'halfmax' = 5, 'epsilon' = 1e-3,
	    'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE,
	   'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
	guesses_control <- list( "maxiter"=10, "guesses"=10, "lin_min"=0.001, "lin_max"=1,
		"loglin_min"=-1, "loglin_max"=1, "lin_method"="uniform", "loglin_method"="uniform",
		 strata=FALSE)
	strat_col <- 'f'
	e <- RunPoissonRegression_Joint_Omnibus(df, pyr, events, name_list, term_n_list,
		                                    tform_list, keep_constant_list, a_n_list,
		                                    modelform, fir, der_iden, control,strat_col)
    expect_equal(e$beta_0, c(0.5742600, -1.0349816, -0.0200000, -0.1647421),tolerance=1e-2)
    expect_equal(e$Converged,TRUE)
})

## ------------------------------------- ##
## Verify errors results
## ------------------------------------- ##
test_that( "Joint data generation, incorrect length error", {
    a <- c(0,0,0,1,1,1)
    b <- c(1,1,1,2,2,2)
    c <- c(0,1,2,2,1,0)
    d <- c(1,1,0,0,1,1)
    e <- c(0,1,1,1,0,0)
    df <- data.table( 't0'=a, 't1'=b, 'e0'=c, 'e1'=d, 'fac'=e)
    time1 <- "t0"
    time2 <- "t1"
    df$pyr <- df$t1 - df$t0
    pyr <- "pyr"
    events <- c( 'e0', 'e1' )
    names_e0 <- c( 'fac' )
    names_e1 <- c( 'fac' )
    names_shared <- c( 't0', 't0' )
    term_n_e0 <- c(0)
    term_n_e1 <- c(0)
    term_n_shared <- c(0,0)
    tform_e0 <- c( "loglin" )
    tform_e1 <- c( "loglin" )
    tform_shared <- c( "quad_slope", "loglin_top" )
    keep_constant_e0 <- c(0)
    keep_constant_e1 <- c(0)
    keep_constant_shared <- c(0,0)
    a_n_e0 <- c(-0.1)
    a_n_e1 <- c(0.1)
    a_n_shared <- c(0.001, -0.02)
    #
    name_list <- list( 'shared'=names_shared, 'e0'=names_e0, 'e1'=names_e1)
    term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
    tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
    keep_constant_list <- list( 'shared'=keep_constant_shared, 'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
    a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=a_n_e1)
    #
    term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=c(1,1,1,1,1,1))
    expect_error(Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list))
    #
    term_n_list <- list( 'shared'=term_n_shared, 'e0'=term_n_e0, 'e1'=term_n_e1)
    tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=c(1,1,1,1,1,1))
    expect_error(Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list))
    #
    tform_list <- list( 'shared'=tform_shared, 'e0'=tform_e0, 'e1'=tform_e1)
    keep_constant_list <- list( 'shared'=keep_constant_shared, 'e0'=keep_constant_e0, 'e1'=c(1,1,1,1,1,1))
    expect_error(Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list))
    #
    keep_constant_list <- list( 'shared'=keep_constant_shared, 'e0'=keep_constant_e0, 'e1'=keep_constant_e1)
    a_n_list <- list( 'shared'=a_n_shared, 'e0'=a_n_e0, 'e1'=c(1,1,1,1,1,1))
    expect_error(Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list))
})
