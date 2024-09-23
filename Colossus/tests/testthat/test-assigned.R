## ------------------------------------- ##
## Verify working results
## ------------------------------------- ##
# test_that( "Poisson Assigned Events, no error", {
#     df <- data.table::data.table( "UserID"=c(112, 114, 213, 214, 115, 116, 117),
#            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#           "Cancer_Status"=c(12, 10, 18, 6, 1, 11, 4),
#                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#                       "d"=c(0,   0,   0,   1,   1,   1,   1))

# 	df$pyr <- df$Ending_Age - df$Starting_Age
# 	pyr <- 'pyr'
# 	event <- "Cancer_Status"
# 	names <- c( 'a', 'b', 'c', 'd' )
# 	term_n <- c(0,1,1,2)
# 	tform <- c( "loglin", "lin", "lin", "plin" )
# 	modelform <- "M"
# 	fir <- 0
# 	a_n <- c(-0.75, 0.1, -0.05, -1.5)

# 	keep_constant <- c(0,0,0,0)
# 	der_iden <- 0

# 	control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 1, 'halfmax' = 5, 'epsilon' = 1e-3,
# 	    'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE,
# 	   'dose_abs_max'=100.0, 'verbose'=4, 'double_step'=1)
#     #
#     expect_no_error(RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
# })
test_that( "Poisson Assigned Events, check results", {
    df <- data.table::data.table( "UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(12, 10, 18, 6, 1, 11, 4),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))

    df$pyr <- df$Ending_Age - df$Starting_Age
    pyr <- 'pyr'
    event <- "Cancer_Status"
    names <- c( 'a', 'b', 'c', 'd' )
    term_n <- c(0,1,1,2)
    tform <- c( "loglin", "lin", "lin", "plin" )
    modelform <- "M"
    fir <- 0
    a_n <- c(-0.75, 0.1, -0.05, -1.5)

    keep_constant <- c(0,0,0,0)
    der_iden <- 0

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 1, 'halfmax' = 5, 'epsilon' = 1e-3,
        'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE,
       'dose_abs_max'=100.0, 'verbose'=0, 'double_step'=1)
    #
    e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)

    e0 <- e$predict
    e1 <- e$caused

    expect_equal(sum(e0),162.8914,tolerance=1)
    expect_equal(sum(e1),124,tolerance=1)

    expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
    expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
})
test_that( "Poisson Assigned Events, check results strata", {
    df <- data.table::data.table( "UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(12, 10, 18, 6, 1, 11, 4),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))
    set.seed(3742)
    df$pyr <- df$Ending_Age - df$Starting_Age
    pyr <- 'pyr'
    event <- "Cancer_Status"
    names <- c( 'a', 'b', 'c' )
    term_n <- c(0,1,2)
    tform <- c( "loglin", "loglin", "loglin" )
    modelform <- "M"
    fir <- 0
    a_n <- c(-0.75, 0.1, -0.05)

    keep_constant <- c(0,0,0)
    der_iden <- 0

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 1, 'halfmax' = 5, 'epsilon' = 1e-3,
        'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE,
       'dose_abs_max'=100.0, 'verbose'=0, 'double_step'=1)
    #
    term_n <- c(0,1,2)
    for (i in 1:10){
        a_n <- 2*runif(3)-1
        model_control <- list( "strata"=FALSE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "null", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
        model_control <- list( "strata"=TRUE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "d", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
    }
    term_n <- c(0,2,1)
    for (i in 1:10){
        a_n <- 2*runif(3)-1
        model_control <- list( "strata"=FALSE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "null", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
        model_control <- list( "strata"=TRUE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "d", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
    }
    term_n <- c(1,0,2)
    for (i in 1:10){
        a_n <- 2*runif(3)-1
        model_control <- list( "strata"=FALSE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "null", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
        model_control <- list( "strata"=TRUE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "d", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
    }
    term_n <- c(2,0,1)
    for (i in 1:10){
        a_n <- 2*runif(3)-1
        model_control <- list( "strata"=FALSE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "null", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
        model_control <- list( "strata"=TRUE)
        e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "d", model_control)

        e0 <- e$predict
        e1 <- e$caused

        expect_equal(sum(e0[,1:2]),sum(e0[,3]),tolerance=1)
        expect_equal(sum(e1[,1:2]),sum(e1[,3]),tolerance=1)
    }
})

test_that( "Poisson Assigned Events, combinations", {
    df <- data.table::data.table( "UserID"=c(112, 114, 213, 214, 115, 116, 117),
		       "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
		         "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
		      "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
		                  "a"=c(0,   1,   1,   0,   1,   0,   1),
		                  "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
		                  "c"=c(10,  11,  10,  11,  12,  9,   11),
		                  "d"=c(0,   0,   0,   1,   1,   1,   1))
	# For the interval case
	time1 <- "Starting_Age"
	time2 <- "Ending_Age"
	df$pyr <- df$Ending_Age - df$Starting_Age
	pyr <- 'pyr'
	event <- "Cancer_Status"
	names <- c( 'a', 'b', 'c', 'd' )
	term_n <- c(0,1,1,2)
	tform <- c( "loglin", "lin", "lin", "plin" )
	modelform <- "M"
	fir <- 0
	a_n <- c(0.1, 0.1, 0.1, 0.1)

	keep_constant <- c(0,0,0,0)
	der_iden <- 0

	control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 1, 'halfmax' = 5,
	   'epsilon' = 1e-3,  'deriv_epsilon' = 1e-3,
	   'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0,
	   'verbose'=4, 'ties'='breslow', 'double_step'=1)
    model_control <- list( "strata"=TRUE)
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    expect_error(RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control, "null", model_control))
	keep_constant <- c(1,1,1,1)
	expect_error(RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
	names <- c( 'a', 'b', 'CONST', 'd' )
	keep_constant <- c(0,0,0,0)
	expect_no_error(RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
	
	df$Cancer_Status <- rep(0,nrow(df))
	expect_error(RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control))
	sink(NULL)
    close(tfile)
})

test_that( "Poisson Assigned Events bounds, check results", {
    df <- data.table::data.table( "UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(12, 10, 18, 6, 1, 11, 4),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))

    df$pyr <- df$Ending_Age - df$Starting_Age
    pyr <- 'pyr'
    event <- "Cancer_Status"
    names <- c( 'a', 'b', 'c', 'd' )
    term_n <- c(0,1,1,2)
    tform <- c( "loglin", "lin", "lin", "plin" )
    modelform <- "M"
    fir <- 0
    a_n <- c(-0.75, 0.1, -0.05, -1.5)

    keep_constant <- c(0,0,0,0)
    der_iden <- 0

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 100, 'halfmax' = 5, 'epsilon' = 1e-3,
        'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE,
       'dose_abs_max'=100.0, 'verbose'=0, 'double_step'=1)
    #

    e0 <- RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)

    e <- RunPoissonEventAssignment_bound(df, pyr, event, e0, keep_constant, modelform, fir, der_iden, 4, 2, control)

    elow <- e$lower_limit$predict
    emid <- e$midpoint$predict
    eupp <- e$upper_limit$predict
    #
    expect_equal(sum(elow), 96.07807,tolerance=1)
    expect_equal(sum(emid), 123.6017,tolerance=1)
    expect_equal(sum(eupp), 151.1252,tolerance=1)
    #
    for (i in 2:4){
        for (j in c(1,2,10)){
            e <- RunPoissonEventAssignment_bound(df, pyr, event, e0, keep_constant, modelform, fir, der_iden, i, j, control)
            elow <- e$lower_limit$predict
            emid <- e$midpoint$predict
            eupp <- e$upper_limit$predict
            expect_equal(elow[,1], emid[,1],tolerance=1)
            expect_equal(elow[,1], eupp[,1],tolerance=1)
            expect_equal(eupp[,1], emid[,1],tolerance=1)
        }
    }
    #
})



















