test_that( "Coxph strata_basic_single_CR_null log_bound", {
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))

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
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    plot_options <- list( "name"=paste(tempfile(), "run",sep="" ), "verbose"=FALSE, "studyid"="studyid", "age_unit"="years" )
    dft <- GetCensWeight(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, control, plot_options)
    #
    #
    t_ref <- dft$t
    surv_ref <- dft$surv
    t_c <- df$t1
    cens_weight <- approx(t_ref, surv_ref, t_c,rule=2)$y
    df$weighting <- cens_weight
    #
    event <- "lung"
    a_n <- c(-0.1,-0.1)
    keep_constant <- c(0,0)

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(-1,-1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)

    verbose <- FALSE

    for (i in c(TRUE, FALSE)){
        for (j in c(TRUE, FALSE)){
            for (k in c(FALSE, FALSE)){
                for (l in c(TRUE, FALSE)){
                    for (m in c(TRUE, FALSE)){
                        model_control <- list( 'strata'=i, 'basic'=j, 'single'=k, 'cr'=l, 'log_bound'=TRUE, 'manual'=m)
                        if (verbose){print(model_control)}
                        a_n <- c(-0.1,-0.1)
                        control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)
                        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control, cens_weight="weighting"))
                        a_n <- c(-0.1,-0.1)
                        control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='efron', 'double_step'=0)
                        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control, cens_weight="weighting"))
                        if (verbose){print( "---------------" )}
                    }
                }
            }
        }
    }
    for (m in c(TRUE, FALSE)){
        model_control <- list( 'null'=T, 'log_bound'=TRUE, 'manual'=m)
        if (verbose){print(model_control)}
        a_n <- c(-0.1,-0.1)
        control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)
        expect_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control, cens_weight="weighting"))
        model_control <- list( 'single'=T, 'log_bound'=TRUE, 'manual'=m)
        expect_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control, cens_weight="weighting"))
    }
    sink(NULL)
    close(tfile)
})
test_that( "Poisson strata_single log_bound", {
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))
    df$pyr <- df$t1 - df$t0
    time1 <- "t0"
    time2 <- "t1"
    pyr <- "pyr"
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
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    #
    event <- "lung"
    a_n <- c(-0.1,-0.1)
    keep_constant <- c(0,0)

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(-1,-1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)

    verbose <- FALSE

    for (i in c(TRUE, FALSE)){
        for (k in c(FALSE, FALSE)){
            for (m in c(TRUE, FALSE)){
                model_control <- list( 'strata'=i, 'single'=k, 'log_bound'=TRUE, 'manual'=m)
                if (verbose){print(model_control)}
                a_n <- c(-0.1,-0.1)
                control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)
                expect_no_error(RunPoissonRegression_Omnibus(df,pyr, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control))
            }
        }
    }
    for (m in c(TRUE, FALSE)){
        for (k in c(TRUE)){
            model_control <- list( 'strata'=F, 'single'=k, 'log_bound'=TRUE, 'manual'=m)
            if (verbose){print(model_control)}
            a_n <- c(-0.1,-0.1)
            control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)
            expect_error(RunPoissonRegression_Omnibus(df,pyr, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control))
        }
    }
    sink(NULL)
    close(tfile)
})
test_that( "Coxph EPICURE validated answers, loglin", {
    fname <- 'base_example.csv'
    df <- fread(fname)

    time1 <- "entry"
    time2 <- "exit"
    event <- "event"
    names <- c( "dose0", "dose1" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(0,0)
    a_n <- c(0,0)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    #

    a_n <- c(-0.6067, 5.019)
    model_control <- list( 'basic'=TRUE, 'log_bound'=TRUE, 'alpha'=0.1)
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    alphas <- c(0.75, 0.5, 1-0.683, 0.25, 0.1, 0.05, 0.025, 0.01, 0.005)
    for (alpha_i in seq_along(alphas)){
        a_n <- c(-0.6067, 5.019)
        model_control <- list( 'basic'=TRUE, 'log_bound'=TRUE, 'alpha'=alphas[alpha_i], 'para_number'=0)
        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
    }
    for (alpha_i in seq_along(alphas)){
        a_n <- c(-0.6067, 5.019)
        model_control <- list( 'basic'=TRUE, 'log_bound'=TRUE, 'alpha'=alphas[alpha_i], 'para_number'=1)
        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
    }
})

test_that( "Coxph EPICURE validated answers, loglin manual", {
    fname <- 'base_example.csv'
    df <- fread(fname)

    time1 <- "entry"
    time2 <- "exit"
    event <- "event"
    names <- c( "dose0", "dose1" )
    term_n <- c(0,0)
    tform <- c( "loglin", "loglin" )
    keep_constant <- c(0,0)
    a_n <- c(0,0)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    #

    a_n <- c(-0.6067, 5.019)
    model_control <- list( 'basic'=TRUE, 'log_bound'=TRUE, 'alpha'=0.1)
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    alphas <- c(0.75, 0.5, 1-0.683, 0.25, 0.1, 0.05, 0.025, 0.01, 0.005)
    for (alpha_i in seq_along(alphas)){
        a_n <- c(-0.6067, 5.019)
        model_control <- list( 'basic'=TRUE, 'log_bound'=TRUE, 'alpha'=alphas[alpha_i], 'para_number'=0, 'manual'=TRUE)
        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
    }
    for (alpha_i in seq_along(alphas)){
        a_n <- c(-0.6067, 5.019)
        model_control <- list( 'basic'=TRUE, 'log_bound'=TRUE, 'alpha'=alphas[alpha_i], 'para_number'=1, 'manual'=TRUE)
        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
    }
})

test_that( "Coxph, lin both", {
   fname <- 'base_example.csv'
   df <- fread(fname)

   time1 <- "entry"
   time2 <- "exit"
   event <- "event"
   names <- c( "dose0", "dose1", "dose0" )
   term_n <- c(0,0,1)
   tform <- c( "loglin", "loglin", "lin" )
   keep_constant <- c(0,0,0)
   #a_n <- c(0.2462, 5.020, -0.5909)
   a_n <- c(0.2462, 5.020,-0.7)
   modelform <- "M"
   fir <- 0
   der_iden <- 0
   #
   model_control <- list( 'basic'=FALSE, 'maxstep'=100, 'log_bound'=FALSE, 'alpha'=0.1)
   control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(10,10), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=3, 'ties'='breslow', 'double_step'=1, 'guesses'=10)

   alpha <- 0.005
   a_n <- c(0.2462, 5.020,-0.599)
   model_control <- list( 'basic'=FALSE, 'maxstep'=5, 'log_bound'=TRUE, 'alpha'=alpha, 'para_number'=1, 'manual'=FALSE)
   expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
   a_n <- c(0.2462, 5.020,-0.599)
   model_control <- list( 'basic'=FALSE, 'maxstep'=5, 'log_bound'=TRUE, 'alpha'=alpha, 'para_number'=1, 'manual'=TRUE)
   expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
   alpha_list <- c(0.75, 0.5, 1-0.683, 0.25, 0.1, 0.05, 0.025, 0.01, 0.005)
   control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1, 'guesses'=10)
   control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(10,10), 'halfmax' = 5, 'epsilon' = 1e-4,  'deriv_epsilon' = 1e-3, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1, 'guesses'=10)
   for (alpha_i in 1:length(alpha_list)){
       alpha <- alpha_list[alpha_i]
       a_n <- c(0.2462, 5.020,-0.599)
       model_control <- list( 'basic'=FALSE, 'maxstep'=100, 'log_bound'=TRUE, 'alpha'=alpha, 'para_number'=1, 'manual'=TRUE)
       expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
   }
   for (alpha_i in 1:length(alpha_list)){
       alpha <- alpha_list[alpha_i]
       a_n <- c(0.2462, 5.020,-0.599)
       model_control <- list( 'basic'=FALSE, 'maxstep'=100, 'log_bound'=TRUE, 'alpha'=alpha, 'para_number'=2, 'manual'=TRUE)
       expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="nan", model_control=model_control))
   }
})

test_that( "Poisson, lin both", {
   fname <- 'base_example.csv'
   df <- fread(fname)

   pyr <- "exit"   
   event <- "event"
   names <- c( "dose0", "dose1" )
   term_n <- c(0,1)
   tform <- c( "loglin", "lin" )
   keep_constant <- c(0,0)
   a_n <- c(-2.917, 0.06526)
   modelform <- "M"
   fir <- 0
   der_iden <- 0
   #
   model_control <- list( 'basic'=FALSE, 'maxstep'=100, 'log_bound'=FALSE, 'alpha'=0.1)
   control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(10,10), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=3, 'ties'='breslow', 'double_step'=1, 'guesses'=10)

   alpha <- 0.005
   a_n <- c(-2.917, 0.06526)
   model_control <- list( 'basic'=FALSE, 'maxstep'=5, 'log_bound'=TRUE, 'alpha'=alpha, 'para_number'=1, 'manual'=FALSE)
   expect_no_error(RunPoissonRegression_Omnibus(df,pyr, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control))
   a_n <- c(-2.917, 0.06526)
   model_control <- list( 'basic'=FALSE, 'maxstep'=5, 'log_bound'=TRUE, 'alpha'=alpha, 'para_number'=1, 'manual'=TRUE)
   expect_no_error(RunPoissonRegression_Omnibus(df,pyr, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control))
})
