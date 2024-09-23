test_that( "Coxph basic_single_null match", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    time2 <- "t1"
    event <- "lung"
    names <- c( "dose" )
    term_n <- c(0)
    tform <- c( "loglin" )
    keep_constant <- c(0)
    a_n <- c(0.0)
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    verbose <- FALSE

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(-1,-1), 'halfmax' = -1, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)
    model_control <- list( 'strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'null'=FALSE)
    e0 <- RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=c( "loglin" ), keep_constant=keep_constant, a_n=a_n, modelform="M", fir=0, der_iden=der_iden, control=control,strat_col="fac", model_control=model_control)
    for (j in c(TRUE,FALSE)){
        for (k in c(TRUE,FALSE)){
            for (l in c(TRUE,FALSE)){
                model_control <- list( 'strata'=FALSE, 'basic'=j, 'single'=k, 'null'=l)
                if (verbose){print(model_control)}
                a_n <- c(0.0)
                expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=c( "loglin" ), keep_constant=keep_constant, a_n=a_n, modelform="M", fir=0, der_iden=der_iden, control=control,strat_col="fac", model_control=model_control))
                if (verbose){print( "---------------" )}
            }
        }
    }
    model_control <- list( 'strata'=TRUE, 'basic'=FALSE, 'single'=FALSE, 'null'=FALSE)
    e0 <- RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=c( "loglin" ), keep_constant=keep_constant, a_n=a_n, modelform="M", fir=0, der_iden=der_iden, control=control,strat_col="fac", model_control=model_control)
    for (j in c(TRUE,FALSE)){
        for (k in c(TRUE,FALSE)){
            for (l in c(TRUE,FALSE)){
                model_control <- list( 'strata'=TRUE, 'basic'=j, 'single'=k, 'null'=l)
                if (verbose){print(model_control)}
                a_n <- c(0.0)
                expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=c( "loglin" ), keep_constant=keep_constant, a_n=a_n, modelform="M", fir=0, der_iden=der_iden, control=control,strat_col="fac", model_control=model_control))
                if (verbose){print( "---------------" )}
            }
        }
    }
    sink(NULL)
    close(tfile)
})
test_that( "Coxph strata_basic_single_CR", {
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
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
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=3, 'ties'='breslow', 'double_step'=1)
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

    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)

    verbose <- FALSE
    j_iterate <- 1
    for (i in c(TRUE,FALSE)){
        for (j in c(TRUE,FALSE)){
            for (k in c(TRUE,FALSE)){
                for (l in c(TRUE,FALSE)){
                    model_control <- list( 'strata'=i, 'basic'=j, 'single'=k, 'cr'=l)
                    if (verbose){print(model_control)}
                    a_n <- c(-0.1,-0.1)
                    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=1)
                    expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control, cens_weight="weighting"))
                    j_iterate <- j_iterate + 1
                    a_n <- c(-0.1,-0.1)
                    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='efron', 'double_step'=0)
                    expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="rand", model_control=model_control, cens_weight="weighting"))
                    j_iterate <- j_iterate + 1
                    if (verbose){print( "---------------" )}
                }
            }
        }
    }
    sink(NULL)
    close(tfile)
})

test_that( "Pois strata_single", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))
    names <- c( "dose", "rand", "rand" )
    term_n <- c(2,1,0)
    tform <- c( "loglin", "lin", "plin" )
    keep_constant <- c(0,0,0)
    a_n <- c(0.01,0.1,0.1)
    modelform <- "PAE"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=0)
    strat_col <- "fac"
    
    verbose <- FALSE
    j_iterate <- 1
    for (i in c(TRUE,FALSE)){
        for (j in c(TRUE,FALSE)){
            model_control <- list( 'strata'=i, 'single'=j)
            if (verbose){print(model_control)}
            a_n <- c(0.01,0.1,0.1)
            modelform <- "PAE"
            expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col,model_control))
            j_iterate <- j_iterate + 1
            a_n <- c(0.01,0.1,0.1)
            modelform <- "A"
            expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col,model_control))
            j_iterate <- j_iterate + 1
            if (verbose){print( "---------------" )}
        }
    }
    sink(NULL)
    close(tfile)
})

test_that( "Pois comb_forms", {
	fname <- 'll_0.csv'
	colTypes <- c( "double", "double", "double", "integer", "integer" )
	df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
	time1 <- "t0"
	df$pyr <- df$t1-df$t0
	pyr <- "pyr"
	event <- "lung"
    set.seed(3742)
	df$rand <- floor(runif(nrow(df), min=1, max=5))
	names <- c( "dose", "rand", "rand", "dose", "dose" )
	term_n <- c(1,0,0, 0, 0)
	tform <- c( "loglin", "lin", "plin", "loglin_slope", "loglin_top" )
	keep_constant <- c(0,0,0, 0, 0)
	a_n <- c(0.01,0.1,0.1, 1.0, 0.1)
	modelform <- "PAE"
	fir <- 0
	der_iden <- 0
	control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=0)
	strat_col <- "fac"

	verbose <- FALSE
	modelforms <- c( "A", "PAE", "M", "PA" )
	j_iterate <- 1
	for (modelform in modelforms){
		model_control <- list( 'strata'=FALSE, 'single'=FALSE)
		if (verbose){print(model_control)}
		a_n <- c(0.01,0.1,0.1, 1.0, 0.1)
		expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col,model_control))
        j_iterate <- j_iterate + 1
		if (verbose){print( "---------------" )}
	}
	term_n <- c(1,1,1, 0, 0)
	for (modelform in modelforms){
		model_control <- list( 'strata'=FALSE, 'single'=FALSE)
		if (verbose){print(model_control)}
		a_n <- c(0.01,0.1,0.1, 1.0, 0.1)
		expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col,model_control))
        j_iterate <- j_iterate + 1
		if (verbose){print( "---------------" )}
	}
})

test_that( "Pois strata_single expanded", {
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))
    names <- c( "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose",  "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand" )
    term_n <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)
    tform <- c( "loglin_slope", "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope", "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope" )
    keep_constant <- c(0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    a_n <-   c(1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)

    modelform <- "PAE"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=0)
    strat_col <- "fac"
    
    verbose <- FALSE
    j_iterate <- 1
    for (i in c(TRUE,FALSE)){
        for (j in c(TRUE,FALSE)){
            model_control <- list( 'strata'=i, 'single'=j)
            if (verbose){print(model_control)}
            a_n <-   c(1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)
            modelform <- "PAE"
            expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col,model_control))
            j_iterate <- j_iterate + 1
            a_n <-   c(1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)
            modelform <- "A"
            expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col,model_control))
            j_iterate <- j_iterate + 1
            if (verbose){print( "---------------" )}
        }
    }
    sink(NULL)
    close(tfile)
})

test_that( "risk check omnibus plain", {
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))

    time1 <- "t0"
    time2 <- "t1"
    df$censor <- (df$lung==0)
    #
    event <- "lung"
    names <- c( "dose", "fac", "dose", "fac", "rand" )
    term_n <- c(0,0,1,1,1)
    tform <- c( "loglin", "lin", "lin", "plin", "loglin" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- c(-0.1,0.1,0.2,0.3,-0.5)
    modelform <- "M"
    fir <- 0
    der_iden <- 0

    cens_weight <- c(0)
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)

    verbose <- FALSE

    df_order <- data.table( "term_n"=term_n, "tform"=tform, "keep_constant"=keep_constant, "a_n"=a_n, "names"=names, "order"=1:5)

    model_list <- c( 'M', 'A', 'PA', 'PAE' )
    for (model_i in 1:4){
        modelform <- model_list[model_i]
        for (fir in c(0,1)){
            for(i in 1:5){
                model_control <- list( 'strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'cr'=FALSE)
                #
                df_order$order <- sample(df_order$order)
                setorderv(df_order, c( "order" ))
                term_n <- df_order$term_n
                tform <- df_order$tform
                keep_constant <- df_order$keep_constant
                a_n <- df_order$a_n
                names <- df_order$names
                #
                control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
                expect_no_error(Cox_Relative_Risk(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, control=control, model_control=model_control)$Risk)

                a_i <- (model_i-1)*10 + fir*5 + i
            }
        }
    }
})

test_that( "risk check omnibus gmix", {
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))

    time1 <- "t0"
    time2 <- "t1"
    df$censor <- (df$lung==0)
    #
    event <- "lung"
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)

    verbose <- FALSE

    model_list <- c( 'GMIX-R', 'GMIX-E', 'GMIX' )
    names <- c( "dose", "fac", "dose", "fac", "rand" )
    term_n <- c(0,0,1,1,2)
    tform <- c( "loglin", "loglin", "plin", "plin", "loglin" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- c(-0.1,0.1,0.2,0.3,-0.5)
    df_order <- data.table( "term_n"=term_n, "tform"=tform, "keep_constant"=keep_constant, "a_n"=a_n, "names"=names, "order"=1:5)
    count <- 0
    for (model_i in 1:3){
        modelform <- model_list[model_i]
        if (modelform=='GMIX' ){
            for (fir in c(0,1,2)){
                for (term_i in 0:3){
                    model_control <- list( 'strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'cr'=FALSE)
                    if (fir==0){
                        model_control$gmix_term <- c(0,term_i%%2, floor(term_i/2))
                    } else if (fir==1){
                        model_control$gmix_term <- c(term_i%%2,0, floor(term_i/2))
                    }  else if (fir==2){
                        model_control$gmix_term <- c(term_i%%2, floor(term_i/2),1)
                    }
                    #
                    df_order$order <- sample(df_order$order)
                    setorderv(df_order, c( "order" ))
                    term_n <- df_order$term_n
                    tform <- df_order$tform
                    keep_constant <- df_order$keep_constant
                    a_n <- df_order$a_n
                    names <- df_order$names
                    #
                    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
                    expect_no_error(Cox_Relative_Risk(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, control=control, model_control=model_control)$Risk)
                    count <- count + 1
                }
            }
        } else {
            for (fir in c(0,1,2)){
                model_control <- list( 'strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'cr'=FALSE)
                #
                df_order$order <- sample(df_order$order)
                setorderv(df_order, c( "order" ))
                term_n <- df_order$term_n
                tform <- df_order$tform
                keep_constant <- df_order$keep_constant
                a_n <- df_order$a_n
                names <- df_order$names
                #
                control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
                expect_no_error(Cox_Relative_Risk(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, control=control, model_control=model_control)$Risk)
                count <- count + 1
            }
        }
    }
})

test_that( "risk check omnibus dose", {
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))

    time1 <- "t0"
    time2 <- "t1"
    df$censor <- (df$lung==0)
    #
    event <- "lung"
    names <- c( "dose", "fac", "dose", "fac", "rand" )
    term_n <- c(0,0,1,1,1)
    tform <- c( "loglin", "lin", "lin", "plin", "loglin" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- c(-0.1,0.1,0.2,0.3,-0.5)
    modelform <- "M"
    fir <- 0
    der_iden <- 0

    cens_weight <- c(0)
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)

    verbose <- FALSE

    df_order <- data.table( "term_n"=term_n, "tform"=tform, "keep_constant"=keep_constant, "a_n"=a_n, "names"=names, "order"=1:5)

    model_list <- c( 'M', 'A', 'PA', 'PAE' )
    names <- c( "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose",  "fac", "fac", "fac", "fac", "fac", "fac", "fac", "fac", "fac", "fac", "fac" )
    term_n <- c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)
    tform <- c( "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope", "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope" )
    keep_constant <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    a_n <-   c(-0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)

    df_order <- data.table( "term_n"=term_n, "tform"=tform, "keep_constant"=keep_constant, "a_n"=a_n, "names"=names, "order"=1:22)
    for (model_i in 1:4){
        modelform <- model_list[model_i]
        for (fir in c(0,1)){
            for(i in 1:22){
                model_control <- list( 'strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'cr'=FALSE)
                #
                df_order$order <- sample(df_order$order)
                setorderv(df_order, c( "order" ))
                term_n <- df_order$term_n
                tform <- df_order$tform
                keep_constant <- df_order$keep_constant
                a_n <- df_order$a_n
                names <- df_order$names
                #
                control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
                expect_no_error(Cox_Relative_Risk(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, control=control, model_control=model_control)$Risk)
            }
        }
    }
})

test_that( "check deviation calc", {
    fname <- 'll_comp_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))

    time1 <- "t0"
    time2 <- "t1"


    #
    event <- "lung"
    names <- c( "dose", "fac", "rand" )
    term_n <- c(0,0,1)
    tform <- c( "loglin", "loglin", "loglin" )
    keep_constant <- c(0,0,0)
    a_n <- c(-0.1,0.1,0.2)
    modelform <- "M"
    fir <- 0
    der_iden <- 0

    cens_weight <- c(0)

    verbose <- FALSE

    devs <- c()

    modelform <- "M"
    model_control <- list( 'strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'cr'=FALSE)
    for (i in 1:3){
        a_n <- c(0.6465390, 0.4260961, 0.1572781)
        keep_constant <- c(0,0,0)
        keep_constant[i] <- 1
        #
        control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
        e <- RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="fac", model_control=model_control)
        devs <- c(devs, sum(e$Standard_Deviation))
    }
    a_n <- c(0.6465390, 0.4260961, 0.1572781)
    keep_constant <- c(0,0,0)
    #
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 2, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n=term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,strat_col="fac", model_control=model_control))
})

test_that( "check Linear Constraints", {
    fname <- 'l_pl_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    df$pyr <- df$t1 - df$t0
    time1 <- "t0"
    time2 <- "t1"
    pyr <- 'pyr'
    event <- "lung"
    names <- c( "dose", "fac" )
    term_n <- c(0,0)
    tform <- c( "loglin", "plin" )
    keep_constant <- c(0,0)
    model_control <- list( 'strata'=F, 'basic'=F, 'single'=F, 'null'=F, 'constraint'=T)
    Constraint_Matrix <- matrix(c(1,-1),nrow=1)
    Constraint_const  <- c(0.0)
    set.seed(3742)
    for (i in 1:20){
        a_n <- 2*runif(2)-1
        del <- abs(a_n[1]-a_n[2])
        a_n0 <- rep(sum(a_n)/2,2)
        a_n <- a_n0 - c(-del/2,del/2)
        modelform <- "M"
        fir <- 0
        der_iden <- 0
        control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
        expect_no_error(RunCoxRegression_Omnibus(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col="fac", model_control=model_control,cons_mat=Constraint_Matrix, cons_vec=Constraint_const))
    }
    for (i in 1:20){
        a_n <- 2*runif(2)-1
        del <- abs(a_n[1]-a_n[2])
        a_n0 <- rep(sum(a_n)/2,2)
        a_n <- a_n0 + c(-del/2,del/2)
        modelform <- "M"
        fir <- 0
        der_iden <- 0
        control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = 20, 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
        expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,strat_col="fac",model_control=model_control,cons_mat=Constraint_Matrix, cons_vec=Constraint_const))
    }
})

test_that( "Pois double_step change_all calcs", {
    tfile <- file(paste(tempfile(), ".txt",sep="" ),open = "wt")
    sink(file=tfile)
    fname <- 'll_0.csv'
    colTypes <- c( "double", "double", "double", "integer", "integer" )
    df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
    time1 <- "t0"
    df$pyr <- df$t1-df$t0
	pyr <- "pyr"
    event <- "lung"
    set.seed(3742)
    df$rand <- floor(runif(nrow(df), min=0, max=5))
    names <- c( "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose", "dose",  "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand", "rand" )
    term_n <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1)
    tform <- c( "loglin_slope", "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope", "loglin_top", "lin_slope", "lin_int", "quad_slope", "step_slope", "step_int", "lin_quad_slope", "lin_quad_int", "lin_exp_slope", "lin_exp_int", "lin_exp_exp_slope" )
    keep_constant <- c(0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    a_n <-   c(1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)

    modelform <- "PAE"
    fir <- 0
    der_iden <- 0
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=0)
    strat_col <- "fac"
    
    verbose <- FALSE
    j_iterate <- 1
    for (i in c(0,1)){
        for (j in seq_len(length(term_n))){
            model_control <- list( 'strata'=F, 'single'=F)
            if (verbose){print(model_control)}
            a_n <-   c(1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1, -0.1          ,-0.1       ,1        ,-0.1        ,1           ,2         ,0.3             ,1.5           ,0.2            ,0.7          ,1)
            modelform <- "PAE"
            control <- list( "ncores"=2, 'lr' = 0.75, 'maxiters' = c(1,1), 'halfmax' = 5, 'epsilon' = 1e-6,  'deriv_epsilon' = 1e-6, 'abs_max'=1.0, 'change_all'=F, 'dose_abs_max'=100.0, 'verbose'=4, 'ties'='breslow', 'double_step'=i)
            expect_no_error(RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant, a_n, modelform, fir, j-1, control,strat_col,model_control))
            if (verbose){print( "---------------" )}
        }
    }
    sink(NULL)
    close(tfile)
})
