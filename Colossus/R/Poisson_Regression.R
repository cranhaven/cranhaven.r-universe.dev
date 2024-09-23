#' Performs basic Poisson regression using the omnibus function
#'
#' \code{RunPoissonRegression_Omnibus} uses user provided data, time/event columns,
#'  vectors specifying the model, and options to control the convergence and starting positions.
#'  Has additional options to starting with several initial guesses
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Poisson Wrapper Functions
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1),
#'                       "e"=c(0,   0,   1,   0,   0,   0,   1))
#' # For the interval case
#' pyr <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' guesses_control <- list("maxiter"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
#'     "loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform","loglin_method"="uniform",
#'      strata=FALSE)
#' strat_col <- 'e'
#' e <- RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant,
#'                                   a_n, modelform, fir, der_iden, control,strat_col)
#' @importFrom rlang .data
RunPoissonRegression_Omnibus <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(),strat_col="null",model_control=list(),cons_mat=as.matrix(c(0)),cons_vec=c(0)){
    df <- data.table(df)
    control <- Def_Control(control)
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n,
                                 names, der_iden, cons_mat, cons_vec,control$verbose)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    cons_mat <- as.matrix(val$cons_mat)
    cons_vec <- val$cons_vec
    if (typeof(a_n)!="list"){
        a_n <- list(a_n)
    }
	df <- df[get(pyr0)>0,]
    if (control$verbose>=2){
        if (any(val$Permutation != seq_along(tform))){
            message("Warning: model covariate order changed")
        }
    }
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    if (sum(df[,event0, with = FALSE])==0){
        if (control$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    if ("CONST" %in% names){
        if ("CONST" %in% names(df)){
            #fine
        } else {
            df$CONST <- 1
        }
    }
    if (model_control$strata==TRUE){
        #
        val <- factorize(df, strat_col)
        df0 <- val$df
        df <- val$df
        #
        val_cols <- c()
        for (col in val$cols){
            dftemp <- df[get(col)==1,]
            temp <- sum(dftemp[,get(event0)])
            if (temp==0){
                if (control$verbose>=2){
                    message(paste("Warning: no events for strata group:",col,sep=" "))
                }
                df <- df[get(col)!=1,]
                df0 <- df0[get(col)!=1,]
            } else {
                val_cols <- c(val_cols,col)				
            }
			data.table::setkeyv(df0, c(pyr0, event0))
        }
    } else {
        df0 <- data.table::data.table("a"=c(0,0))
        val <- list(cols=c("a"))
        val_cols <- c("a")
    }
    #
    data.table::setkeyv(df, c(pyr0, event0))
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    ce <- c(pyr0,event0)
    #
    a_ns <- c()
    for (i in a_n){
        a_ns <- c(a_ns, i)
    }
    if (model_control$log_bound){
        if ("maxiters" %in% names(control)){
        	if (length(control$maxiters) == length(a_n)+1){
        		#all good, it matches
        	} else {
        		if (control$verbose>=3){
                    message(paste("Note: Initial starts:",length(a_n),
                          ", Number of iterations provided:",length(control$maxiters),
                          ". Colossus requires one more iteration counts than number of guesses (for best guess)",sep=" "))
                }
                if (length(control$maxiters) < length(a_n)+1){
		            additional <- length(a_n)+1 - length(control$maxiters)
		            control$maxiters <- c(control$maxiters, rep(1, additional))
	            } else {
	            	additional <- length(a_n)+1
	            	control$maxiters <- control$maxiters[1:additional]
	            }
        	}
	        if ("guesses" %in% names(control)){
	            #both are in
	            if (control$guesses+1 == length(control$maxiters)){
	                #all good, it matches
	            } else if (length(control$maxiters)==2){
	                iter0 <-control$maxiters[1]
	                iter1 <-control$maxiters[2]
	                applied_iter <- c(rep(iter0,control$guesses),iter1)
	                control$maxiters <- applied_iter
	            } else {
	                if (control$verbose>=1){
                        message(paste("Error: guesses:",control["guesses"],
                              ", iterations per guess:",control["maxiters"],sep=" "))
                    }
                    stop()
	            }
	        } else {
	            control$guesses = length(control$maxiters)-1
	        }
	    } else {
	        if ("guesses" %in% names(control)){
	        	if (control$guesses == length(a_n)){
	        		#both match, all good
        		} else {
        			control$guesses = length(a_n)
        		}
                control$maxiters = rep(1,control$guesses+1)
            } else {
                control$guesses = length(a_n)
                control$maxiters = c(rep(1,length(a_n)),control$maxiter)
            }
        }
        e <- pois_Omnibus_Bounds_transition(as.matrix(df[,ce, with = FALSE]),
             term_n,tform,a_ns,dfc,x_all, fir, modelform, control,
             keep_constant,term_tot,as.matrix(df0[,val_cols, with=FALSE]),
             model_control, cons_mat, cons_vec)
        if ("Status" %in% names(e)){
            if (e$Status=="FAILED"){
	            if (control$verbose>=1){message("Error: Invalid model")}
	            stop()
            }
        }
    } else {
        if ("maxiters" %in% names(control)){
            if (length(control$maxiters) == length(a_n)+1){
                #all good, it matches
            } else {
                if (control$verbose>=3){
                    message(paste("Note: Initial starts:",length(a_n),
                        ", Number of iterations provided:",length(control$maxiters),
                        ". Colossus requires one more iteration counts than number of guesses (for best guess)",sep=" "))
                }
                if (length(control$maxiters) < length(a_n)+1){
                    additional <- length(a_n)+1 - length(control$maxiters)
                    control$maxiters <- c(control$maxiters, rep(1, additional))
                } else {
                    additional <- length(a_n)+1
                    control$maxiters <- control$maxiters[1:additional]
                }
            }
            if ("guesses" %in% names(control)){
                #both are in
                if (control$guesses+1 == length(control$maxiters)){
                    #all good, it matches
                } else {
                    if (control$verbose>=1){
                        message(paste("Error: guesses:",control["guesses"],
                            ", iterations per guess:",control["maxiters"],sep=" "))
                    }
                    stop()
                }
            } else {
                control$guesses <- length(control$maxiters)-1
            }
        } else {
            if ("guesses" %in% names(control)){
                if (control$guesses == length(a_n)){
                    #both match, all good
                } else {
                    control$guesses <- length(a_n)
                }
                control$maxiters <- rep(1,control$guesses+1)
            } else {
                control$guesses <- length(a_n)
                control$maxiters <- c(rep(1,length(a_n)),control$maxiter)
            }
        }
        #
        #
        e <- pois_Omnibus_transition(as.matrix(df[,ce, with = FALSE]),term_n,tform,
                                    matrix(a_ns,nrow=length(control$maxiters)-1,byrow=TRUE),
                                    dfc,x_all, fir,der_iden, modelform, control,keep_constant,
                                    term_tot,as.matrix(df0[,val_cols, with=FALSE]),model_control,
                                    cons_mat, cons_vec)
        e$Parameter_Lists$names <- names
        if (is.nan(e$LogLik)){
            if (control$verbose>=1){
                message("Error: Invalid risk")
            }
            stop()
        }
        #fine
    }
    return (e)
}

#' Performs joint Poisson regression using the omnibus function
#'
#' \code{RunPoissonRegression_Joint_Omnibus} uses user provided data, time/event columns,
#'  vectors specifying the model, and options to control the convergence and starting positions.
#'  Has additional options to starting with several initial guesses, uses joint competing risks equation
#'
#' @inheritParams R_template
#' @param events vector of event column names
#' @param term_n_list list of vectors for term numbers for event specific or shared model elements, defaults to term 0
#' @param tform_list list of vectors for subterm types for event specific or shared model elements, defaults to loglinear
#' @param keep_constant_list list of vectors for constant elements for event specific or shared model elements, defaults to free (0)
#' @param a_n_list list of vectors for parameter values for event specific or shared model elements, defaults to term 0
#' @param name_list list of vectors for columns for event specific or shared model elements, required
#'
#' @return returns a list of the final results
#' @export
#' @family Poisson Wrapper Functions
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' a <- c(0,0,0,1,1,1)
#' b <- c(1,1,1,2,2,2)
#' c <- c(0,1,2,2,1,0)
#' d <- c(1,1,0,0,1,1)
#' e <- c(0,1,1,1,0,0)
#' f <- c(0,1,0,0,1,1)
#' df <- data.table('t0'=a,'t1'=b,'e0'=c,'e1'=d,'fac'=e)
#' time1 <- "t0"
#' time2 <- "t1"
#' df$pyr <- df$t1 - df$t0
#' pyr <- "pyr"
#' events <- c('e0','e1')
#' names_e0 <- c('fac')
#' names_e1 <- c('fac')
#' names_shared <- c('t0','t0')
#' term_n_e0 <- c(0)
#' term_n_e1 <- c(0)
#' term_n_shared <- c(0,0)
#' tform_e0 <- c("loglin")
#' tform_e1 <- c("loglin")
#' tform_shared <- c("quad_slope","loglin_top")
#' keep_constant_e0 <- c(0)
#' keep_constant_e1 <- c(0)
#' keep_constant_shared <- c(0,0)
#' a_n_e0 <- c(-0.1)
#' a_n_e1 <- c(0.1)
#' a_n_shared <- c(0.001, -0.02)
#' name_list <- list('shared'=names_shared,'e0'=names_e0,'e1'=names_e1)
#' term_n_list <- list('shared'=term_n_shared,'e0'=term_n_e0,'e1'=term_n_e1)
#' tform_list <- list('shared'=tform_shared,'e0'=tform_e0,'e1'=tform_e1)
#' keep_constant_list <- list('shared'=keep_constant_shared,
#'                            'e0'=keep_constant_e0,'e1'=keep_constant_e1)
#' a_n_list <- list('shared'=a_n_shared,'e0'=a_n_e0,'e1'=a_n_e1)
#' 
#' der_iden <- 0
#' modelform <- "M"
#' fir <- 0
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' guesses_control <- list("maxiter"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
#'     "loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform","loglin_method"="uniform",
#'      strata=FALSE)
#' strat_col <- 'f'
#' e <- RunPoissonRegression_Joint_Omnibus(df, pyr, events, name_list, term_n_list,
#'                                         tform_list, keep_constant_list, a_n_list,
#'                                         modelform, fir, der_iden, control,strat_col)
#' 
#' @importFrom rlang .data
RunPoissonRegression_Joint_Omnibus <- function(df,pyr0, events, name_list, term_n_list=list(), tform_list=list(), keep_constant_list=list(), a_n_list=list(), modelform="M", fir=0, der_iden=0, control=list(),strat_col="null",model_control=list(),cons_mat=as.matrix(c(0)),cons_vec=c(0)){
    df <- data.table(df)
    val <- Joint_Multiple_Events(df, events, name_list, term_n_list, tform_list, keep_constant_list, a_n_list)
    df <- val$df
    names <- val$names
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    #
    e <- RunPoissonRegression_Omnibus(df, pyr0, 'events', names, term_n, tform, keep_constant,
                                   a_n, modelform, fir, der_iden, control,strat_col)
    return (e)
}



#' Performs basic poisson regression
#'
#' \code{RunPoissonRegression} uses user provided data, person-year/event columns, vectors specifying the model, and options to control the convergence and starting positions with no special options
#'
#' @inheritParams R_template
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' df$pyr <- df$Ending_Age - df$Starting_Age
#' pyr <- 'pyr'
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' a_n <- c(0.1, 0.1, 0.1, 0.1)
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'double_step'=1)
#' 
#' e <- RunPoissonRegression(df, pyr, event, names, term_n, tform, keep_constant,
#'                           a_n, modelform, fir, der_iden, control)
#' @export
#'
RunPoissonRegression <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list()){
    control <- Def_Control(control)
    control$maxiters <- c(1,control$maxiter)
    control$guesses <- 1
    e <- RunPoissonRegression_Omnibus(df, pyr0, event0, names, term_n, tform, keep_constant,
                                      a_n, modelform, fir, der_iden, control)
    #
    return (e)
}

#' Predicts how many events are due to baseline vs excess
#'
#' \code{RunPoissonEventAssignment} uses user provided data, person-year/event columns, vectors specifying the model, and options to calculate background and excess events
#'
#' @inheritParams R_template
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' df$pyr <- df$Ending_Age - df$Starting_Age
#' pyr <- 'pyr'
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' a_n <- c(0.1, 0.1, 0.1, 0.1)
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'double_step'=1)
#' 
#' e <- RunPoissonEventAssignment(df, pyr, event, names, term_n, tform, keep_constant,
#'                           a_n, modelform, fir, der_iden, control)
#' @export
#'
RunPoissonEventAssignment <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(),strat_col="null",model_control=list()){
    df <- data.table(df)
    control <- Def_Control(control)
    control$maxiters <- c(1,control$maxiter)
    control$guesses <- 1
    #
    control <- Def_Control(control)
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n,
                                 names, der_iden, as.matrix(c(0)), c(0),control$verbose)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
	df <- df[get(pyr0)>0,]
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    if (sum(df[,event0, with = FALSE])==0){
        if (control$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    if ("CONST" %in% names){
        if ("CONST" %in% names(df)){
            #fine
        } else {
            df$CONST <- 1
        }
    }
    if (model_control$strata==TRUE){
        #
        val <- factorize(df, strat_col)
        df0 <- val$df
        df <- val$df
        #
        val_cols <- c()
        for (col in val$cols){
            dftemp <- df[get(col)==1,]
            temp <- sum(dftemp[,get(event0)])
            if (temp==0){
                if (control$verbose>=2){
                    message(paste("Warning: no events for strata group:",col,sep=" "))
                }
                df <- df[get(col)!=1,]
                df0 <- df0[get(col)!=1,]
            } else {
                val_cols <- c(val_cols,col)				
            }
			data.table::setkeyv(df0, c(pyr0, event0))
        }
    } else {
        df0 <- data.table::data.table("a"=c(0,0))
        val <- list(cols=c("a"))
        val_cols <- c("a")
    }
    #
    data.table::setkeyv(df, c(pyr0, event0))
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    ce <- c(pyr0,event0)
    #
    a_ns <- c()
    for (i in a_n){
        a_ns <- c(a_ns, i)
    }
    #
    e <- Assigned_Event_Poisson_transition(as.matrix(df[,ce, with = FALSE]), as.matrix(df0), term_n, tform,
                                           a_n, dfc, x_all, fir, der_iden, modelform, control, keep_constant,
                                           term_tot, model_control)
    #
    return (e)
}

#' Predicts how many events are due to baseline vs excess at the confidence bounds of a single parameter
#'
#' \code{RunPoissonEventAssignment_bound} uses user provided data, the results of a poisson regression, and options to calculate background and excess events
#'
#' @inheritParams R_template
#' @param check_num the parameter number to check at the bounds of, indexed from 1 using the order returned by Colossus
#' @param z Z score to use for confidence interval
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1),
#'                       "e"=c(0,   0,   1,   0,   0,   0,   1))
#' # For the interval case
#' pyr <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' guesses_control <- list("maxiter"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
#'     "loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform","loglin_method"="uniform",
#'      strata=FALSE)
#' strat_col <- 'e'
#' e0 <- RunPoissonRegression_Omnibus(df, pyr, event, names, term_n, tform, keep_constant,
#'                                   a_n, modelform, fir, der_iden, control,strat_col)
#' 
#' e <- RunPoissonEventAssignment_bound(df, pyr, event, e0, keep_constant,
#'                           modelform, fir, der_iden, 4, 2, control)
#' @export
#'
RunPoissonEventAssignment_bound <- function(df, pyr0="pyr", event0="event", alternative_model=list(), keep_constant=c(0), modelform="M", fir=0, der_iden=0, check_num=1, z=2, control=list(),strat_col="null",model_control=list()){
    df <- data.table(df)
    #
    names <- alternative_model$Parameter_Lists$names
    term_n <- alternative_model$Parameter_Lists$term_n
    tform <- alternative_model$Parameter_Lists$tforms
    a_n <- alternative_model$beta_0
    stdev <- alternative_model$Standard_Deviation
    #
    e_mid <- RunPoissonEventAssignment(df, pyr0, event0, names, term_n, tform, keep_constant, a_n, modelform,
                                       fir, der_iden, control, strat_col, model_control)
    #
    a_n <- alternative_model$beta_0
    a_n[check_num] <- a_n[check_num] - z* stdev[check_num] 
    e_low <- RunPoissonEventAssignment(df, pyr0, event0, names, term_n, tform, keep_constant, a_n, modelform,
                                       fir, der_iden, control, strat_col, model_control)
    #
    a_n <- alternative_model$beta_0
    a_n[check_num] <- a_n[check_num] + z* stdev[check_num] 
    e_high <- RunPoissonEventAssignment(df, pyr0, event0, names, term_n, tform, keep_constant, a_n, modelform,
                                        fir, der_iden, control, strat_col, model_control)
    #
    bound_results <- list("lower_limit"=e_low, "midpoint"=e_mid, "upper_limit"=e_high)
    return (bound_results)
}

#' Performs poisson regression with no derivative calculations
#'
#' \code{RunPoissonRegression_Single} uses user provided data, person-year/event columns, vectors specifying the model, and returns the results
#'
#' @inheritParams R_template
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' df$pyr <- df$Ending_Age - df$Starting_Age
#' pyr <- 'pyr'
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' a_n <- c(0.1, 0.1, 0.1, 0.1)
#' 
#' keep_constant <- c(0,0,0,0)
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,
#'              'epsilon' = 1e-3, 'deriv_epsilon' = 1e-3,
#'              'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,
#'              'verbose'=FALSE, 'double_step'=1)
#' 
#' e <- RunPoissonRegression_Single(df, pyr, event, names, term_n, tform, a_n, modelform, fir, control)
#' @export
#'
RunPoissonRegression_Single <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", a_n=c(0), modelform="M", fir=0, control=list(),keep_constant=rep(0,length(names))){
    control <- Def_Control(control)
    control$maxiters <- c(1,control$maxiter)
    control$guesses <- 1
    e <- RunPoissonRegression_Omnibus(df, pyr0, event0, names, term_n, tform, keep_constant,
                                      a_n, modelform, fir, 0, control,
                                      model_control=list("single"=TRUE))
    return (e)
}

#' Performs poisson regression with strata effect
#'
#' \code{RunPoissonRegression_STRATA} uses user provided data, time/event columns, vectors specifying the model, and options to control the convergence and starting positions
#'
#' @inheritParams R_template
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @export
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1),
#'                       "e"=c(0,   0,   0,   0,   1,   0,   1))
#' # For the interval case
#' df$pyr <- df$Ending_Age - df$Starting_Age
#' pyr <- 'pyr'
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' a_n <- c(0.1, 0.1, 0.1, 0.1)
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,
#'              'epsilon' = 1e-3, 'deriv_epsilon' = 1e-3,
#'              'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,
#'              'verbose'=FALSE, 'double_step'=1)
#' strat_col <- c("e")
#' e <- RunPoissonRegression_STRATA(df, pyr, event, names, term_n, tform, keep_constant,
#'      a_n, modelform, fir, der_iden, control, strat_col)
#'
RunPoissonRegression_STRATA <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(),strat_col="null"){
    control <- Def_Control(control)
    control$maxiters <- c(1,control$maxiter)
    control$guesses <- 1
    e <- RunPoissonRegression_Omnibus(df, pyr0, event0, names, term_n, tform, keep_constant, a_n,
                                      modelform, fir, der_iden, control,strat_col,
                                      model_control=list("strata"=TRUE))
    return (e)
}


#' Performs basic poisson regression, with multiple guesses, starts with a single term
#'
#' \code{RunPoissonRegression_Tier_Guesses} uses user provided data, time/event columns, vectors specifying the model, and options to control the convergence and starting positions, with additional guesses
#'
#' @inheritParams R_template
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @export
#'
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1),
#'                       "e"=c(0,   0,   0,   0,   1,   0,   1))
#' # For the interval case
#' df$pyr <- df$Ending_Age - df$Starting_Age
#' pyr <- 'pyr'
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE,'double_step'=1)
#' guesses_control <- list("iterations"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
#'   "loglin_min"=-1,"loglin_max"=1,"lin_method"="uniform",
#'   "loglin_method"="uniform",strata=TRUE,term_initial = c(0,1))
#' strat_col=c('e')
#' 
#' e <- RunPoissonRegression_Tier_Guesses(df, pyr, event, names,
#'      term_n, tform, keep_constant, a_n, modelform,
#'      fir, der_iden, control, guesses_control, strat_col)
#'
#' @importFrom rlang .data
RunPoissonRegression_Tier_Guesses <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(), guesses_control=list(),strat_col="null", model_control=list()){
    df <- data.table(df)
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    t_initial <- guesses_control$term_initial
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    #
    rmin <- guesses_control$rmin
    rmax <- guesses_control$rmax
    if (length(rmin)!=length(rmax)){
        if (control$verbose>=2){
            message("Warning: rmin and rmax lists not equal size, defaulting to lin and loglin min/max values")
        }
    }
    #
    name_initial <- c()
    term_n_initial <- c()
    tform_initial <- c()
    constant_initial <- c()
    a_n_initial <- c()
    guess_constant <- c()
    #
    for (i in seq_len(length(a_n))){
        if (term_n[i] %in% t_initial){
            name_initial <- c(name_initial, names[i])
            term_n_initial <- c(term_n_initial, term_n[i])
            tform_initial <- c(tform_initial, tform[i])
            constant_initial <- c(constant_initial, keep_constant[i])
            a_n_initial <- c(a_n_initial, a_n[i])
            guess_constant <- c(guess_constant, 0)
        }
    }
    guesses_control$guess_constant <- guess_constant
    guess_second <- guesses_control$guesses
    guesses_control$guesses <- guesses_control$guesses_start
    e <- RunPoissonRegression_Guesses_CPP(df, pyr0, event0, name_initial, term_n_initial,
                                          tform_initial, constant_initial, a_n_initial,
                                          modelform, fir, der_iden, control, guesses_control,
                                          strat_col, model_control)
    if (guesses_control$verbose>=3){
        message("Note: INITIAL TERM COMPLETE")
        message(e)
    }
    #
    a_n_initial <- unlist(e$beta_0,use.names=FALSE)
    guess_constant <- c()
    j <- 1
    for (i in seq_len(length(a_n))){
        if (term_n[i] %in% t_initial){
            a_n[i] <- a_n_initial[j]
            j <- j+1
            guess_constant <- c(guess_constant, 1)
        } else {
            guess_constant <- c(guess_constant, 0)
        }
    }
    guesses_control$guess_constant <- guess_constant
    guesses_control$guesses <- guess_second
    e <- RunPoissonRegression_Guesses_CPP(df, pyr0, event0, names, term_n, tform,
         keep_constant, a_n, modelform, fir, der_iden,
         control, guesses_control, strat_col, model_control)
    #
    return(e)
}

#' Performs basic Poisson regression, generates multiple starting guesses on c++ side
#'
#' \code{RunPoissonRegression_Guesses_CPP} uses user provided data, time/event columns, vectors specifying the model, and options to control the convergence and starting positions. Has additional options to starting with several initial guesses
#'
#' @inheritParams R_template
#' @family Poisson Wrapper Functions
#' @return returns a list of the final results
#' @export
#'
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1),
#'                       "e"=c(0,   0,   1,   0,   0,   0,   1))
#' # For the interval case
#' pyr <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' guesses_control <- list("maxiter"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
#'     "loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform","loglin_method"="uniform",strata=FALSE)
#' strat_col <- 'e'
#' 
#' e <- RunPoissonRegression_Guesses_CPP(df, pyr, event, names, term_n,
#'                               tform, keep_constant, a_n, modelform, fir,
#'                               der_iden, control,guesses_control,strat_col)
#' @importFrom rlang .data
RunPoissonRegression_Guesses_CPP <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(), guesses_control=list(),strat_col="null", model_control=list()){
    df <- data.table(df)
    if (typeof(a_n)!="list"){
        a_n <- list(a_n)
    }
    control <- Def_Control(control)
    if ("strata" %in% names(guesses_control)){
        if ("strata" %in% names(model_control)){
            if (guesses_control$strata != model_control$strata){
                if (guesses_control$verbose>=1){
                    message("Error: guesses_control and model_control have different strata options")
                }
                stop()
            }
        } else {
            model_control$strata <- guesses_control$strata
        }
    } else if ("strata" %in% names(model_control)){
        guesses_control$strata <- model_control$strata
    }
    guesses_control <- Def_Control_Guess(guesses_control, a_n[[1]])
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    if ("CONST" %in% names){
        if ("CONST" %in% names(df)){
            #fine
        } else {
            df$CONST <- 1
        }
    }
    a_n_default <- rep(0,length(a_n[[1]]))
    for (i in seq_along(a_n[[1]])){
        a_n_default[i] <- a_n[[1]][i]
    }
    #
    data.table::setkeyv(df, c(pyr0, event0))
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
    #
    dat_val <- Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n,
                                  x_all, a_n_default, modelform, fir, control, guesses_control)
    a_ns <- dat_val$a_ns
    maxiters <- dat_val$maxiters
    #
    control$maxiters <- c(maxiters,control$maxiter)
    control$guesses <- length(maxiters)
    #
    #
    a_n_mat <- matrix(a_ns,nrow=length(control$maxiters)-1,byrow=TRUE)
    a_n <- lapply(seq_len(nrow(a_n_mat)), function(i) a_n_mat[i,])
    e <- RunPoissonRegression_Omnibus(df, pyr0, event0, names, term_n, tform, keep_constant, a_n,
                                      modelform, fir, der_iden, control,model_control=model_control,
                                      strat_col=strat_col)
    #fine
    return (e)
}
    
#' Calculates poisson residuals
#'
#' \code{RunPoissonRegression_Residual} uses user provided data, time/event columns,
#'  vectors specifying the model, and options. Calculates residuals or sum of residuals
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Poisson Wrapper Functions
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1),
#'                       "e"=c(0,   0,   1,   0,   0,   0,   1))
#' # For the interval case
#' pyr <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' guesses_control <- list("maxiter"=10,"guesses"=10,"lin_min"=0.001,"lin_max"=1,
#'     "loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform","loglin_method"="uniform",
#'      strata=FALSE)
#' strat_col <- 'e'
#' e <- RunPoissonRegression_Residual(df, pyr, event, names, term_n, tform, keep_constant,
#'                                   a_n, modelform, fir, der_iden, control,strat_col)
#' @importFrom rlang .data
RunPoissonRegression_Residual <- function(df, pyr0="pyr", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(),strat_col="null",model_control=list()){
    df <- data.table(df)
    cons_mat=as.matrix(c(0))
    cons_vec=c(0)
    control <- Def_Control(control)
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n,
                                 names, der_iden, cons_mat, cons_vec,control$verbose)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    cons_mat <- as.matrix(val$cons_mat)
    cons_vec <- val$cons_vec
    if (typeof(a_n)!="list"){
        a_n <- list(a_n)
    }
	df <- df[get(pyr0)>0,]
    if (control$verbose>=2){
        if (any(val$Permutation != seq_along(tform))){
            message("Warning: model covariate order changed")
        }
    }
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    if (sum(df[,event0, with = FALSE])==0){
        if (control$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    if ("CONST" %in% names){
        if ("CONST" %in% names(df)){
            #fine
        } else {
            df$CONST <- 1
        }
    }
    if (model_control$strata==TRUE){
        #
        val <- factorize(df, strat_col)
        df0 <- val$df
        df <- val$df
        #
        val_cols <- c()
        for (col in val$cols){
            dftemp <- df[get(col)==1,]
            temp <- sum(dftemp[,get(event0)])
            if (temp==0){
                if (control$verbose>=2){
                    message(paste("Warning: no events for strata group:",col,sep=" "))
                }
                df <- df[get(col)!=1,]
                df0 <- df0[get(col)!=1,]
            } else {
                val_cols <- c(val_cols,col)				
            }
			data.table::setkeyv(df0, c(pyr0, event0))
        }
    } else {
        df0 <- data.table::data.table("a"=c(0,0))
        val <- list(cols=c("a"))
        val_cols <- c("a")
    }
    #
    data.table::setkeyv(df, c(pyr0, event0))
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    ce <- c(pyr0,event0)
    #
    #
    #
    e <- pois_Residual_transition(as.matrix(df[,ce, with = FALSE]),term_n,tform, a_n[[1]],
                                 dfc,x_all, fir,der_iden, modelform, control,keep_constant,
                                 term_tot,as.matrix(df0[,val_cols, with=FALSE]),model_control)
    #fine
    return (e)
}
