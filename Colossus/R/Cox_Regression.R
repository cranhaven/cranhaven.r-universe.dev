#' Performs Cox Proportional Hazards regression using the omnibus function
#'
#' \code{RunCoxRegression_Omnibus} uses user provided data, time/event columns,
#'       vectors specifying the model, and options to control the convergence
#'       and starting positions. Has additional options for starting with several
#'       initial guesses, using stratification, multiplicative loglinear 1-term,
#'       competing risks, and calculation without derivatives
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Cox Wrapper Functions
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
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- list(c(1.1, -0.1, 0.2, 0.5),c(1.6, -0.12, 0.3, 0.4))
#' #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiters' = c(5,5,5),
#'    'halfmax' = 5,'epsilon' = 1e-3, 'deriv_epsilon' = 1e-3,
#'    'abs_max'=1.0,'change_all'=TRUE, 'dose_abs_max'=100.0,'verbose'=FALSE,
#'    'ties'='breslow','double_step'=1, "guesses"=2)
#' 
#' e <- RunCoxRegression_Omnibus(df, time1, time2, event,
#'                               names, term_n, tform, keep_constant,
#'                               a_n, modelform, fir, der_iden, control,
#'                               model_control=list("single"=FALSE,
#'                               "basic"=FALSE, "cr"=FALSE, 'null'=FALSE))
#' @importFrom rlang .data
RunCoxRegression_Omnibus <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(),strat_col="null", cens_weight="null", model_control=list(),cons_mat=as.matrix(c(0)),cons_vec=c(0)){
    Rstart <- Sys.time()
    df <- data.table(df)
    ##
    ce <- c(time1,time2,event0)
    t_check <- Check_Trunc(df,ce)
    df <- t_check$df
    ce <- t_check$ce
    ## Cox regression only uses intervals which contain an event time
    time1 <- ce[1]
    time2 <- ce[2]
    dfend <- df[get(event0)==1, ]
    tu <- sort(unlist(unique(dfend[,time2,with=FALSE]), use.names=FALSE))
    # remove rows that end before first event
    df <- df[get(time2)>= tu[1],]
    # remove rows that start after the last event
    df <- df[get(time1)<= tu[length(tu)],]
    ##
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
    #
    if (typeof(a_n)!="list"){
        a_n <- list(a_n)
    }
    if (control$verbose>=2){
        if (any(val$Permutation != seq_along(tform))){
            message("Warning: model covariate order changed")
        }
    }
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    if ("CONST" %in% names){
        if ("CONST" %in% names(df)){
            #fine
        } else {
            df$CONST <- 1
        }
    }
    if (model_control$cr==TRUE){
        if (cens_weight %in% names(df)){
            # good
        } else {
            if (control$verbose>=1){
                message("Error: censoring weight column not in the dataframe.")
            }
            stop()
        }
    } else {
        df[[cens_weight]] <- 1
    }
    if (model_control$strata==FALSE){
        data.table::setkeyv(df, c(time2, event0))
        uniq <- c(0)
        ce <- c(time1,time2,event0)
    } else {
        #
        dfend <- df[get(event0)==1, ]
        uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]),
                            use.names=FALSE))
        #
        for (i in seq_along(uniq)){
            df0 <- dfend[get(strat_col)==uniq[i],]
            tu0 <- unlist(unique(df0[,time2,with=FALSE]), use.names=FALSE)
            if (length(tu0)==0){
                if (control$verbose>=2){
                    message(paste("Warning: no events for strata group:",
                                 uniq[i],sep=" "))
                }
                df <- df[get(strat_col)!=uniq[i],]
            }
        }
        uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]),
                            use.names=FALSE))
        if (control$verbose>=3){
            message(paste("Note:",length(uniq)," strata used",sep=" "))
        }
        #
        data.table::setkeyv(df, c(time2, event0, strat_col))
        ce <- c(time1,time2,event0,strat_col)
    }
    dfend <- df[get(event0)==1, ]
    tu <- sort(unlist(unique(dfend[,time2, with = FALSE]),use.names=FALSE))
    if (length(tu)==0){
        if (control$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    if (control$verbose>=3){
        message(paste("Note: ",length(tu)," risk groups",sep=""))
    }
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    ##
    # make sure any constant 0 columns are constant
   for (i in 1:length(keep_constant)){
       if ((keep_constant[i]==0)&&(names[i] %in% names(df))){
           if (names[i] != 'CONST'){
               if (min(df[[names[i]]])==max(df[[names[i]]])){
                   keep_constant[i] <- 1
                   if (control$verbose>=2){
                       message(paste("Warning: element ",i," with column name ",names[i]," was set constant",sep=""))
                   }
               }
           }
       }
   }
    if (min(keep_constant)>0){
        if (control$verbose>=1){
            message("Error: Atleast one parameter must be free")
        }
        stop()
    }
    ##
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
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
        e <- cox_ph_Omnibus_Bounds_transition(term_n,tform,a_ns,dfc,x_all, fir,
             modelform, control, as.matrix(df[,ce, with = FALSE]),tu,
             keep_constant,term_tot, uniq, df[[cens_weight]], model_control,
             cons_mat, cons_vec)
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
        if (model_control$null){
            a_ns <- matrix(a_ns)
        } else {
            a_ns <- matrix(a_ns,nrow=length(control$maxiters)-1,byrow=TRUE)
        }
        Rend <- Sys.time()
        e <- cox_ph_Omnibus_transition(term_n,tform,a_ns,dfc,x_all, fir,der_iden,
             modelform, control, as.matrix(df[,ce, with = FALSE]),tu,
             keep_constant,term_tot, uniq, df[[cens_weight]], model_control,
             cons_mat, cons_vec)
	    if (is.nan(e$LogLik)){
		    if (control$verbose>=1){message("Error: Invalid risk")}
		    stop()
	    }
        e$Parameter_Lists$names <- names
    }
    return (e)
}

#' Performs basic Cox Proportional Hazards regression without special options
#'
#' \code{RunCoxRegression} uses user provided data, time/event columns,
#' vectors specifying the model, and options to control the convergence
#' and starting position
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Cox Wrapper Functions
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
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
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
#'    'epsilon' = 1e-3, 'deriv_epsilon' = 1e-3,
#'    'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,
#'    'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' 
#' e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform,
#'                      keep_constant, a_n, modelform, fir, der_iden, control)
#' @importFrom rlang .data

RunCoxRegression <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list()){
    #
    control <- Def_Control(control)
    control$maxiters <- c(1, control$maxiter)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n,
                                  tform, keep_constant, a_n, modelform,
                                  fir, der_iden, control, model_control=list())
    #
    return (e)
}

#' Performs basic Cox Proportional Hazards calculation with no derivative
#'
#' \code{RunCoxRegression_Single} uses user provided data, time/event columns, vectors specifying the model, and options and returns the log-likelihood
#'
#' @inheritParams R_template
#' @family Cox Wrapper Functions
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
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' 
#' keep_constant <- c(0,0,0,0)
#' 
#' control <- list("ncores"=2,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' 
#' e <- RunCoxRegression_Single(df, time1, time2, event, names, term_n, tform,
#'                              keep_constant, a_n, modelform, fir, control)
#'
#' @importFrom rlang .data

RunCoxRegression_Single <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, control=list()){
    control <- Def_Control(control)
    control$maxiters <- c(1, control$maxiter)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n,
         tform, keep_constant, a_n, modelform, fir, 0, control,
         model_control=list('single'=TRUE))
    #
    return (e)
}

#' Performs basic Cox Proportional Hazards regression with a multiplicative log-linear model
#'
#' \code{RunCoxRegression_Basic} uses user provided data, time/event columns, vectors specifying the model, and options to control the convergence and starting positions
#'
#' @inheritParams R_template
#' @family Cox Wrapper Functions
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
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,
#'    'epsilon' = 1e-3,'deriv_epsilon' = 1e-3, 'abs_max'=1.0,
#'    'change_all'=TRUE,'dose_abs_max'=100.0,'verbose'=FALSE,
#'    'ties'='breslow','double_step'=1)
#' 
#' e <- RunCoxRegression_Basic(df, time1, time2, event, names, keep_constant,
#'                             a_n, der_iden, control)
#'
#' @importFrom rlang .data

RunCoxRegression_Basic <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), keep_constant=c(0), a_n=c(0), der_iden=0, control=list()){
    control <- Def_Control(control)
    control$maxiters <- c(1, control$maxiter)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names,
         rep(0,length(names)), rep('loglin',length(names)), keep_constant, a_n,
         "M", 0, der_iden, control, model_control=list("basic"=TRUE))
    #
    return (e)
}


#' Performs basic Cox Proportional Hazards regression with strata effect
#'
#' \code{RunCoxRegression_STRATA} uses user provided data,
#' time/event columns, vectors specifying the model, and options to control
#' the convergence and starting positions
#'
#' @inheritParams R_template
#' @family Cox Wrapper Functions
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
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
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
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,
#'    'epsilon' = 1e-3,'deriv_epsilon' = 1e-3,
#'    'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,
#'    'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' strat_col <- 'e'
#' 
#' e <- RunCoxRegression_STRATA(df, time1, time2, event, names, term_n,
#'                              tform, keep_constant, a_n, modelform,
#'                              fir, der_iden, control,strat_col)
#'
RunCoxRegression_STRATA <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(), strat_col="null"){
    control <- Def_Control(control)
    control$maxiters <- c(1, control$maxiter)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n,
                                  tform, keep_constant, a_n, modelform, fir,
                                  der_iden, control,strat_col=strat_col,
                                  model_control=list("strata"=TRUE))
    return (e)
}


#' Calculates hazard ratios for a reference vector
#'
#' \code{RunCoxRegression} uses user provided data,  vectors specifying the model,
#' and options to calculate relative risk for every row in the provided data
#'
#' @inheritParams R_template
#' @family Plotting Wrapper Functions
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
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' fir <- 0
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' a_n <- c(1.1, 0.1, 0.2, 0.5) #used to test at a specific point
#' 
#' keep_constant <- c(0,0,0,0)
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,
#'    'epsilon' = 1e-3,
#'    'deriv_epsilon' = 1e-3, 'abs_max'=1.0,'change_all'=TRUE,
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' 
#' e <- Cox_Relative_Risk(df, time1, time2, event, names, term_n, tform,
#'      keep_constant, a_n, modelform, fir, control)
#'
Cox_Relative_Risk <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, control=list(), model_control=list()){
    df <- data.table(df)
    control <- Def_Control(control)
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
    #
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    #
    all_names <- unique(names)
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
    model_control$risk_subset <- TRUE
    e <- Plot_Omnibus_transition(term_n, tform, a_n, dfc, x_all, fir,
                                 0, modelform, control, matrix(c(0)),
                                 c(1), keep_constant, term_tot, c(0),
                                 c(0), model_control)
    return (e)
}

#' Performs basic Cox Proportional Hazards regression with the null model
#'
#' \code{RunCoxRegression} uses user provided data and time/event columns
#' to calculate the log-likelihood with constant hazard ratio
#'
#' @inheritParams R_template
#' @family Cox Wrapper Functions
#' @return returns a list of the final results
#' @export
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0))
#' # For the interval case
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' 
#' control <- list("ncores"=2,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' 
#' e <- RunCoxNull(df, time1, time2, event, control)
#'
RunCoxNull <- function(df, time1="start", time2="end", event0="event",control=list()){
    control <- Def_Control(control)
    control$maxiters <- c(1, control$maxiter)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0,  control=control,
                                  model_control=list("null"=TRUE))
    #
    return (e)

}


#' Performs Cox Proportional Hazard model plots
#'
#' \code{RunCoxPlots} uses user provided data, time/event columns,
#' vectors specifying the model, and options to choose and save plots
#'
#' @inheritParams R_template
#'
#' @return saves the plots in the current directory and returns a string
#' @export
#' @family Plotting Wrapper Functions
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
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' a_n <- c(-0.1, 0.5, 1.1, -0.3)
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = -1,'halfmax' = 5,
#'    'epsilon' = 1e-3,'deriv_epsilon' = 1e-3,
#'    'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,
#'    'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' #setting maxiter below 0 forces the function to calculate the score
#' # and return
#' plot_options <- list("type"=c("surv",paste(tempfile(),"run",sep="")), "studyid"="UserID",
#'                   'verbose'=FALSE)
#' 
#' RunCoxPlots(df, time1, time2, event, names, term_n, tform, keep_constant,
#'             a_n, modelform, fir, control, plot_options)
#'
RunCoxPlots <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, control=list(), plot_options=list(), model_control=list()){
#    if (system.file(package='ggplot2')==""){
#        message("Error: ggplot2 is not detected, required to run plotting functions")
#        return ("Passed")
#    }
    names(plot_options) <- tolower(names(plot_options))
    df <- data.table(df)
    control <- Def_Control(control)
    plot_options$verbose <- Check_Verbose(plot_options$verbose)
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    if (plot_options$verbose>=3){
        message("Note: Starting Plot Function")
    }
    if ("CONST" %in% names){
        if ("CONST" %in% names(df)){
            #fine
        } else {
            df$CONST <- 1
        }
    }
    data.table::setkeyv(df, c(time2, event0))
    base  <- NULL
    der_iden <- 0
    plot_type <- plot_options$type
    if (plot_options$verbose>=3){
        message("Note: Getting Plot Info")
    }
    dfend <- df[get(event0)==1, ]
    tu <- sort(unlist(unique(dfend[,time2, with = FALSE]), use.names=FALSE))
    if (length(tu)==0){
        message("Error: no events")
        stop()
    }
    if (plot_options$verbose>=3){
        message(paste("Note: ",length(tu)," risk groups",sep=""))
    }
    if ("type" %in% names(plot_options)){
        #fine
    } else {
        if (plot_options$verbose>=1){
            message("Error: Plot type not given")
        }
        stop()
    }
    if ("age_unit" %in% names(plot_options)){
        #fine
    } else {
        plot_options$age_unit <- "unitless"
    }
    if ("strat_haz" %in% names(plot_options)){
        if (plot_options$strat_haz){
            if ("strat_col" %in% names(plot_options)){
                if (plot_options$strat_col %in% names(df)){
                    #fine
                } else {
                    if (plot_options$verbose>=1){
                        message("Error: Stratification Column not in dataframe")
                    }
                    stop()
                }
            } else {
                if (plot_options$verbose>=1){
                    message("Error: Stratification Column not given")
                }
                stop()
            }
        }
    } else {
        plot_options$strat_haz <- FALSE
    }
    if ("martingale" %in% names(plot_options)){
        if (plot_options$martingale){
            if ("cov_cols" %in% names(plot_options)){
                for (cov_i in seq_along(plot_options$cov_cols)){
                    dose_col <- unlist(plot_options$cov_cols,
                                       use.names=FALSE)[cov_i]
                    if (dose_col%in% names(df)){
                        #fine
                    } else {
                        if (plot_options$verbose>=1){
                            message("Error: Covariate column "+
                                   dose_col+" is not in the dataframe")
                        }
                        stop()
                    }
                }
            } else {
                if (plot_options$verbose>=1){
                    message("Error: dose column not given")
                }
                stop()
            }
        }
    } else {
        plot_options$martingale <- FALSE
    }
    if ("km" %in% names(plot_options)){
        if (plot_options$km){
            if ("studyid" %in%  names(plot_options)){
                if (plot_options$studyid%in% names(df)){
                    #fine
                } else {
                    if (plot_options$verbose>=1){
                        message("Error: ID column is not in the dataframe")
                    }
                    stop()
                }
            } else {
                if (plot_options$verbose>=1){
                    message("Error: ID column not given")
                }
                stop()
            }
        }
    }
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    if (tolower(plot_type[1])=="surv"){
        if ("time_lims" %in% names(plot_options)){
            #fine
        } else {
            plot_options$time_lims <- c(min(tu),max(tu))
        }
    }
    for (iden_col in c("verbose","martingale","surv_curv","strat_haz","km")){
        if (iden_col %in% names(plot_options)){
            #fine
        } else {
            plot_options[iden_col] <- FALSE
        }
    }
    #
    plot_options$verbose <- Check_Verbose(plot_options$verbose)
    #
    control <- Def_Control(control)
    verbose <- data.table::copy(plot_options$verbose)
    maxiterc <- data.table::copy(control$maxiter)
    dfend <- df[get(event0)==1, ]
    tu <- sort(unlist(unique(dfend[,time2, with = FALSE]), use.names=FALSE))
    if (length(tu)==0){
        if (control$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    all_names <- unique(names)
    #
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    ce <- c(time1,time2,event0)
    #
    #
    t_check <- Check_Trunc(df,ce)
    df <- t_check$df
    ce <- t_check$ce
    time1 <- ce[1]
    time2 <- ce[2]
    #
    #
    #
    control$maxiters <- c(-1, -1)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n,
                                  tform, keep_constant, a_n, modelform, fir,
                                  der_iden, control, model_control)
    control$maxiter <- maxiterc
    b <- e$beta_0
    er <- e$Standard_Deviation
    #
    plot_table <- list()
    #
    if (tolower(plot_type[1])=="surv"){
        if (verbose>=3){
            message("Note: starting ph_plot")
        }
        #
        if (plot_options$strat_haz==FALSE){
        	if (verbose>=3){
		        message("Note: nonStratified survival curve calculation")
		    }
		    model_control$surv <- TRUE
		    e <- Plot_Omnibus_transition(term_n, tform, a_n, dfc, x_all, fir,
		                                 der_iden, modelform, control,
		                                 as.matrix(df[,ce, with = FALSE]),tu,
		                                 keep_constant, term_tot, c(0), c(0),
		                                 model_control)
		    #
		    t <- c()
		    h <- c()
		    ch <- c()
		    surv <- c()
		    if (verbose>=3){
		        message("Note: writing survival data")
		    }
		    dft <- data.table::data.table("time"=tu,"base"=e$baseline,
		                      "basehaz"=e$standard_error)
		    for (i in tu){
		        t <- c(t,i)
		        temp <- sum(dft[time<i, base])
		        ch <- c(ch, temp)
		        if (length(h)==0){
		            h <- c(temp)
		        } else {
		            h <- c(h, ch[length(ch)]-ch[length(ch)-1])
		        }
		        surv <- c(surv, exp(-1*temp))
		    }
		    #
		    age_unit <- plot_options$age_unit
		    if (plot_options$martingale==TRUE){
		        #
		        plot_table <- CoxMartingale(verbose, df, time1, time2, event0, e, t, ch,
		                      plot_options$cov_cols,
		                      plot_type[2], age_unit,plot_options$studyid)
		        #
		    }
		    if (plot_options$surv_curv==TRUE){
		        plot_table <- CoxSurvival(t,h,ch,surv,plot_type[2],verbose,
		                    plot_options$time_lims, age_unit)
            }
        } else {
        	age_unit <- plot_options$age_unit
        	if (verbose>=3){
		        message("Note: Stratified survival curve calculation")
		    }
            if (plot_options$surv_curv==TRUE){
                model_control$strata <- TRUE
                plot_table <- CoxStratifiedSurvival(verbose, df, event0, time1, time2,
                     all_names,term_n, tform, a_n, er, fir, der_iden,
                     modelform, control, keep_constant, plot_type,
                     plot_options$strat_col, plot_options$time_lims,age_unit)
            }
        }
        if (plot_options$km==TRUE){
            #
            plot_table <- CoxKaplanMeier(verbose, plot_options$studyid,
                           all_names,df,event0,time1, time2,tu,term_n,
                           tform, a_n, er, fir, der_iden, modelform,
                           control,keep_constant, plot_type,age_unit)
        }
    } else if (tolower(plot_type[1])=="risk"){
        plot_table <- CoxRisk(verbose, df, event0, time1, time2, names,term_n, tform,
                a_n, fir, der_iden, modelform, control,keep_constant,
                plot_type, b, er)
    } else if (tolower(plot_type[1])=="schoenfeld"){
        age_unit <- plot_options$age_unit
        plot_table <- PlotCox_Schoenfeld_Residual(df, time1, time2, event0, names, term_n,
                                    tform, keep_constant, a_n, modelform, fir,
                                    der_iden, control,age_unit,plot_type[2])
    }
    return (plot_table)
}

#' Performs basic cox regression, with multiple guesses, starts with
#' solving for a single term
#'
#' \code{RunCoxRegression_Tier_Guesses} uses user provided data, time/event
#' columns, vectors specifying the model, and options to control the
#' convergence and starting positions, with additional guesses
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Cox Wrapper Functions
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
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
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
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 5,'halfmax' = 5,
#'    'epsilon' = 1e-3,'deriv_epsilon' = 1e-3,
#'    'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,
#'    'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' guesses_control <- list("iterations"=10,"guesses"=10,"lin_min"=0.001,
#'    "lin_max"=1,"loglin_min"=-1,"loglin_max"=1, "lin_method"="uniform",
#'    "loglin_method"="uniform",strata=TRUE,term_initial = c(0,1))
#' strat_col <- 'e'
#' 
#' e <- RunCoxRegression_Tier_Guesses(df, time1, time2, event, names,
#'                                    term_n, tform, keep_constant,
#'                                    a_n, modelform, fir, der_iden,
#'                                    control,guesses_control,
#'                                    strat_col)
#'
#' @importFrom rlang .data
RunCoxRegression_Tier_Guesses <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(), guesses_control=list(),strat_col="null",model_control=list(),cens_weight="null"){
    df <- data.table(df)
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
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
    t_initial <- guesses_control$term_initial
    #
    rmin <- guesses_control$rmin
    rmax <- guesses_control$rmax
    if (length(rmin)!=length(rmax)){
        if (control$verbose>=2){
            message("Warning: rmin/rmax not equal size, lin/loglin min/max used")
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
    for (i in seq_along(a_n)){
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
    e <- RunCoxRegression_Guesses_CPP(df, time1, time2, event0, name_initial,
                                      term_n_initial, tform_initial,
                                      constant_initial, a_n_initial,
                                      modelform, fir, der_iden, control,
                                      guesses_control,strat_col,cens_weight=cens_weight,
                                      model_control=model_control)
    #
    if (guesses_control$verbose>=3){
        message("Note: INITIAL TERM COMPLETE")
        message(e)
    }
    #
    a_n_initial <- e$beta_0
    guess_constant <- c()
    j <- 1
    for (i in seq_along(a_n)){
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
    e <- RunCoxRegression_Guesses_CPP(df, time1, time2, event0, names,
         term_n, tform,keep_constant, a_n, modelform, fir, der_iden, control,
         guesses_control,strat_col,cens_weight=cens_weight,
         model_control=model_control)
    #
    return(e)
}


#' Performs basic Cox Proportional Hazards regression with competing risks
#'
#' \code{RunCoxRegression_CR} uses user provided data, time/event columns, vectors specifying the model, and options to control the convergence, starting positions, and censoring adjustment
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Cox Wrapper Functions
#' @examples
#' library(data.table)
#' ## basic example code reproduced from the starting-description vignette
#' 
#' df <- data.table::data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
#'            "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
#'              "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
#'           "Cancer_Status"=c(0,   0,   1,   2,   1,   2,   0),
#'                       "a"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
#'                       "c"=c(10,  11,  10,  11,  12,  9,   11),
#'                       "d"=c(0,   0,   0,   1,   1,   1,   1))
#' # For the interval case
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
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
#'    'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
#' #weights the probability that a row would continue to extend without censoring,
#' #    for risk group calculation 
#' df$cens_weight <- c(0.83, 0.37, 0.26, 0.34, 0.55, 0.23, 0.27)
#' #censoring weight is generated by the survival library finegray function, or by hand.
#' #The ratio of weight at event end point to weight at row endpoint is used.
#' e <- RunCoxRegression_CR(df, time1, time2, event, names, term_n, tform,
#'      keep_constant, a_n, modelform, fir, der_iden, control, 'cens_weight')
#'
#' @importFrom rlang .data
RunCoxRegression_CR <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(), cens_weight="null"){
    control <- Def_Control(control)
    control$maxiters <- c(1, control$maxiter)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n, tform, keep_constant,
                                  a_n, modelform, fir, der_iden, control,cens_weight=cens_weight,
                                  model_control=list("cr"=TRUE))
    #
    return (e)
}

#' Performs basic Cox Proportional Hazards regression, Generates multiple starting guesses on c++ side
#'
#' \code{RunCoxRegression_Guesses_CPP} uses user provided data, time/event columns, vectors specifying the model, and options to control the convergence and starting positions. Has additional options to starting with several initial guesses
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results
#' @export
#' @family Cox Wrapper Functions
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
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
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
#' e <- RunCoxRegression_Guesses_CPP(df, time1, time2, event, names, term_n,
#'                               tform, keep_constant, a_n, modelform, fir,
#'                               der_iden, control,guesses_control,strat_col)
#' @importFrom rlang .data
RunCoxRegression_Guesses_CPP <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, control=list(), guesses_control=list(),strat_col="null",model_control=list(),cens_weight="null"){
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
        if (control$verbose>=1){
            message("Error: Atleast one parameter must be free")
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
    a_n_default <- rep(0,length(a_n[[1]]))
    for (i in seq_along(a_n[[1]])){
        a_n_default[i] <- a_n[[1]][i]
    }
    if (guesses_control$strata==FALSE){
        data.table::setkeyv(df, c(time2, event0))
        dfend <- df[get(event0)==1, ]
        tu <- sort(unlist(unique(dfend[,time2, with = FALSE]),use.names=FALSE))
        if (length(tu)==0){
            if (guesses_control$verbose>=1){
                message("Error: no events")
            }
            stop()
        }
        if (guesses_control$verbose>=3){
            message(paste("Note: ",length(tu)," risk groups",sep=""))
        }
        all_names <- unique(names)
        #
        df <- Replace_Missing(df,all_names,0.0,control$verbose)
        #
        dfc <- match(names,all_names)

        term_tot <- max(term_n)+1
        x_all <- as.matrix(df[,all_names, with = FALSE])
        ce <- c(time1,time2,event0)
        #
    } else {
        #
        dfend <- df[get(event0)==1, ]
        uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]), use.names=FALSE))
        #
        for (i in seq_along(uniq)){
            df0 <- dfend[get(strat_col)==uniq[i],]
            tu0 <- unlist(unique(df0[,time2,with=FALSE]), use.names=FALSE)
            if (length(tu0)==0){
                if (control$verbose>=2){
                    message(paste("Warning: no events for strata group:",uniq[i],sep=" "))
                }
                df <- df[get(strat_col)!=uniq[i],]
            }
        }
        uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]), use.names=FALSE))
        if (control$verbose>=3){
            message(paste("Note:",length(uniq)," strata used",sep=" "))
        }
        #
        data.table::setkeyv(df, c(time2, event0, strat_col))
        dfend <- df[get(event0)==1, ]
        tu <- sort(unlist(unique(dfend[,time2, with = FALSE]),use.names=FALSE))
        if (length(tu)==0){
            if (guesses_control$verbose>=1){
                message("Error: no events")
            }
            stop()
        }
        if (guesses_control$verbose>=3){
            message(paste("Note: ",length(tu)," risk groups",sep=""))
        }
        all_names <- unique(names)
        #
        df <- Replace_Missing(df,all_names,0.0,control$verbose)
        #
        dfc <- match(names,all_names)

        term_tot <- max(term_n)+1
        x_all <- as.matrix(df[,all_names, with = FALSE])
        ce <- c(time1,time2,event0,strat_col)
        #
    }
    t_check <- Check_Trunc(df,ce)
    df <- t_check$df
    ce <- t_check$ce
    #
    dat_val <- Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant,
                                  a_n, x_all, a_n_default, modelform, fir, control,
                                  guesses_control, model_control)
    a_ns <- dat_val$a_ns
    maxiters <- dat_val$maxiters
    #
    control$maxiters <- c(maxiters,control$maxiter)
    control$guesses <- length(maxiters)
    #fine
    a_n_mat <- matrix(a_ns,nrow=length(control$maxiters)-1,byrow=TRUE)
    a_n <- lapply(seq_len(nrow(a_n_mat)), function(i) a_n_mat[i,])
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n, tform, keep_constant,
                                  a_n, modelform, fir, der_iden, control, strat_col=strat_col,
                                  model_control=model_control,cens_weight=cens_weight)
    return (e)
}

#' Performs Cox Proportional Hazards regression using the omnibus function with multiple column realizations
#'
#' \code{RunCoxRegression_Omnibus_Multidose} uses user provided data, time/event columns,
#'       vectors specifying the model, and options to control the convergence
#'       and starting positions. Used for 2DMC column uncertainty methods.
#'       Returns optimized parameters, log-likelihood, and standard deviation for each realization. 
#'       Has additional options for using stratification,
#'       multiplicative loglinear 1-term,
#'       competing risks, and calculation without derivatives
#'
#' @inheritParams R_template
#'
#' @return returns a list of the final results for each realization
#' @export
#' @family Cox Wrapper Functions
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
#'                       "e"=c(0,   0,   1,   0,   0,   0,   1),
#'                       "a0"=c(0,   1,   1,   0,   1,   0,   1),
#'                       "a1"=c(0,   1,   1,   0,   1,   0,   1))
#' # For the interval case
#' time1 <- "Starting_Age"
#' time2 <- "Ending_Age"
#' event <- "Cancer_Status"
#' names <- c('a','b','c','d')
#' a_n <- list(c(1.1, -0.1, 0.2, 0.5),c(1.6, -0.12, 0.3, 0.4))
#' #used to test at a specific point
#' term_n <- c(0,1,1,2)
#' tform <- c("loglin","lin","lin","plin")
#' modelform <- "M"
#' fir <- 0
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' realization_columns = matrix(c("a0","a1"),nrow=1)
#' realization_index=c("a")
#' 
#' control <- list("ncores"=2,'lr' = 0.75,'maxiters' = c(5,5,5),
#'    'halfmax' = 5,'epsilon' = 1e-3, 'deriv_epsilon' = 1e-3,
#'    'abs_max'=1.0,'change_all'=TRUE, 'dose_abs_max'=100.0,'verbose'=FALSE,
#'    'ties'='breslow','double_step'=1, "guesses"=2)
#' 
#' #e <- RunCoxRegression_Omnibus_Multidose(df, time1, time2, event,
#' #                              names, term_n, tform, keep_constant,
#' #                              a_n, modelform, fir, der_iden,
#' #                              realization_columns, realization_index,control,
#' #                              model_control=list("single"=FALSE,
#' #                              "basic"=FALSE, "cr"=FALSE, 'null'=FALSE))
#' @importFrom rlang .data
RunCoxRegression_Omnibus_Multidose <- function(df, time1="start", time2="end", event0="event", names=c("CONST"), term_n=c(0), tform="loglin", keep_constant=c(0), a_n=c(0), modelform="M", fir=0, der_iden=0, realization_columns = matrix(c("temp00","temp01","temp10","temp11"),nrow=2), realization_index=c("temp0","temp1"), control=list(),strat_col="null", cens_weight="null", model_control=list(),cons_mat=as.matrix(c(0)),cons_vec=c(0)){
    df <- data.table(df)
    ##
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
        if (control$verbose>=1){
            message("Error: Atleast one parameter must be free")
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
    if (model_control$cr==TRUE){
        if (cens_weight %in% names(df)){
            # good
        } else if (length(cens_weight)<nrow(df)){
            if (control$verbose>=1){
                message("Error: censoring weight column not in the dataframe.")
            }
            stop()
        }
    } else {
        df[[cens_weight]] <- 1
    }
    if (model_control$strata==FALSE){
        data.table::setkeyv(df, c(time2, event0))
        uniq <- c(0)
        ce <- c(time1,time2,event0)
    } else {
        #
        dfend <- df[get(event0)==1, ]
        uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]),
                            use.names=FALSE))
        #
        for (i in seq_along(uniq)){
            df0 <- dfend[get(strat_col)==uniq[i],]
            tu0 <- unlist(unique(df0[,time2,with=FALSE]), use.names=FALSE)
            if (length(tu0)==0){
                if (control$verbose>=2){
                    message(paste("Warning: no events for strata group:",
                                 uniq[i],sep=" "))
                }
                df <- df[get(strat_col)!=uniq[i],]
            }
        }
        uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]),
                            use.names=FALSE))
        if (control$verbose>=3){
            message(paste("Note:",length(uniq)," strata used",sep=" "))
        }
        #
        data.table::setkeyv(df, c(time2, event0, strat_col))
        ce <- c(time1,time2,event0,strat_col)
    }
    dfend <- df[get(event0)==1, ]
    tu <- sort(unlist(unique(dfend[,time2, with = FALSE]),use.names=FALSE))
    if (length(tu)==0){
        if (control$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    if (control$verbose>=3){
        message(paste("Note: ",length(tu)," risk groups",sep=""))
    }
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    # replace_missing equivalent for the realization columns
    #
    if (length(realization_index)==length(realization_columns[,1])){
        #pass
    } else {
        #the number of columns per realization does not match the number of indexes provided
        if (control$verbose>=1){
            message(paste("Error:",length(realization_index)," column indexes provided, but ",
                          length(realization_columns[,1])," rows of realizations columns provided",sep=" "))
        }
        stop()
    }
    if (all(realization_index %in% all_names)){
        #pass
    } else {
        if (control$verbose>=1){
            message(paste("Error: Atleast one realization column provided was not used in the model",sep=" "))
        }
        stop()
    }
    all_names <- unique(c(all_names, as.vector(realization_columns)))
    if (all(all_names %in% names(df))){
        #pass
    } else {
        if (control$verbose>=1){
            message(paste("Error: Atleast one realization column provided was not in the data.table",sep=" "))
        }
        stop()
    }
    #
    dfc <- match(names,all_names)
    dose_cols <- matrix(match(realization_columns, all_names), nrow=nrow(realization_columns))
    dose_index <- match(realization_index, all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
    #
    t_check <- Check_Trunc(df,ce)
    df <- t_check$df
    ce <- t_check$ce
    #
    #
    e <- cox_ph_multidose_Omnibus_transition(term_n, tform, a_n,
            as.matrix(dose_cols, with=FALSE), dose_index,dfc,x_all,
            fir, der_iden, modelform, control,
            as.matrix(df[,ce, with = FALSE]),tu,
            keep_constant,term_tot, uniq, df[[cens_weight]], model_control,
            cons_mat, cons_vec)
    if ("Status" %in% names(e)){
        if (e$Status=="FAILED"){
	        if (control$verbose>=1){message("Error: Invalid model")}
	        stop()
        }
    }
    e$Parameter_Lists$names <- names
    return (e)
}
