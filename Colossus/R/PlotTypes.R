#' calculates and plots martingale residuals with a named dose column
#'
#' \code{CoxMartingale} uses user provided data, columns, and identifier to create plots
#'
#' @inheritParams R_template
#' @family Plotting Functions
#' @return saves the plots in the current directory and returns a list of data-tables plotted
#' @noRd
CoxMartingale <- function(verbose, df, time1, time2, event0,e, t, ch, dnames, plot_name, age_unit, studyID){
    IDS <- base <- res <- doses <- NULL
    if (verbose>=3){
        message("Note: Plotting Martingale Residuals")
    }
    #
    time_s <- df[,get(time1)]
    time_e <- df[,get(time2)]
    ch_fun <- approxfun(x=t,y=ch,rule=2)
    ch_e <- ch_fun(time_e)
    ch_s <- ch_fun(time_s)
    #
    e_i <- df[,get(event0)]
    #
    table_out <- list()
    #
    for (cov_i in seq_len(length(dnames))){
        dname <- dnames[cov_i]
        if (verbose>=3){
            message(paste("Note: Martingale Plot: ",dname,sep=""))
        }
        if (studyID %in% names(df)){
            dfr <- data.table::data.table("Risks"=e$Risks,"ch_e"=ch_e,"ch_s"=ch_s,"e"=e_i,
                "IDS"=unlist(df[,studyID,with=FALSE],use.names=FALSE),
                "time"=time_e,"cov"=unlist(df[, dname, with = FALSE],use.names=FALSE))
            #
            name_temp <- names(dfr)
            for (i in seq_len(length(name_temp))){
                if (grepl( "cov", name_temp[i], fixed = TRUE)){
                    data.table::setnames(dfr,name_temp[i],c("cov"))
                } else if (grepl( "IDS", name_temp[i], fixed = TRUE)){
                    data.table::setnames(dfr,name_temp[i],c("IDS"))
                }
            }
            #
            dfr$res <- dfr$e - (dfr$ch_e-dfr$ch_s) * dfr$Risks
            #
            #
            Martingale_Error <- dfr[, lapply(.SD,sum), by=IDS]
            times <- dfr[, lapply(.SD,max), by=IDS]
        } else {
            dfr <- data.table::data.table("Risks"=e$Risks,"ch_e"=ch_e,"ch_s"=ch_s,"e"=e_i,"time"=time_e,
                "cov"=unlist(df[, dname, with = FALSE],use.names=FALSE))
            #
            name_temp <- names(dfr)
            for (i in seq_len(length(name_temp))){
                if (grepl( "cov", name_temp[i], fixed = TRUE)){
                    data.table::setnames(dfr,name_temp[i],c("cov"))
                } else if (grepl( "IDS", name_temp[i], fixed = TRUE)){
                    data.table::setnames(dfr,name_temp[i],c("IDS"))
                }
            }
            #
            dfr$res <- dfr$e - (dfr$ch_e) * dfr$Risks
            #
            Martingale_Error <- dfr
            times <- dfr
        }
        #
        ##
        dft <- data.table::data.table("cov_max"=times$cov,"time_max"=times$time,"res_sum"=Martingale_Error$res, "event"=Martingale_Error$e)
        table_out[[dname]] <- dft
        ##
        if (system.file(package='ggplot2')!=""){
            g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$cov_max, y=.data$res_sum)) +
                 ggplot2::geom_point(color="black") +
                 ggplot2::labs(x=paste("Max",dname,sep=" "), y="Martingale Residuals")
            ggplot2::ggsave(paste(plot_name,"_",dname,'_martin_plot.jpeg',sep=""),
                            device="jpeg",dpi="retina")
        }
        ##
    }
    #
    if (studyID %in% names(df)){
        dfr <- data.table::data.table("Risks"=e$Risks,"ch_e"=ch_e,"ch_s"=ch_s,"e"=e_i,
                       "IDS"=unlist(df[,studyID,with=FALSE],use.names=FALSE),"time"=time_e)
        #
        #
        name_temp <- names(dfr)
        for (i in seq_len(length(name_temp))){
            if (grepl( "IDS", name_temp[i], fixed = TRUE)){
                data.table::setnames(dfr,name_temp[i],c("IDS"))
            }
        }
        #
        dfr$res <- dfr$e - (dfr$ch_e-dfr$ch_s) * dfr$Risks
        #
        Martingale_Error <- dfr[, lapply(.SD,sum), by=IDS]
        times <- dfr[, lapply(.SD,max), by=IDS]
    } else {
        dfr <- data.table::data.table("Risks"=e$Risks,"ch_e"=ch_e,"ch_s"=ch_s,"e"=e_i,"time"=time_e)
        #
        name_temp <- names(dfr)
        for (i in seq_len(length(name_temp))){
            if (grepl( "IDS", name_temp[i], fixed = TRUE)){
                data.table::setnames(dfr,name_temp[i],c("IDS"))
            }
        }
        #
        dfr$res <- dfr$e - (dfr$ch_e) * dfr$Risks
        #
    }
    dft <- data.table::data.table("time_max"=dfr$time,"res_sum"=dfr$res, "event"=dfr$e)
#    print(dft)
    table_out[['survival_time']] <- dft
    #
    if (system.file(package='ggplot2')!=""){
        g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$time_max, y=.data$res_sum)) +
            ggplot2::geom_point(color="black") +
            ggplot2::labs(x=paste("Max Age",sep=" "), y="Martingale Residuals")
        ggplot2::ggsave(paste(plot_name,'_martin_plot.jpeg',sep=''),device="jpeg",dpi="retina")
    }
    ##
    return (table_out)
}

#' calculates and plots survival plots of the estimated baseline
#'
#' \code{CoxSurvival} uses user provided data, columns, and identifier to create plots
#'
#' @inheritParams R_template
#' @family Plotting Functions
#' @noRd
#' @return saves the plots in the current directory and returns a list of data-tables plotted
CoxSurvival <- function(t,h,ch,surv,plot_name,verbose,time_lims, age_unit){
    # verbose <- as.logical(verbose)
    if (verbose>=3){
        message("Note: Plotting Survival Curves")
    }
    #
    table_out <- list()
    #
    dft <- data.table::data.table("t"=t,"h"=h,"ch"=ch,"surv"=surv)
    data.table::setkeyv(dft,"t")
    dft <- dft[(t>=time_lims[1])&(t<=time_lims[2]),]
    #
    table_out[['standard']] <- dft
    #
    if (system.file(package='ggplot2')!=""){
        g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$t, y=.data$ch)) +
            ggplot2::geom_point(color="black") +
            ggplot2::labs(x=paste("age (",age_unit,")",sep=""), y="Cumulative Hazard")
        ggplot2::ggsave(paste(plot_name,"_ch_plot.jpeg",sep=""),device="jpeg",dpi="retina")
        #
        g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$t, y=.data$surv)) +
            ggplot2::geom_point(color="black")  +
            ggplot2::labs(x=paste("age (",age_unit,")",sep=""), y="Survival")
        ggplot2::ggsave(paste(plot_name,"_surv_plot.jpeg",sep=""),device="jpeg",dpi="retina")
        #
        g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$t, y=.data$h)) +
            ggplot2::geom_point(color="black")
            ggplot2::labs(x=paste("age (",age_unit,")",sep=""), y="Hazard Estimate")
        ggplot2::ggsave(paste(plot_name,"_H_plot.jpeg",sep=""),device="jpeg",dpi="retina")
    }
    #
    Ls <- log(surv)
    Lls_u <- log(-Ls)
    Lt_u <- log(t)
    ##
    #
    #
    dft <- data.table::data.table("t"=Lt_u,"s"=Lls_u)
    table_out[['log']] <- dft
    #
    if (system.file(package='ggplot2')!=""){
        g <- ggplot2::ggplot(data=dft,ggplot2::aes(x=.data$t, y=.data$s)) +
             ggplot2::geom_line() + ggplot2::labs(x="Log-Age", y="Log of Log Survival")
        ggplot2::ggsave(paste(plot_name,"_log_log_surv_plot.jpeg",sep=""),
                        device="jpeg",dpi="retina")
    }
    return (table_out)
}

#' calculates and plots Kaplan-Meier survival plots
#'
#' \code{CoxKaplanMeier} uses user provided data, columns, and identifier to create plots, plots the kaplan-meier survival and log(time) vs log(-log(survival))
#'
#' @inheritParams R_template
#' @family Plotting Functions
#' @noRd
#' @return saves the plots in the current directory and returns a list of data-tables plotted
CoxKaplanMeier <- function(verbose, studyID,names,df,event0,time1,time2,tu,term_n, tform, a_n, er, fir, der_iden, modelform, control,keep_constant, plot_type, age_unit, model_control=list()){
    # verbose <- as.logical(verbose)
    if (verbose>=3){
        message("Note: Plotting Kaplan-Meier Curve")
    }
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    base  <- NULL
    ce <- c(time1,time2,event0)
    all_names <- unique(names)
    dfc <- match(names,all_names)
    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
    df_study <- df[, lapply(.SD,max), by=studyID]
    #
    dfend <- df_study[get(event0)==1, ]
    t_num <- length(unlist(unique(df_study[,studyID, with = FALSE]),use.names=FALSE))
    #number of unique individuals in total set
    #
    t_t <- c(0)
    n_t <- c(1)
    tu <- sort(unlist(unique(dfend[,time2, with = FALSE]), use.names=FALSE)) #all event times
    #
    #
    tu_s <- c(0.0,tu)
    for (i_t in seq_len(length(tu))){
        i <- tu[i_t]
        #
        df0 <- dfend[get(time2)<=i,] #set of all intervals prior to this point in lower setv
        df0 <- df0[(get(time2)>tu_s[i_t]),]
        df1 <- df_study[(get(time2)>tu_s[i_t]),]
        t_ev <- sum(df0[, get(event0)])
        #number of intervals with event in lower set prior to the time point
        t_num <- nrow(df1)
        #
        if (t_ev>0){
            temp <- (t_num - t_ev)/t_num
            t_t <- c(t_t,i)
            n_t <- c(n_t, n_t[length(n_t)]*temp)
        }
    }
    #
    table_out <- list()
    #
    dft <- data.table::data.table("t_t"=t_t,"n_t"=n_t)
    table_out[['kaplin-meier']] <- dft
    #
    if (system.file(package='ggplot2')!=""){
        g <- ggplot2::ggplot(data=dft,ggplot2::aes(x=.data$t_t, y=.data$n_t)) + ggplot2::geom_line() +
            ggplot2::labs(x=paste("age (",age_unit,")",sep=""), y="Survival")
        ggplot2::ggsave(paste(plot_type[2],"_KM.jpeg",sep=""),device="jpeg",dpi="retina")
    }
    #
    return (table_out)
}


#' calculates and plots relative risk
#'
#' \code{CoxRisk} uses user provided data, columns, and identifier to create plots of risk by covariate value for each column
#'
#' @inheritParams R_template
#' @family Plotting Functions
#' @noRd
#' @return saves the plots in the current directory and returns a list of data-tables plotted
CoxRisk <- function(verbose,df, event0, time1, time2, names,term_n, tform, a_n, fir, der_iden, modelform, control,keep_constant, plot_type, b, er, model_control=list()){
    # verbose <- as.logical(verbose)
    fir_KM <- 0
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    dfend <- df[get(event0)==1, ]
    #
    ce <- c(time1,time2,event0)
    all_names <- unique(names)
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
    tu <- unlist(unique(dfend[,time2, with = FALSE]), use.names=FALSE)
    #
    table_out <- list()
    #
    for (fir_KM in seq_len(length(names))){
        lfir <- c(names[fir_KM])
        uniq <- unlist(unique(df[,lfir, with = FALSE]), use.names=FALSE)
        #
        der_iden <- fir_KM-1
        model_control$risk <- TRUE
        model_control$unique_values <- length(uniq)
        e <- Plot_Omnibus_transition(term_n, tform, a_n, dfc, x_all, fir, der_iden, modelform,
                                     control, as.matrix(df[,ce, with = FALSE]),tu,
                                     keep_constant, term_tot, c(0), c(0), model_control)
        if ("Failure" %in% names(e)){
            message("Error: ")
            message(e)
            stop()
        }
        x <- e$x
        y <- e$y
        #
        dft <- data.table::data.table("x"=x,"y"=y)
        table_out[[names[fir_KM]]] <- dft
        if (system.file(package='ggplot2')!=""){
            if (length(uniq)>=10){
                #
                g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) +
                     ggplot2::geom_line(color="black") +
                     ggplot2::labs(x=names[fir_KM], y="Relative Risk")
                ggplot2::ggsave(paste(plot_type[2],"_risk_plot_",fir_KM,".jpeg",sep=""),
                                device="jpeg",dpi="retina")
                #
            } else {
                g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) +
                     ggplot2::geom_point(color="black") +
                     ggplot2::labs(x=names[fir_KM], y="Relative Risk")
                ggplot2::ggsave(paste(plot_type[2],"_risk_plot_",fir_KM,".jpeg",sep=""),
                                device="jpeg",dpi="retina")
                #
            }
        }
        #
    }
    return (table_out)
}


#' calculates and plots survival curves for each unique value of the stratification column
#'
#' \code{CoxStratifiedSurvival} uses user provided data, columns, and identifier to calculate the survival fraction for each strata
#'
#' @inheritParams R_template
#' @family Plotting Functions
#' @noRd
#' @return saves the plots in the current directory and returns a list of data-tables plotted
CoxStratifiedSurvival <- function(verbose, df, event0, time1, time2, names,term_n, tform, a_n, er, fir, der_iden, modelform, control,keep_constant, plot_type, strat_col,time_lims, age_unit, model_control=list()){
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
    ce <- c(time1,time2,event0,strat_col)
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    base  <- NULL
    #
    ce <- c(time1,time2,event0,strat_col)
    all_names <- unique(names)
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    #
    tu <- unlist(unique(dfend[,time2, with = FALSE]), use.names=FALSE)
    uniq <- sort(unlist(unique(df[,strat_col, with = FALSE]), use.names=FALSE))
    #
    control$maxiters <- c(-1, -1)
    control$guesses <- 1
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n, tform, keep_constant,
                                  a_n, modelform, fir, der_iden, control,strat_col=strat_col,
                                  model_control=list("strata"=TRUE))
    a_n <- e$beta_0
    plot_name <- plot_type[2]
    tt <- c()
    tsurv <- c()
    categ <- c()
    
    if (verbose>=3){
        message(paste("Note: Starting Stratification: Calculation"))
    }
    model_control$surv <- TRUE
    model_control$strata <- TRUE
    e <- Plot_Omnibus_transition(term_n, tform, a_n, dfc, x_all, fir, der_iden,
                                 modelform, control, as.matrix(df[,ce, with = FALSE]),
                                 tu, keep_constant, term_tot, uniq, c(0), model_control)
	for (col_i in seq_along(uniq)){
        if (verbose>=3){
            message(paste("Note: Starting Stratification calculation ",col_i))
        }
        col_u <- uniq[col_i]
#        #
        t <- c()
        h <- c()
        ch <- c()
        surv <- c()
        dft<- data.table::data.table("time"=tu,"base"=e$baseline[,col_i])
        for (i in tu){
            if ((i<=time_lims[2])&&(i>=time_lims[1])){
                t <- c(t,i)
                temp <- sum(dft[time<i, base])
                ch <- c(ch, temp)
                if (length(h)==0){
                    h <- c(temp)
                } else {
                    h <- c(h, ch[length(ch)]-ch[length(ch)])
                }
                surv <- c(surv, exp(-1*temp))
            }
        }
        tt <- c(tt, t)
        tsurv <- c(tsurv, surv)
        categ <- c(categ, rep(paste(col_u),length(t)))
    }
    dft<- data.table::data.table("t"=tt,"surv"=tsurv,"cat_group"=categ)
    sbreaks <- c()
    slabels <- c()
    for (i in seq_len(length(uniq))){
        sbreaks <- c(sbreaks, paste(uniq[i]))
        slabels <- c(slabels, paste("For ",strat_col,"=",uniq[i],sep=""))
    }
    #
    table_out <- list()
    #
    table_out[['stratified_survival']] <- dft
    if (system.file(package='ggplot2')!=""){
        g <- ggplot2::ggplot() + ggplot2::geom_point(data=dft,
             ggplot2::aes(x=.data$t, y=.data$surv,group=.data$cat_group,color=.data$cat_group))
        g <- g + ggplot2::scale_colour_discrete(breaks=sbreaks, labels=slabels)
        g <- g + ggplot2::labs(x=paste("age (",age_unit,")",sep=""), y="Survival") + ggplot2::ylim(0,1)
        ggplot2::ggsave(paste(plot_name,'_strat_surv_plot_',strat_col,'.jpeg',sep=""),
                        device="jpeg",dpi="retina")
    }
    return (table_out)
}
        
        
#' Calculates Schoenfeld residuals for a Cox Proportional Hazards regression and plots
#'
#' \code{RunCox_Schoenfeld_Residual} uses user provided data, time/event columns, vectors specifying the model, and options to calculate the residuals
#'
#' @inheritParams R_template
#'
#' @return saves the plots in the current directory and returns a list of data-tables plotted
#' @family Plotting Functions
#' @noRd
#' @importFrom rlang .data

PlotCox_Schoenfeld_Residual <- function(df, time1, time2, event0, names, term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control,age_unit,plot_name, model_control=list()){        
    data.table::setkeyv(df, c(time2, event0))
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
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
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names, der_iden)
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
    ce <- c(time1,time2,event0)
    #
    t_check <- Check_Trunc(df,ce)
    df <- t_check$df
    ce <- t_check$ce
    #
    control <- Def_Control(control)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    model_control$schoenfeld <- TRUE
    res_list <- Plot_Omnibus_transition(term_n, tform, a_n, dfc, x_all, fir, der_iden,
                                        modelform, control, as.matrix(df[,ce, with = FALSE]),
                                        tu, keep_constant, term_tot, c(0), c(0), model_control)
    res        <- res_list$residual
    res_scaled <- res_list$scaled
    #
    table_out <- list()
    #
    for (cov in seq_len(length(a_n))){
        if (keep_constant[cov]==0){
            cov_res <- cov - sum(head(keep_constant,cov))
            y <- unlist(res[,cov_res],use.names=FALSE)
            y_scale <- unlist(res_scaled[,cov_res],use.names=FALSE)
            #
            dft <- data.table::data.table("time"=tu,"y"=y)
            dft$y_scale <- y_scale
            table_out[[names[cov]]] <- dft
            #
            if (system.file(package='ggplot2')!=""){
                g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$time, y=.data$y)) +
                    ggplot2::geom_point(color="black") +
                    ggplot2::labs(x=paste("age (",age_unit,")",sep=""),
                    y=paste("Schoenfeld Residual (",names[cov], tform[cov],")",sep=" "))
                ggplot2::ggsave(paste(plot_name,"_schoenfeld_",cov_res,".jpeg",sep=""),
                                device="jpeg",dpi="retina")
                #
                g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$time, y=.data$y_scale)) +
                    ggplot2::geom_point(color="black") +
                    ggplot2::labs(x=paste("age (",age_unit,")",sep=""),
                    y=paste("Schoenfeld Residual Scaled (",names[cov], tform[cov],")",sep=" "))
                ggplot2::ggsave(paste(plot_name,"_schoenfeld_scaled_",cov_res,".jpeg",sep=""),
                                device="jpeg",dpi="retina")
            }
            #
        }
    }
    return (table_out)
}
          
        
#' Calculates and returns data for time by hazard and survival to estimate censoring rate
#'
#' \code{GetCensWeight} uses user provided data, time/event columns, vectors specifying the model, and options generate an estimate of the censoring rate, plots, and returns the data
#'
#' @inheritParams R_template
#' @family Plotting Functions
#' @return saves the plots in the current directory and returns a data.table of time and corresponding hazard, cumulative hazard, and survival
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
#' a_n <- c(0.1, 0.1, 0.1, 0.1)
#' 
#' keep_constant <- c(0,0,0,0)
#' der_iden <- 0
#' df$censor <- (df$Cancer_Status==0)
#' event <- "censor"
#' control <- list("ncores"=2,'lr' = 0.75,'maxiter' = 20,'halfmax' = 5,
#'    'epsilon' = 1e-6,'deriv_epsilon' = 1e-6,
#'    'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,'verbose'=FALSE,
#'    'ties'='breslow','double_step'=1)
#' plot_options <- list("name"=paste(tempfile(),"run_06",sep=""),"verbose"=FALSE,
#'                      "studyID"="studyID","age_unit"="years")
#' dft <- GetCensWeight(df, time1, time2, event, names, term_n, tform,
#'                      keep_constant, a_n, modelform, fir, control, plot_options)
#' t_ref <- dft$t
#' surv_ref <- dft$surv
#' t_c <- df$t1
#' cens_weight <- approx(t_ref, surv_ref, t_c,rule=2)$y
#'
GetCensWeight <- function(df, time1, time2, event0, names, term_n, tform, keep_constant, a_n, modelform, fir, control, plot_options, model_control=list(),strat_col="e"){
    df <- data.table(df)
    ##
    ##
    if (plot_options$verbose>=3){
        message("Note: Starting Censoring weight Plot Function")
    }
    if (min(keep_constant)>0){
        message("Error: Atleast one parameter must be free")
        stop()
    }
    model_control <- Def_model_control(model_control)
    val <- Def_modelform_fix(control,model_control,modelform,term_n)
    modelform <- val$modelform
    model_control <- val$model_control
    data.table::setkeyv(df, c(time2, event0))
    base  <- NULL
    plot_name <- plot_options$name
    dfend <- df[get(event0)==1, ]
    tu <- sort(unlist(unique(dfend[,time2, with = FALSE]), use.names=FALSE))
    if (length(tu)==0){
        if (plot_options$verbose>=1){
            message("Error: no events")
        }
        stop()
    }
    if (plot_options$verbose>=3){
        message(paste("Note: ",length(tu)," risk groups",sep=""))
    }
    #
    if ("age_unit" %in% names(plot_options)){
        #fine
    } else {
        plot_options$age_unit <- "unitless"
    }
    for (iden_col in c("verbose")){
        if (iden_col %in% names(plot_options)){
            #fine
        } else {
            plot_options[iden_col] <- 0
        }
    }
    #
    control <- Def_Control(control)
    verbose <- data.table::copy(plot_options$verbose)
    maxiterc <- data.table::copy(control$maxiter)
    all_names <- unique(names)
    #
    df <- Replace_Missing(df,all_names,0.0,control$verbose)
    #
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    ce <- c(time1,time2,event0)
    #
    t_check <- Check_Trunc(df,ce)
    df <- t_check$df
    ce <- t_check$ce
    time1 <- ce[1]
    time2 <- ce[2]
    control$maxiter <- -1
    #
    e <- RunCoxRegression_Omnibus(df, time1, time2, event0, names, term_n, tform, keep_constant,
                                  a_n, modelform, fir, 0, control,strat_col=strat_col,
                                  model_control=model_control)
    control$maxiter <- maxiterc
    model_control$surv <- TRUE
    e <- Plot_Omnibus_transition(term_n, tform, a_n, dfc, x_all, fir, 0, modelform, control,
                                 as.matrix(df[,ce, with = FALSE]), tu, keep_constant, term_tot,
                                 c(0), c(0), model_control)
    #
    t <- c()
    h <- c()
    ch <- c()
    surv <- c()
    dft<- data.table::data.table("time"=tu,"base"=e$baseline,"basehaz"=e$standard_error)
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
    age_unit <- plot_options$age_unit
    #
    dft <- data.table::data.table("t"=t,"h"=h,"ch"=ch,"surv"=surv)
    data.table::setkeyv(dft,"t")
    #
    if (system.file(package='ggplot2')!=""){
        g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$t, y=.data$surv)) +
            ggplot2::geom_point(color="black") +
            ggplot2::labs(x=paste("age (",age_unit,")",sep=""), y="Survival")
        ggplot2::ggsave(paste(plot_name,"_weight_surv_plot.jpeg",sep=""),device="jpeg",dpi="retina")
    }
    #
    return (dft)
}
        
        
        
