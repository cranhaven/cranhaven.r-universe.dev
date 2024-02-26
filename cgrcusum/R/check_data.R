#
#
#
# check_data <- function(data, coxphmod, cbaseh, ctimes, h, stoptime,
#                        C, theta){
#   #--------------General data checks and transformations------------------
#   #-----Applicable to cgrcusum, bercusum, bkcusum and funnelplot----------
#
#   # check data.frame and column names
#   if(!is.data.frame(data)){
#     warning("Data provided is not a data frame, attempting to convert.",
#             immediate. = TRUE)
#     data <- as.data.frame(data)
#   }
#   if(!"entrytime" %in% colnames(data)){
#     stop("Entry time missing for subjects. Please specify them as named column
#         'entrytime' in your data frame.")
#   }
#   if(!"survtime" %in% colnames(data)){
#     stop("Survival time missing for subjects. Please specify them as named
#           column 'survtime' in your data frame.")
#   }
#   if(!"censorid" %in% colnames(data)){
#     cat("No censoring mechanism specified (no named column $censorid).
#         Assuming data is uncensored.")
#     data$censorid <- rep(1, nrow(data))
#   }
#   compriskcheck <- "cause" %in% colnames(data)
#   if(compriskcheck){
#     cat("Competing risks specified.")
#   }
#   # determine chronological failure times
#   data$otime <- data$entrytime + data$survtime
#
#   #----------chart specific checks---------------------
#   if(chart == "bkcusum" | chart == "cgrcusum" | chart == "bercusum" ){
#     if(!missing(C)){
#       tempidx <- which(data$otime < data$entrytime + C)
#       data[tempidx,]$otime <- data$entrytime + C
#       data[tempidx,]$censorid <- rep(length(tempidx), 0)
#     }
#     # determine the default construction times (all failtimes), if none specified
#     if(missing(ctimes)){
#       ctimes <- unique(data$otime)
#     }
#
#
#
#
#
#   }
#
#
# }
#
#
# check_data <- function(data, coxphmod, glmmod, cbaseh, chart, verbose = TRUE){
#   checkdf <- checkentry <- checksurv <- checkcensor <- checkcomprisk <- checkctimes <- checktheta <- FALSE
#   if(missing(chart)){
#     checkall <- TRUE
#   }
#   if(chart == "bkcusum"){
#     checkbk <- TRUE
#   } else if(chart == "cgrcusum"){
#     checkcgr <- TRUE
#   } else if(chart == "funnelplot"){
#     checkfunnel <- TRUE
#   } else if(chart == "bercusum"){
#     checkber <- TRUE
#   }
#   if(verbose == FALSE){
#     #--------------GENERAL DATA CHECKS------------------
#     if(!is.data.frame(data)){
#       checkdf <- TRUE
#       warning("Data provided is not a data frame, attempting to convert.", immediate. = TRUE)
#       data <- as.data.frame(data)
#     }
#     if(!"entrytime" %in% colnames(data)){
#       checkentry <- TRUE
#       stop("Entry time missing for subjects. Please specify them as named column
#         'entrytime' in your data frame.")
#     }
#     if(!"survtime" %in% colnames(data)){
#       stop("Survival time missing for subjects. Please specify them as named
#           column 'survtime' in your data frame.")
#     }
#     if(!"censorid" %in% colnames(data)){
#       cat("No censoring mechanism specified. Assuming data is uncensored.")
#       data$censorid <- rep(1, nrow(data))
#     }
#     #FUTURE - COMP RISK?
#     #compriskcheck <- "cause" %in% colnames(data)
#     #if(compriskcheck){
#     #  cat("Competing risks specified.")
#     #}
#
#     #---------------DATA MANIPULATION-------------------
#     # determine chronological failure times
#     data$otime <- data$entrytime + data$survtime
#
#     #--------------CGR and BK DATA CHECKS------------------
#     # determine the default construction times (all failtimes + entrytimes), if none specified
#     if(missing(ctimes)){
#       ctimes <- union(unique(data$otime), unique(data$entrytime))
#     } else{
#       ctimes <- union(ctimes, unique(data$otime))
#     }
#     if(missing(stoptime)){
#       stoptime <- max(data$otime[is.finite(data$otime)])
#     }
#     checkcbase <- FALSE
#     if(missing(coxphmod)){
#       coxphmod <- NULL
#     } else if(inherits(coxphmod, "coxph")){
#       if(missing(cbaseh)){
#         checkcbase <- TRUE
#         cat("Missing cumulative baseline hazard. Determining using provided Cox PH model.")
#         cbasetemp <- basehaz(coxphmod, centered = FALSE)
#         cbaselo <- loess(cbasetemp$hazard~cbasetemp$time)
#         cbaseh <- function(x) predict(cbaselo, x)
#       }
#     } else if(is.list(coxphmod)){
#       if(all(c("formula", "coefficients") %in% names(coxphmod))){
#         checkcoxlist <- TRUE
#       } else{
#         stop("coxphmod does not contain $formula and/or $coefficients.")
#       }
#     } else{ stop("coxphmod is not a list or survival object.")}
#     if(missing(cbaseh)){
#       if(!checkcbase){
#         stop("Please specify cbaseh (function) or coxphmod as Survival object.")
#       }
#     }
#
#
#     #--------------BK-CUSUM DATA CHECKS------------------------
#     if(missing(theta)){
#       stop("Please specify a value for theta (expected excess hazard rate).")
#     }
#   } else if(verbose == TRUE){
#     #--------------GENERAL DATA CHECKS------------------
#     if(!is.data.frame(data)){
#       checkdf <- TRUE
#     }
#     if(!"entrytime" %in% colnames(data)){
#       checkentry <- TRUE
#     }
#     if(!"survtime" %in% colnames(data)){
#       checksurv <- TRUE
#
#     }
#     if(!"censorid" %in% colnames(data)){
#       checkcensor <- TRUE
#     }
#     #FUTURE - COMP RISK?
#     #compriskcheck <- "cause" %in% colnames(data)
#     #if(compriskcheck){
#     #  cat("Competing risks specified.")
#     #}
#
#     #--------------CGR and BK DATA CHECKS------------------
#     # determine the default construction times (all failtimes + entrytimes), if none specified
#     if(missing(ctimes)){
#       ctimes <- union(unique(data$otime), unique(data$entrytime))
#     } else{
#       ctimes <- union(ctimes, unique(data$otime))
#     }
#     if(missing(stoptime)){
#       stoptime <- max(data$otime[is.finite(data$otime)])
#     }
#     checkcbase <- FALSE
#     if(missing(coxphmod)){
#       coxphmod <- NULL
#     } else if(inherits(coxphmod, "coxph")){
#       if(missing(cbaseh)){
#         checkcbase <- TRUE
#         cat("Missing cumulative baseline hazard. Determining using provided Cox PH model.")
#         cbasetemp <- basehaz(coxphmod, centered = FALSE)
#         cbaselo <- loess(cbasetemp$hazard~cbasetemp$time)
#         cbaseh <- function(x) predict(cbaselo, x)
#       }
#     } else if(is.list(coxphmod)){
#       if(all(c("formula", "coefficients") %in% names(coxphmod))){
#         checkcoxlist <- TRUE
#       } else{
#         stop("coxphmod does not contain $formula and/or $coefficients.")
#       }
#     } else{ stop("coxphmod is not a list or survival object.")}
#     if(missing(cbaseh)){
#       if(!checkcbase){
#         stop("Please specify cbaseh (function) or coxphmod as Survival object.")
#       }
#     }
#
#
#     #--------------BK-CUSUM DATA CHECKS------------------------
#     if(missing(theta)){
#       stop("Please specify a value for theta (expected excess hazard rate).")
#     }
#   }
#
# }
