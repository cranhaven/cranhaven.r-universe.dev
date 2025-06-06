#' Prepares formula and data for further use
#' 
#' This function checks, whether the formula is in the right form. 
#' It will also prepare the data. If the "data" argument is set to NULL
#' it will look for it in the environment of the provided formula. It will throw
#' out all the columns that are not mentioned in the formula.
#'  
#' @noRd 
#' @importFrom formula.tools lhs rhs rhs.vars
#' @importFrom stats model.matrix update.formula

prepare_formula <-
  function(formula, data, expect = FALSE){
    
    if(!inherits(formula,"formula"))stop('The argument "fomula" needs to be of data type formula.')
    check_formula(formula, expect)
    links <- formula.tools::lhs(formula)

    if(is.null(data)){
      Surv_obj       <- eval(links, envir=environment(formula))
      data           <- model.matrix(as.formula(paste0('~',paste0(all.vars(rhs(formula)), collapse = "+")), env=environment(formula)))
      cn             <- colnames(data)
      if(is.matrix(Surv_obj)){
        data           <- cbind(Surv_obj[,1],data)
        colnames(data) <- c(as.list(match.call(Surv,links))$time, cn)
      } else{
        data           <- cbind(Surv_obj,data)
        colnames(data)[1] <- as.character(links)
      }
      data <- data.frame(data)
      
    }else{
      # If data IS provided
      e <- new.env()
      sapply(colnames(data),function(x)assign(x, data[[x]], envir = e))
      Surv_obj <- eval(links, envir=e)
      data <- data[,names(data) %in% all.vars(formula)]
    }
    if(is.null(data) || ncol(data) == 0)stop('The data belonging to the formula could not be found.')
    if(survival::is.Surv(Surv_obj)){
      if(attributes(Surv_obj)$type != 'right')stop('Only right-censoring is allowed.')
      
      response  <- Surv_obj[,1]
      #attr(response, "name") <- as.character(as.list(match.call(Surv,links))$time)
      
      newformula <- update.formula(formula, paste(as.character(as.list(match.call(Surv,links))$time)," ~ .", sep = ""))
      delta <- Surv_obj[,2]
      
    } else{
      response  <- Surv_obj
      delta <- rep(1, length(response))
      newformula <- formula
    }

    
    return(list(response = response, delta = delta, data = data, newformula = newformula))
  }