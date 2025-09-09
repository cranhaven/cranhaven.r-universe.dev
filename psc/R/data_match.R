#' A function to ensure that data from the cfm and data cohort are compatible
#'
#' The purpose of this function is to run a series of checks to ensure that the
#' data included in the data cohort is comparable to the counter-factual model
#'
#' @param cls a list of extracted data classes
#' @param lev a list of factor levels
#' @param dc.data dataset to be 'cleaned'
#' @return a dataset which is checked and compatible with the CFM

######### data_match
data_match <- function(cls,lev,dc.data){

  ### duplicated namse in dc.data
  dup <- duplicated(names(dc.data))
  if(any(dup)) dc.data <- dc.data[,-which(dup)]

  ## Getting term names
  nm <- names(cls);nm

  ## removing 'weights' column if there
  wid <- which(nm=="(weights)");wid
  if(length(wid)>0) cls <- cls[-wid]

  # creating output
  dc.new <- dc.data

  for(i in 1:length(cls)){

    con <- which(names(dc.data)%in%nm[i]);con
    if(length(con)==0) stop("DC missing covariate included in CFM")

    old <-dc.data[,con];old
    new <- old
    cl <- cls[[i]];cl

    if(cl%in%"character"){
      new <- factor(old);new
      warning(nm[i]," specified as a character in the model, consider respecifying
                as a factor to ensure categories match between CFM and DC")
    }

    if(cl%in%c("factor")){
      new <- factor(old);new
      if(!any(levels(new)%in%lev[[i]])) stop(paste("Factor levels in",nm[i],"not
                                                 represented in model"))
      att <- list("levels"=lev[[i]],class=cl)
      attributes(new) <- att
    }

    if(cl%in%c("numeric","integer")){
      new <- as.numeric(as.character(old))
    }

    dc.new[,con] <- new;dc.new
    rm(cl)
  }

  ret <- dc.new[,which(names(dc.new)%in%nm)]
  ret
}

