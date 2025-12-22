#' Visualising data within a CFM
#'
#' The pscCFM creates a model object which is stripped of identifiable
#' information.  The cfmDataVis function supplies a visualised form of the dataset
#' for summary information
#'
#' @param cfm a 'glm' or 'flexsurvreg' model object
#' @return a list of grobs for each model covariate
cfmDataVis <- function(cfm){

  ### Getting data from object
  data <- model.frame(cfm)

  ### reordering each factor in the dataset - this maintains identifiability
  nr <- nrow(data)
  for(i in 1:ncol(data)){
    data[,i] <- data[sample(nr,nr,replace=F),i]
  }

  ## Formula
  fix.form <- formula(cfm,fixed.only=T)
  fix.term <- terms(fix.form)
  terms <- attributes(fix.term)$term.labels

  ### Selecting variables which are fixed effects
  data <- data[,which(names(data)%in%terms)]
  if(length(terms)==1){
    data <- data.frame(data)
    names(data) <- terms
  }


  ## Getting classes
  cls <- lapply(data,class)

  ## Creating a list of grobs
  gglist <- list()

  for(i in 1:ncol(data)){

    x <- data[,i];x
    nm <- names(data)[i]
    if(cls[i]%in%c("factor","character")){
      gglist[[i]] <- cfmDataVis_fac(x,nm)
    }

    if(cls[i]%in%c("numeric","integer")){
      jit.sd <- sd(x,na.rm=T)/10
      x <- x+rnorm(nr,0,jit.sd) ## jittering
      gglist[[i]] <- cfmDataVis_num(x,nm)
    }

  }

  names(gglist) <- names(data)
  class(gglist) <- c("quiet_list",class(gglist))
  gglist
}
