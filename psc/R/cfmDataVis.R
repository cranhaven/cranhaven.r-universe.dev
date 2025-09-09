#' Visualising data within a CFM
#'
#' The pscCFM creates a model object which is stripped of identifiable
#' information.  The cfmDataVis fucntion supplies a visualised form of the dataset
#' for summary information
#'
#' @param cfm a 'glm' or 'flexsurvreg' model object
#' @return a list of grobs for each model covariate
#' @export
cfmDataVis <- function(cfm){

  ### Getting data from object
  data <- model.frame(cfm)

  ### reordering each factor in the dataset - this maintains individual
  nr <- nrow(data)
  for(i in 1:ncol(data)){
    data[,i] <- data[sample(nr,nr,replace=F),i]
  }

  ## removing outcome
  out <- data[,1]
  data<- data[,-1]

  ## removing "weights" column
  w.id <- which(names(data)=="(weights)")
  if(length(w.id)>0) data <- data[,-w.id]

  ## Getting classes
  cls <- lapply(data,class)

  ## Creating a list of grobs
  gglist <- list()

  for(i in 1:ncol(data)){

    x <- data[,i];x
    nm <- names(data)[i]
    if(cls[i]%in%c("factor","character")){
      gglist[[i]] <- facVis(x,nm)
    }

    if(cls[i]%in%c("numeric","integer")){
      jit.sd <- sd(x)/10
      x <- x+rnorm(nr,0,jit.sd)
      gglist[[i]] <- numVis(x,nm)
    }

  }

  names(gglist) <- names(data)
  class(gglist) <- c("quiet_list",class(gglist))
  gglist
}
