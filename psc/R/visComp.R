#' Visualising Comparisons between a CFM and a DC
#'
#' The visComp function takes the data visualisations supplied as part of the
#' CFM model and appends summaries of the equivalent datapoints from the
#' Data Cohort.
#'
#' @param CFM an object of class pscCFM
#' @param DC A dataset including columns to match to covariates in the model
#' @param id Numeric vector stating which patient(s) from the dataset should be included in the analysis.
#'  Defaults to all patients
#' @return a list of grobs for each model covariate
#' @export
visComp <- function(CFM,DC,id=NULL){

  ### error (CFP shoudl be of class pscCFM)
  if(!inherits(CFM,what ="pscCFM")) stop("Only models of class 'pscCFM' are allowed,
    please see pscCFM for details")

  cfmVis <- CFM$datavis
  vis_nm <- names(cfmVis)
  vis_cls <- CFM$cov_class
  ncov <- length(vis_nm)

  gglist <- list()

  for(i in 1:ncov){

    cls <- vis_cls[names(vis_cls)==vis_nm[i]]
    p.old <- cfmVis[names(cfmVis)==vis_nm[i]][[1]]
    data_new <- DC[,names(DC)==vis_nm[i]]

    if(cls%in%c("character","factor")){
      data_new <- factor(data_new)
      p.new <- facVisComp(p.old,data_new)
    }

    if(cls%in%c("numeric","integer")){
      data_new <- as.numeric(as.character(data_new))
      p.new <- numVisComp(p.old,data_new)
    }

    gglist[[i]] <- p.new
  }

  names(gglist) <- vis_nm
  class(gglist) <- c("quiet_gglist",class(gglist))
  gglist

}
