#' A function which structures the Data Cohort in a format for model estimation
#'
#' This function ensures the data are supplied in a structure which allows for
#' estimation.  This is performed by re-fitting the original CFM with the DC and
#' extracting the appropriate structures.  Data are returned in terms of "Y" for
#' model outcomes, "X" for data and "Z" for random effects where mixed models
#' are supplied.
#'
#' @param CFM a Counter Factual Model
#' @param DC a Data Cohort object
#' @return A set of structures for use with estimation procedures
#' re-export Surv from survival
#' @importFrom lme4 lmer getME
#' @importFrom survival Surv
#' @import flexsurv
pscData_structure <- function(CFM,DC){


  if("lmerMod"%in%CFM$mod_class){
    mod <- lmer(CFM$formula,data=DC)
    Z <- getME(mod,"Z")
    X <- getME(mod,"X")
    Y <- getME(mod,"y")
    ret <- list("Y"=Y,"X"=X,"Z"=Z)
  }

  if("glm"%in%CFM$mod_class){
    mf <- model.frame(CFM$formula,data=DC)
    mm <- model.matrix(CFM$formula,data=DC)
    mod <- glm(CFM$formula,data=DC,family=CFM$family)
    ret <- list("Y"=mf[,1],"X"=mm)
  }

  if("flexsurvreg"%in%CFM$mod_class){
    #mf <- model.frame(CFM$formula,data=DC)
    #ys <- mf[,1]
    ys <- Surv(DC$time,DC$cen)
    mm <- model.matrix(CFM$formula,data=DC)[,-1]
    yd <- data.frame("time"=ys[,1],"cen"=ys[,2])
    ret <- list("Y"=yd,"X"=mm)
  }

  return(ret)

}
