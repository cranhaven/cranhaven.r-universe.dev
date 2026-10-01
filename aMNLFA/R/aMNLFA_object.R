#' aMNLFA object function
#'
#' This function creates an aMNLFA object based on user specifications to pass to aMNLFA functions.
#' @param dir The directory in which data, inputs, and outputs are to be stored. Must be supplied.
#' @param mrdata The R dataframe containing the multiple-record dataset. Must be supplied.
#' @param indicators The names of all indicators (items, observed variables) in the MNLFA.
#' @param catindicators The list of indicators which are categorical. Defaults to NULL.
#' @param countindicators The list of indicators which are count. Defaults to NULL.
#' @param meanimpact The list of covariates (predictors) which may generate impact on the latent variable mean. Defaults to NULL.
#' @param varimpact The list of covariates (predictors) which may generate impact on the latent variable variance. Defaults to NULL.
#' @param measinvar The list of covariates (predictors) which may generate DIF. Defaults to NULL.
#' @param factors The list of covariates which are categorical. Defaults to NULL.
#' @param time The variable which indexes time (or multiple records within a single case). If left blank, assumes single-record data. Defaults to NULL.
#' @param auxiliary The list of variables to be considered as auxiliary (i.e., retained in the dataset but not used in the analysis). Defaults to NULL.
#' @param ID The variable which identifies cases. Defaults to NULL.
#' @param thresholds A Boolean operator indicating whether to test for threshold DIF.
#' @keywords MNLFA
#' @export
#' @examples
#' 
#'  \dontrun{
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
#'  the.list <- list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'  
#'  ob <- aMNLFA::aMNLFA.object(dir = wd, 
#'  mrdata = xstudy, 
#'  indicators = paste0("BIN_", 1:12),
#'  catindicators = paste0("BIN_", 1:12), 
#'  meanimpact = c("AGE", "GENDER", "STUDY"), 
#'  varimpact = c("AGE", "GENDER", "STUDY"), 
#'  measinvar = c("AGE", "GENDER", "STUDY"),
#'  factors = c("GENDER", "STUDY"),
#'  ID = "ID",
#'  thresholds = FALSE)
#'  }
#'  
aMNLFA.object<-function(dir, mrdata, indicators=NULL, catindicators=NULL, countindicators=NULL, meanimpact=NULL, varimpact=NULL, measinvar=NULL, factors=NULL, time=NULL, auxiliary=NULL, ID=NULL, thresholds=NULL)
{
  charOrNull <- function(x) {
    is.character(x) || is.null(x)
  }
  
  if (thresholds == TRUE) {
    stop("thresholds == TRUE is disabled in this version of aMNLFA. Reset thresholds to FALSE to run this function.")
  }
  
  stopifnot(charOrNull(indicators))
  stopifnot(charOrNull(catindicators))
  stopifnot(charOrNull(countindicators))
  stopifnot(charOrNull(meanimpact))
  stopifnot(charOrNull(varimpact))
  stopifnot(charOrNull(measinvar))
  stopifnot(charOrNull(factors))
  stopifnot(charOrNull(time))
  stopifnot(charOrNull(auxiliary))
  stopifnot(charOrNull(ID))

  object<-list(dir=dir, mrdata=mrdata, indicators=indicators, catindicators=catindicators, countindicators=countindicators, meanimpact=meanimpact, varimpact=varimpact, measinvar=measinvar, factors=factors, time=time, auxiliary=auxiliary, ID=ID, thresholds=thresholds)

  class(object)<-c("list","aMNLFA.object")
  return(object)
}
