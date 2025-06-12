#' @title Internal function
#' 
#' @description Internal function to select a predefined method/function available in this package.
#' 
#' @encoding UTF-8
#' @param method A predefined method/function available in PCPS package, partial match to "mantel", "adonis", "glm", "rda", "gls.marginal", "gls.sequential", "lme.marginal", "lme.sequential" and "none".
#' @export
select.pcpsmethod <- function(method = c("mantel", "adonis", "glm", "rda", "gls.marginal", "gls.sequential", "lme.marginal", "lme.sequential", "none")){
  METHOD <- c("mantel", "adonis", "adonis2.global", "adonis2.margin", "glm", "rda", "gls.marginal", "gls.sequential", "lme.marginal", "lme.sequential", "none")
  if(inherits(method, "function")){
    FUN <- method
  } else{
    method <- pmatch(method, METHOD)
    if (any(is.na(method)) | length(method) > 1) {
      stop("\n Invalid method. Only one argument is accepted in method \n")
    }
    if(method==1){
      FUN <- FUN.MANTEL
    }
    if(method==2){
      FUN <- FUN.ADONIS
    }
    # if(method==3){
    #   FUN <- FUN.ADONIS2.global
    # }
    # if(method==4){
    #   FUN <- FUN.ADONIS2.margin
    # }
    if(method==5){
      FUN <- FUN.GLM
    }
    if(method==6){
      FUN <- FUN.RDA
    }
    if(method==7){
      FUN <- FUN.GLS.marginal
    }
    if(method==8){
      FUN <- FUN.GLS.sequential
    }
    if(method==9){
      FUN <- FUN.LME.marginal
    }
    if(method==10){
      FUN <- FUN.LME.sequential
    }
    if(method==11){
      FUN <- NULL
    }
  }
  return(FUN)
}
