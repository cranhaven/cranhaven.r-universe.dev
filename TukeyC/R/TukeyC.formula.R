##
## S3 method to design matrix and response variable or data.frame objects
##

TukeyC.formula <- function(formula,
                           data            = NULL,
                           which           = NULL,
                           fl1             = NULL,
                           fl2             = NULL,
                           error           = NULL,
                           sig.level       = .05,
                           round           = 2,
                           adjusted.pvalue = 'none', ...)    
{

  aux <- regexpr("Error", 
                 as.character(formula), 
                 perl=TRUE)
  aux_err <- regmatches(as.character(formula), 
                        aux)

  cl <- match.call()  
  if(length(aux_err) == 0){

    model <- lm(formula,
                data = data)

   
    res <- TukeyC(x               = model,
                  which           = which,
                  fl1             = fl1,
                  fl2             = fl2,
                  error           = error,
                  sig.level       = sig.level,
                  round           = round,
                  adjusted.pvalue = adjusted.pvalue,
                  ...) 

  } else {

    basee <- aov(formula,
                 data = data)

    oc <- attr(basee, 
               "call")
    Terms <- attr(basee, 
                  "terms")
    indError <- attr(Terms, 
                     "specials")$Error
    errorterm <- attr(Terms, 
                      "variables")[[1 + indError]]
    form <- update.formula(Terms, 
                           paste(". ~ .-", 
                                 deparse(errorterm, 
                                         width.cutoff = 500L, backtick = TRUE), "+", deparse(errorterm[[2L]], 
                                         width.cutoff = 500L, backtick = TRUE))) 
    model <- lm(form,
                data = data)

    res <- TukeyC(x               = model,
                  which           = which,
                  fl1             = fl1,
                  fl2             = fl2,
                  error           = error,
                  sig.level       = sig.level,
                  round           = round,
                  adjusted.pvalue = adjusted.pvalue,
                  ...) 
  }
  res$call <- cl
  class(res) <- c('TukeyC.formula',class(res))
  return(res)
}
