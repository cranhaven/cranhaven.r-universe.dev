# A function that fits SCCS model with continuous exposures. 
quantsccs <- function(formula, indiv, event, data) {
                
  fdata <- data
  indiv <- eval(substitute(indiv), data, parent.frame())
  event <- eval(substitute(event), data, parent.frame())
  
  fmla <- paste(formula, "+", "strata(indiv)")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- clogit(formula = fmla1, method = "breslow", data = fdata)
  summary <- summary(mod)
  return(summary)
  
  
}

