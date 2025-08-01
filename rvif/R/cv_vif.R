cv_vif <-
function(x, tol=1e-30){
  x = as.matrix(x)
  n = nrow(x)
  p = ncol(x)
  #
  cut_off = 0
  if (p <= 2){
    message("At least 3 independent variables are needed (including the interceptin the first column) to carry out the calculations.")
    cut_off = 1
  } 
  for (i in 2:p){
    if (var(x[,i]) == 0){
      message("There is a constant variable. Delete it before running the code or, if it is the intercept, it must be the first column of the design matrix.")
      cut_off = 1
    }
  }
  #
  if (det(crossprod(x)) == 0) {
    message("Perfect multicollinearity exists. Modify the design matrix before running the code.")
    cut_off = 1
  }
  if (det(crossprod(x)) < tol) {
    message("System is computationally singular. Modify the design matrix before running the code.")
    cut_off = 1
  }
  #
  if (cut_off == 0){
    cvs = CVs(x) # CVs is a multiColl R package function
    fivs = VIF(x) # VIF is a multiColl R package function
    #
    output = data.frame(cvs, fivs)
    row_names = paste("Variable", 2)
    for (i in 3:p){row_names = c(row_names, paste("Variable", i))}
    rownames(output) = row_names
    colnames(output) = c("CV", "VIF")
    return(output)    
  }
}
