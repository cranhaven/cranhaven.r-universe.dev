rvifs <-
function(x, ul=TRUE, intercept=TRUE, tol=1e-30){
  if (is.matrix(x) == FALSE) x = as.matrix(x)
  n = nrow(x)
  p = ncol(x)
  ###
  cut_off = 0
  if (p < 2) {
    message("At least 2 independent variables (including the intercept) are needed to running the code.")
    cut_off = 1
  }
  #
  start = 1
  if (intercept==TRUE) start = 2
  for (i in start:p){
    if (var(x[,i]) == 0){
      message("There is a constant variable. Delete it before running the code or, if it is the intercept, it must be the first column of the design matrix.")
      cut_off = 1
    }
  }
  #
  if (det(crossprod(x)) == 0) {
    message("Perfect multicollinearity exists. Modify the design matrix before running the code.")
    cut_off = 1
  } else if (det(crossprod(x)) < tol) {
    message("System is computationally singular. Modify the design matrix before running the code.")
    cut_off = 1
  }
  ###
  if (cut_off == 0){
    if (ul == TRUE){x = lu(x)} # lu is a multiColl R package function
    RVIFs = integer()
    a = integer()
    if (intercept == TRUE) row_names = c("Intercept") else row_names = paste("Variable", 1)
    for (i in 1:p){
      if (det(crossprod(x[,-i],x[,-i])) != 0) a_i = crossprod(x[,i],x[,-i])%*%
          solve(crossprod(x[,-i],x[,-i]))%*%crossprod(x[,-i],x[,i]) else a_i = NaN
          d_i = crossprod(x[,i])
          RVIFs[i] = 1/(d_i-a_i)
          a[i] = round(a_i*100/d_i, digits=4)
          if (i>1){row_names = c(row_names, paste("Variable", i))}
    }
    #
    output = data.frame(RVIFs, a)
    rownames(output) = row_names
    colnames(output) = c("RVIF", "%")
    return(output)
  }
}
