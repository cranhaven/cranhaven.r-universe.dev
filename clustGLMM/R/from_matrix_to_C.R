from_matrix_to_C <-
function(draws,
         iterations,
         p,
         settings)
{
  if(missing(iterations)){iterations <- draws$m}
  
  ### We just need to take those columns that belong to parameter p
  # Every colname begins with p (--> "^")
  # be careful about "Sigma" being contained in "InvSigma",...
  # "pUig"
  # also careful about p = "b" and "beta"
  
  cols <- grep(paste0("^",p), colnames(draws), value = TRUE)
  # These cols begin with parameter p,
  # but we need to exclude those which might be confused with other parameter
  otherp <- setdiff(rownames(settings), p)
  for(op in otherp){
    if(length(grep(p, op))>0){ # p is contained in op
      # --> draws op must be deleted from this list of columns
      cols <- setdiff(cols, grep(paste0("^",op), cols, value = TRUE))
    }
  }
  
  # matrix needs to be stored by rows --> needs to be transposed
  RET <- c(t(draws[is.element(draws$m, iterations), cols]))
  if(length(RET) == 0){
    RET <- double(0)
  }
  return(RET)
}
