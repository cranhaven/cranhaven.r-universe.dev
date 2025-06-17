dependence <-
function(mat, cont = NULL, sc.order = 0){
#
# Input:   mat = k-columnn matrix with n observations of a k-dimensional random vector
#                NA values are allowed
#         cont = vector of column numbers to consider/coerce as continuous random variables (optional)
#     sc.order = order of subcopula approximation (continuous random variables)
#                if 0 (default) then maximum order m = n is used
#                ** usually m = 50 is a good recommended value, higher values demand more computing time
#
# Output: A 3-dimensional array (k x k x 4) with pairwise monotone and supremum dependence,
#         monotone/supremum dependence ratio, and proportion of pairwise NAs.
#
  k <- ncol(mat); n <- nrow(mat)
  if (sc.order == 0) sc.order <- n
  if(is.null(colnames(mat))) colnames(mat) <- paste0("V", 1:k)
  deparray <- array(NA, dim = c(k, k, 4), dimnames = list(colnames(mat), colnames(mat),
                    c("depMon", "depSup", "depMonSupRatio", "propNAs")))
  diag(deparray[ , , "depMon"]) <- 1
  diag(deparray[ , , "depSup"]) <- 1
  diag(deparray[ , , "propNAs"]) <- apply(is.na(mat), 2, sum)/n
  for (i in 1:(k - 1)){
    for (j in (i+1):k){
      NArows <- which(is.na(mat[ , i]*mat[ , j]))
      deparray[i, j, "propNAs"] <- length(NArows)/n
      deparray[j, i, "propNAs"] <- deparray[i, j, "propNAs"] 
      if (length(NArows) < (n - 1)){
        if (i%in%cont & j%in%cont){
          if (length(NArows) > 0){
            SC <- subcopemc(mat[-NArows, c(i, j)], min(sc.order, n - length(NArows)))
          }
          else{
            SC <- subcopemc(mat[ , c(i, j)], min(sc.order, n - length(NArows)))
          }
        } 
        else{
          if (length(NArows) > 0){
            SC <- subcopem(mat[-NArows, c(i, j)])
          }
          else{
            SC <- subcopem(mat[ , c(i, j)])
          }
        }
        deparray[i, j, "depMon"] <- SC$depMon
        deparray[j, i, "depMon"] <- deparray[i, j, "depMon"]
        deparray[i, j, "depSup"] <- SC$depSup
        deparray[j, i, "depSup"] <- deparray[i, j, "depSup"]
        deparray[i, j, "depMonSupRatio"] <- SC$depMonNonSTD[2]/SC$depSupNonSTD[2]
        deparray[j, i, "depMonSupRatio"] <- deparray[i, j, "depMonSupRatio"]
      }
    }
  }
  return(deparray)
}
