
# This variable is for fix precision errors in numeric comparisons using the operator '==', '<=' or '>='
.tolerance <- 1e-10

#
#
# This function creates all the fuzzy intervals for a dataset
#
#
.create_fuzzyIntervals <- function(min, max, num_sets, types){
  
  n_mats <- length(min)
  #Creates the fuzzy intervals for each variable
  lst <- lapply(X = 1:n_mats, FUN = function(x) .FuzzyIntervals(min = min[x], max = max[x], num_sets = num_sets, types = types[x]))
  
  #Collect on an array
  arr <- do.call(cbind, lst)
  dim(arr) <- c(num_sets, 3, n_mats)
  arr
}


#
#
#  Function to create fuzzy intervals over a single variable.
#
#

.FuzzyIntervals <- function(min, max, num_sets, types){
  
  xmin <- min - ( (max - min) / (num_sets - 1) )
  xmedio <- min
  xmax <- 0
  
  
  
  fuzzySet <- matrix(nrow = num_sets, ncol = 3)
  
  for(j in 1:num_sets){ # For each fuzzy set
    if(types != "c"){ #If the variable is not categorical 
      xmax <- min + ( (max - min) / (num_sets - 1) ) * j
      
     
      
      #Save fuzzy set
      fuzzySet[j, 1] <- xmin
      fuzzySet[j, 2] <- xmedio
      fuzzySet[j, 3] <- xmax
      
      #Modify variables to calculate the next set
      xmin <- xmedio
      xmedio <- xmax
    }
  }
  #Return
  fuzzySet
  
}





#-----------------------------------------------------------------------------------
#    Creates crisp sets relative to the fuzzy sets generated before.
#    This works because the fuzzy sets are all triangular and for a given fuzzy sets we know 
#    the exact cut point. If we implement another kind of fuzzy set, this method does not work anymore
#-----------------------------------------------------------------------------------

.createCrispIntervals <- function(fuzzyIntervals){
  n_mat <- dim(fuzzyIntervals)[3]
  n_vars <- dim(fuzzyIntervals)[1]
  crispIntervals <- lapply(X = 1:n_mat, FUN = function(x, fuzzy) .CrispIntervals(fuzzy[,,x]) , fuzzyIntervals)
  
  arr <- do.call(cbind, crispIntervals)
  dim(arr) <- c(n_vars, 2, n_mat)
  arr
}






#
#
# Create crisp sets for a single variable 
# 
#
.CrispIntervals <- function(fuzzyInterval){
  n_vars <- dim(fuzzyInterval)[1] # each row of the matrix is a variable
  
  crispMatrix <- matrix(nrow = n_vars, ncol = 2)
  min <- fuzzyInterval[1,1]
  # The maximum of the crisp sets is the mean point between 'xmin' and 'xmedio' for the first fuzzy sets
  max <- (fuzzyInterval[1,3] + fuzzyInterval[1,2]) / 2
  crispMatrix[1,1] <- min
  crispMatrix[1,2] <- max
  for(i in 2:n_vars){
    # The maximum before is now the new minimum
    min <- max
    #For the second in advance, 'max' is the mean point between 'xmedio' and 'xmax' of the given set.
    max <- (fuzzyInterval[i,3] + fuzzyInterval[i,2]) / 2
    crispMatrix[i,1] <- min
    crispMatrix[i,2] <- max
  }
  crispMatrix
}





#
#
# Compute the belonging degree of an entire example set for the specified
#   fuzzy sets, this sets must be specified by means of their min, max 
#   and half value.
#   If you want to get the belonging degree for a subset of variables,
#   you must get only the values of the example corresponding to those variables
#   before execute this function.
#
#
.fuzzyBelongingDegree <- function(x, xmin, xmedio, xmax, n_matrices){
  
  x <- as.numeric(x)
  
  result <- numeric(length(x)) + 1
  #Sum the tolerance value to the values
  xminX <- numeric(length(x)) + xmin + .tolerance
  xmedioX <- numeric(length(x)) + xmedio + .tolerance
  xmaxX <- numeric(length(x)) + xmax + .tolerance
  
  #Calculate which values are out of bounds, are less than xmedio or greater then xmedio 
  #because each one have a diferente belonging degree computation
  outOfBounds <- which( x <= xminX | x >= xmaxX )
  lessXMedio <- which( x < xmedioX & x > xminX )
  greaterXMedio <- which( x > xmedioX & x < xmaxX )
  
  #Calculate the belonging degree (TRIANGULAR SETS ONLY !)
  result[outOfBounds] <- 0
  result[lessXMedio] <- (( x[lessXMedio] - xminX[lessXMedio] ) * (1 / (xmedioX[lessXMedio] - xminX[lessXMedio] )))
  result[greaterXMedio] <- (( xmaxX[greaterXMedio] - x[greaterXMedio] ) * (1 / (xmaxX[greaterXMedio] - xmedioX[greaterXMedio] )))
  
  result <- matrix(result, ncol = n_matrices, byrow = TRUE)
  
  result
}




#
#
# It is like fuzzyBelongingDegree but for crisp belonging and you must specify the min and max
#   value for crisp sets
#
#

.crispBelongingDegree <- function(x, xmin, xmax, DNF = FALSE){
  
  x <- as.numeric(x)
  result <- numeric(length(x))
  
    #Gets elements inside crisp sets bounds
    resulta <- which((x > xmin + .tolerance) & x <= (xmax + .tolerance))
    #Assing value of 1 to elements inside bounds
    result[resulta] <- 1
    result <- matrix(result, ncol = length(xmax), byrow = TRUE)
    result
 
    
}





#
#
#  Return the fuzzy belonging degree for a DNF rule
#
#
.getMaxFuzzyForAVariable2 <- function(values, example_num){
  #Return fuzzy belonging degree of each value-variable 
  result <- .fuzzyBelongingDegree(x = as.vector(example_num), xmin = values[2,], xmedio = values[3,], xmax = values[4,], n_matrices = NCOL(values))
  
  a <- which(!duplicated(values[1,]))
  long <- length(a)
  rangos <- vector(mode = "list", length = long)
  
  for(i in seq_len(long)){
    if(! is.na(a[i +1 ])){
      rangos[[i]]  <- a[i]:(a[i + 1] - 1)  
    } else {
      rangos[[i]] <- a[i]:NCOL(result)
    }
  }
  
# Calculate the fuzzy belonging degree as the minimum of each variable. The value of each variable is the maximum
# belonging degree of a value that participate in the rule
if(NCOL(result) > 1){
  result <- t( apply(X = result, MARGIN = 1, FUN = function(x, rangos)  vapply(X = rangos, FUN = function(x, vector) max(vector[x]), FUN.VALUE = 1, x) , rangos) )
  result <- apply(X = result, MARGIN = 1, FUN = min)
} else {
  result <- t( apply(X = result, MARGIN = 1, FUN = function(x, rangos)  vapply(X = rangos, FUN = function(x, vector) max(vector[x]), FUN.VALUE = 1, x) , rangos) )
  
}
  result
  
}








#
#
# The same but for crips set on DNF Rules
#
#
.getMaxCrispForAVariable2 <- function(values, example_num){
  result <- .crispBelongingDegree(x = as.vector(example_num), xmin = values[2,], xmax = values[3,], DNF = TRUE)

  a <- which(!duplicated(values[1,]))
  long <- length(a)
  rangos <- vector(mode = "list", length = long)
  
  for(i in seq_len(long)){
    if(! is.na(a[i +1 ])){
      rangos[[i]]  <- a[i]:(a[i + 1] - 1)  
    } else {
      rangos[[i]] <- a[i]:NCOL(result)
    }
  }
  
  if(NCOL(result) > 1){
    result <- t( apply(X = result, MARGIN = 1, FUN = function(x, rangos)  vapply(X = rangos, FUN = function(x, vector) max(vector[x]), FUN.VALUE = 1, x) , rangos) )
    result <- apply(X = result, MARGIN = 1, FUN = min)
  } else {
    result <- t( apply(X = result, MARGIN = 1, FUN = function(x, rangos)  vapply(X = rangos, FUN = function(x, vector) max(vector[x]), FUN.VALUE = 1, x) , rangos) )
    
  }
  result
  
}