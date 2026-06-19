#########################################
#GREEDY CHAIN MODEL
# Selects the largest value of mutual information from a matrix
# the variables that have the max MI are the center of the chain
# algorithm iterates too look for the next largest MI value to the left or right of the chain
# then adds the respective variable where it corresponds.

chain_graphical_model <- function(M){
  n=nrow(M)
  max_ind = which(M == max(M), arr.ind=TRUE)  #obtains indices of argmax of matrix
  total_MI <- max(M) # will accumulate Mutual Information
  
  border1 <- max_ind[1] #number in the left side of the permutation
  border2 <- max_ind[2] #number in the right side of the permutation
  permutation =c(border1,border2) #array to form permutation. starts with indices of argmax of matrix
  
  M[border1,border2] <- 0 #set to 0 MI in selected connection
  M[border2,border1] <- 0
  
  finish=FALSE
  
  while (finish == FALSE){
    
    MI_with_b1 <- M[border1,] #check MI of border1 with the other variables.
    MI_with_b2 <- M[,border2] #check MI of border2 with the other variables.
    
    if( max(MI_with_b1) > max(MI_with_b2)){ # if the max MI value is with border 1
      max_ind2 = which(M == max(MI_with_b1), arr.ind=TRUE) # get indices of new border
      new_border <-  max_ind2[ which(max_ind2!=border1)[1]]
      
      if(new_border %in% permutation){ #if new border is already in permutation 
        M[max_ind2[1], max_ind2[2]] <- 0 #set to 0 values so they wont bother again
        M[max_ind2[2], max_ind2[1]] <- 0
        next
      }
      M[max_ind2[1], max_ind2[2]] <- 0 #set to 0 values so they wont bother again
      M[max_ind2[2], max_ind2[1]] <- 0
      
      border1 <- new_border #reset border
      permutation <- c(border1,permutation) #add border to permutation
      total_MI <- total_MI + max(MI_with_b1) # add the correspondent MI
      
      
    }else{
      max_ind2 = which(M == max(MI_with_b2), arr.ind=TRUE)
      new_border <-  max_ind2[ which(max_ind2!=border2)[1]]
      
      if(new_border %in% permutation){
        M[max_ind2[1], max_ind2[2]] <- 0
        M[max_ind2[2], max_ind2[1]] <- 0
        next
      }
      M[max_ind2[1], max_ind2[2]] <- 0
      M[max_ind2[2], max_ind2[1]] <- 0
      
      border2 <- new_border
      permutation <- c(permutation,border2)
      total_MI <- total_MI + max(MI_with_b2)
      
    }
    if(length(permutation) == n){ #when all variables are in permutation while loop stops
      finish = TRUE
    }
  }
  names <- colnames(M)
  permutation <- names[permutation]
  return (list( total_MI = total_MI, table = data.frame(from = permutation[-length(permutation)],
                                                                to = permutation[-1])))
}

