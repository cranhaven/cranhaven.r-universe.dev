Correction2Vect <-
  function(X, Y, te=NULL, vector_prog, vector_pred, top_grill.=c(1:length(vector_prog)), delta=0.95, toZero=FALSE){
  
    if(toZero){
      matrix_top_fix <- sapply(top_grill., top, vect=vector_prog)
      matrix_top_opt <- sapply(top_grill., top, vect=vector_pred)
    } else {
      matrix_top_fix <- sapply(top_grill., top_thresh, vect=vector_prog)
      matrix_top_opt <- sapply(top_grill., top_thresh, vect=vector_pred)
    }
    
    
    opt_top_opt <- mse_fix <- c()
    for(j in 1:length(top_grill.)){
      fix_temp <- matrix_top_fix[,j]
      mse_temp <- c()
      yhat <- X%*%c(te, fix_temp, matrix_top_opt[,1])

      mse_temp[1] <- sum((Y-yhat)^2)
      for(m in 2:length(top_grill.)){
        opt_temp <- matrix_top_opt[,m]
        threshed_vect <- c(te, fix_temp, opt_temp)
        yhat <- X%*%threshed_vect
        mse_temp[m] <- sum((Y-yhat)^2)
        ratio_mse <- round(mse_temp[m]/mse_temp[m-1], 6)
        if(ratio_mse >= 0.999){
          opt_top_opt[j] <- top_grill.[m]
          mse_fix[j] <- mse_temp[m]
          break
        }
      }
      if(m==length(top_grill.)){
        opt_top_opt[j] <- top_grill.[m]
        mse_fix[j] <- mse_temp[m]
      }
      if(j==1){
        ratio_final <- 0
      } else {
        ratio_final <- mse_fix[j]/mse_fix[j-1]
      }
      if(ratio_final >= 1){
        opt_fix <- j
        opt_opt <- m
        break
      }
    }
    
    if(exists("opt_fix")==FALSE){
      opt_fix <- ncol(matrix_top_fix)
      opt_opt <- ncol(matrix_top_opt)
    }
    

    return(c(matrix_top_fix[,opt_fix], matrix_top_opt[,opt_opt]))

  }
