solve_sdpt3 <- function(U, equality_cons, upper_bounds, lower_bounds, vdw_bounds,f){
  
  UTe <- as.matrix(colSums(U))
  V <- qr.Q(qr(UTe), complete=TRUE)
  U <- U %*% V[,2:ncol(V)]
  
  hydrogen_factor <- f[1]
  torsionu_factor <- f[2]
  torsionl_factor <- f[5]
  vdw_factor <- f[4]
  
  num_upper_bounds <- nrow(upper_bounds)
  num_equality_cons <- nrow(equality_cons)
  num_lower_bounds <- nrow(lower_bounds)
  num_vdw_bounds <- nrow(vdw_bounds)
  
  num_atoms <- nrow(U)
  sdp_dim <- ncol(U)
  
  num_up_slacks <- 2*num_upper_bounds
  num_lo_slacks <- 2*num_lower_bounds
  num_vdw_slacks <- 2*num_vdw_bounds
  
  gamma <- 50*num_atoms/num_upper_bounds
  lambda <- 0*num_atoms/num_upper_bounds
  num_slacks <- num_up_slacks + num_lo_slacks + num_vdw_slacks
  
  num_cons <- num_equality_cons + num_upper_bounds + num_lower_bounds + num_vdw_bounds
  
  b <- matrix(0, num_cons, 1)
  Vec_cons <- matrix(0, sdp_dim, num_cons)
  
  S_slacks <- matrix(0, num_slacks, 3)
  C_slacks <- matrix(0, num_slacks, 1)
  
  cons_cnt <- 1
  if(num_equality_cons > 0){
    for(i in 1:num_equality_cons){
      ti <- equality_cons[i,1]
      tj <- equality_cons[i,2]
      tmpU <- U[ti,]-U[tj,]
      Vec_cons[,cons_cnt] <- tmpU 
      b[cons_cnt] <- equality_cons[i,3]^2
      cons_cnt <- cons_cnt+1
    }
  }
  
  slack_cnt <- 1
  if(num_upper_bounds > 0){
    for(i in 1:num_upper_bounds){
      ti <- upper_bounds[i,1]
      tj <- upper_bounds[i,2]
      tmpU <- U[ti,]-U[tj,]
      Vec_cons[,cons_cnt] <- tmpU 
      b[cons_cnt] <- upper_bounds[i,3]^2
      S_slacks[slack_cnt,] <- c(slack_cnt, cons_cnt, 1)
      C_slacks[slack_cnt] <- lambda
      slack_cnt <- slack_cnt + 1
      S_slacks[slack_cnt,] <- c(slack_cnt, cons_cnt, -1)
      
      if(upper_bounds[i,4] >= 0){
        C_slacks[slack_cnt] <- gamma
      }else if(upper_bounds[i,4] == -1){ #hydrogen bonds
        C_slacks[slack_cnt] <- hydrogen_factor*gamma
      }else if(upper_bounds[i,4] == -2){ #torsion angles
        C_slacks[slack_cnt] <- torsionl_factor*gamma
      }
      
      slack_cnt <- slack_cnt + 1
      cons_cnt <- cons_cnt + 1
    }
  }
  
  if(num_lower_bounds > 0){
    for(i in 1:num_lower_bounds){
      ti <- lower_bounds[i,1]
      tj <- lower_bounds[i,2]
      tmpU <- U[ti,] - U[tj,]
      Vec_cons[,cons_cnt] <- tmpU
      b[cons_cnt] <- lower_bounds[i,3]^2
      S_slacks[slack_cnt,] <- c(slack_cnt, cons_cnt, -1)
      C_slacks[slack_cnt] <- 0
      slack_cnt <- slack_cnt + 1
      S_slacks[slack_cnt,] <- c(slack_cnt, cons_cnt, 1)
      
      if(lower_bounds[i,4] == -2){
        C_slacks[slack_cnt] <- torsionu_factor*gamma
      }
      
      slack_cnt <- slack_cnt + 1
      cons_cnt <- cons_cnt + 1
    }
  }
  
  if(num_vdw_bounds > 0){
    for(i in 1:num_vdw_bounds){
      ti <- vdw_bounds[i,1]
      tj <- vdw_bounds[i,2]
      tmpU <- U[ti,] - U[tj,]
      Vec_cons[,cons_cnt] <- tmpU
      b[cons_cnt] <- vdw_bounds[i,3]^2
      S_slacks[slack_cnt,] <- c(slack_cnt, cons_cnt, -1)
      C_slacks[slack_cnt] <- 0
      slack_cnt <- slack_cnt + 1
      S_slacks[slack_cnt,] <- c(slack_cnt, cons_cnt, +1)
      C_slacks[slack_cnt] <- vdw_factor*gamma
      slack_cnt <- slack_cnt + 1
      cons_cnt <- cons_cnt + 1
    }
  }
  
  num_indep_cons <- length(b)
  
  Cs <- matrix(list(), nrow=2, ncol=1)
  Cs[[1,1]] <- C_slacks
  Cs[[2,1]] <- -diag(1,sdp_dim)
  
  blk <- matrix(list(),2,3) #2x3 cell array of matrices (matches cell(2,3) in matlab)
  At <- matrix(list(),2,3) 
  
  blk[[1,1]] <- "l"
  blk[[1,2]] <- as.matrix(num_slacks)
  blk[[2,1]] <- "s"
  blk[[2,2]] <- as.matrix(sdp_dim)
  blk[[2,3]] <- matrix(1, 1, num_indep_cons)
  
  
  tmp <- matrix(rep(0,num_slacks*num_cons), ncol=num_cons)
  for(i in 1:length(S_slacks[,1])){
    tmp[S_slacks[i,1], S_slacks[i,2]] <- S_slacks[i,3]
  }
  
  At[[1,1]] <- tmp
  At[[2,1]] <- matrix(,nrow=0,ncol=0)
  At[[2,2]] <- Vec_cons
  At[[2,3]] <- matrix(1, num_indep_cons,1)
  
  X <- sqlp(blk,At,Cs,b)
  
  return(X)
  
}
