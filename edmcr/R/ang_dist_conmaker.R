ang_dist_conmaker <- function(phi_cons, psi_cons, Comp){
  
  atom_names <- unlist(Comp$atom_names)
  residue <- Comp$residue
  seq_num <- Comp$seq_num
  bias <- Comp$residue_bias
  
  num_phi <- nrow(phi_cons)
  num_psi <- nrow(psi_cons)
  num_all <- num_phi + num_psi
  
  lo_cons <- matrix(rep(NaN,4*num_all), nrow=num_all)
  up_cons <- matrix(rep(NaN,4*num_all), nrow=num_all)
  
  for(i in 1:num_phi){
    if(phi_cons[i,2] - phi_cons[i,1] < 360 && i > 1){
      dmin = -Inf
      d1 <- DCC(phi_cons[i,1])
      d2 <- DCC(phi_cons[i,2])
      if(phi_cons[i,2]*phi_cons[i,1] > 0){
        dmax <- max(d1,d2)
        dmin <- min(d1,d2)
      }else{
        dmax <- max(d1,d2)
      }
      
      atoms_cur <- atom_names[which(residue == seq_num[i])]
      atoms_per <- atom_names[which(residue == seq_num[i-1])]
      cur_index <- which(atoms_cur == "C") + bias[i]
      per_index <- which(atoms_per == "C") + bias[i-1]
      up_cons[i,] <- c(per_index, cur_index, dmax, -2)
      if(!is.infinite(dmin)){
        lo_cons[i,] <- c(per_index, cur_index, dmin, -2)
      }
    }
  }
  
  for(i in 1:num_psi){
    if(psi_cons[i,2] - psi_cons[i,1] < 360 && i < num_psi){
      dmin = -Inf
      d1 <- DNN(psi_cons[i,1])
      d2 <- DNN(psi_cons[i,2])
      if(psi_cons[i,2]*psi_cons[i,1] > 0){
        dmax <- max(d1,d2)
        dmin <- min(d1,d2)
      }else{
        dmax <- max(d1,d2)
      }
      
      atoms_cur <- atom_names[which(residue == seq_num[i])]
      atoms_nex <- atom_names[which(residue == seq_num[i+1])]
      cur_index <- which(atoms_cur == "N") + bias[i]
      nex_index <- which(atoms_nex == "N") + bias[i+1]
      up_cons[num_phi + i,] <- c(cur_index, nex_index, dmax, -2)
      if(!is.infinite(dmin)){
        lo_cons[num_phi + i,] <- c(cur_index, nex_index, dmin, -2)
      }
    }
  }
  
  if(sum(is.nan(lo_cons[,1])) > 0){
    lo_cons <- lo_cons[-which(is.nan(lo_cons[,1])),]
  }
  
  if(sum(is.nan(up_cons[,1]))){
    up_cons <- up_cons[-which(is.nan(up_cons[,1])),]
  }
  
  return(list(lo_cons = lo_cons, up_cons=up_cons))
  
}

DCC <- function(phi){
  phi <- phi*pi/180
  out <- 3.221 - 0.4886*cos(1.044*phi)
}

DNN <- function(psi){
  psi <- psi*pi/180
  out <- 3.157 - 0.5022*cos(1.046*psi)
}