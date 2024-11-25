generate_protein <- function(seq, num, phi, psi, A){
  phi <- phi*pi/180
  psi <- psi*pi/180
  
  #add dummy ALA at the end
  seq <- c(seq, 1)
  phi <- c(phi, -60)
  
  seq_len <- length(seq)
  num_atoms <- 0
  
  for(i in 1:(seq_len-1)){
    num_atoms <- num_atoms + A[[seq[i]]]$last_meta - A[[seq[i]]]$first_meta + 1
  }
  num_atoms <- as.numeric(num_atoms)
  
  #Compute atom omission map
  #####################################
  kept_atom_cnt <- 0
  atom_cnt <- 0
  atoms_to_keep <- rep(TRUE, num_atoms)
  atoms_map <- matrix(rep(NaN, num_atoms*2), ncol=2)
  atoms_map[,1] <- 1:num_atoms
  hresidue_start <- rep(NaN,seq_len-1)
  
  for(i in 1:(seq_len-1)){
    hresidue_start[i] <- kept_atom_cnt
    cur_num_atoms <- as.numeric(A[[seq[i]]]$last_meta - A[[seq[i]]]$first_meta + 1)
    atoms_to_keep[(atom_cnt+1):(atom_cnt+cur_num_atoms)] <- as.numeric(A[[seq[i]]]$omission_map)
    atom_cnt <- atom_cnt + cur_num_atoms
    kept_atom_cnt <- kept_atom_cnt + sum(A[[seq[i]]]$omission_map)
  }
  
  index_kept <- which(atoms_to_keep != 0)
  num_atoms_kept <- length(index_kept)
  atoms_map[index_kept,2] <- 1:num_atoms_kept
  ##################################################
  
  X <- matrix(rep(0, 3*num_atoms), nrow=3)
  bonds <- matrix(rep(NaN, 8*num_atoms), ncol=2)
  info <- matrix(rep(NaN, 6*num_atoms), nrow=6)
  atom_names <- matrix(list(), nrow=as.numeric(num_atoms))
  atom_types <- matrix(0, nrow=as.numeric(num_atoms))
  planes <- matrix(rep(0,(seq_len-1)*6), ncol=6)
  sides <- matrix(list(), ncol=1, nrow=(seq_len-1))
  schains <- matrix(list(), ncol=1, nrow=(seq_len-1))
  new_schains <- matrix(list(), ncol=1, nrow=(seq_len-1))
  residue <- rep(NaN, seq_len-1)
  residue_start <- matrix(rep(NaN, seq_len), ncol=1)
  
  #First residue only has valid PSI angle
  res <- seq[1]
  curX <- A[[res]]$X
  max_j <- nrow(A[[res]]$dihedral$name)
  for(j in 1:max_j){
    if(A[[res]]$dihedral$name[[j]] == "PSI"){
      psi_index <- as.numeric(A[[res]]$dihedral$atoms[j,][1:3])
      break
    }
  }
  psiX <- curX[,psi_index]
  
  start <- 0
  bond_cnt <- 0
  for(i in 1:(seq_len-1)){
    res <- seq[i]
    nextres <- seq[i+1]
    last <- A[[res]]$last_meta
    first <- A[[res]]$first_meta
    cur_num_atoms <- last - first + 1
    temp_X <- matrix(rep(0,3*cur_num_atoms), nrow=3)
    residue_start[i] <- start
    ############################
    #Find the next N by psi
    next_N <- ditocord(psiX,res,nextres,psi[i],"PSI")
    ############################
    cur_N <- psiX[,1]
    cur_CA <- psiX[,2]
    cur_C <- psiX[,3]
    ###########################
    #Find the atoms in a plane
    PFinput <- cbind(cur_CA, cur_C, next_N)
    outX <- PlaneFinder(PFinput, res, nextres) #2x6 matrix
    cur_O <- outX[,2]
    
    if(res != 15){    #i.e. residual is PRO
      
      planes[i,] <- start + c(3, (last-first)+c(0,1), cur_num_atoms+c(1,2,3))
      if(i == 1){
        cur_HN <- A[[res]]$X[,4]
      }else{
        cur_HN <- next_HN
      }
      cur_backbone <- cbind(cur_N, cur_HN, cur_CA, cur_C, cur_O)
      index_backbone <- c(1,2,3, (last-first) + c(0,1))
      index_sidechain <- 4:(last-first-1)
      atom_types[start + index_backbone] <- 1    #backbone = 1
      atom_types[start + index_sidechain] <- 2   #sidechain = 2
      temp_X[,index_backbone] <- cur_backbone
      ###################################################
      #Atoms in sides[i] are N, CA, C, and side chain atoms
      sides[[i,1]] <- start + c(1, 3, last-first, index_sidechain)
      num_cliq <- length(A[[res]]$cliqs)
      schains[[i,1]] <- matrix(list(), nrow=num_cliq, ncol=1)
      
      for(q in 1:num_cliq){
        schains[[i,1]][[q,1]] <- A[[res]]$cliqs[[q]] + residue_start[i]
      }
      
      num_new_cliq <- length(A[[res]]$cliqs_woh)
      new_schains[[i,1]] <- matrix(list(), nrow=num_new_cliq, ncol=1)
      for(q in 1:num_new_cliq){
        new_schains[[i,1]][[q,1]] <- A[[res]]$cliqs_woh[[q]] + residue_start[i]
      }
      
      cur_sidechain <- SidechainFinder(cur_backbone[,c(1,3,4)], index_backbone[c(1,3,4)],A[[res]])
      cur_sidechain <- cur_sidechain[,index_sidechain]
      temp_X[,index_sidechain] <- cur_sidechain
    }else{ #For PRO
      planes[i,] <- start + c(3, (last-first)+c(0,1), cur_num_atoms+c(1,2,3))
      if(i == 1){
        cur_CD <- A[[res]]$X[,4]
      }else{
        cur_CD <- next_CD
      }
      
      cur_backbone <- cbind(cur_N, cur_CD, cur_CA, cur_C, cur_O)
      index_backbone <- c(1,2,3, last-first + c(0,1))
      index_sidechain <- 4:(last-first-1)
      atom_types[start + index_backbone] <- 1
      atom_types[start + index_sidechain] <- 2
      temp_X[,index_backbone] <- cur_backbone
      
      #Atoms in sides[i] are N, CA, CD, are side chain atoms
      sides[[i,1]] <- start + c(1, 3, last-first, 2, index_sidechain)
      num_cliq <- length(A[[res]]$cliqs)
      schains[[i,1]] <- matrix(list(), nrow=num_cliq, ncol=1)
      
      for(q in 1:num_cliq){
        schains[[i,1]][[q,1]] <- A[[res]]$cliqs[[q]] + residue_start[i]
      }
      
      num_new_cliq <- length(A[[res]]$cliqs_woh)
      new_schains[[i,1]] <- matrix(list(), nrow=num_new_cliq, ncol=1)
      for(q in 1:num_new_cliq){
        new_schains[[i,1]][[q,1]] <- A[[res]]$cliqs_woh[[q]] + residue_start[i]
      }
      
      cur_sidechain <- SidechainFinder(cur_backbone[,c(1,3,4)], index_backbone[c(1,3,4)],A[[res]])
      cur_sidechain <- cur_sidechain[,index_sidechain]
      temp_X[,index_sidechain] <- cur_sidechain
    }
    
    #Put the computed residue into the protein
    X[,(start+1):(start+cur_num_atoms)] <- temp_X
    residue[(start+1):(start+cur_num_atoms)] <- num[i]
    
    for(j in 1:cur_num_atoms){
      atom_names[start+j] <- A[[res]]$atom$name[j+first-1]

      if(regexpr("PSEUD", atom_names[start+j]) != -1){
        atom_types[start+j] <- 3
      }
      
      info[,start+j] <- A[[res]]$info[,j+first-1]
      
      if(info[6, start+j]){
        info[6, start+j] <- info[6,start+j] + residue_start[i]
      }
    }
    
    bond_increment <- nrow(A[[res]]$meta_bonds)
    bonds[bond_cnt+(1:bond_increment),] <- start + as.matrix(A[[res]]$meta_bonds)
    bond_cnt <- bond_cnt + bond_increment
    ##################################
    ##################################
    #Peptide bond between C[i] and N[i+1]
    if(i < (seq_len -1)){
      bond_cnt <- bond_cnt + 1
      if(res != 15){
        bonds[bond_cnt,] <- start + c(index_backbone[4], cur_num_atoms+1)
      }else{
        bonds[bond_cnt,] <- start + c(index_backbone[3], cur_num_atoms+1)
      }
    }
    ###################################
    next_CA <- outX[,6]
    next_HN <- outX[,5]
    next_CD <- outX[,5]
    
    #atoms required for determining next C' by PHI angle
    phiX <- cbind(cur_C, next_N, next_CA)
    
    #Find the next C' by PHI
    next_C <- ditocord(phiX, res, nextres,phi[i+1],"PHI")
    
    #For the next residues PSI angle
    psiX <- cbind(next_N, next_CA, next_C)
    
    start <- start + cur_num_atoms
  }
  
  if(sum(is.nan(bonds[,1])) > 0){
    bonds <- bonds[-which(is.nan(bonds[,1])),]
  }
  if(sum(is.nan(info[,1])) > 0){
    info <- info[,-which(is.nan(info[1,]))]
  }
  if(sum(is.nan(residue_start)) > 0){
    residue_start <- residue_start[-which(is.nan(residue_start))]
  }

  Comp <- list(info=info,
               bonds=bonds,
               atom_names=atom_names,
               atom_types=atom_types,
               planes = planes,
               sides=sides,
               residue=residue,
               schains = schains,
               residue_bias=residue_start,
               seq = seq,
               seq_num=num,
               residue_type = c(),
               Cq = c())

  residue_type <- rep(NaN, length(residue))
  
  for(i in 1:length(residue_type)){
    temp_res <- residue[i]
    ind <- which(num == temp_res)
    residue_type[i] <- seq[ind]
  }
  
  Comp$residue_type <- residue_type
  
  #Form the clique matrix
  
  num_atoms <- ncol(X)
  num_planes <- nrow(Comp$planes)
  num_res <- length(Comp$schains)
  num_sides <- 0
  for(i in 1:num_res){
    cliqs_per_side <- nrow(Comp$schains[[i,1]])
    num_sides <- num_sides + cliqs_per_side
  }
  raw_estimate <- num_planes*6+num_sides*30
  I <- rep(NaN,raw_estimate)
  J <- rep(NaN,raw_estimate)
  V <- rep(1,raw_estimate)
  
  #############################################
  I[1:3] <- 1:3
  J[1:3] <- 1
  start <- 3
  cliq_cnt <- 1
  
  for(i in 1:num_planes){
    cliq_cnt <- cliq_cnt + 1
    num_atoms_clique <- sum(Comp$planes[i,] <= num_atoms)
    I[(start+1):(start+num_atoms_clique)] <- Comp$planes[i,Comp$planes[i,] <= num_atoms]
    J[(start+1):(start+num_atoms_clique)] <- cliq_cnt
    start <- start + num_atoms_clique
  }
  
  for(i in 1:num_res){
    cliqs_per_side <- nrow(Comp$schains[[i,1]])
    for(j in 1:cliqs_per_side){
      cliq_cnt <- cliq_cnt + 1
      num_atoms_clique <- sum(as.matrix(Comp$schains[[i,1]][[j,1]]) <= num_atoms)
      I[(start+1):(start+num_atoms_clique)] <- Comp$schains[[i,1]][[j,1]][Comp$schains[[i,1]][[j,1]] <= num_atoms]
      J[(start+1):(start+num_atoms_clique)] <- cliq_cnt
      start <- start + num_atoms_clique
    }
  }
  
  index_bad <- is.nan(I)
  if(sum(index_bad) > 0){
    I <- I[-which(index_bad)]
    J <- J[-which(index_bad)]
    V <- V[-which(index_bad)]
  }
  
  #Create cq matrix
  cq <- matrix(rep(0,num_atoms*cliq_cnt), nrow=num_atoms)
  for(i in 1:length(I)){
    cq[I[i],J[i]] <- V[i]
  }
  Comp$Cq <- cq
  ##############################################################
  #Drop some hydrogen atoms
  
  new_X <- X[,which(atoms_to_keep==1)]
  
  hComp <- list(info = c(),
                atom_names = c(),
                atom_types = c(),
                residue = c(),
                seq = c(),
                num_seq = c(),
                residue_bias=c(),
                residue_type = c(),
                bonds = c(),
                planes = c(),
                sides = c(),
                schains = c(),
                Cq = c(),
                atoms_map = c())
  
  hComp$info <- Comp$info[,which(atoms_to_keep==1)]
  hComp$atom_names <- Comp$atom_names[which(atoms_to_keep==1)]
  hComp$atom_types <- Comp$atom_types[which(atoms_to_keep==1)]
  hComp$residue <- Comp$residue[which(atoms_to_keep==1)]
  hComp$seq <- Comp$seq
  hComp$num_seq <- Comp$seq_num
  hComp$residue_bias <- hresidue_start
  
  residue_type <- rep(NaN,length(hComp$residue))
  for(i in 1:length(residue_type)){
    temp_res <- hComp$residue[i]
    ind <- which(hComp$num_seq == temp_res)
    residue_type[i] <- hComp$seq[ind]
  }
  
  hComp$residue_type <- residue_type
  
  #New Bonds
  num_bonds <- nrow(bonds)
  new_bonds <- matrix(rep(NaN,2*num_bonds), ncol=2)
  for(i in 1:num_bonds){
    ti <- bonds[i,1]
    tj <- bonds[i,2]
    if(!is.nan(atoms_map[ti,2]*atoms_map[tj,2])){
      new_bonds[i,] <- c(atoms_map[ti,2], atoms_map[tj,2])
    }
  }
  if(sum(is.nan(new_bonds[,1])) > 0){
    new_bonds <- new_bonds[-which(is.nan(new_bonds[,1])),]
  }
  hComp$bonds <- new_bonds
  
  #New Planes
  num_planes <- nrow(planes)
  new_planes <- matrix(rep(NaN,6*num_planes), ncol=6)
  for(i in 1:num_planes){
    for(j in 1:6){
      if(planes[i,j] <= num_atoms){
        new_planes[i,j] <- atoms_map[planes[i,j],2]
        if(is.nan(new_planes[i,j])){
          stop("There is an error in the mapping atoms")
        }
      }
    }
  }
  hComp$planes <- new_planes
  
  #New Side Chains
  new_sides <- sides
  for(i in 1:num_res){
    num_atoms_in_side <- length(sides[[i,1]])
    index_bad <- rep(FALSE, num_atoms_in_side)
    for(j in 1:num_atoms_in_side){
      if(is.nan(atoms_map[sides[[i,1]][j],2])){
        index_bad[j] <- TRUE
      }
    }
    if(sum(index_bad) > 0){
      new_sides[[i,1]] <- new_sides[[i,1]][!index_bad]
    }
    new_sides[[i,1]] <- t(atoms_map[new_sides[[i,1]],2])
  }
  hComp$sides <- new_sides
  
  #New Side Chain Cliques
  for(i in 1:num_res){
    num_cliq <- nrow(new_schains[[i,1]])
    for(q in 1:num_cliq){
      cliq_len <- length(new_schains[[i,1]][[q,1]])
      for(j in 1:cliq_len){
        new_schains[[i,1]][[q,1]][j] <- atoms_map[as.numeric(new_schains[[i,1]][[q,1]][j]),2]
        if(is.nan(as.numeric(new_schains[[i,1]][[q,1]][j]))){
          stop("There is an error in the mapping atoms")
        }
      }
    }
  }
  hComp$schains <- new_schains
  
  #Form the Clique
  num_atoms <- ncol(new_X)
  num_planes <- nrow(hComp$planes)
  num_res <- length(hComp$schains)
  num_sides <- 0
  for(i in 1:num_res){
    cliqs_per_side <- nrow(hComp$schains[[i,1]])
    num_sides <- num_sides + cliqs_per_side
  }
  raw_estimate <- num_planes*6 + num_sides*30
  I <- rep(NaN,raw_estimate)
  J <- rep(NaN,raw_estimate)
  K <- rep(1,raw_estimate)
  
  ##################################################
  I[1:3] <- 1:3
  J[1:3] <- 1
  start <- 3
  cliq_cnt <- 1
  for(i in 1:num_planes){
    cliq_cnt <- cliq_cnt + 1
    index <- hComp$planes[i,] <= num_atoms
    if(any(is.na(index))){
      index[is.na(index)] <- FALSE
    }
    num_atoms_clique <- sum(index)
    I[(start+1):(start+num_atoms_clique)] <- hComp$planes[i,index]
    J[(start+1):(start+num_atoms_clique)] <- cliq_cnt
    start <- start + num_atoms_clique
  }
  
  for(i in 1:num_res){
    cliqs_per_side <- length(hComp$schains[[i,1]])
    for(j in 1:cliqs_per_side){
      cliq_cnt <- cliq_cnt + 1
      
      index <- hComp$schains[[i,1]][[j,1]] <= num_atoms
      if(any(is.na(index))){
        index[is.na(index)] <- FALSE
      }
      
      num_atoms_clique <- sum(index)
      I[(start+1):(start+num_atoms_clique)] <- hComp$schains[[i,1]][[j,1]][index]
      J[(start+1):(start+num_atoms_clique)] <- cliq_cnt
      start <- start + num_atoms_clique
    }
  }
  
  index_bad <- is.nan(I)
  if(sum(index_bad) > 0){
    I <- I[-which(index_bad)]
    J <- J[-which(index_bad)]
    K <- K[-which(index_bad)]
  }
  
  #Create cq matrix
  cq <- matrix(rep(0,num_atoms*cliq_cnt), nrow=num_atoms)
  for(i in 1:length(I)){
    cq[I[i],J[i]] <- K[i]
  }
  hComp$Cq <- cq
  hComp$atoms_map <- atoms_map
  
  return(list(X=X, Comp=Comp, new_X = new_X, hComp = hComp))
  
}