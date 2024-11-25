#Read Data input in the same style as CYANA

#Read Sequence files (i.e. ".seq" files) in CYANA form. Two columns, as follows:
##Column 1: Residue name (Case sensitive)
##Column 2: residue number
read.seq <- function(raw_data){
  #raw_data <- read.table(filename)
  
  first_residue <- 0
  residue_cnt <- 0
  max_i <- nrow(raw_data)
  
  seq <- rep(NaN, max_i)
  num <- rep(NaN, max_i)
  
  aminos <- c("ALA", "ARG", "ASN", "ASP", "CYS", "GLU", "GLN", "GLY", "HIS", "ILE", "LEU", "LYS",
              "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL", "CYSS")
  
  #Process the residue names first
  for(i in 1:max_i){
    for(j in 1:2){ #Process the residue name, then the residue number
      temp_name <- raw_data[i,j]
      if(j == 1){
        residue_cnt <- residue_cnt + 1
        seq[residue_cnt] <- which(aminos == temp_name)
      }else{
        if(first_residue == 0){
          first_residue <- temp_name
          num[residue_cnt] <- first_residue
        }else{
          num[residue_cnt] <- temp_name
        }
      }
    }
  }
  
  index_positive <- which(num > 0)
  index_positive <- c(index_positive, residue_cnt + 1)

  for(i in 1:(length(index_positive)-1)){
    len <- index_positive[i+1] - index_positive[i]
    num[index_positive[i]:(index_positive[i+1]-1)] <- num[index_positive[i]] + 0:(len-1)
  }
  
  return(list(seq=seq, num=num))
}

#Read the distance restraint file (.upl) in CYANA format. 7 columns
## Column 1: Residue number (first residue)
## Column 2: residue name (first residue)
## Column 3: atom name (first residue)
## Column 4: Residue number (second residue)
## Column 5: Residue name (second residue)
## Column 6: atom name (second residue)
## Column 7: The distance limit (measured in angstroms)
read.dist <- function(raw_data, A, type=0){
  #raw_data <- read.table(uplFile,as.is=TRUE)
  
  #Remove empty lines
  XPLOR_format <- 0
  max_i <- nrow(raw_data)
  index_empty <- rep(FALSE, max_i)
  for(i in 1:max_i){
    if(length(raw_data[i,]) == 0){
      index_empty[i] <- TRUE 
    }else{
      if(any(regexpr("HB1[^1-9]",raw_data[i,]) != -1) || any(regexpr("HA1[^1-9]",raw_data[i,]) != -1)){
        XPLOR_format <- 1
      }
    }
  }
  
  if(any(index_empty)){
    raw_data <- raw_data[-index_empty]
  }
  max_i <- nrow(raw_data)
  
  dist_con <- data.frame(sres = rep(0,max_i),
                         stype = rep(0,max_i),
                         satom = rep("A",max_i),
                         tres = rep(0,max_i),
                         ttype = rep(0,max_i),
                         tatom = rep("A",max_i),
                         dist = rep(0,max_i),
                         peak = rep(0,max_i),stringsAsFactors = FALSE)
  index_bad <- rep(FALSE, max_i) 
  
  aminos <- c("ALA", "ARG", "ASN", "ASP", "CYS", "GLU", "GLN", "GLY", "HIS", "ILE", "LEU", "LYS",
              "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL", "CYSS")
  
  for(i in 1:max_i){
    
    #temp_line <- raw_data[i,]
    
    #Handle the S Part
    dist_con[i,]$sres <- raw_data[i,1]
    stype <- raw_data[i,2]
    dist_con[i,]$stype <- which(aminos == stype)
    dist_con[i,]$satom <- raw_data[i,3]
    dist_con[i,]$peak <- 0
    
    delta_s <- 0
    if(dist_con[i,]$satom == "HN"){
      dist_con[i,]$satom <- "H"
    }
    
    if(XPLOR_format){
      temp <- atom_nom(stype, dist_con[i,]$satom, "xplor")
      if(length(temp$natom) > 0){
        dist_con[i,]$satom <- temp$natom
      }
    }
    old_atom <- dist_con[i,]$satom
    if(type > 0){
      Out <- atom_nom(stype, dist_con[i,]$satom, "homitted")
      temp <- Out$natom
      flag <- Out$delta
      if(nchar(temp) > 0){
        dist_con[i,]$satom <- temp
      }
    }else{
      Out <- atom_nom(stype, dist_con[i,]$satom, "full")
      temp <- Out$natom
      flag <- Out$delta
      if(nchar(temp) > 0){
        dist_con[i,]$satom <- temp
      }
    }
    if(flag > 0){
     tempName <- A[[dist_con[i,]$stype]]$atom$name
     ind1 <- which(tempName == dist_con[i,]$satom)
     ind2 <- which(tempName == old_atom)
     delta_s <- sqrt(sum((A[[dist_con[i,]$stype]]$X[,ind1] - A[[dist_con[i,]$stype]]$X[,ind2])^2))
    }
    
    #Handle the T Part
    dist_con[i,]$tres <- raw_data[i,4]
    ttype <- raw_data[i,5]
    dist_con[i,]$ttype <- which(aminos == ttype)
    dist_con[i,]$tatom <- raw_data[i,6]
    
    delta_t <- 0
    if(dist_con[i,]$tatom == "HN"){
      dist_con[i,]$tatom <- "H"
    }
    
    if(XPLOR_format){
      temp <- atom_nom(ttype, dist_con[i,]$tatom, "xplor")
      if(length(temp$natom) > 0){
        dist_con[i,]$tatom <- temp$natom
      }
    }
    old_atom <- dist_con[i,]$tatom
    if(type > 0){
      Out <- atom_nom(ttype, dist_con[i,]$tatom, "homitted")
      temp <- Out$natom
      flag <- Out$delta
      if(nchar(temp) > 0){
        dist_con[i,]$tatom <- temp
      }
    }else{
      Out <- atom_nom(ttype, dist_con[i,]$tatom, "full")
      temp <- Out$natom
      flag <- Out$delta
      if(nchar(temp) > 0){
        dist_con[i,]$tatom <- temp
      }
    }
    if(flag > 0){
      tempName <- A[[dist_con[i,]$ttype]]$atom$name
      ind1 <- which(tempName == dist_con[i,]$tatom)
      ind2 <- which(tempName == old_atom)
      delta_t <- sqrt(sum((A[[dist_con[i,]$ttype]]$X[,ind1] - A[[dist_con[i,]$ttype]]$X[,ind2])^2))
    }
    
    if(raw_data[i,7] == 0){
      index_bad[i] <- 1
    }
    
    dist_con[i,]$dist <- raw_data[i,7] + delta_s + delta_t
    
    if(any(regexpr("O",dist_con[i,]$satom) != -1) || any(regexpr("O",dist_con[i,]$tatom) != -1)  || any(regexpr("S",dist_con[i,]$satom) != -1) || any(regexpr("S",dist_con[i,]$tatom) != -1)){
      dist_con[i,]$peak <- -1
    }
  }
  
  if(any(index_bad)){
    dist_con <- dist_con[-index_bad,]
  }
  max_new <- nrow(dist_con)
  
  if(max_new != max_i){
    warning(paste("zero-distance restraints omitted:", max_i - max_new))
  }
  
  return(dist_con)
}

#Read the torsion angle restraint file in CYANA format (i.e. a ".aco" file). Five columns as follows:
## Column 1: Residue Number
## Column 2: amino acid
## COlumn 3: angle identifier (phi or psi)
## Column 4: the angle's lower limit
## Column 5: the angle's upper limit
read.angle <- function(raw_data, num){
  
  len_seq <- length(num)
  
  phi <- matrix(rep(1,len_seq*2), ncol=2)
  psi <- matrix(rep(1,len_seq*2), ncol=2)
  
  phi[,1] <- phi[,1]*-180
  phi[,2] <- phi[,2]*180
  
  psi[,1] <- psi[,1]*-180
  psi[,2] <- psi[,2]*180
  
  #raw_data <- read.table(filename)
  max_i <- nrow(raw_data)
  
  #remove residues with ambiguous torsion angle restraints
  bad_residues <- matrix(rep(NaN,2*len_seq), nrow=2)
  bad_residues[1,] <- num
  num_fields <- matrix(rep(NaN,max_i), ncol=1)
  for(i in 1:max_i){
    #temp_str <- raw_data[1,]
    num_fields[i] <- length(raw_data[i,])
    if(raw_data[i,num_fields[i]] == "OR"){
      #temp_res <- raw_data[i,1]
      bad_residues[2,which(bad_residues[1,] == raw_data[i,1])] <- 1
    }
  }
  
  aminos <- c("ALA", "ARG", "ASN", "ASP", "CYS", "GLU", "GLN", "GLY", "HIS", "ILE", "LEU", "LYS",
              "MET", "PHE", "PRO", "SER", "THR", "TRP", "TYR", "VAL", "CYSS")
  
  for(i in 1:max_i){
    #temp_data <- raw_data[i,]
    
    temp_res <- raw_data[i,1]
    temp_res_name <- raw_data[i,2]
    temp_ang_name <- raw_data[i,3]
    temp_ang_low <- raw_data[i,4]
    temp_ang_hi <- raw_data[i,5]
    index_num <- which(num == temp_res)
    
    if(length(index_num) > 0 && length(which(aminos == temp_res_name)) > 0){
      if(is.nan(bad_residues[2, index_num])){
        switch(as.character(temp_ang_name),
               PHI = {phi[index_num,1] = temp_ang_low
                      phi[index_num,2] = temp_ang_hi},
               PSI = {psi[index_num,1] = temp_ang_low
                      psi[index_num,2] = temp_ang_hi})
      }
    }
  }
  
  return(list(phi=phi, psi=psi))
  
} 

read.hbond <- function(file_name, write_file_name){
  
}