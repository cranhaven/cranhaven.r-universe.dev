#' Semidefinite Programming-based Protein Structure Determination
#' 
#' \code{sprosr} compute the three dimensional strucutre of a protein 
#' molecule using its amino acid sequences using the semidefinite programming-based
#' protein structure determination (SPROS) method of Ramandi (2011)
#' 
#' @details 
#' 
#' The input files requires by sprosr follow the typical CYANA format. Each is a table with the following columns (no headers required).
#' 
#' Sequence File (seq) \cr
#' column 1: amino acid residue name  \cr
#' column 2: residue number \cr
#' 
#' Torsion Angle Restraint File (aco)  \cr
#' column 1: residue number (corresponding to seq file)  \cr
#' column 2: amino acid residue name  \cr
#' column 3: angle identifier, one of PHI or PSI  \cr
#' column 4: the lower limit of the angle specified in column 3  \cr
#' column 5: the upper limit of the angle specified in column 3  \cr
#' 
#' Distance Restraint File (upl) \cr
#' column 1: residue number of the first atom (corresponding to seq file) \cr
#' column 2: amino acid residue name of the first atom \cr
#' column 3: atom name of the first atom \cr
#' column 4: residue number of the second atom (corresponding to seq file) \cr
#' column 5: amino acid residue name of the second atom \cr
#' column 6: atom name of the second atom \cr
#' column 7: upper distance limit (in Angstroms) \cr
#' 
#' @param seq A table containing the amino acid sequence of the protein in CYANA .seq format
#' @param aco A table containing the angle constraint information in CYANA .aco format
#' @param upl A table containing the distance constraint information in CYANA .upl format
#' @param hydrogen_omission Should side-chain hydrogen atoms be omitted? TRUE/FALSE. Default is FALSE
#' @param in_min_res User overwrite of the minimum residue number.  
#' @param in_max_res User overwrite of the maximum residue number.
#' @param f Vector of length five detailing the multiplicative factors to be used. See details for more.
# #' @param hbond The file path locating the hydrogen bond restraint file. See details for more information on the format of this file.
# #' @param hbondWrite The file path to which the updated hydrogen bond restraint file is written.
#' 
#' @return
#' \item{X}{Matrix containing the three dimensional point configuration of the protein structure.}
#' \item{report}{A list containing the final violations of the protein}
#' 
#' 
#' @references 
#' Ramandi, Babak A., (2011). New Approaches to Protein NMR Automation. PhD Thesis.
#' https://uwspace.uwaterloo.ca/bitstream/handle/10012/6389/Alipanahi_Ramandi_Babak.pdf;sequence=1
#' 
#' @export
#' @import sdpt3r
#' @importFrom utils data
sprosr <- function(seq, aco, upl, hydrogen_omission=1, f=c(10,10,10,10,10), in_max_res=NULL, in_min_res=NULL){
  
  ############################################################
  ################### READ IN DATA FILES #####################
  ############################################################
  
  #Read in sequence file
  temp.seq <- read.seq(seq)
  seq <- temp.seq$seq
  num <- temp.seq$num
  
  max_res <- max(num)
  min_res <- min(num)
  
  num_upl <- length(upl) #If there is more than one UPL table
  ######################################
  #Hydrogen Bonds
  #if(!is.null(hbondFile)){
  #  if(is.null(hbondWriteFile)){
  #    stop("Must provide a file path to write hbond data")
  #  }
  #  read.hbond(hbondFile)
  # num_upl <- num_opl + 1
  #  uplFile[[num_upl]] <- hbondWriteFile
  #}
  ######################################
  
  raw_up <- matrix(list(), nrow=1, ncol=num_upl)
  raw_up_ho <- matrix(list(), nrow=1, ncol=num_upl)
  
  temp_min_res <- Inf
  temp_max_res <- -Inf
  
  for(i in 1:num_upl){
    raw_up[[1,i]] <- read.dist(upl[[i]], A)
    if(hydrogen_omission){
      raw_up_ho[[1,i]] <- read.dist(upl[[i]], A, hydrogen_omission)
    }
    temp_min_res <- min(temp_min_res, min(raw_up[[i]]$tres, raw_up[[i]]$sres))
    temp_max_res <- max(temp_max_res, max(raw_up[[i]]$tres, raw_up[[i]]$sres))
  }
  
  if(is.null(in_min_res)){
    min_res <- max(temp_min_res-1,min_res)
  }else{
    min_res <- in_min_res
  }
  if(is.null(in_max_res)){
    max_res <- min(temp_max_res+1,max_res)
  }else{
    max_res <- in_max_res
  }
  
  #Remove informationless parts (w/o constraints)
  ind_del_N <- num < min_res
  ind_del_C <- num > max_res
  ind_del <- ind_del_N | ind_del_C
  if(any(ind_del)){
    seq <- seq[-ind_del]
    num <- num[-ind_del]
  }
  
  #Read in angle constraints
  out <- read.angle(aco, num)
  phi_cons <- out$phi
  psi_cons <- out$psi
  
  ##########################################################
  ############ GENERATE A RANDOM STRUCTURE #################
  ##########################################################
  
  out <- ang_sampler(seq, phi_cons, psi_cons)
  phi <- out$phi
  psi <- out$psi
  
  if(hydrogen_omission){
    out <- generate_protein(seq, num, phi, psi, A)
    wh_rand_X <- out$X
    wh_Comp <- out$Comp
    rand_X <- out$new_X
    Comp <- out$hComp
  }else{
    out <- generate_protein(seq, num, phi, psi, A)
    rand_X <- out$X
    Comp <- out$Comp
    wh_rand_X <- rand_X
    wh_Comp <- Comp
  }
  
  out <- ang_dist_conmaker(phi_cons, psi_cons, wh_Comp)
  ang_lo_cons <- out$lo_cons
  ang_up_cons <- out$up_cons
  
  ######################################################
  ############### PERFORM REDUCTION ####################
  ######################################################
  
  if(hydrogen_omission){
    dont_compute_U <- 1
    out <- reducer(rand_X, Comp)
    U <- out$U
    Comp$cliq.dims <- out$cliq_dims
    
    out <- reducer(wh_rand_X, wh_Comp, dont_compute_U)
    wh_U <- out$U
    wh_Comp$cliq.dims <- out$cliq_dims
  }else{
    out <- reducer(rand_X, Comp)
    U <- out$U
    Comp$cliq_dims <- out$cliq_dims
    wh_Comp <- Comp
  }
  
  ######################################################
  ################ GENERATE BOUNDS #####################
  ######################################################
  
  sdp_lo_bounds <- c()
  
  start <- 0
  wh_up_bounds <- matrix(rep(NaN,50000*4), ncol=4)
  for(i in 1:num_upl){
    temp_upl <- upper_maker(raw_up[[1,i]], wh_Comp)
    wh_up_bounds[(start+1):(start+nrow(temp_upl)),] <- temp_upl
    start <- start + nrow(temp_upl)
  }
  
  index_bad <- which(is.nan(wh_up_bounds[,1]))
  if(length(index_bad) > 0){
    wh_up_bounds <- wh_up_bounds[-index_bad,]
  }
  
  if(hydrogen_omission){
    start <- 0
    ho_up_bounds <- matrix(rep(NaN,50000*4), ncol=4)
    for(i in 1:num_upl){
      temp_upl <- upper_maker(raw_up_ho[[1,i]], wh_Comp)
      ho_up_bounds[(start+1):(start+nrow(temp_upl)),] <- temp_upl
      start <- start + nrow(temp_upl)
    }
    
    index_bad <- which(is.nan(ho_up_bounds[,1]))
    if(length(index_bad) > 0){
      ho_up_bounds <- ho_up_bounds[-index_bad,]
    }
  }
  
  #Torsion angles
  wh_up_bounds <- rbind(wh_up_bounds, ang_up_cons)
  wh_sdp_lo_bounds <- ang_lo_cons
  
  if(hydrogen_omission){
    #Equality Constraints
    wh_eq_cons <- equality_con_former(wh_rand_X, wh_Comp)
    eq_cons <- equality_con_former(rand_X, Comp)
    
    #upper bounds
    up_bounds <- map_bounds(ho_up_bounds, Comp$atoms_map)
    ang_up_bounds <- map_bounds(ang_up_cons, Comp$atoms_map)
    up_bounds <- rbind(up_bounds, ang_up_bounds)
    
    #vdw bounds
    wh_vdw_bounds <- vdw_bound_maker(wh_Comp)
    vdw_bounds <- vdw_bound_maker(Comp)
    sdp_lo_bounds <- map_bounds(ang_lo_cons, Comp$atoms_map)
    up_bounds[,4] <- wh_up_bounds[,4]
  }else{
    #Equality Constraints
    wh_eq_cons <- equality_con_former(rand_X, Comp)
    eq_cons <- wh_eq_cons
    
    #upper bounds
    up_bounds <- wh_up_bounds
    
    #vdw bounds
    wh_vdw_bounds <- vdw_bound_maker(wh_Comp)
    vdw_bounds <- wh_vdw_bounds
    sdp_lo_bounds <- wh_sdp_lo_bounds
  }
  
  lo_bounds <- rbind(vdw_bounds, sdp_lo_bounds)
  wh_lo_bounds <- rbind(wh_vdw_bounds, sdp_lo_bounds)
  
  ##########################################################
  ################### SOLVE THE SDP ########################
  ##########################################################
  
  rawX <- solve_sdpt3(U, eq_cons, up_bounds, sdp_lo_bounds,matrix(,nrow=0,ncol=0), f)
  
  orig_rawX <- rawX
  rawX <- rawX[1:3,]
  
  ##########################################################
  ############### ANALYSIS OF OUTPUT #######################
  ##########################################################
  
  message("Violation (raw)")
  check_eq_cons <- equality_con_former(rand_X, Comp, 2)
  report <- protchecker(rawX, Comp, check_eq_cons, lo_bounds, up_bounds, 1)
  
  ##########################################################
  ################# POST PROCESSING ########################
  ##########################################################
  
  if(hydrogen_omission == 0){
    W <- c(2, 1, 1, -1) 
  }else if(hydrogen_omission == 1){
    W <- c(2, 1, 1, -1)
  }else{
    stop("Incorrect hydrogen_omission")
  }
       
  #Phase I
  #Refinement by HANSO
  
  out <- hanso_post_processing(rand_X, rawX, Comp, lo_bounds, up_bounds, W, f)
  pX <- out$X

  message("Violations (GD-I)")
  p_report <- protchecker(pX, Comp, check_eq_cons, lo_bounds, up_bounds, 1)
  
  if(sum(p_report$phi[!is.nan(p_report$phi)] > 0) > 0.5*length(p_report$phi)){
    pX[1,] <- -pX[1,]
  }
  
  p_report <- protchecker(pX, Comp, check_eq_cons, lo_bounds, up_bounds, 0)
  
  #Correct Chiralities
  message("Correcting Chiralities")
  pXc <- chirality_correction(pX, Comp, p_report$chiral)
  message("Violations (after fixing chiralities)")
  p_report <- protchecker(pX, Comp, check_eq_cons, lo_bounds, up_bounds, 1)
  
  out <- hanso_post_processing(rand_X, pXc, Comp, lo_bounds, up_bounds, W, f)
  pX <- out$X

  message("Violations (GD-II)")
  pc_report <- protchecker(pX, Comp, check_eq_cons, lo_bounds, up_bounds, 1)
  
  message("Correcting Chiralities")
  pXc <- chirality_correction(pX, Comp, pc_report$chiral)
  
  if(hydrogen_omission){
    message("Put Hydrogen atoms back")
    pX_wh <- hydrogen_mapper(pXc, wh_rand_X, Comp, wh_Comp, A)
    check_eq_cons_wh <- equality_con_former(wh_rand_X, wh_Comp, 2)
    message("Violations (after putting hydrogen atoms back")
    f_report <- protchecker(pX_wh, wh_Comp, check_eq_cons_wh, wh_lo_bounds, wh_up_bounds,1)
    
    out <- hanso_post_processing(wh_rand_X, pX_wh, wh_Comp, wh_lo_bounds, wh_up_bounds, W, f)
    fX <- out$X
    wh_info <- out$info
    
    message("Violation (FINAL)")
    final_report <- protchecker(fX, wh_Comp, check_eq_cons_wh, wh_lo_bounds, wh_up_bounds,1)
  }
  
  return(list(X=fX, report=final_report))
  
}