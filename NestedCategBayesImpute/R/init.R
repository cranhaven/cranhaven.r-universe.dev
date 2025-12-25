
initData <- function(md) {
  orig <- list()
  orig$origdata <- md$household
  if (which(names(md$household) == "Hhindex") != 1) {
    stop("The first column of input has to be 'Hhindex'")
  }
  orig$n_i <- as.data.frame(table(md$household[,'Hhindex']))$Freq
  orig$n <- length(orig$n_i)

  HHrowIndex <- c(1, cumsum(orig[["n_i"]])+1)

  #household level data is in household_variable_index
  orig$HHdataorigT <- t(md$household[HHrowIndex[1:orig$n],md$household_variable_index])
  orig$HHserial <- md$household[,"Hhindex"]

  orig$n_individuals <- dim(md$household)[1]
  orig$n_individuals_real <- dim(md$household)[1] + orig$n #the real number of individuals

  #orig$p holds the number of individual level variables
  orig$p <- length(md$individual_variable_index)
  #levels for each variable in the model. This might be different from the sub-sampled data in use
  orig$d <- rep(0,orig$p)
  for (i in 1:length(orig$d)) {
    orig$d[i] <- max(md$household[,md$individual_variable_index[i]])
  }
  orig$IndivDataInCol <- t(md$household[,md$individual_variable_index])
  orig$maxd <- max(orig$d)

  orig$n_star_h <- groupcount1D(orig$n_i,max(orig$n_i)) #number of households for each household size
  orig$n_star_h <- orig$n_star_h[which(orig$n_star_h>0)]

  return(orig)
}


initParameters <- function(data,hyper,HHhead_at_group_level) {
  para <- list()
  para$alpha <- 1 #hyperparameters for stick-breaking weights
  para$beta <- 1

  #intilize phi
  para$phi <- matrix(0, nrow  = data$maxd*data$p, ncol = hyper$FF*hyper$SS) #cell probabilities
  phi_1 <- matrix(0, nrow = data$maxd, ncol = data$p)
  for (i in 1:data$p) {
    for (j in 1:data$d[i]) {
      phi_1[j,i] <-sum(data$IndivDataInCol[i,]==j)/data$n_individuals
    }
  }
  phi_1 <- as.vector(phi_1)
  for (i in 1:dim(para$phi)[2]) {
    para$phi[,i] <- phi_1
  }

  para$HHdata_all <- data$HHdataorigT
  if (!HHhead_at_group_level) {
    para$HHdata_all[2,] <- para$HHdata_all[2,] - 1 #household size
  }
  #initialize lambda
  para$lambda <- list()
  for (i in 1:length(hyper$dHH)) {
    lambda <- matrix(0, nrow = hyper$FF,ncol = hyper$dHH[i])
    for (j in 1:hyper$dHH[i]) {
      lambda[,j] <- sum(para$HHdata_all[i,]==j) / data$n
    }
    para$lambda[[i]] <- lambda
  }

  para$u <- c(rbeta(hyper$FF-1, 1,para$alpha),1)
  para$pi <- para$u * cumprod(c(1,1.0-para$u[1:hyper$FF-1]))

  ones <- matrix(1.0, hyper$FF,1)
  para$v <- c(rbeta(hyper$FF * (hyper$SS-1), 1, para$beta),ones)
  dim(para$v) <- c(hyper$FF, hyper$SS)

  para$omega <- matrix(0, nrow = hyper$FF,ncol = hyper$SS)
  for (i in 1:hyper$FF) {
    v1 <- para$v[i,]
    para$omega[i,]  <- v1 * cumprod(c(1, 1-v1[1:hyper$SS-1]))
  }

  return(para)
}

initOutput <- function(data,hyper,mc) {
  output <- list()
  output$alphaout <- matrix(0,nrow = mc$eff.sam,ncol = 1)
  output$betaout <- matrix(0, nrow = mc$eff.sam,ncol = 1)
  output$piout <- matrix(0, mc$eff.sam,hyper$FF)
  output$omegaout <- array(0, dim=c(mc$eff.sam,hyper$FF,hyper$SS))
  output$nout <- matrix(0,nrow = mc$nrun,ncol = 1)
  output$extrasize <- matrix(0,nrow = mc$nrun,ncol = length(data$n_star_h))
  output$F_occupied <- matrix(0, nrow = mc$eff.sam, ncol = 1)
  output$S_occupied_max <- matrix(0, nrow = mc$eff.sam, ncol = 1)
  output$elapsed_time <-  matrix(0,nrow = mc$nrun,ncol = 1)
  output$newphiout <- array(0, dim=c(mc$eff.sam,data$maxd*data$p,hyper$FF*hyper$SS))
  output$lambdaout = list()
  for (i in 1:length(hyper$dHH)) {
    output$lambdaout[[i]] = array(0, dim=c(mc$eff.sam,hyper$FF, hyper$dHH[i]))
  }
  return(output)
}

initMissing <- function(data,struc_zero_variables,miss_batch){
  md <- list()
  #keep a copy of raw data
  md$household_with_miss <- data$household
  md$individual_variable_index <- data$individual_variable_index
  md$household_variable_index <- data$household_variable_index

  #find the index of structure zero varaibles
  struc_zero_variables_index <- which(is.element(colnames(data$household),struc_zero_variables))
  #find the index of structure zero varaibles for house level
  struc_zero_variables_house <- intersect(struc_zero_variables_index, data$household_variable_index)
  #find the index of structure zero varaibles for individual level
  struc_zero_variables_indiv <- intersect(struc_zero_variables_index,data$individual_variable_index)

  #indexing relative to house_variable index and invidual varible index
  md$house_szv_index <- match(struc_zero_variables_house,md$household_variable_index)
  md$indiv_szv_index <- match(struc_zero_variables_indiv,md$individual_variable_index)

  #precompute NA (misisng status)
  md$NA_indiv_missing_status <- is.na(as.matrix(md$household_with_miss[,md$individual_variable_index]))
  md$NA_house_missing_status <- is.na(as.matrix(md$household_with_miss[,md$household_variable_index]))

  all_variables_index <- c(md$individual_variable_index,md$household_variable_index)
  nonstruc_zero_variables_index <-
    all_variables_index[!is.element(all_variables_index,struc_zero_variables_index)]
  md$house_non_szv_index_raw <- intersect(nonstruc_zero_variables_index, data$household_variable_index)
  md$indiv_non_szv_index_raw <- intersect(nonstruc_zero_variables_index,data$individual_variable_index)
  md$house_non_szv_index <- match(md$house_non_szv_index_raw,md$household_variable_index)
  md$indiv_non_szv_index <- match(md$indiv_non_szv_index_raw,md$individual_variable_index)

  n <- length(unique(data$household$Hhindex)) #number of households
  md$n_batch_imp_sum <- rep(miss_batch,n)
  md$n_0_reject <- rep(1,n)
  md$miss_Hhindex <- sort(unique(data$household$Hhindex[!complete.cases(data$household[,struc_zero_variables_index])]))
  md$miss_Hh_invidual_index <- list()
  for(s in md$miss_Hhindex){
    md$miss_Hh_invidual_index[[s]] <- which(md$household_with_miss$Hhindex==s)
  }
  md$n_miss <- length(md$miss_Hhindex)

  if (sum(is.na(data$household[,c("Hhindex","pernum")])) > 0) {
    stop("Hhindex and pernum cannot contain missing entries")
  }

  if (sum(is.na(data$household)) > 0) {
    md$hasMissingData = TRUE
    for(j in md$individual_variable_index){
      levels_j <- sort(unique(data$household[,j]),na.last = NA)
      data$household[is.na(data$household[,j]),j] <-
        sample(levels_j,sum(is.na(data$household[,j])),replace=T,prob=summary(as.factor(na.omit(data$household[,j]))))
    }
    for(jj in md$household_variable_index){
      levels_j <- sort(unique(data$household[,jj]),na.last = NA)
      for(i in 1:n){
        which_indiv <- which(data$household$Hhindex==i)
        if(is.na(data$household[which_indiv[1],jj])==TRUE){
          data$household[which_indiv,jj] <-
            sample(levels_j,1,replace=T,prob=summary(as.factor(na.omit(data$household[,jj]))))
        }
      }
    }
  } else {
    md$hasMissingData = FALSE
  }
  md$household <- data$household

  print("missing data must be coded as 'NA'")
  return(md)
}

