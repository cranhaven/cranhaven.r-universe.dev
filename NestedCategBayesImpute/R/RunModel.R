

RunModel <- function(orig,mc,hyper,para,output,synindex,individual_variable_index,household_variable_index,
                     HHhead_at_group_level,weight_option,struc_weight,MissData=NULL,Parallel=FALSE){
  ErrorData <- NULL
  synData <- list()
  impData <- list()

  if(weight_option){
    struc_weight <- c(1,struc_weight) #add 1 for the weight of the observed data
    G_all_weighted <- vector("list",length(struc_weight))
    HHdata_all_weighted <- vector("list",length(struc_weight))
    IndividualData_all_weighted <- vector("list",length(struc_weight))
    M_all_weighted <- vector("list",length(struc_weight))
  }

  parallel <- 0
  if (Parallel) {
    parallel <- 1
  }

  for (i in 1:mc$nrun) {
    cat(paste("iteration ", i,"\n", sep = ""))
    t <- proc.time()

    G_household <- sampleG(para$phi,orig$IndivDataInCol,para$omega,para$pi,orig$n_i,t(para$HHdata_all[,1:orig$n]),para$lambda,parallel)
    M <- sampleM(para$phi,orig$IndivDataInCol,para$omega,G_household$G,orig$HHserial, parallel)

    if(weight_option){
      data.extra <- GetImpossibleHouseholds(orig$d,ceiling(orig$n_star_h*struc_weight[-1]), para$lambda,para$omega,para$phi,
                                            para$pi,hyper$blocksize,orig$n,is.element(i,synindex),HHhead_at_group_level, Parallel)
      para$hh_size_new <- as.vector(data.extra$hh_size_new)
      DIM <- dim(data.extra$IndividualData_extra)[1]
      if (is.element(i,synindex)) {
        forsynData <- GetImpossibleHouseholds(orig$d,orig$n_star_h, para$lambda,para$omega,para$phi,
                                              para$pi,hyper$blocksize,orig$n,is.element(i,synindex),HHhead_at_group_level, Parallel) #synthetic data
        synData[[which(synindex ==i)]] <- t(forsynData$synIndividuals_all[1:DIM,])
        colnames(synData[[which(synindex ==i)]]) <- colnames(orig$origdata)[-ncol(orig$origdata)]
      }

      #combine data and indicators -- use lists for weighting
      n_i_extra <- data.extra$HHdata_extra[dim(data.extra$HHdata_extra)[1],]
      n_i_extra_index <- rep(n_i_extra,n_i_extra)

      G_all_weighted[[1]] <- G_household$G
      HHdata_all_weighted[[1]] <- orig$HHdataorigT
      if (!HHhead_at_group_level) {
        HHdata_all_weighted[[1]][2,] <- HHdata_all_weighted[[1]][2,] - 1
      }

      #IndividualData_all_weighted[[1]] <-orig$allT
      IndividualData_all_weighted[[1]] <-orig$IndivDataInCol
      M_all_weighted[[1]] <- rbind(G_household$G_Individuals,M)
      M_all_vector <- cbind(M_all_weighted[[1]],data.extra$G_Individuals_and_M_extra)

      hhsize_addjust <- 0
      if (HHhead_at_group_level) {hhsize_addjust = -1}

      for(w_i in 2:length(struc_weight)){
        G_all_weighted[[w_i]] <- data.extra$G_extra[(n_i_extra == w_i + hhsize_addjust)]
        HHdata_all_weighted[[w_i]] <- data.extra$HHdata_extra[,(n_i_extra == w_i+hhsize_addjust)]
        IndividualData_all_weighted[[w_i]] <- data.extra$IndividualData_extra[individual_variable_index,
                                                                              (n_i_extra_index == w_i+hhsize_addjust)]
        M_all_weighted[[w_i]] <- data.extra$G_Individuals_and_M_extra[,(n_i_extra_index == w_i+hhsize_addjust)]
      }

      # update phi
      para$phi <- UpdatePhiWeighted(IndividualData_all_weighted,M_all_weighted,hyper$FF,hyper$SS,orig$d,orig$maxd,struc_weight)
      #update omega
      Omega <- UpdateOmegaWeighted(para$beta,M_all_weighted, hyper$FF, hyper$SS,struc_weight)
      para$omega <- Omega$omega
      para$v <- Omega$v

      # update lambda
      para$lambda <- UpdateLambdaWeighted(HHdata_all_weighted,G_all_weighted,hyper$dHH,hyper$FF,struc_weight)

      # update pi
      Pi <- UpdatePiWeighted(para$alpha,G_all_weighted,hyper$FF,struc_weight)
      para$pi <- Pi$pi
      para$u <- Pi$u

    } else {
      data.extra <- GetImpossibleHouseholds(orig$d,orig$n_star_h,para$lambda,para$omega,para$phi,
                                            para$pi,hyper$blocksize,orig$n,is.element(i,synindex),HHhead_at_group_level, Parallel)
      para$hh_size_new <- as.vector(data.extra$hh_size_new)
      DIM <- dim(data.extra$IndividualData_extra)[1]
      if (is.element(i,synindex)) {
        synData[[which(synindex ==i)]] <- t(data.extra$synIndividuals_all[1:DIM,])
      }

      #combine data and indicators
      para$G_all <- c(G_household$G, data.extra$G_extra)
      para$HHdata_all <- orig$HHdataorigT
      if (!HHhead_at_group_level) {
        para$HHdata_all[2,] <- para$HHdata_all[2,] -1
      }
      para$HHdata_all <- cbind(para$HHdata_all,data.extra$HHdata_extra)

      #row 1 for FF groups and row 2 for SS groups
      temp <- rbind(G_household$G_Individuals,M)
      para$M_all  <- cbind(temp,data.extra$G_Individuals_and_M_extra)

      # update phi
      Individual_data_all =cbind(orig$IndivDataInCol,data.extra$IndividualData_extra[individual_variable_index,])
      para$phi <- UpdatePhi(Individual_data_all, para$M_all,hyper$FF,hyper$SS,orig$d,orig$maxd)

      #update Omega
      Omega <- UpdateOmega(para$beta,para$M_all, hyper$FF, hyper$SS)
      para$omega <- Omega$omega
      para$v <- Omega$v

      # update lambda
      para$lambda <- UpdateLambda(para$HHdata_all, para$G_all,hyper$dHH,hyper$FF)

      # update pi
      Pi <- UpdatePi(para$alpha,para$G_all,hyper$FF)
      para$pi <- Pi$pi
      para$u <- Pi$u
    }

    #update alpha
    para$alpha <- UpdateAlpha(hyper$aa,hyper$ab,para$u)

    #update beta
    para$beta <- UpdateBeta(hyper$ba,hyper$bb,para$v)

    #update erroneous data, E -- the error indicators and epsilon
    if (!is.null(ErrorData)) {
      if (ErrorData$hasErrorData){
        ErrorData$n_batch_imp_sum <- ErrorData$n_batch_imp_sum + ceiling(ErrorData$n_0_reject*ErrorData$prop_batch)
        ErrorData$n_batch_imp <- ceiling(ErrorData$n_batch_imp_sum/i) + 1 #no. of batches of imputations to sample
        ErrorData$n_0_reject[] <- 0
        ErrorData$X_house <- ErrorData$Y_house
        ErrorData$X_indiv <- ErrorData$Y_indiv
        ErrorData$origdata <- orig$origdata
        ErrorData <- .SampleTrueResponse(ErrorData,orig,para,G_household,M,hyper)
        ErrorData$n_0_reject <- ErrorData$n_0_reject

        orig$origdata <- as.matrix(ErrorData$origdata)
        HHrowIndex <- c(1, cumsum(orig$n_i)+1)
        orig$HHdataorigT <- t(orig$origdata[HHrowIndex[1:orig$n],household_variable_index])
        orig$IndivDataInCol <- t(orig$origdata[,individual_variable_index])
        para$HHdata_all <- orig$HHdataorigT

        #error indicators
        ErrorData$E_house <- data.matrix(ErrorData$X_house)-data.matrix(ErrorData$Y_house)
        ErrorData$E_house[ErrorData$E_house!=0] <- 1
        ErrorData$E_indiv <- data.matrix(ErrorData$X_indiv)-data.matrix(ErrorData$Y_indiv)
        ErrorData$E_indiv[ErrorData$E_indiv!=0] <- 1

        #epsilon
        ErrorData$epsilon_house <- .SampleEpsilonHouse(ErrorData)
        ErrorData$epsilon_indiv <- .SampleEpsilonIndiv(ErrorData)
      }
    }


    #update missing data
    if (MissData$hasMissingData){
      MissData$n_batch_imp_sum <- MissData$n_batch_imp_sum + ceiling(MissData$n_0_reject*MissData$prop_batch)
      MissData$n_batch_imp <- ceiling(MissData$n_batch_imp_sum/i) + 1 #no. of batches of imputations to sample
      MissData$n_0_reject[] <- 0
      #MissData$household <- as.matrix(MissData$household)
      MissData$household <- as.matrix(orig$origdata)
      #sample non structural zeros variables for everyone at once
      storage.mode(MissData$household) <- "integer" #very important if used to do in place update
      MissData <- SampleMissing(MissData,para,orig,G_household,M,hyper)
      MissData$household <- as.data.frame(MissData$household)

      #MissData <- SampleMissing(MissData,para,orig,G_household,M,hyper)
      orig$origdata <- MissData$household
      HHrowIndex <- c(1, cumsum(orig$n_i)+1)
      orig$HHdataorigT <- t(MissData$household[HHrowIndex[1:orig$n],household_variable_index])
      orig$IndivDataInCol <- t(MissData$household[,individual_variable_index])
      para$HHdata_all <- orig$HHdataorigT
    }

    #head(MissData$household_with_miss)
    #head(MissData$household)

    #save imputed data
    if (MissData$hasMissingData || (!is.null(ErrorData) && ErrorData$hasErrorData)){
      if (is.element(i,MissData$miss_index)){
        impData[[which(MissData$miss_index ==i)]] <- MissData$household
      }
    }

    #post save
    if(weight_option){
      G_all <- unlist(G_all_weighted)
      rep_G_all <- t(M_all_vector[1,])
      M_all <- t(M_all_vector[2,])
    } else {
      G_all <- para$G_all
      rep_G_all <- t(para$M_all[1,])
      M_all <- t(para$M_all[2,])
    }

    counts <- groupcount(rep_G_all,M_all,hyper$FF, hyper$SS) >0
    F_occup <- max(colSums(counts))
    S_occup <- max(rowSums(counts))

    cat(paste("number of occupied household classes is ", F_occup, "\n", sep = ''))
    cat(paste("max number of occupied individual classes is ", S_occup, "\n", sep = ''))

    if (!is.null(ErrorData)) {
      cat(paste("epsilon_indiv:", round(ErrorData$epsilon_indiv,3), "\n", sep = ''))
      cat(paste("epsilon_house:", round(ErrorData$epsilon_house,3), "\n", sep = ''))
    }

    total_household <- sum(c(orig$n,para$hh_size_new))
    if(weight_option){
      cat(paste("total number of households (capped) sampled is ", total_household, "\n", sep = ''))
      est_total_household <- sum(c(orig$n,para$hh_size_new)/struc_weight)
      cat(paste("true (estimated) total number of households is ", est_total_household, "\n", sep = ''))
    } else {
      cat(paste("total number of households sampled is ", total_household, "\n", sep = ''))
    }
    if (MissData$hasMissingData){
      cat(paste("total number of rejected households sampled for missing data is ", sum(MissData$n_0_reject), "\n", sep = ''))
    }
    if (!is.null(ErrorData)) {
      if (ErrorData$hasErrorData){
        cat(paste("total number of rejected households sampled for faulty data is ", sum(ErrorData$n_0_reject), "\n", sep = ''))
      }
    }
    cat(paste("elapsed time = ", (proc.time() - t)[["elapsed"]], "\n\n", sep = ' '))

    output$nout[i] <- total_household
    output$extrasize[i,] <- para$hh_size_new
    output$elapsed_time[i] <- (proc.time() - t)[["elapsed"]]


    if (i %% mc$thin == 0 && i > mc$burn)  {
      index <- (i-mc$burn)/mc$thin
      output$piout[index,] <- para$pi
      output$omegaout[index,,] <- para$omega
      output$newphiout[index,,] <- para$phi
      for (i in 1:length(hyper$dHH)) {
        output$lambdaout[[i]][index,,] = para$lambda[[i]]
      }
      output$F_occupied[index] <- F_occup
      output$S_occupied_max[index] <- S_occup
      output$alphaout[index] <- para$alpha
      output$betaout[index] <- para$beta
    }
  }

  return(list(synData=synData,impData=impData,output=output))
}
