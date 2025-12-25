
.SetErrorPara_EI <- function(orig,var_in_error_house,var_in_error_indiv,imp_batch,error_batch,prop_batch){
  z_i <- matrix(0,ncol=1,nrow=orig$n)
  Y_house <- data.frame(t(orig$HHdataorigT)); rownames(Y_house) <- NULL
  Y_indiv <- data.frame(t(orig$IndivDataInCol));
  Error_Mat_house <- Y_house; Error_Mat_house[] <- 0
  Error_Mat_indiv <- Y_indiv; Error_Mat_indiv[] <- 0
  H <- sort(unique(orig$n_i))
  house_index <- orig$origdata$Hhindex
  for(hh_size in H){
    house_index_hh <- which(orig$n_i == hh_size)
    comb_to_check <- Y_indiv[is.element(house_index,house_index_hh),]
    comb_to_check$relate <- comb_to_check$relate + 1L #recode relate to start from 2
    comb_to_check <- apply(comb_to_check,2,function(x) as.numeric(as.character(x)))
    comb_to_check <- matrix(t(comb_to_check),byrow=T,ncol=(orig$p*hh_size))

    #add the household head before check
    hh_head_variables <- unlist(lapply(1:orig$p,function(x) paste0("head",colnames(Y_indiv)[x])))
    hh_head_variables <- hh_head_variables[is.element(hh_head_variables,colnames(Y_house))]
    comb_to_check <- cbind(Y_house[house_index_hh,hh_head_variables],1,comb_to_check) #add relate=1 for head
    comb_to_check <- apply(comb_to_check,2,function(x) as.numeric(as.character(x)))
    colnames(comb_to_check) <- NULL
    z_i[house_index_hh] <- ifelse(checkSZ(comb_to_check,(hh_size+1))==1,0,1)
  }
  z_i_index_house <- which(z_i == 1)
  z_i_index_indiv <- which(is.element(house_index,z_i_index_house)==TRUE)
  Error_Mat_house[z_i_index_house,var_in_error_house] <- 1
  Error_Mat_indiv[z_i_index_indiv,var_in_error_indiv] <- 1
  Error_Mat_house <- Error_Mat_house==1
  Error_Mat_indiv <- Error_Mat_indiv==1
  Error_Mat_house[Error_Mat_house] <- NA #I set this to NA since we don't know for sure that the locations are truly in error. We only know the household as a whole is!
  Error_Mat_indiv[Error_Mat_indiv] <- NA #When the true error status is known, the entry in this matrices should be set to TRUE (error) or FALSE (no error) not NA
  hasErrorData <- sum(z_i)>0
  NumberOfErrorHH <- sum(z_i)

  var_in_error_index_house <- which(is.element(colnames(Y_house),var_in_error_house))
  var_in_error_index_indiv <- which(is.element(colnames(Y_indiv),var_in_error_indiv))
  nonvar_in_error_index_indiv <- c(1:ncol(Y_indiv))[-var_in_error_index_indiv]
  nonvar_in_error_index_house <- c(1:ncol(Y_house))[-var_in_error_index_house]
  n_batch_sum <- rep(imp_batch,length(H))
  n_0 <- rep(1,length(H))
  n_batch_imp_sum <- rep(error_batch,orig$n)
  n_0_reject <- rep(1,orig$n)

  a_epsilon_house <- rep(1,length(var_in_error_house))
  b_epsilon_house <- rep(1,length(var_in_error_house))
  a_epsilon_indiv <- rep(1,length(var_in_error_indiv))
  b_epsilon_indiv <- rep(1,length(var_in_error_indiv))
  epsilon_house <- rbeta(length(var_in_error_house),t(t(a_epsilon_house)),t(t(b_epsilon_house)))
  epsilon_indiv <- rbeta(length(var_in_error_indiv),t(t(a_epsilon_indiv)),t(t(b_epsilon_indiv)))

  return(list(Y_house=Y_house,Y_indiv=Y_indiv,
              H=H,house_index=house_index,
              var_in_error_house=var_in_error_house,
              var_in_error_indiv=var_in_error_indiv,
              hasErrorData=hasErrorData,NumberOfErrorHH=NumberOfErrorHH,
              z_i_index_house=z_i_index_house,z_i_index_indiv=z_i_index_indiv,
              Error_Mat_house=Error_Mat_house,Error_Mat_indiv=Error_Mat_indiv,
              a_epsilon_house=a_epsilon_house,b_epsilon_house=b_epsilon_house,
              a_epsilon_indiv=a_epsilon_indiv,b_epsilon_indiv=b_epsilon_indiv,
              epsilon_house=epsilon_house,epsilon_indiv=epsilon_indiv,
              var_in_error_index_house=var_in_error_index_house,
              var_in_error_index_indiv=var_in_error_index_indiv,
              nonvar_in_error_index_house=nonvar_in_error_index_house,
              nonvar_in_error_index_indiv=nonvar_in_error_index_indiv,n_batch_sum=n_batch_sum,n_0=n_0,
              n_batch_imp_sum=n_batch_imp_sum,n_0_reject=n_0_reject,prop_batch=prop_batch))
}


## Function to sample true responses for a given household
.SampleTrueResponse_h <- function(ErrorData,orig,para,G_household,M,sss,hyper){
  p <- orig$p; q <- ncol(ErrorData$Y_house);
  house_index <- ErrorData$house_index
  Y_house <- ErrorData$Y_house; Y_indiv <- ErrorData$Y_indiv
  Error_Mat_house <- ErrorData$Error_Mat_house; Error_Mat_indiv <- ErrorData$Error_Mat_indiv
  d_k_house <- hyper$dHH; d_k_indiv <- orig$d;
  epsilon_house <- ErrorData$epsilon_house; epsilon_indiv <- ErrorData$epsilon_indiv
  n_batch_imp <- ErrorData$n_batch_imp; n_0_reject <- ErrorData$n_0_reject;
  var_in_error_index_house <- ErrorData$var_in_error_index_house;
  var_in_error_index_indiv <- ErrorData$var_in_error_index_indiv;

  another_index <- which(house_index==sss);
  G_prop <- rep(G_household$G[sss],n_batch_imp[sss])
  M_prop <- rep(M[another_index],n_batch_imp[sss])
  lambda_g <- lapply(para$lambda,function(x) x[G_prop,])
  phi_m_g <- t(para$phi[,(M_prop + (G_household$G[sss]-1)*hyper$SS)])

  X_house_sss_prop <- Y_house[sss,]
  X_house_sss_prop <- matrix(rep(t(X_house_sss_prop),n_batch_imp[sss]),byrow=T,ncol=q)
  X_house_sss_prop_nf <- X_house_sss_prop
  X_indiv_sss_prop <- Y_indiv[another_index,]
  X_indiv_sss_prop <- matrix(rep(t(X_indiv_sss_prop),n_batch_imp[sss]),byrow=T,ncol=p)
  X_indiv_sss_prop_nf <- X_indiv_sss_prop
  Error_Mat_house_sss <- Error_Mat_house[sss,]
  Error_Mat_house_sss <- matrix(rep(t(Error_Mat_house_sss),n_batch_imp[sss]),byrow=T,ncol=q)
  Error_Mat_indiv_sss <- Error_Mat_indiv[another_index,]
  Error_Mat_indiv_sss <- matrix(rep(t(Error_Mat_indiv_sss),n_batch_imp[sss]),byrow=T,ncol=p)

  #level_house <- GlobalPara$level_house; level_indiv <- GlobalPara$level_indiv;
  #d_k_house_cum <- ErrorData$d_k_house_cum; d_k_indiv_cum <- ErrorData$d_k_indiv_cum;

  check_counter_sss <- 0;
  while(check_counter_sss < 1){
    #First sample household data
    for(kkk in var_in_error_index_house){
      q_house_k <- 1/(d_k_house[kkk] - 1)
      if(sum(is.na(Error_Mat_house_sss[,kkk]))>0){
        corr_factor_Y_house_k <- matrix(0,ncol=d_k_house[kkk],nrow=length(G_prop))
        corr_factor_Y_house_k <- cbind(corr_factor_Y_house_k,X_house_sss_prop_nf[,kkk])
        corr_factor_house_k <- t(apply(corr_factor_Y_house_k,1,function(x)
          replace(x,x[(d_k_house[kkk]+1)],1-epsilon_house[which(var_in_error_index_house==kkk)])))
        corr_factor_house_k <- corr_factor_house_k[,-ncol(corr_factor_house_k)]
        corr_factor_house_k[corr_factor_house_k==0] <- epsilon_house[which(var_in_error_index_house==kkk)]*q_house_k
        pr_X_house_k <- lambda_g[[kkk]]*corr_factor_house_k
        pr_X_house_k <- t(apply(pr_X_house_k,1,function(x) x/sum(x)))
        Ran_unif_X_house_k <- runif(nrow(pr_X_house_k))
        cumul_X_house_k <- pr_X_house_k%*%upper.tri(diag(ncol(pr_X_house_k)),diag=TRUE)
        X_house_sss_prop[is.na(Error_Mat_house_sss[,kkk]),kkk] <- rowSums(Ran_unif_X_house_k>cumul_X_house_k) + 1L
      } else if(sum(Error_Mat_house_sss[,kkk],na.rm=T)>0){
        corr_factor_house_k <- matrix(1,ncol=d_k_house[kkk],nrow=length(G_prop))
        corr_factor_Y_house_k <- cbind(corr_factor_house_k,X_house_sss_prop_nf[sss,kkk])
        corr_factor_house_k <- t(apply(corr_factor_Y_house_k,1,function(x) replace(x,x[(d_k_house[kkk]+1)],0)))
        corr_factor_house_k <- corr_factor_house_k[,-ncol(corr_factor_house_k)]
        corr_factor_house_k[corr_factor_house_k==1] <- epsilon_house[which(var_in_error_index_house==kkk)]*q_house_k
        pr_X_house_k <- lambda_g[[kkk]]*corr_factor_house_k
        pr_X_house_k <- t(apply(pr_X_house_k,1,function(x) x/sum(x)))
        Ran_unif_X_house_k <- runif(nrow(pr_X_house_k))
        cumul_X_house_k <- pr_X_house_k%*%upper.tri(diag(ncol(pr_X_house_k)),diag=TRUE)
        X_house_sss_prop[Error_Mat_house_sss[,kkk],kkk] <- rowSums(Ran_unif_X_house_k>cumul_X_house_k) + 1L
      }
    }
    #Then sample individual data
    for(kkkk in var_in_error_index_indiv){
      q_indiv_k <- 1/(d_k_indiv[kkkk] - 1)
      if(sum(is.na(Error_Mat_indiv_sss[,kkkk]))>0){
        corr_factor_indiv_k <- matrix(0,ncol=d_k_indiv[kkkk],nrow=length(M_prop))
        corr_factor_Y_indiv_k <- cbind(corr_factor_indiv_k,X_indiv_sss_prop_nf[,kkkk])
        corr_factor_indiv_k <- t(apply(corr_factor_Y_indiv_k,1,function(x)
          replace(x,x[(d_k_indiv[kkkk]+1)],1-epsilon_indiv[which(var_in_error_index_indiv==kkkk)])))
        corr_factor_indiv_k <- corr_factor_indiv_k[,-ncol(corr_factor_indiv_k)]
        corr_factor_indiv_k[corr_factor_indiv_k==0] <- epsilon_indiv[which(var_in_error_index_indiv==kkkk)]*q_indiv_k
        pr_X_indiv_k <- phi_m_g[is.na(Error_Mat_indiv_sss[,kkkk]),((1:d_k_indiv[kkkk])+(kkkk-1)*orig$maxd)]*corr_factor_indiv_k
        pr_X_indiv_k <- t(apply(pr_X_indiv_k,1,function(x) x/sum(x)))
        Ran_unif_X_indiv_k <- runif(nrow(pr_X_indiv_k))
        cumul_X_indiv_k <- pr_X_indiv_k%*%upper.tri(diag(ncol(pr_X_indiv_k)),diag=TRUE)
        X_indiv_sss_prop[is.na(Error_Mat_indiv_sss[,kkkk]),kkkk] <- rowSums(Ran_unif_X_indiv_k>cumul_X_indiv_k) + 1L
      } else if(sum(Error_Mat_indiv_sss[,kkkk],na.rm=T)>0){
        corr_factor_indiv_k <- matrix(1,ncol=d_k_indiv[kkkk],nrow=length(M_prop))
        corr_factor_Y_indiv_k <- cbind(corr_factor_indiv_k,X_indiv_sss_prop_nf[,kkkk])
        corr_factor_indiv_k <- t(apply(corr_factor_Y_indiv_k,1,function(x) replace(x,x[(d_k_indiv[kkkk]+1)],0)))
        corr_factor_indiv_k <- corr_factor_indiv_k[,-ncol(corr_factor_indiv_k)]
        corr_factor_indiv_k[corr_factor_indiv_k==1] <- epsilon_indiv[which(var_in_error_index_indiv==kkkk)]*q_indiv_k
        pr_X_indiv_k <- phi_m_g[Error_Mat_indiv_sss[,kkkk],((1:d_k_indiv[kkkk])+(kkkk-1)*orig$maxd)]*corr_factor_indiv_k
        pr_X_indiv_k <- t(apply(pr_X_indiv_k,1,function(x) x/sum(x)))
        Ran_unif_X_indiv_k <- runif(nrow(pr_X_indiv_k))
        cumul_X_indiv_k <- pr_X_indiv_k%*%upper.tri(diag(ncol(pr_X_indiv_k)),diag=TRUE)
        X_indiv_sss_prop[Error_Mat_indiv_sss[,kkkk],kkkk] <- rowSums(Ran_unif_X_indiv_k>cumul_X_indiv_k) + 1L
      }
    }
    #Check edit rules
    colnames(X_indiv_sss_prop) <- colnames(Y_indiv)
    colnames(X_house_sss_prop) <- colnames(Y_house)
    X_indiv_sss_prop[,"relate"] <- X_indiv_sss_prop[,"relate"] + 1L #recode relate to start from 2
    comb_to_check <- matrix(t(X_indiv_sss_prop),nrow=n_batch_imp[sss],byrow=TRUE)
    hh_head_variables <- unlist(lapply(1:orig$p,function(x) paste0("head",colnames(Y_indiv)[x])))
    hh_head_variables <- hh_head_variables[is.element(hh_head_variables,colnames(Y_house))]
    comb_to_check <- cbind(X_house_sss_prop[,hh_head_variables],1,comb_to_check) #add relate=1 for head
    comb_to_check <- apply(comb_to_check,2,function(x) as.numeric(as.character(x)))
    colnames(comb_to_check) <- NULL
    check_counter <- checkSZ(comb_to_check,(length(another_index) + 1))
    check_counter_sss <- check_counter_sss + sum(check_counter)
    if(length(which(check_counter==1))>0){
      n_0_reject[sss] <- n_0_reject[sss] + length(which(check_counter[1:which(check_counter==1)[1]]==0))
    } else{
      n_0_reject[sss] <- n_0_reject[sss] + n_batch_imp[sss]
    }
  }
  X_house <- X_house_sss_prop[which(check_counter==1)[1],]
  X_indiv <- matrix(comb_to_check[which(check_counter==1)[1],-c(1:p)],byrow=TRUE,ncol=p) #remove household head
  colnames(X_indiv) <- colnames(Y_indiv)
  X_indiv[,"relate"] <- X_indiv[,"relate"] - 1L #recode relate back to start from 1

  return(list(X_house=X_house, X_indiv=X_indiv, n_0_reject=n_0_reject))
}


## Function to sample true responses
.SampleTrueResponse <- function(ErrorData,orig,para,G_household,M,hyper){

  ErrorData$n_0_reject <- ErrorData$n_0_reject;
  for(sss in ErrorData$z_i_index_house){
    ErrorData_h <- .SampleTrueResponse_h(ErrorData,orig,para,G_household,M,sss,hyper)
    ErrorData$X_house[sss,] <- ErrorData_h$X_house
    ErrorData$origdata[which(ErrorData$house_index==sss),ErrorData$var_in_error_house] <-
    rep(ErrorData_h$X_house[ErrorData$var_in_error_house],each=sum(ErrorData$house_index==sss))
    ErrorData$X_indiv[which(ErrorData$house_index==sss),] <- ErrorData_h$X_indiv
    ErrorData$origdata[which(ErrorData$house_index==sss),ErrorData$var_in_error_indiv] <-
      ErrorData_h$X_indiv[,ErrorData$var_in_error_indiv]
    ErrorData$n_0_reject[sss] <- ErrorData_h$n_0_reject[sss]
  }

  return(ErrorData)
}


## Function to sample household epsilon
.SampleEpsilonHouse <- function(ErrorData){
  a_epsilon_house_star <- ErrorData$a_epsilon_house +
    colSums(ErrorData$E_house[ErrorData$z_i_index_house,ErrorData$var_in_error_index_house])
  b_epsilon_house_star <- ErrorData$b_epsilon_house + length(ErrorData$z_i_index_house) -
    colSums(ErrorData$E_house[ErrorData$z_i_index_house,ErrorData$var_in_error_index_house])
  epsilon_house <- rbeta(length(ErrorData$var_in_error_index_house),t(t(a_epsilon_house_star)),t(t(b_epsilon_house_star)))

  return(epsilon_house)
}


## Function to sample individual epsilon
.SampleEpsilonIndiv <- function(ErrorData){
  a_epsilon_indiv_star <- ErrorData$a_epsilon_indiv +
    colSums(ErrorData$E_indiv[ErrorData$z_i_index_indiv,ErrorData$var_in_error_index_indiv])
  b_epsilon_indiv_star <- ErrorData$b_epsilon_indiv + length(ErrorData$z_i_index_indiv) -
    colSums(ErrorData$E_indiv[ErrorData$z_i_index_indiv,ErrorData$var_in_error_index_indiv])
  epsilon_indiv <- rbeta(length(ErrorData$var_in_error_index_indiv),t(t(a_epsilon_indiv_star)),t(t(b_epsilon_indiv_star)))

  return(epsilon_indiv)
}


