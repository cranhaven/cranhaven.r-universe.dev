###
# In this version, the mediation analysis only includes one mediator
###
BANOVA.mediation <-
  function(sol_1, sol_2, xvar, mediator, individual = F, return_posterior_samples = F, 
           multi_samples_beta1_raw_m = NULL){
    #Function which prepares labels for the results returened to the user
    prepare_list_name <- function(moderated_var, table_colnames){
      #check if there are factors at other levels
      n_col <- length(table_colnames)
      if (n_col-3 > 0){
        table_colnames <- table_colnames[1:(n_col-3)]
        moderators <- table_colnames[table_colnames != moderated_var]
        num_moderators <- length(moderators)
        if (num_moderators > 1){
          moderators <- paste(moderators, collapse ="_and_")
        } 
        if (num_moderators!=0){
          title <- paste0("Direct_effects_of_", moderated_var, "_moderated_by_", moderators)
        } else {
          title <- NULL
        }
      } else {
        title <- NULL
      }
      return(title)
    }
    
    #Model for the outome variable (sol_1) can follow any distribution except for the Multinomial
    if(!(class(sol_1) %in% c('BANOVA', 'BANOVA.Normal', 'BANOVA.T', 'BANOVA.Poisson', 'BANOVA.Bern', 
                             'BANOVA.Bin', 'BANOVA.ordMultinomial'))) stop('The Model is not supported yet')
    if(sol_1$model_name == 'BANOVA.Multinomial') stop('The Model is not supported yet')
    #Model for the mediator (sol_2) must be Normal
    if(sol_2$model_name != 'BANOVA.Normal') stop('The mediator must follow the Normal distribution, use BANOVA Normal models instead.')
    
    #####Outcome variable
    #Extract names of the regressors in the outome model
    X_names = colnames(sol_1$dMatrice$X)
    Z_names = colnames(sol_1$dMatrice$Z)
    #Extract indexes of regressors with correspondace to the variables (Intercept - 0)
    X_assign = attr(sol_1$dMatrice$X, 'assign')
    Z_assign = attr(sol_1$dMatrice$Z, 'assign')
    num_l1 <- length(X_assign) #num level 1 regressors in the outome model
    num_l2 <- length(Z_assign) #num level 2 regressors in the outome model
    #Extract relevant coefficients in the outome model
    if (sol_1$single_level){
      samples_l2_param <- sol_1$samples_l1_param
    } else {
      samples_l2_param <- sol_1$samples_l2_param
    }
    n_sample <- nrow(samples_l2_param)
    est_matrix <- array(0 , dim = c(num_l1, num_l2, n_sample), dimnames = list(X_names, Z_names, NULL))
    for (i in 1:num_l1){
      for (j in 1:n_sample)
        est_matrix[i,,j] <- samples_l2_param[j,((i-1)*num_l2+1):((i-1)*num_l2+num_l2)]
    }
    
    #####Mediator
    #Extract names of the regressors in the mediator model
    X_names_m = colnames(sol_2$dMatrice$X)
    Z_names_m = colnames(sol_2$dMatrice$Z)
    #Extract indexes of regressors with correspondace to the variables (Intercept - 0)
    X_assign_m = attr(sol_2$dMatrice$X, 'assign')
    Z_assign_m = attr(sol_2$dMatrice$Z, 'assign')
    num_l1_m <- length(X_assign_m) #num level 1 regressors in the mediator model
    num_l2_m <- length(Z_assign_m) #num level 2 regressors in the mediator model
    #Extract relevant coefficients in the mediator model
    if (sol_2$single_level) {
      samples_l2_param_m <- sol_2$samples_l1_param
    } else {
      samples_l2_param_m <- sol_2$samples_l2_param
    }
    n_sample_m <- nrow(samples_l2_param_m)
    est_matrix_m <- array(0, dim = c(num_l1_m, num_l2_m, n_sample_m), dimnames = list(X_names_m, Z_names_m, NULL))
    for (i in 1:num_l1_m){
      for (j in 1:n_sample_m)
        est_matrix_m[i,,j] <- samples_l2_param_m[j,((i-1)*num_l2_m+1):((i-1)*num_l2_m+num_l2_m)]
    }
    
    sol <- list()
    #Individal effects can be calculated the outome and mediator model are multi-level
    if (individual){
      model1_level1_var_matrix <- attr(attr(sol_1$mf1, 'terms'),'factors')
      model1_level1_var_dataClasses <- attr(attr(sol_1$mf1, 'terms'),'dataClasses')
      model1_level2_var_matrix <- attr(attr(sol_1$mf2, 'terms'),'factors')
      model1_level2_var_dataClasses <- attr(attr(sol_1$mf2, 'terms'),'dataClasses')
      
      model2_level1_var_matrix <- attr(attr(sol_2$mf1, 'terms'),'factors')
      model2_level1_var_dataClasses <- attr(attr(sol_2$mf1, 'terms'),'dataClasses')
      model2_level2_var_matrix <- attr(attr(sol_2$mf2, 'terms'),'factors')
      model2_level2_var_dataClasses <- attr(attr(sol_2$mf2, 'terms'),'dataClasses')
      
      if (sol_1$single_level || sol_2$single_level){
        stop("It seems to be a between-subject design, set individual = FALSE instead.")
      }
      fit_betas <- rstan::extract(sol_1$stan_fit, permuted = T)
      samples_l1_raw <- fit_betas$beta1
      samples_l1_individual <- aperm(samples_l1_raw, c(2,1,3)) # dimension: num_l1 x sample size x num_id
      dimnames(samples_l1_individual) <- list(X_names, NULL, NULL)
      
      # mediator
      if(is.null(multi_samples_beta1_raw_m)){
        fit_betas_m <- rstan::extract(sol_2$stan_fit, permuted = T)
        samples_l1_raw_m <- fit_betas_m$beta1
        samples_l1_individual_m <- aperm(samples_l1_raw_m, c(2,1,3)) # dimension: num_l1 x sample size x num_id
      } else {
        samples_l1_raw_m <- multi_samples_beta1_raw_m
        samples_l1_individual_m <- aperm(samples_l1_raw_m, c(3,1,2))
      }
      
      dimnames(samples_l1_individual_m) <- list(X_names_m, NULL, NULL)
      
      # calc id map
      id_map = idmap(sol_1$old_id, sol_1$new_id)
      
      # calculate direct effect of xvar in model 1
      sol$dir_effects <- list()
      if (xvar %in% rownames(model1_level1_var_matrix)){
        sol$individual_direct <- list()
        direct_effects <- cal.mediation.effects.individual(sol_1, samples_l1_individual, xvar, "NA")
        for (i in 1:length(direct_effects)){
          # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
          idx_to_rm <- c()
          for (j in 1:dim(direct_effects[[i]]$table_m[,,1,drop = F])[2]){
            if (all(direct_effects[[i]]$table_m[, j, 1] == '1') || all(direct_effects[[i]]$table_m[, j, 1] == 1))
              idx_to_rm <- c(idx_to_rm, j)
          }
          if(length(idx_to_rm) > 0)
            sol$dir_effects[[i]] <- direct_effects[[i]]$table_m[, -idx_to_rm, ,drop = F]
          else
            sol$dir_effects[[i]] <- direct_effects[[i]]$table_m
          sol$individual_direct[[i]] <- rabind(sol$dir_effects[[i]], id_map)
          
          #prepare a title for the table if moderation is present
          element_name <- prepare_list_name(mediator, colnames(sol$dir_effects[[i]]))
          if(!is.null(element_name))
            names(sol$individual_direct)[i] <- element_name
            names(sol$dir_effects)[i] <- element_name
        }
      }else{
        direct_effects <- cal.mediation.effects(sol_1, est_matrix, n_sample, xvar, "NA")
        for (i in 1:length(direct_effects)){
          # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
          idx_to_rm <- c()
          for (j in 1:ncol(direct_effects[[i]]$table_m)){
            if (all(direct_effects[[i]]$table_m[, j] == '1') || all(direct_effects[[i]]$table_m[, j] == 1))
              idx_to_rm <- c(idx_to_rm, j)
          }
          if(length(idx_to_rm) > 0)
            sol$dir_effects[[i]] <- direct_effects[[i]]$table_m[, -idx_to_rm]
          else
            sol$dir_effects[[i]] <- direct_effects[[i]]$table_m
        }
        #prepare a title for the table if moderation is present
        element_name <- prepare_list_name(xvar, colnames(sol$dir_effects[[i]]))
        if(!is.null(element_name))
          names(sol$dir_effects)[i] <- element_name
      }
      # calculate (direct) effects of the mediator in model 1
      if (!(mediator %in% rownames(model1_level1_var_matrix))) stop("The mediator is between subjects, please set individual = FALSE.")
      mediator_l1_effects <- cal.mediation.effects.individual(sol_1, samples_l1_individual, mediator, xvar = "NA", is_mediator = T)
      sol$m1_effects <- list()
      for (i in 1:length(mediator_l1_effects)){
        # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
        idx_to_rm <- c()
        for (j in 1:dim(mediator_l1_effects[[i]]$table_m[,,1, drop = F])[2]){
          if (all(mediator_l1_effects[[i]]$table_m[, j, 1] == '1') || all(mediator_l1_effects[[i]]$table_m[, j, 1] == 1))
            idx_to_rm <- c(idx_to_rm, j)
        }
        if(length(idx_to_rm) > 0)
          sol$m1_effects[[i]] <- mediator_l1_effects[[i]]$table_m[, -idx_to_rm, , drop = F]
        else
          sol$m1_effects[[i]] <- mediator_l1_effects[[i]]$table_m
        #prepare a title for the table if moderation is present
        element_name <- prepare_list_name(mediator, colnames(sol$m1_effects[[i]]))
        if(!is.null(element_name))
          names(sol$m1_effects)[i] <- element_name
      }
      
      # calculate (direct) effects of the xvar on mediator (in model 2)
      sol$m2_effects <- list()
      if (xvar %in% rownames(model2_level1_var_matrix)){ 
        mediator_xvar_effects <- cal.mediation.effects.individual(sol_2, samples_l1_individual_m, xvar)
        for (i in 1:length(mediator_xvar_effects)){
          temp_array <- array(1, dim = c(nrow(mediator_xvar_effects[[i]]$table_m), 1), dimnames = list(NULL, mediator))
          mediator_xvar_effects[[i]]$table_m <- abind(temp_array, mediator_xvar_effects[[i]]$table_m)
          mediator_xvar_effects[[i]]$index_name <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$index_name), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$index_name)
          # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
          idx_to_rm <- c()
          for (j in 1:dim(mediator_xvar_effects[[i]]$table_m[,,1, drop = F])[2]){
            if (all(mediator_xvar_effects[[i]]$table_m[, j, 1] == '1') || all(mediator_xvar_effects[[i]]$table_m[, j, 1] == 1))
              idx_to_rm <- c(idx_to_rm, j)
          }
          if(length(idx_to_rm) > 0)
            sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m[, -idx_to_rm,, drop = F]
          else
            sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m
          
          #prepare a title for the table if moderation is present
          element_name <- prepare_list_name(xvar, colnames(sol$m2_effects[[i]]))
          if(!is.null(element_name))
            names(sol$m2_effects)[i] <- element_name
        }
      }else if (xvar %in% rownames(model2_level2_var_matrix)){
        mediator_xvar_effects <- cal.mediation.effects(sol_2, est_matrix_m, n_sample_m, xvar)
        for (i in 1:length(mediator_xvar_effects)){
          mediator_xvar_effects[[i]]$table_m <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$table_m), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$table_m)
          mediator_xvar_effects[[i]]$index_name <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$index_name), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$index_name)
          # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
          idx_to_rm <- c()
          for (j in 1:ncol(mediator_xvar_effects[[i]]$table_m)){
            if (all(mediator_xvar_effects[[i]]$table_m[, j] == '1') || all(mediator_xvar_effects[[i]]$table_m[, j] == 1))
              idx_to_rm <- c(idx_to_rm, j)
          }
          if(length(idx_to_rm) > 0)
            sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m[, -idx_to_rm]
          else
            sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m
          
          #prepare a title for the table if moderation is present
          element_name <- prepare_list_name(xvar, colnames(sol$m2_effects[[i]]))
          if(!is.null(element_name))
            names(sol$m2_effects)[i] <- element_name
        }
      }
      if (return_posterior_samples){
        sol$direct_effects_samples <- direct_effects
        sol$indirect_effects_samples <- list()
      }
      
      k <- 1
      sol$indir_effects <- list()
      sol$effect_size <- list()
      sol$individual_indirect <- list()
      for (i in 1:length(mediator_l1_effects)){
        for (j in 1:length(mediator_xvar_effects)){
          comb_eff <- combine.effects.individual(mediator_l1_effects[[i]], mediator_xvar_effects[[j]], 
                                                 sol_1$tau_ySq, sol_1$data, mediator, id_map,
                                                 return_posterior_samples)
          indirect_effects <- comb_eff$table
          sol$effect_size[[k]] <- comb_eff$effect_size
          if (return_posterior_samples){
            sol$indirect_effects_samples[[k]] <- comb_eff$samples
          }
          
          idx_to_rm <- c()
          for (j in 1:ncol(indirect_effects)){
            if (all(indirect_effects[, j, 1] == '1') || all(indirect_effects[, j, 1] == 1))
              idx_to_rm <- c(idx_to_rm, j)
          }
          if(length(idx_to_rm) > 0)
            sol$indir_effects[[k]] <- indirect_effects[, -idx_to_rm,, drop = F]
          else
            sol$indir_effects[[k]] <- indirect_effects
          sol$individual_indirect[[k]] <- rabind(sol$indir_effects[[k]])
          
          #Prepare label for the table
          name_effect_X_on_M <- names(sol$m2_effects)
          name_effect_M_on_Y <- names(sol$m1_effects[i])
          
          #Prepare a label for the table with indirect effects
          if (sum(is.null(name_effect_X_on_M), is.null(name_effect_M_on_Y)) != 2){
            if (is.null(name_effect_X_on_M)){
              table_label <- paste0("Indirect_effects_of_", xvar)
            } else {
              name_effect_X_on_M <- gsub("Direct_effects_of_", "", name_effect_X_on_M)
              table_label <- paste0("Indirect_effects_of_", name_effect_X_on_M)
            }
            if (!is.null(name_effect_M_on_Y)){
              name_effect_M_on_Y <- gsub("Direct_effects_of_", "", name_effect_M_on_Y)
              table_label <- paste0(table_label, "_through_", name_effect_M_on_Y)
            }
            names(sol$indir_effects)[k] <- table_label
            names(sol$individual_indirect)[k] <- table_label
          }
          k <- k + 1
        }
      }
      sol$xvar = xvar
      sol$mediator = mediator
      sol$individual = individual
      class(sol) <- 'BANOVA.mediation'
      return(sol)
    }else{
  
      #################
      if(0){ # temporarily replaced by the function cal.mediation.effects
      model1_level1_var_matrix <- attr(attr(sol_1$mf1, 'terms'),'factors')
      model1_level1_var_dataClasses <- attr(attr(sol_1$mf1, 'terms'),'dataClasses')
      model1_level2_var_matrix <- attr(attr(sol_1$mf2, 'terms'),'factors')
      model1_level2_var_dataClasses <- attr(attr(sol_1$mf2, 'terms'),'dataClasses')
      # find the direct effects
      # find corresponding names in X or Z for xvar, see floodlight analysis, then used in est_matrix
      xvar_in_l1 <- xvar %in% rownames(model1_level1_var_matrix)
      xvar_in_l2 <- xvar %in% rownames(model1_level2_var_matrix)
      if (!xvar_in_l1 & !xvar_in_l2) stop("xvar is not included in the model!")
      if (xvar_in_l1){
        attr(xvar, 'class') = model1_level1_var_dataClasses[xvar]
        xvar_index <- which(rownames(model1_level1_var_matrix) == xvar)
        xvar_index_assign <- which(model1_level1_var_matrix[xvar_index, ] == 1)
        xvar_related_names <- X_names[which(X_assign %in% xvar_index_assign)]
        for (xvar_name in xvar_related_names){
          print('Direct effects:')
          est_samples <- array(0, dim = c(n_sample))
          for (n_s in 1:n_sample){
            est_samples[n_s] <- est_matrix[xvar_name, 1, n_s]
          }
          direct_effec_mean <- mean(est_samples)
          quantiles <- quantile(est_samples, c(0.025, 0.975))
          tmp_output <- array(0, dim = c(1,3), dimnames = list(NULL, c('mean', '2.5%', '97.5%')))
          tmp_output[1,1] <- direct_effec_mean
          tmp_output[1,2:3] <- quantiles
          print(tmp_output)
        }
      }else{
        attr(xvar, 'class') = model1_level2_var_dataClasses[xvar]
        xvar_index <- which(rownames(model1_level2_var_matrix) == xvar)
        xvar_index_assign <- which(model1_level2_var_matrix[xvar_index, ] == 1)
        xvar_related_names <- Z_names[which(Z_assign %in% xvar_index_assign)]
        for (xvar_name in xvar_related_names){
          print('Direct effects:')
          est_samples <- array(0, dim = c(n_sample))
          for (n_s in 1:n_sample){
            est_samples[n_s] <- est_matrix[1, xvar_name, n_s]
          }
          direct_effec_mean <- mean(est_samples)
          quantiles <- quantile(est_samples, c(0.025, 0.975))
          tmp_output <- array(0, dim = c(1,3), dimnames = list(NULL, c('mean', '2.5%', '97.5%')))
          tmp_output[1,1] <- direct_effec_mean
          tmp_output[1,2:3] <- quantiles
          print(tmp_output)
        }
      }
      }
      ##################
      # calculate direct effect of xvar in model 1 (based on the outcome model)
      direct_effects <- cal.mediation.effects(sol_1, est_matrix, n_sample, xvar, mediator)
      sol$dir_effects <- list()
      for (i in 1:length(direct_effects)){
        # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
        idx_to_rm <- c()
        for (j in 1:ncol(direct_effects[[i]]$table_m)){
          if (all(direct_effects[[i]]$table_m[, j] == '1') || all(direct_effects[[i]]$table_m[, j] == 1))
            idx_to_rm <- c(idx_to_rm, j)
        }
        if(length(idx_to_rm) > 0)
          sol$dir_effects[[i]] <- direct_effects[[i]]$table_m[, -idx_to_rm]
        else
          sol$dir_effects[[i]] <- direct_effects[[i]]$table_m
        
        #prepare a title for the table if moderation is present
        element_name <- prepare_list_name(xvar, colnames(sol$dir_effects[[i]]))
        if(!is.null(element_name))
          names(sol$dir_effects)[i] <- element_name
      }
      
      # calculate effects of the mediator in model 1
      mediator_l1_effects <- cal.mediation.effects(sol_1, est_matrix, n_sample, mediator, xvar = "NA")
      sol$m1_effects <- list()
      for (i in 1:length(mediator_l1_effects)){
        temp_table <- mediator_l1_effects[[i]]$table_m
        # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
        idx_to_rm <- c()
        for (j in 1:ncol(temp_table)){
          if (all(temp_table[, j] == '1') || all(temp_table[, j] == 1))
            idx_to_rm <- c(idx_to_rm, j)
        }
        if(length(idx_to_rm) > 0)
          sol$m1_effects[[i]] <- temp_table[, -idx_to_rm]
        else
          sol$m1_effects[[i]] <- temp_table
        
        #prepare a title for the table if moderation is present
        element_name <- prepare_list_name(mediator, colnames(sol$m1_effects[[i]]))
        if(!is.null(element_name))
          names(sol$m1_effects)[i] <- element_name
      }
      # calculate effects of the xvar in model 2
      mediator_xvar_effects <- cal.mediation.effects(sol_2, est_matrix_m, n_sample_m, xvar, "NA")
      sol$m2_effects <- list()
      for (i in 1:length(mediator_xvar_effects)){
        mediator_xvar_effects[[i]]$table_m <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$table_m), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$table_m)
        mediator_xvar_effects[[i]]$index_name <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$index_name), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$index_name)
        # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
        idx_to_rm <- c()
        for (j in 1:ncol(mediator_xvar_effects[[i]]$table_m)){
          if (all(mediator_xvar_effects[[i]]$table_m[, j] == '1') || all(mediator_xvar_effects[[i]]$table_m[, j] == 1))
            idx_to_rm <- c(idx_to_rm, j)
        }
        if(length(idx_to_rm) > 0)
          sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m[, -idx_to_rm]
        else
          sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m
        
        #prepare a title for the table if moderation is present
        element_name <- prepare_list_name(xvar, colnames(sol$m2_effects[[i]]))
        if(!is.null(element_name))
          names(sol$m2_effects)[i] <- element_name
      }
  
      k <- 1
      sol$indir_effects <- list()
      sol$effect_size <- list()
      if (return_posterior_samples){
        sol$direct_effects_samples <- direct_effects
        sol$indirect_effects_samples <- list()
      }
      # calculate effects of the xvar in model 2
      for (i in 1:length(mediator_l1_effects)){
        for (j in 1:length(mediator_xvar_effects)){
          comb_eff <- combine.effects(mediator_l1_effects[[i]], mediator_xvar_effects[[j]], sol_1$tau_ySq, 
                                      sol_1$data, mediator, return_posterior_samples)
          
          indirect_effects <- comb_eff$table
          sol$effect_size[[k]] <- comb_eff$effect_size
          if (return_posterior_samples){
            sol$indirect_effects_samples[[k]] <- comb_eff$samples
          }
          
          idx_to_rm <- c()
          for (j in 1:ncol(indirect_effects)){
            if (all(indirect_effects[, j] == '1') || all(indirect_effects[, j] == 1))
              idx_to_rm <- c(idx_to_rm, j)
          }
          
          if(length(idx_to_rm) > 0){
            sol$indir_effects[[k]] <- indirect_effects[, -idx_to_rm]
          } else {
            sol$indir_effects[[k]] <- indirect_effects
          }
          #Prepare label for the table
          name_effect_X_on_M <- names(sol$m2_effects[1])
          name_effect_M_on_Y <- names(sol$m1_effects[i])
          
          #Prepare a label for the table with indirect effects
          if (sum(is.null(name_effect_X_on_M), is.null(name_effect_M_on_Y)) !=2){
            if (is.null(name_effect_X_on_M)){
              table_label <- paste0("Indirect_effects_of_", xvar)
            } else {
              name_effect_X_on_M <- gsub("Direct_effects_of_", "", name_effect_X_on_M)
              table_label <- paste0("Indirect_effects_of_", name_effect_X_on_M)
            }
            if (!is.null(name_effect_M_on_Y)){
              name_effect_M_on_Y <- gsub("Direct_effects_of_", "", name_effect_M_on_Y)
              table_label <- paste0(table_label, "_through_", name_effect_M_on_Y)
            }
            names(sol$indir_effects)[k] <- table_label
          }
          k <- k + 1
        }
      }
      sol$xvar = xvar
      sol$mediator = mediator
      sol$individual = individual
      class(sol) <- 'BANOVA.mediation'
      return(sol)
    }
  }

combine.effects.individual <- function (mediator_l1_effects, mediator_xvar_effects, tau_ySq, data, mediator, id_map,
                                        return_posterior_samples){
  num_id = dim(mediator_l1_effects$samples)[3]
  table_1_names <- mediator_l1_effects$index_name
  table_2_names <- mediator_xvar_effects$index_name
  # find common columns 
  temp_1 <- mediator_l1_effects$index
  colnames(temp_1) <- paste(colnames(temp_1), '.1', sep = "")
  table_1_names_index <- cbind(table_1_names, temp_1)
  temp_2 <- mediator_xvar_effects$index
  colnames(temp_2) <- paste(colnames(temp_2), '.2', sep = "")
  table_2_names_index <- cbind(table_2_names, temp_2)
  table_2_names_index.df <- table_2_names_index
  table_1_names_index.df <- table_1_names_index
  temp_table_index <- merge(table_2_names_index.df, table_1_names_index.df, 
                            by = intersect(colnames(table_1_names), colnames(table_2_names)), all.x = T)
  table_1_est_sample_index <- temp_table_index[,colnames(temp_1), drop = F]
  table_2_est_sample_index <- temp_table_index[,colnames(temp_2), drop = F]
  # standardize the names of this table, so that the output table looks consistant, direct vs indirect, e.g. sort the column names
  union_names <- union(colnames(table_1_names), colnames(table_2_names))
  union_names <- union_names[order(union_names)]
  union_names_original <- union_names
  result_table <- array('1', dim = c(nrow(temp_table_index), length(union_names) + 6, num_id), 
                        dimnames = list(rep("",nrow(temp_table_index)), c(union_names, 'mean', '2.5%', '97.5%', 'p.value', 'id', 'effect size'), NULL))
  
  common_n_sample <- min(dim(mediator_l1_effects$samples)[2], dim(mediator_xvar_effects$samples)[2])
  #result_table_sample <- array('1', dim = c(nrow(temp_table_index), ncol(temp_table_index) - 2 + common_n_sample, num_id), dimnames = list(rep("",nrow(temp_table_index)), c(union_names, paste('s_', 1:common_n_sample, sep = "")), NULL))
  result_table_sample <- array('1', dim = c(nrow(temp_table_index), length(union_names) + common_n_sample, num_id), 
                               dimnames = list(rep("",nrow(temp_table_index)), c(union_names, paste('s_', 1:common_n_sample, sep = "")), NULL))
  
  effect_size <- rep("", num_id)
  for (i in 1:num_id){
    union_names <- union_names_original
    result_table[, 'id', i] <- id_map[i]
    for (nm in union_names){
      result_table[, nm, i] <- as.character(temp_table_index[[nm]])
      result_table_sample[, nm, i] <- as.character(temp_table_index[[nm]])
    }
    for (ind in 1:nrow(table_1_est_sample_index)){
      index_1 <- as.integer(table_1_est_sample_index[ind,1])
      index_2 <- as.integer(table_2_est_sample_index[ind,1])
      m_samples <- mediator_l1_effects$samples[index_1, 1:common_n_sample, i] * mediator_xvar_effects$samples[index_2, 1:common_n_sample, i]
      result_table[ind,'mean', i] <- round(mean(m_samples), 4)
      result_table[ind,c('2.5%', '97.5%'), i] <- round(quantile(m_samples, probs = c(0.025, 0.975)),4)
      result_table[ind,'p.value', i] <- ifelse(round(pValues(array(m_samples, dim = c(length(m_samples), 1))), 4) == 0,
                                               '<0.0001', round(pValues(array(m_samples, dim = c(length(m_samples), 1))), 4))
      result_table_sample[ind, paste('s_', 1:common_n_sample, sep = ""), i] <- m_samples
    }
    # compute effect size for the indirect effect
    if (mediator %in% union_names)
      union_names <- union_names[-which(union_names == mediator)]
    if ('(Intercept)' %in% union_names)
      union_names <- union_names[-which(union_names == '(Intercept)')]
    #remove numeric variable
    to_rm <- c()
    for (to_rm_ind in 1:length(union_names)){
      if (is.numeric(data[,union_names[to_rm_ind]])){
        to_rm <- c(to_rm, to_rm_ind)
      }
    }
    if (length(to_rm) > 0)
      union_names <- union_names[-to_rm]
    data_eff <- data[, union_names, drop = F]
    #If result_table_sample contains only one row it is treated as "character and mergind fails
    if (!inherits(result_table_sample[,,i], "matrix")){
      data_eff_sample <- merge(data_eff, t(data.frame(result_table_sample[,,i])), by = union_names, all.x = TRUE)
    } else {
      data_eff_sample <- merge(data_eff, result_table_sample[,,i], by = union_names, all.x = TRUE)
    }
    data_eff_sample <- apply(data_eff_sample[, paste('s_', 1:common_n_sample, sep = "")], 2, as.character)
    data_eff_sample <- apply(data_eff_sample, 2, as.numeric)
    var_sample <- apply(data_eff_sample, 2, var)
    eff_sample <- var_sample/(var_sample + tau_ySq)
    effect_size[i] <- paste(round(mean(eff_sample), 3), " (", paste(round(quantile(eff_sample, probs = c(0.025, 0.975)),3), collapse = ','), ")", sep="")
    result_table[, 'effect size', i] <- effect_size[i]
  }
  if (return_posterior_samples){
    return_list <- list(table = result_table, effect_size = effect_size, samples = result_table_sample)
  } else {
    return_list <- list(table = result_table, effect_size = effect_size)
  }
  return(return_list)
}

combine.effects <- function (mediator_l1_effects, mediator_xvar_effects, tau_ySq, data, mediator,
                             return_posterior_samples){
  table_1_names <- mediator_l1_effects$index_name   #indexing of the table with direct effects of mediator
  table_2_names <- mediator_xvar_effects$index_name #indexing of the table with indirect effects of xvar
  #Prepare tables with variables and corresponding index of the effects
  temp_1 <- mediator_l1_effects$index
  colnames(temp_1) <- paste(colnames(temp_1), '.1', sep = "")
  table_1_names_index.df <- cbind(table_1_names, temp_1)
  temp_2 <- mediator_xvar_effects$index
  colnames(temp_2) <- paste(colnames(temp_2), '.2', sep = "")
  table_2_names_index.df <- cbind(table_2_names, temp_2)
  temp_table_index <- merge(table_2_names_index.df, table_1_names_index.df, 
                            by = intersect(colnames(table_1_names), colnames(table_2_names)), all.x = T)
  #TODO: what's really going on here?
  if(all(is.na(temp_table_index[colnames(temp_1)]))){
    temp_table_index[colnames(temp_1)] <- table_1_names_index.df[, colnames(temp_1)]
  }
  #Final indexing of tables
  table_1_est_sample_index <- temp_table_index[,colnames(temp_1), drop = F]
  table_2_est_sample_index <- temp_table_index[,colnames(temp_2), drop = F]
  # standardize the names of this table, so that the output table looks consistant, direct vs indirect
  # e.g. sort the column names
  union_names <- union(colnames(table_1_names), colnames(table_2_names))
  union_names <- union_names[order(union_names)]
  
  #Number of samples in the calculation should be the same for the direct and indirect effects
  common_n_sample <- min(dim(mediator_l1_effects$samples)[3], dim(mediator_xvar_effects$samples)[3])
  #Prepare results
  result_table        <- array('1', dim = c(nrow(temp_table_index), length(union_names) + 4), 
                               dimnames = list(rep("",nrow(temp_table_index)), 
                                               c(union_names, 'mean', '2.5%', '97.5%', 'p.value')))
  result_table_sample <- array('1', dim = c(nrow(temp_table_index), length(union_names) + common_n_sample), 
                               dimnames = list(rep("",nrow(temp_table_index)), 
                                               c(union_names, paste('s_', 1:common_n_sample, sep = ""))))
  return_samples <- data.frame(matrix(nrow = nrow(temp_table_index),  ncol = length(union_names) + common_n_sample))
  colnames(return_samples) <- c(union_names, paste('s_', 1:common_n_sample, sep = ""))
  for (name in union_names){
    result_table[, name]        <- as.character(temp_table_index[[name]])
    result_table_sample[, name] <- as.character(temp_table_index[[name]])
    return_samples[, name ]      <- as.character(temp_table_index[[name]])
  }
  #Fill in the table with results by row
  for (ind in 1:nrow(table_1_est_sample_index)){
    #Selct and multiply samples
    m_samples <- mediator_l1_effects$samples[as.integer(table_1_est_sample_index[ind,1]), as.integer(table_1_est_sample_index[ind,2]), 1:common_n_sample] * 
      mediator_xvar_effects$samples[as.integer(table_2_est_sample_index[ind,1]), as.integer(table_2_est_sample_index[ind,2]), 1:common_n_sample]
    #Calculate summary statistics
    result_table[ind,'mean'] <- round(mean(m_samples), 4)
    result_table[ind,c('2.5%', '97.5%')] <- round(quantile(m_samples, probs = c(0.025, 0.975)), 4)
    result_table[ind,'p.value'] <- ifelse(round(pValues(array(m_samples, dim = c(length(m_samples), 1))), 4) == 0, '<0.0001', round(pValues(array(m_samples, dim = c(length(m_samples), 1))), 4))
    #Prepare samples to be returned
    result_table_sample[ind, paste('s_', 1:common_n_sample, sep = "")] <- m_samples
    return_samples[ind, paste('s_', 1:common_n_sample, sep = "")] <- m_samples
  }
  
  ######Compute effect size for the indirect effect
  #Remove mediator and intercept form the union of names
  if (mediator %in% union_names)
    union_names <- union_names[-which(union_names == mediator)]
  if ('(Intercept)' %in% union_names)
    union_names <- union_names[-which(union_names == '(Intercept)')]
  #Remove numeric variable
  to_rm <- c()
  for (i in 1:length(union_names)){
    to_rm <- c(to_rm, i)
  }
  if (length(to_rm) > 0)
    union_names <- union_names[-to_rm]
  
  #Select relevant columns 
  data_eff        <- data[, union_names, drop = F]
  
  data_eff_sample <- merge(data_eff, result_table_sample, by = union_names, all.x = TRUE)
  # remove redundant columns and convert the variance
  data_eff_sample <- apply(data_eff_sample[, paste('s_', 1:common_n_sample, sep = "")], 2, as.character)
  data_eff_sample <- apply(data_eff_sample, 2, as.numeric)
  # culculate effect sizes
  var_sample <- apply(data_eff_sample, 2, var) #variance of each column of the indirect effects samples
  eff_sample <- var_sample/(var_sample + tau_ySq)
  effect_size <- paste(round(mean(eff_sample), 3), " (", paste(round(quantile(eff_sample, probs = c(0.025, 0.975), na.rm = T),3), collapse = ','), ")", sep="")
  
  #sort values column by column
  result_table <- data.frame(result_table, check.names=FALSE)
  result_table <- result_table[do.call(order, result_table), ]
  if (return_posterior_samples){
    return_list <- list(table = result_table, effect_size = effect_size, samples = return_samples)
  } else {
    return_list <- list(table = result_table, effect_size = effect_size)
  }
  return(return_list)
}

# a three dimensional array b cbind its first two dimension matrice with a matrix a (2 dim)
abind <- function(a, b){
  n_id <- dim(b)[3]
  n_r <- dim(b)[1]
  n_c <- dim(b)[2]
  
  dim_a <- dim(a)
  mediator <- colnames(a)
  res <- array(NA, dim = c(n_r, n_c+ncol(a), n_id), dimnames = list(NULL, c(colnames(a), dimnames(b)[[2]]), NULL))
  if(dim_a[1] != 1){
    for (i in 1:n_id){
      res[,,i] <- cbind(a, b[,,i])
    }
  } else {
    for (i in 1:n_id){
      temp <- c(a, b[,,i])
      names(temp)[1:dim_a[2]] <- mediator
      res[,,i] <- temp
    }
  }
  return(res)
}

# rbind all first two dimesion matrice for a 3 dimension matrix
rabind <- function(a, id_map = NULL){
  if(is.null(id_map)){
    res = array(0, dim = c(dim(a)[1], dim(a)[2],0))
    for (i in 1:dim(a)[3]){
      res <- rbind(res, a[,,i])
    }
  }else{
    # the last dimension is the new id
    res = array(0, dim = c(dim(a)[1], dim(a)[2] + 1,0))
    for (i in 1:dim(a)[3]){
      res <- rbind(res, cbind(a[,,i], id = id_map[i]))
    }
  }
  return(res)
}
