results.BANOVA.mlvNormal <- function(fit_beta, dep_var_names, dMatrice, single_level = F){
  # This function combines tables for the multivariate (normal) models. The results for multivariate 
  # models are first obtained for each individual dependent variable (in a for loop) and then is 
  # combined in this function.
  # Param: list_with_tables list with tables to be combined. Each element in the list is expected 
  # to have a name of the corresponding dependent variabel (for example "y1", "y2", etc). 
  # list_with_tables[["y1"]] is expected to have one or more elemnts which are matrices or data frames
  # Param: list_names_first logical. If TRUE the rownames of tables have the names of dependent 
  # variables first, followed by default rownames (for example "y1 (Intercept)"). 
  # If FLASE default rownames are followed by variable names  (for example "(Intercept) y1")
  # Param: list_elements_length numeric. how many elemnts are in each list (must be the same for all dep vars)
  # Param: anova logical. Used to assign a class of the tables
  # Param: conv logical. Used to assign a class of the tables
  combine.tables <- function(list_with_tables, list_names_first = T, list_elements_length = 1,
                                       anova = F, conv = F){
    combine <- function(list_with_tables, element_name = "NA"){
      row_names <- rownames(list_with_tables[[1]])
      col_names <- colnames(list_with_tables[[1]])
      
      if(is.null(row_names)){
        if (is.null(dim(list_with_tables[[1]]))){
          if (length(list_with_tables[[1]]) != 1){
            row_names = " "
            col_names = rep("", length(list_with_tables[[1]]))
          } else {
            row_names = " "
            col_names = " "
          }
        } else {
          dimensions <- dim(list_with_tables[[1]])
          row_names = c(1:dimensions[1])
          col_names = c(1:dimensions[2])
        }
      }
      
      n_row <- length(row_names)
      n_col <- length(col_names)
      
      new_row_names <- c()
      if(list_names_first){
        for (element in list_names)
          new_row_names <- c(new_row_names, format(paste0(element, " ", row_names)))
      }else{
        for (element in list_names)
          new_row_names <- c(new_row_names, format(paste0(row_names, " ", element)))
      }
      
      combined_table <- matrix(NA, nrow = n_row*list_length, ncol = n_col)
      for (i in 1:list_length){
        dv_name <- list_names[i]
        combined_table[(1+n_row*(i-1)):(n_row+n_row*(i-1)),] <- as.matrix(list_with_tables[[dv_name]])
      }
      
      if (element_name == "pass_ind"){
        combined_table <- all(combined_table == TRUE) 
      } else if (element_name == "row_indices"){
        colnames(combined_table) <- c(col_names)
        rownames(combined_table) <- new_row_names
      } else {
        combined_table <- as.data.frame(combined_table)
        colnames(combined_table) <- c(col_names)
        rownames(combined_table) <- new_row_names
      }
      
      return(combined_table)
    }
    
    list_names  <- names(list_with_tables)
    list_length <- length(list_with_tables)
    
    if (list_elements_length == 1){
      combined_tables <- combine(list_with_tables)
    } else {
      element_names <- names(list_with_tables[[1]])
      combined_tables <- list()
      for (element_name in element_names){
        temp_list <- list()
        for (name in list_names)
          temp_list[[name]] <- list_with_tables[[name]][[element_name]]
        combined_tables[[element_name]] <- combine(temp_list, element_name)
      }
      if (anova){
        class(combined_tables) <- "ancova.effect"
      } else if (conv){
        class(combined_tables) <- "conv.diag"
      }
    }
    return(combined_tables)
  }
  combine.samples <- function(list_with_samples){
    list_names  <- names(list_with_samples)
    list_length <- length(list_with_samples)
    n_row <- nrow(list_with_samples[[1]])
    n_col <- ncol(list_with_samples[[1]])
    combined_samples <- matrix(NA, nrow = n_row, ncol = n_col*list_length)
    new_colnames     <- c()
    init_val <- 1
    term_val <- n_col
    for (i in 1:list_length){
      samples <- list_with_samples[[i]]
      combined_samples[, init_val:term_val] <- samples 
      new_colnames <- c(new_colnames, paste0(list_names[i], "_", colnames(samples)))
      init_val <- term_val + 1
      term_val <- n_col*(i+1)
    }
    colnames(combined_samples) <- new_colnames
    return(combined_samples)
    
  }
  
  prep.return.table <- function(matrix_with_samples){
    mean         <- apply(matrix_with_samples, 2, mean)
    sd           <- apply(matrix_with_samples, 2, sd)
    quantile_025 <- apply(matrix_with_samples, 2, quantile, probs = 0.025, type = 3, na.rm = FALSE)
    quantile_975 <- apply(matrix_with_samples, 2, quantile, probs = 0.975, type = 3, na.rm = FALSE)
    p_value      <- pValues(matrix_with_samples)
    
    result <- cbind(mean, sd, pmin(quantile_025, quantile_975), pmax(quantile_025, quantile_975),
                    p_value)
    result <- format(round(result, 4), nsmall = 4)
    #format pValues
    n_columns <- ncol(result)
    p_values  <- as.numeric(result[,  n_columns])
    result[, n_columns] <- ifelse(round(p_values, 4) == 0, '<0.0001', result[,  n_columns])
    colnames(result) <- c("Mean", "SD", "Quantile 0.025", "Quantile 0.975", "p-value")
    return(result)
  }
  
  y <- dMatrice$y
  X <- dMatrice$X
  Z <- dMatrice$Z
  X_names <- colnames(X)
  Z_names <- colnames(Z)
  n_iter  <-  dim(fit_beta$beta1)[1] # number MCMC iterations / number of samples
  n_dv    <- length(dep_var_names)
  
  ##### 
  #Extract R2 
  R2 = NULL
  if (!is.null(fit_beta$r_2)){
    R2 <- colMeans(fit_beta$r_2)
    R2 <- round(R2, 4)
  }
  
  #Extract Sigma (covariane matrix) and variances of y variables
  tau_ySq = NULL
  Sigma   = NULL
  Sigma_samples <- fit_beta$Sigma
  if (!is.null(Sigma_samples)){
    Sigma <- colMeans(Sigma_samples)
    tau_ySq <- diag(Sigma)           # Vector with variances for dep vars
    Sigma <- as.data.frame(round(Sigma, digits = 4), row.names = dep_var_names) 
    colnames(Sigma) <- dep_var_names

    variance_samples <- matrix(NA, nrow = n_iter, ncol = dim(Sigma_samples)[2])
    for (i in 1:n_iter){
      variance_samples[i, ] <- diag(Sigma_samples[i, ,])
    }
    dep_var_sd <- prep.return.table(variance_samples)
    dep_var_sd <- as.data.frame(dep_var_sd)
    rownames(dep_var_sd) <- paste0(dep_var_names," SD")
  }
  
  #Extract Omega (correlation matrix) and variances of y variables
  Omega = NULL
  Omega_samples <- fit_beta$Omega
  if (!is.null(Omega_samples)){
    Omega <- colMeans(Omega_samples)
    Omega <- as.data.frame(round(Omega, digits = 4), row.names = dep_var_names)
    colnames(Omega) <- dep_var_names
    
    correlation_samples <- matrix(NA, nrow = n_iter, ncol = length(Omega[upper.tri(Omega)]))
    for (i in 1:n_iter){
      Omega_sample <- Omega_samples[i, ,]
      correlation_samples[i, ] <- Omega_sample[upper.tri(Omega_sample)]
    }
    cor_names <- c()
    dep_var_names_temp <-  dep_var_names
    for (dep_var in dep_var_names[-n_dv]){
      cor_name <- paste0("Corr(", dep_var, ",", 
                         dep_var_names_temp[dep_var_names_temp!=dep_var],")")
      cor_names <- c(cor_names, cor_name)
      dep_var_names_temp <- dep_var_names_temp[dep_var_names_temp!=dep_var]
    }
   
    dep_var_corr <- prep.return.table(correlation_samples)
    dep_var_corr <- as.data.frame(dep_var_corr)
    rownames(dep_var_corr) <- cor_names
  }
  
  ##### Extract first and second level coefficients ##### 
  if (single_level){

    samples_l2_param <- NULL
    # Identify dimentsions
    beta1_dim <- dim(fit_beta$beta1)
    
    L = beta1_dim[2] # number of dependent variables
    J = beta1_dim[3] # number of subject-level effects (1st level)
    
    # Prepare names of the coefficients 
    beta1_names <- c()
    for (i in 1:J) 
      beta1_names <- c(beta1_names, paste("beta1_",i, sep = ""))
    
    ##### Prepare results for each of L dependent variables ##### 
    samples_l1.list    <- list()
    samples_l2.list    <- NULL
    anova.tables.list  <- list()
    coef.tables.list   <- list()
    pvalue.tables.list <- list()
    conv.list          <- list()
    cat('Constructing ANOVA/ANCOVA tables...\n')
    for (l in 1:L){
      dep_var_name <- dep_var_names[l] #column name of the l_th dependent variable
      
      # Reformat level one coefficient samples sample of the l_th dependent variable
      samples_l1_param <- array(0, dim = c(n_iter, J), dimnames = list(NULL, beta1_names))
      for (i in 1:J){
        samples_l1_param[, i] <- fit_beta$beta1[, l, i]
      }
      samples_l1.list[[dep_var_name]] <- samples_l1_param
      
      # Result tables for of the l_th dependent variable
      anova.tables.list[[dep_var_name]] <- table.ANCOVA(samples_l2_param, Z, X, samples_l1_param, 
                                                         y_val = array(y[,l], dim = c(length(y[,l]), 1)), 
                                                         model = 'Normal')
        
      coef.tables.list[[dep_var_name]]  <- table.coefficients(samples_l1_param, beta1_names, 
                                                              Z_names, X_names, attr(Z, 'assign') + 1, 
                                                              attr(X, 'assign') + 1)
  
      pvalue.tables.list[[dep_var_name]] <- table.pvalue(coef.tables.list[[dep_var_name]]$coeff_table, 
                                                         coef.tables.list[[dep_var_name]]$row_indices, 
                                                         l1_names = attr(dMatrice$Z, 'varNames'), 
                                                         l2_names = attr(dMatrice$X, 'varNames'))
      
      conv.list[[dep_var_name]]          <- conv.geweke.heidel(samples_l1_param, Z_names, X_names)
      class(conv.list[[dep_var_name]]) <- 'conv.diag'
    }
    combined.samples.l2 <- NULL
  } else {
    # Identify dimentsions
    beta1_dim <- dim(fit_beta$beta1)
    beta2_dim <- dim(fit_beta$beta2)
    L = beta2_dim[2] # number of dependent variables
    M = beta1_dim[2] # number of unique subjects
    J = beta1_dim[4] # number of subject-level effects (1st level)
    K = beta2_dim[3] # number of population-level effects (2nd level)

    # Prepare names of the coefficients 
    beta1_names <- c()
    for (i in 1:J){
      for (j in 1:M){
        beta1_names <- c(beta1_names, paste("beta1_",i,"_",j, sep = ""))
      }
    }
    beta2_names <- c()
    for (i in 1:J){
      for (j in 1:K){
        beta2_names <- c(beta2_names, paste("beta2_",i,"_",j, sep = ""))
      }
    }
    
    ##### Prepare results for each of L dependent variables ##### 
    samples_l1.list    <- list()
    samples_l2.list    <- list()
    anova.tables.list  <- list()
    coef.tables.list   <- list()
    pvalue.tables.list <- list()
    conv.list          <- list()
    cat('Constructing ANOVA/ANCOVA tables...\n')
    for (l in 1:L){
      dep_var_name <- dep_var_names[l] #column name of the l_th dependent variable
      
      # Reformat level one and level two coefficient samples sample of the l_th dependent variable
      samples_l1_param <- array(0, dim = c(n_iter, J*M), dimnames = list(NULL, beta1_names))
      for (i in 1:J){
        for (j in 1:M){
          samples_l1_param[, (i-1) * M + j] <- fit_beta$beta1[, j, l, i]
        }
      }
      samples_l2_param <- array(0, dim = c(n_iter, K*J), dimnames = list(NULL, beta2_names))
      for (i in 1:J){
        for (j in 1:K){
          samples_l2_param[, (i-1) * K + j] <- fit_beta$beta2[, l, j, i]
        }
      }
      samples_l1.list[[dep_var_name]] <- samples_l1_param
      samples_l2.list[[dep_var_name]] <- samples_l2_param
      # Result tables for of the l_th dependent variable
      anova.tables.list[[dep_var_name]] <- table.ANCOVA(samples_l1_param, X, Z, 
                                                        samples_l2_param, l1_error = tau_ySq[l])
      coef.tables.list[[dep_var_name]]  <- table.coefficients(samples_l2_param, beta2_names, 
                                                              X_names, Z_names, attr(X, 'assign') + 1, 
                                                              attr(Z, 'assign') + 1)
      pvalue.tables.list[[dep_var_name]] <- table.pvalue(coef.tables.list[[dep_var_name]]$coeff_table, 
                                                         coef.tables.list[[dep_var_name]]$row_indices, 
                                                         l1_names = attr(dMatrice$X, 'varNames'), 
                                                         l2_names = attr(dMatrice$Z, 'varNames'))
      conv.list[[dep_var_name]]          <- conv.geweke.heidel(samples_l2_param, X_names, Z_names)
      class(conv.list[[dep_var_name]]) <- 'conv.diag'
    }
    combined.samples.l2 <- combine.samples(samples_l2.list)
  }
  ##### Combine the results #####
  combined.anova  <- combine.tables(anova.tables.list, T, 2, anova = T)
  combined.coef   <- combine.tables(coef.tables.list, T, 3)
  combined.pvalue <- combine.tables(pvalue.tables.list, T, 1)
  combined.conv   <- combine.tables(conv.list, T, 3, conv = T)
  
  combined.samples.l1 <- combine.samples(samples_l1.list)
  
  cat('Done.\n')
  return(list(combined.anova = combined.anova,
              combined.coef = combined.coef, 
              combined.pvalue = combined.pvalue,
              combined.conv = combined.conv,
              combined.samples.l1 = combined.samples.l1,
              combined.samples.l2 = combined.samples.l2, 
              samples_l1.list = samples_l1.list,
              samples_l2.list = samples_l2.list,
              covariance.matrix = Sigma,
              correlation.matrix = Omega,
              dep_var_sd = dep_var_sd,
              dep_var_corr = dep_var_corr,
              R2 = R2,
              tau_ySq = tau_ySq,
              anova.tables.list = anova.tables.list,
              coef.tables.list = coef.tables.list,
              pvalue.tables.list = pvalue.tables.list,
              conv.list = conv.list,
              num_depenent_variables = length(dep_var_names),
              names_of_dependent_variables = dep_var_names))
}
