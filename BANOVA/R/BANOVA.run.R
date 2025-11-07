#' run a BANOVA model
#'
#' @return A BANOVA stan model.
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
BANOVA.run <- function (l1_formula = 'NA', 
                        l2_formula = 'NA', 
                        fit = NULL, # BANOVA.build object, if it is specified, then directly use the stan model included
                        model_name = 'NA',
                        dataX = NULL, 
                        dataZ = NULL,
                        data = NULL,
                        y_value = NULL,
                        id, 
                        iter = 2000,
                        num_trials = 1,
                        contrast = NULL, # 1.1.2
                        y_lowerBound = -Inf,
                        y_upperBound = Inf,
                        ...
                        ){
  
  # auxiliary function for conversion of effect or dummy coded numeric vectors to factors
  convert.numeric.2.factor <- function(data_vec){ 
    levels <- unique(data_vec)
    dummy_condition <- (length(levels) == 2) && (0 %in% levels) && (1 %in% levels)
    effect_condition <- sum(levels) == 0
    if (effect_condition || dummy_condition){
      #if factors are effect or dummy coded, levels count from the postive to negative values, 
      # so (1, 0, -1) or (1, 0), where "1" is level 1, "0" is level 2, and "-1" is level 3
      lvl <- as.numeric(sort(levels, decreasing = T))
      data_vec <- factor(data_vec, levels = lvl, labels = lvl)
    } else {
      data_vec <- as.factor(data_vec)
    }
    return(data_vec)
  }
  check.numeric.variables <- function(y_var){
    if (!inherits(y_var, 'numeric')){
      warning("The response variable must be numeric (data class also must be 'numeric')")
      y_var <- as.numeric(y_var)
      warning("The response variable has been converted to numeric")
    }
    return(y_var)
  }

  # data sanity check
  if (!is.null(data)){
    if (!is.data.frame(data)) stop("data needs to be a data frame!")
  }
  if (l1_formula == 'NA'){
    stop("Formula in level 1 is missing or not correct!")
  }else{
    single_level = F
    if (is(fit, "BANOVA.build"))
      model_name = fit$model_name
    # validate y
    if (model_name == 'Multinomial'){
      if (is.null(y_value)) stop("y_value (the dependent variable) must be provided!")
      y <- y_value
      mf1 <- model.frame(formula = l1_formula, data = dataX[[1]])
    }else{
      mf1 <- model.frame(formula = l1_formula, data = data)
      y <- model.response(mf1)
    }
    if (model_name %in% c('Normal', 'T')){
      y <- check.numeric.variables(y)
    }else if (model_name %in% c('Poisson', 'Binomial', 'Bernoulli', 'Multinomial', 'ordMultinomial')){
      if (!inherits(y, 'integer')){
        warning("The response variable must be integers (data class also must be 'integer')..")
        y <- as.integer(as.character(y))
        warning("The response variable has been converted to integers..")
      }
      if (model_name == 'ordMultinomial'){
        DV_sort <- sort(unique(y))
        n_categories <- length(DV_sort)
        if (n_categories < 3) stop('The number of categories must be greater than 2!')
        if (DV_sort[1] != 1 || DV_sort[n_categories] != n_categories) stop('Check if response variable follows categorical distribution!') 
        n.cut <- n_categories - 1
      }
      if (model_name == 'Multinomial'){
        DV_sort <- sort(unique(y))
        n_categories <- length(DV_sort)
        if (n_categories < 3) stop('The number of categories must be greater than 2!')
        if (DV_sort[1] != 1 || DV_sort[n_categories] != n_categories) stop('Check if response variable follows categorical distribution!') 
      }
    } else if (model_name == "multiNormal") {
      if (is.null(ncol(y))) stop('The number of dependent variables must be greater than 1!')
      if (is.null(colnames(y))) stop(paste0('Please, specify the names of dependent variables!\n',
                                     'See Examples in help(BANOVA.run) for the expected specification of the dependent variables.'))
      num_dv <- ncol(y)
      for (l in 1:num_dv){
        y[,l] <- check.numeric.variables(y[,l])
      }
    } else if (model_name == 'truncNormal'){
      y <- check.numeric.variables(y)
      #check the specified bounds
      if (y_lowerBound > y_upperBound){
        stop(paste0("The lower bound should be below upper bound!\n", 
                    "Current lower bound of y is ", y_lowerBound, ", and upper bound is ", y_upperBound))
      }
      if (y_lowerBound == y_upperBound){
        stop(paste0("The lower bound should be different from upper bound!\n", 
                    "Current lower bound of y is ", y_lowerBound, ", and upper bound is ", y_upperBound))
      }
      #check if there is an unbounded tail
      no_lower_bound = 0
      no_upper_bound = 0
      if (y_lowerBound == -Inf) no_lower_bound = 1
      if (y_upperBound == Inf) no_upper_bound = 1
      if (no_lower_bound == 1 & no_upper_bound == 1){
        stop(paste0("If the dependent variable is unbounded, please use Normal distributoin!\n", 
                    "Current lower bound of y is ", y_lowerBound, ", and upper bound is ", y_upperBound))
      }
      #check the values of the dependent variable 
      min_y <- min(y)
      max_y <- max(y)
      if (min_y < y_lowerBound){
        stop(paste0("At least one value of the dependent variable exceeds the specified lower bound!\n", 
                    "The lowest value of y is ", min_y, ", while specified lower bound is ", y_lowerBound))
      }
      if (max_y > y_upperBound){
        stop(paste0("At least one value of the dependent variable exceeds the specified upper bound!\n", 
                    "The highest value of y is ", max_y, ", while specified lower bound is ", y_upperBound))
      }
      #check how well the boundaries cover the data
      three_sd_y <- 3*sd(y)
      if(y_lowerBound!=(-Inf) && y_lowerBound<(min_y-three_sd_y)){
        warning("The specified lower bound is more than three standard deviations away from the lowest value of y.\nThis may cause problems with initialization of starting values in the MCMC chains.")
      }
      if(y_upperBound!=(Inf) && y_upperBound>(max_y+three_sd_y)){
        warning("The specified upper bound is more than three standard deviations away from the highest value of y.\nThis may cause problems with initialization of starting values in the MCMC chains.")
      }
    } else{
      stop(model_name, " is not supported currently!")
    }
  }
  if (is.character(id) && length(id) == 1){
    if (id %in% colnames(data))
      old_id = data[, id]
    else
      stop(id, ' is not found in the input data, please assign values directly!')
  }else{
    if (model_name != 'Multinomial') {
      stop('id ambiguous!')
    }else{
      old_id = id
    }
  }
  if (l2_formula == 'NA'){
    # single level models
    single_level = T
    if (is(fit, "BANOVA.build")){
      fit_single_level = fit$single_level
      if (single_level != fit_single_level) stop("Please check the single level settings(T/F)!")
    }
    if (model_name == "Multinomial"){
      # check each column in the dataframe should have the class 'factor' or 'numeric', no other classes such as 'matrix'...
      # for (i in 1:ncol(dataZ)){
      #   if(class(dataZ[,i]) != 'factor' && class(dataZ[,i]) != 'numeric' && class(dataZ[,i]) != 'integer') stop("data class must be 'factor', 'numeric' or 'integer'")
      #   if ((class(dataZ[,i]) == 'numeric' | class(dataZ[,i]) == 'integer') & length(unique(dataZ[,i])) <= 3){
      #     dataZ[,i] <- as.factor(dataZ[,i])
      #     warning("Between-subject variables(levels <= 3) have been converted to factors")
      #   }
      # }
      for (i in 1:length(dataX))
        for (j in 1:ncol(dataX[[i]])){
          if(!inherits(dataX[[i]][,j], 'factor') && !inherits(dataX[[i]][,j], 'numeric') && !inherits(dataX[[i]][,j], 'integer')) stop("data class must be 'factor', 'numeric' or 'integer'")
          if ((inherits(dataX[[i]][,j], 'numeric') | inherits(dataX[[i]][,j], 'integer')) & length(unique(dataX[[i]][,j])) <= 3){
            #convert the column to factors
            dataX[[i]][,j] <- convert.numeric.2.factor(dataX[[i]][,j])
            warning("Within-subject variables(levels <= 3) have been converted to factors")
          }
        }
      n <- length(dataX)
      uni_id <- unique(old_id)
      num_id <- length(uni_id)
      new_id <- rep(0, length(old_id)) # store the new id from 1,2,3,...
      for (i in 1:length(old_id))
        new_id[i] <- which(uni_id == old_id[i])
      id <- new_id
      dMatrice <- multi.design.matrix(l1_formula, l2_formula, dataX = dataX, id = id)
      # create 3-dimensional matrix of X for stan data
      X_new <- array(0, dim = c(n, n_categories, ncol(dMatrice$X_full[[1]])))
      for (i in 1:n_categories){
        X_new[,i,] <- dMatrice$X_full[[i]]
      }
      pooled_data_dict <- list(N = dim(X_new)[1], 
                               J = dim(X_new)[3],
                               n_cat = dim(X_new)[2],
                               X = X_new,
                               y = y)
      if (!is(fit, "BANOVA.build")){
        fit <- get_BANOVA_stan_model(model_name, single_level)
      }else{
        # overwrite the model name and single level using the attributes from the fit
        model_name <- fit$model_name
        single_level <- fit$single_level
      }
      #library(rstan)
      stan.fit <- rstan::sampling(fit$stanmodel, data = pooled_data_dict, iter=iter, ...)
      ### find samples ###
      # beta1 J
      fit_beta <- rstan::extract(stan.fit, permuted = T)
      R2 = NULL
      tau_ySq = NULL
      beta1_dim <- dim(fit_beta$beta1)
      beta1_names <- c()
      for (i in 1:beta1_dim[2]) #J
        beta1_names <- c(beta1_names, paste("beta1_",i, sep = ""))
      samples_l1_param <- array(0, dim = c(beta1_dim[1], beta1_dim[2]), dimnames = list(NULL, beta1_names))
      for (i in 1:beta1_dim[2])
        samples_l1_param[, i] <- fit_beta$beta1[, i]
      samples_l2_sigma_param = NA
      samples_cutp_param = array(dim = 0)
      cat('Constructing ANOVA/ANCOVA tables...\n')
      dMatrice$Z <-  array(1, dim = c(1,1), dimnames = list(NULL, ' '))
      attr(dMatrice$Z, 'assign') <- 0
      attr(dMatrice$Z, 'varNames') <- " "
      samples_l2_param <- NULL
      # print one table for each alternative
      anova.table <- list()
      for (i in 1:n_categories)
        anova.table[[i]] <- table.ANCOVA(samples_l2_param, dMatrice$Z, dMatrice$X_full[[i]], samples_l1_param, error = pi^2/6, multi = T, n_cat = n_categories, choice = i-1, y_val = y_value, model = model_name) #intercept_1 doesn't exist
      coef.tables <- table.coefficients(samples_l1_param, beta1_names, colnames(dMatrice$Z), colnames(dMatrice$X_full[[1]]), 
                                        attr(dMatrice$Z, 'assign') + 1, attr(dMatrice$X_full[[1]], 'assign'), samples_cutp_param )
      pvalue.table <- table.pvalue(coef.tables$coeff_table, coef.tables$row_indices, l1_names = attr(dMatrice$Z, 'varNames'), 
                                   l2_names = attr(dMatrice$X_full[[1]], 'varNames'))
      conv <- conv.geweke.heidel(samples_l1_param, colnames(dMatrice$Z), colnames(dMatrice$X_full[[1]]))
    }else{
      #check only relevant columns
      data_colnames <- colnames(data)
      var_names <- colnames(mf1)[-1]# exclude dv, as it was checked before
      # check each column in the dataframe should have the class 'factor' or 'numeric', no other classes such as 'matrix'...
      for (i in 1:ncol(data)){
        if (data_colnames[i] %in% var_names){
          if(!inherits(data[,i], 'factor') && !inherits(data[,i], 'numeric') && !inherits(data[,i], 'integer')) stop("data class must be 'factor', 'numeric' or 'integer'")
          # checking numerical predictors, converted to categorical variables if the number of levels is <= 3
          if ((inherits(data[,i], 'numeric') | inherits(data[,i], 'integer')) & length(unique(data[,i])) <= 3){
            #convert the column to factors
            data[,i] <- convert.numeric.2.factor(data[,i])
            warning("Variables(levels <= 3) have been converted to factors")
          }
        }
      }
      n <- nrow(data)
      uni_id <- unique(old_id)
      num_id <- length(uni_id)
      new_id <- rep(0, length(old_id)) # store the new id from 1,2,3,...
      for (i in 1:length(old_id))
        new_id[i] <- which(uni_id == old_id[i])
      id <- new_id
      dMatrice <- design.matrix(l1_formula, l2_formula, data = data, id = id, contrast = contrast)
      if (model_name == "Binomial"){
        trials <- num_trials
        if (length(trials) == 1) trials <- rep(num_trials, n) 
        if (length(trials) != n) stop('The length of num_trials must be equal to the number of observations!')
        # handle missing values
        if (sum(y > num_trials, na.rm = T) > 0) stop('The number of trials is less than observations!')
        pooled_data_dict <- list(N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 trials = trials,
                                 X = dMatrice$X,
                                 y = y)
      }else if (model_name == "ordMultinomial"){
        pooled_data_dict <- list(cat = n.cut + 1,
                                 N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 X = dMatrice$X,
                                 Z = dMatrice$Z,
                                 y = y)
      }else if (model_name == 'multiNormal'){
        pooled_data_dict <- list(L = num_dv,
                                 N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 X = dMatrice$X,
                                 y = y)
      }else if (model_name == 'truncNormal'){
        pooled_data_dict <- list(L = y_lowerBound,
                                 U = y_upperBound,
                                 no_lower_bound = no_lower_bound,
                                 no_upper_bound = no_upper_bound,
                                 N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 X = dMatrice$X,
                                 y = y)
      }else{
        pooled_data_dict <- list(N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 X = dMatrice$X,
                                 y = y)
      }
      if (!is(fit, "BANOVA.build")){
        fit <- get_BANOVA_stan_model(model_name, single_level)
      }else{
        # overwrite the model name and single level using the attributes from the fit
        model_name <- fit$model_name
        single_level <- fit$single_level
      }
      stan.fit <- rstan::sampling(fit$stanmodel, data = pooled_data_dict, iter=iter, ...)
      ### find samples ###
      # beta1 J
      fit_beta <- rstan::extract(stan.fit, permuted = T)
      if (model_name != "multiNormal"){
        # For R2 of models with Normal distribution
        R2 = NULL
        if (!is.null(fit_beta$r_2)){
          R2 <- mean(fit_beta$r_2)
          R2 <- round(R2, 4)
        }
        # For the calculation of effect sizes in mediation
        tau_ySq = NULL
        beta1_dim <- dim(fit_beta$beta1)
        beta1_names <- c()
        for (i in 1:beta1_dim[2]) #J
          beta1_names <- c(beta1_names, paste("beta1_",i, sep = ""))
        samples_l1_param <- array(0, dim = c(beta1_dim[1], beta1_dim[2]), dimnames = list(NULL, beta1_names))
        for (i in 1:beta1_dim[2])
          samples_l1_param[, i] <- fit_beta$beta1[, i]
        samples_l2_sigma_param = NA
        if (model_name == 'Poisson'){
          samples_l2_sigma_param <- 0
        }
        samples_cutp_param = array(dim = 0)
        if (model_name == 'ordMultinomial'){
          c_dim <- dim(fit_beta$c_trans)
          c_names <- c()
          for (i in 2:c_dim[2])
            c_names <- c(c_names, paste("c",i, sep = "_"))
          # c_trans[1] == 0
          samples_cutp_param <- array(0, dim = c(c_dim[1], c_dim[2] - 1), dimnames = list(NULL, c_names))
          for (i in 2:c_dim[2])
            samples_cutp_param[, i-1] <- fit_beta$c_trans[,i]
        }
        cat('Constructing ANOVA/ANCOVA tables...\n')
        dMatrice$Z <-  array(1, dim = c(1,1), dimnames = list(NULL, ' '))
        attr(dMatrice$Z, 'assign') <- 0
        attr(dMatrice$Z, 'varNames') <- " "
        samples_l2_param <- NULL
        if (model_name == 'Poisson'){
          anova.table <- table.ANCOVA(samples_l2_param, dMatrice$Z, dMatrice$X, samples_l1_param, y_val = array(y, dim = c(length(y), 1)), error = pi^2/3, model = model_name)
        }else if (model_name %in% c('Normal', 'T', 'truncNormal')){
          anova.table <- table.ANCOVA(samples_l2_param, dMatrice$Z, dMatrice$X, samples_l1_param, array(y, dim = c(length(y), 1)), model = model_name) # for ancova models
          if (!is.null(fit_beta$tau_ySq)){
            tau_ySq <- mean(fit_beta$tau_ySq)
          }
        }else if (model_name == 'Bernoulli' || model_name == 'Binomial'){
          anova.table <- table.ANCOVA(samples_l2_param, dMatrice$Z, dMatrice$X, samples_l1_param, y_val = array(y, dim = c(length(y), 1)), error = pi^2/3, num_trials = num_trials, model = model_name)
          tau_ySq = pi^2/3
        }else if (model_name == 'ordMultinomial'){
          anova.table <- table.ANCOVA(samples_l2_param, dMatrice$Z, dMatrice$X, samples_l1_param, y_val = array(y, dim = c(length(y), 1)), error = pi^2/6, model = model_name)
          tau_ySq = pi^2/6
        }
        coef.tables <- table.coefficients(samples_l1_param, beta1_names, colnames(dMatrice$Z), colnames(dMatrice$X), 
                                          attr(dMatrice$Z, 'assign') + 1, attr(dMatrice$X, 'assign') + 1, samples_cutp_param )
        pvalue.table <- table.pvalue(coef.tables$coeff_table, coef.tables$row_indices, l1_names = attr(dMatrice$Z, 'varNames'), 
                                     l2_names = attr(dMatrice$X, 'varNames'))
        conv <- conv.geweke.heidel(samples_l1_param, colnames(dMatrice$Z), colnames(dMatrice$X))
        mf2 <- NULL
        class(conv) <- 'conv.diag'
        cat('Done.\n')
    } else {
      dMatrice$Z <- array(1, dim = c(1,1), dimnames = list(NULL, ' '))
      attr(dMatrice$Z, 'assign') <- 0
      attr(dMatrice$Z, 'varNames') <- " "
      mlvResult <- results.BANOVA.mlvNormal(fit_beta, dep_var_names = colnames(y), dMatrice, single_level)
      mf2 = NULL
    }
    }
  }else{
    if (model_name == "Multinomial"){
      if (is.null(dataX) || is.null(dataZ)) stop("dataX or dataZ must be specified!")
      mf1 <- model.frame(formula = l1_formula, data = dataX[[1]])
      mf2 <- model.frame(formula = l2_formula, data = dataZ)
      # check each column in the dataframe should have the class 'factor' or 'numeric', no other classes such as 'matrix'...
      for (i in 1:ncol(dataZ)){
        if(!inherits(dataZ[,i], 'factor') && !inherits(dataZ[,i], 'numeric') && !inherits(dataZ[,i], 'integer')) stop("data class must be 'factor', 'numeric' or 'integer'")
        # checking numerical predictors, converted to categorical variables if the number of levels is <= 3
        if ((inherits(dataZ[,i], 'numeric') | inherits(dataZ[,i], 'integer')) & length(unique(dataZ[,i])) <= 3){
          #convert the column to factors
          dataZ[,i] <- convert.numeric.2.factor(dataZ[,i])
          warning("Between-subject variables(levels <= 3) have been converted to factors")
        }
      }
      for (i in 1:length(dataX))
        for (j in 1:ncol(dataX[[i]])){
          if(!inherits(dataX[[i]][,j], 'factor') && !inherits(dataX[[i]][,j], 'numeric') && !inherits(dataX[[i]][,j], 'integer')) stop("data class must be 'factor', 'numeric' or 'integer'")
          # checking numerical predictors, converted to categorical variables if the number of levels is <= 3
          if ((inherits(dataX[[i]][,j], 'numeric') | inherits(dataX[[i]][,j], 'integer')) & length(unique(dataX[[i]][,j])) <= 3){
            #convert the column to factors
            dataX[[i]][,j] <- convert.numeric.2.factor(dataX[[i]][,j])
            warning("Within-subject variables(levels <= 3) have been converted to factors")
          }
        }
      n <- nrow(dataZ)
      uni_id <- unique(old_id)
      num_id <- length(uni_id)
      new_id <- rep(0, length(old_id)) # store the new id from 1,2,3,...
      for (i in 1:length(old_id))
        new_id[i] <- which(uni_id == old_id[i])
      id <- new_id
      dMatrice <- multi.design.matrix(l1_formula, l2_formula, dataX = dataX, dataZ = dataZ, id = id)
      
      # create 3-dimensional matrix of X for stan data
      X_new <- array(0, dim = c(n, n_categories, ncol(dMatrice$X_full[[1]])))
      for (i in 1:n_categories){
        X_new[,i,] <- dMatrice$X_full[[i]]
      }
      pooled_data_dict <- list(N = dim(X_new)[1], 
                               J = dim(X_new)[3],
                               n_cat = dim(X_new)[2],
                               M = nrow(dMatrice$Z),
                               K = ncol(dMatrice$Z),
                               X = X_new,
                               Z = dMatrice$Z,
                               id = id,
                               y = y)
    
    }else{
      if (is.null(data)) stop("data must be specified!")
      mf2 <- model.frame(formula = l2_formula, data = data)
      #check only relevant columns 
      data_colnames <- colnames(data)
      var_names <- c(colnames(mf1), colnames(mf2))[-1] # exclude dv, as it was checked before
      # check each column in the dataframe should have the class 'factor' or 'numeric', no other classes such as 'matrix'...
      for (i in 1:ncol(data)){
        if (data_colnames[i] %in% var_names){
          if(!inherits(data[,i], 'factor') && !inherits(data[,i], 'numeric') && !inherits(data[,i], 'integer')) stop("data class must be 'factor', 'numeric' or 'integer'")
          # checking numerical predictors, converted to categorical variables if the number of levels is <= 3
          if ((inherits(data[,i], 'numeric') | inherits(data[,i], 'integer')) & length(unique(data[,i])) <= 3){
            #convert the column to factors 
            data[,i] <- convert.numeric.2.factor(data[,i])
            warning("Variables(levels <= 3) have been converted to factors")
          }
        }
      }
      n <- nrow(data)
      uni_id <- unique(old_id)
      num_id <- length(uni_id)
      new_id <- rep(0, length(old_id)) # store the new id from 1,2,3,...
      for (i in 1:length(old_id))
        new_id[i] <- which(uni_id == old_id[i])
      id <- new_id
      dMatrice <- design.matrix(l1_formula, l2_formula, data = data, id = id, contrast = contrast)
      if (model_name == "Binomial"){
        trials <- num_trials
        if (length(trials) == 1) trials <- rep(num_trials, n) 
        if (length(trials) != n) stop('The length of num_trials must be equal to the number of observations!')
        # handle missing values
        if (sum(y > num_trials, na.rm = T) > 0) stop('The number of trials is less than observations!')
        pooled_data_dict <- list(N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 M = nrow(dMatrice$Z),
                                 K = ncol(dMatrice$Z),
                                 trials = trials,
                                 X = dMatrice$X,
                                 Z = dMatrice$Z,
                                 id = id,
                                 y = y)
      }else if(model_name == 'ordMultinomial'){
        pooled_data_dict <- list(cat = n.cut + 1,
                                 N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 M = nrow(dMatrice$Z),
                                 K = ncol(dMatrice$Z),
                                 X = dMatrice$X,
                                 Z = dMatrice$Z,
                                 id = id,
                                 y = y)
      }else if(model_name == 'multiNormal'){
        pooled_data_dict <- list(L = num_dv,
                                 N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 M = nrow(dMatrice$Z),
                                 K = ncol(dMatrice$Z),
                                 X = dMatrice$X,
                                 Z = dMatrice$Z,
                                 id = id,
                                 y = y)
      }else if (model_name == 'truncNormal'){
        pooled_data_dict <- list(L = y_lowerBound,
                                 U = y_upperBound,
                                 no_lower_bound = no_lower_bound,
                                 no_upper_bound = no_upper_bound,
                                 N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 M = nrow(dMatrice$Z),
                                 K = ncol(dMatrice$Z),
                                 X = dMatrice$X,
                                 Z = dMatrice$Z,
                                 id = id,
                                 y = y)
      }else{
        pooled_data_dict <- list(N = nrow(dMatrice$X), 
                                 J = ncol(dMatrice$X),
                                 M = nrow(dMatrice$Z),
                                 K = ncol(dMatrice$Z),
                                 X = dMatrice$X,
                                 Z = dMatrice$Z,
                                 id = id,
                                 y = y)
      }
    }
    # get the stan model stored in the package if model is not specified 
    if (!is(fit, "BANOVA.build")){
      fit <- get_BANOVA_stan_model(model_name, single_level)
    }else{
      # overwrite the model name and single level using the attributes from the fit
      model_name <- fit$model_name
      single_level <- fit$single_level
    }
    
    stan.fit <- rstan::sampling(fit$stanmodel, data = pooled_data_dict, iter=iter, verbose=TRUE, ...)
    ### find samples ###
    #Sizes for all models except for Multivariate:beta1 JxM,  beta2 KxJ
    #Sizes for Multivariate Normal model: beta1 MxLxJ, beta2 lxKxJ
    fit_beta <- rstan::extract(stan.fit, permuted = T)
    if (model_name != "multiNormal"){
      # For R2 of models with Normal distribution
      R2 = NULL
      if (!is.null(fit_beta$r_2)){
        R2 <- mean(fit_beta$r_2)
        R2 <- round(R2, 4)
      }
      # For the calculation of effect sizes in mediation
      tau_ySq = NULL
      if (model_name %in% c('Normal', 'T', 'truncNormal')){
        if (!is.null(fit_beta$tau_ySq)){
          tau_ySq <- mean(fit_beta$tau_ySq)
        }
      }else if (model_name == 'Bernoulli' || model_name == 'Binomial'){
        tau_ySq = pi^2/3
      }else if (model_name == 'ordMultinomial' || model_name == 'Multinomial'){
        tau_ySq = pi^2/6
      }else{
        tau_ySq = 0
      }
      beta1_dim <- dim(fit_beta$beta1)
      beta2_dim <- dim(fit_beta$beta2)
      beta1_names <- c()
      for (i in 1:beta1_dim[2]) #J
        for (j in 1:beta1_dim[3]) #M
          beta1_names <- c(beta1_names, paste("beta1_",i,"_",j, sep = ""))
      samples_l1_param <- array(0, dim = c(beta1_dim[1], beta1_dim[2]*beta1_dim[3]), dimnames = list(NULL, beta1_names))
      for (i in 1:beta1_dim[2])
        for (j in 1:beta1_dim[3])
          samples_l1_param[, (i-1) * beta1_dim[3] + j] <- fit_beta$beta1[, i, j]
      beta2_names <- c()
      for (i in 1:beta2_dim[3]) #J
        for (j in 1:beta2_dim[2]) #K
          beta2_names <- c(beta2_names, paste("beta2_",i,"_",j, sep = ""))
      samples_l2_param <- array(0, dim = c(beta2_dim[1], beta2_dim[2]*beta2_dim[3]), dimnames = list(NULL, beta2_names))
      for (i in 1:beta2_dim[3])
        for (j in 1:beta2_dim[2])
          samples_l2_param[, (i-1) * beta2_dim[2] + j] <- fit_beta$beta2[, j, i]
      samples_l2_sigma_param = NA
      if (model_name == 'Poisson'){
        tau_beta1Sq_dim <- dim(fit_beta$tau_beta1Sq)
        tau_beta1Sq_names <- c()
        for (i in 1:tau_beta1Sq_dim[2])
          tau_beta1Sq_names <- c(tau_beta1Sq_names, paste("tau_beta1Sq",i, sep = "_"))
        samples_l2_sigma_param <- array(0, dim = c(tau_beta1Sq_dim[1], tau_beta1Sq_dim[2]), dimnames = list(NULL, tau_beta1Sq_names))
        for (i in 1:tau_beta1Sq_dim[2])
          samples_l2_sigma_param[, i] <- sqrt(fit_beta$tau_beta1Sq[,i])
      }
      samples_cutp_param = array(dim = 0)
      if (model_name == 'ordMultinomial'){
        c_dim <- dim(fit_beta$c_trans)
        c_names <- c()
        for (i in 2:c_dim[2])
          c_names <- c(c_names, paste("c",i, sep = "_"))
        # c_trans[1] == 0
        samples_cutp_param <- array(0, dim = c(c_dim[1], c_dim[2] - 1), dimnames = list(NULL, c_names))
        for (i in 2:c_dim[2])
          samples_cutp_param[, i-1] <- fit_beta$c_trans[,i]
      }
      cat('Constructing ANOVA/ANCOVA tables...\n')
      if (model_name == 'Multinomial'){
        anova.table <- table.ANCOVA(samples_l1_param, dMatrice$X_full[[1]], dMatrice$Z, samples_l2_param, l1_error = tau_ySq) # for ancova models
        coef.tables <- table.coefficients(samples_l2_param, beta2_names, colnames(dMatrice$X_full[[1]]), colnames(dMatrice$Z), 
                                          attr(dMatrice$X_full[[1]], 'assign'), attr(dMatrice$Z, 'assign') + 1, samples_cutp_param)
        pvalue.table <- table.pvalue(coef.tables$coeff_table, coef.tables$row_indices, l1_names = attr(dMatrice$X_full[[1]], 'varNames'), 
                                     l2_names = attr(dMatrice$Z, 'varNames'))
        conv <- conv.geweke.heidel(samples_l2_param, colnames(dMatrice$X_full[[1]]), colnames(dMatrice$Z))
      }else{
        anova.table <- table.ANCOVA(samples_l1_param, dMatrice$X, dMatrice$Z, samples_l2_param, l1_error = tau_ySq) # for ancova models
        coef.tables <- table.coefficients(samples_l2_param, beta2_names, colnames(dMatrice$X), colnames(dMatrice$Z), 
                                          attr(dMatrice$X, 'assign') + 1, attr(dMatrice$Z, 'assign') + 1, samples_cutp_param)
        pvalue.table <- table.pvalue(coef.tables$coeff_table, coef.tables$row_indices, l1_names = attr(dMatrice$X, 'varNames'), 
                                     l2_names = attr(dMatrice$Z, 'varNames'))
        conv <- conv.geweke.heidel(samples_l2_param, colnames(dMatrice$X), colnames(dMatrice$Z))
      }
      class(conv) <- 'conv.diag'
      cat('Done.\n')
    } else {
      mlvResult <- results.BANOVA.mlvNormal(fit_beta, dep_var_names = colnames(y), dMatrice)
    }
  }
    if (model_name == 'Multinomial'){
      sol <- list(anova.table = anova.table,
                  coef.tables = coef.tables,
                  pvalue.table = pvalue.table, 
                  conv = conv,
                  dMatrice = dMatrice, 
                  samples_l1_param = samples_l1_param,
                  samples_l2_param = samples_l2_param, 
                  samples_l2_sigma_param = samples_l2_sigma_param,
                  samples_cutp_param = samples_cutp_param,
                  data = data, 
                  dataX = dataX, 
                  dataZ = dataZ,
                  mf1 = mf1, 
                  mf2 = mf2, 
                  n_categories = n_categories,
                  model_code = fit$stanmodel@model_code, 
                  single_level = single_level,
                  stan_fit = stan.fit,
                  R2 = R2,
                  tau_ySq = tau_ySq,
                  model_name = paste('BANOVA', model_name, sep = "."),
                  contrast = contrast,
                  new_id = new_id,
                  old_id = old_id)
    }else if (model_name == "multiNormal"){
      sol <- list(anova.table = mlvResult$combined.anova,
                  coef.tables = mlvResult$combined.coef,
                  pvalue.table = mlvResult$combined.pvalue, 
                  conv = mlvResult$combined.conv,
                  correlation.matrix = mlvResult$correlation.matrix,
                  covariance.matrix = mlvResult$covariance.matrix,
                  test.standard.deviations.of.dep.var = mlvResult$dep_var_sd,
                  test.residual.correlation = mlvResult$dep_var_corr,
                  dMatrice = dMatrice, 
                  samples_l1_param = mlvResult$combined.samples.l1,
                  samples_l2_param = mlvResult$combined.samples.l2, 
                  samples_l2_sigma_param = NA,
                  samples_cutp_param = array(dim = 0),
                  data = data, 
                  num_trials = num_trials,
                  mf1 = mf1, 
                  mf2 = mf2, 
                  model_code = fit$stanmodel@model_code, 
                  single_level = single_level,
                  stan_fit = stan.fit,
                  R2 = mlvResult$R2,
                  tau_ySq = mlvResult$tau_ySq,
                  model_name = paste('BANOVA', model_name, sep = "."),
                  contrast = contrast,
                  new_id = new_id,
                  old_id = old_id,
                  samples_l1.list = mlvResult$samples_l1.list,
                  samples_l2.list = mlvResult$samples_l2.list,
                  anova.tables.list = mlvResult$anova.tables.list,
                  coef.tables.list = mlvResult$coef.tables.list,
                  pvalue.tables.list = mlvResult$pvalue.tables.list,
                  conv.list = mlvResult$conv.list,
                  num_depenent_variables = mlvResult$num_depenent_variables,
                  names_of_dependent_variables = mlvResult$names_of_dependent_variables)
    } else {
      sol <- list(anova.table = anova.table,
                  coef.tables = coef.tables,
                  pvalue.table = pvalue.table, 
                  conv = conv,
                  dMatrice = dMatrice, 
                  samples_l1_param = samples_l1_param,
                  samples_l2_param = samples_l2_param, 
                  samples_l2_sigma_param = samples_l2_sigma_param,
                  samples_cutp_param = samples_cutp_param,
                  data = data, 
                  num_trials = num_trials,
                  mf1 = mf1, 
                  mf2 = mf2, 
                  model_code = fit$stanmodel@model_code, 
                  single_level = single_level,
                  stan_fit = stan.fit,
                  R2 = R2,
                  tau_ySq = tau_ySq,
                  model_name = paste('BANOVA', model_name, sep = "."),
                  contrast = contrast,
                  new_id = new_id,
                  old_id = old_id)
    }
  sol$call <- match.call()
  class(sol) <- "BANOVA"
  sol
}

# Load BANOVA compiled model
get_BANOVA_stan_model <- function(model, single_level) {
  if (model == 'NA') stop("a model name must be provided!")
  if (single_level)
    name <- paste('single', 'BANOVA.RData', sep = '_')
  else
    name <- 'BANOVA.RData'
  stanmodel <- tryCatch({
    model_file <- system.file('libs', Sys.getenv('R_ARCH'), name,
                          package = 'BANOVA',
                          mustWork = TRUE)
    load(model_file)
    if(single_level)
      obj <- paste(model, 'Normal_stanmodel_1', sep = '_')
    else
      obj <- paste(model, 'Normal_stanmodel', sep = '_')
    stanm <- eval(parse(text = obj))
    stanm
  }, error = function(cond) {
    compile_BANOVA_stan_model(model, single_level)
  })
  return(stanmodel)
}

# Compile BANOVA stan model
compile_BANOVA_stan_model <- function(model, single_level) {
  if (single_level)
    name <- paste('stan/single_', model, '.stan', sep = '')
  else
    name <- paste('stan/',model, '_Normal.stan', sep = '')
  model <- BANOVA.model(model, single_level)
  stanmodel <- BANOVA.build(model)

  return(stanmodel)
}
