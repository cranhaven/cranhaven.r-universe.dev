#' Simple effects calculation
#' 
#' \code{BANOVA.simple} is a function for probing interaction effects in models where
#' both  moderator  and  explanatory  variables  are  factors  with an arbitrary  number 
#' of levels. The function estimates and tests simple or partial effects, also known as simple main
#' or conditional effects. Both single-level and multi-level models with any of the distributions accommodated in 
#' the package can be analyzed. 
#' @usage BANOVA.simple(BANOVA_output, base = NULL, quantiles = c(0.025, 0.975), 
#' dep_var_name = NULL, return_posterior_samples = FALSE)
#' @param BANOVA_output an object of class "BANOVA" returned by BANOVA.run function with 
#' an outcome of the hierarchical Bayesian ANOVA analysis.
#' @param base a character string which specifies the name of the mediator variable used as a base 
#' for calculation.
#' @param quantiles a numeric vector with quantiles for the posterior interval of the simple effects.
#' Must include two elements with values between 0 and 1 in ascending order, default c(0.025, 0.975)
#' @param dep_var_name a character string with a name of the dependent variable, for the Multinomial model only, 
#' default NULL.
#' @param return_posterior_samples logical indicator of whether samples of the posterior simple effects
#' distributions should be returned, default \code{FALSE}.
#' @return Returns a list with the summary tables of the results; optionally returns the 
#' samples drawn from the posterior simple effects distributions.
#
#' \item{\code{results_summary}}{a list of tables with summaries of the posterior simple effects distributions
#'   for all factors and their combinations that are interacting with a moderating variable.}
#' \item{\code{samples_simple_effects}}{if \code{return_posterior_samples} is set to \code{TRUE} 
#' a list of tables with samples of the posterior simple effects is returned. The tables include results
#' for all levels of all factors and their combinations that are interacting with a moderating variable.}
#' @details  The function identifies all factors and their combinations that are interacting with a moderating of "base"
#' variable. For each interaction, it determines all possible level combinations of the involved regressors,
#' which are further used to combine the posterior samples of the selected regression coefficients to calculate 
#' simple effects. 
#' 
#' When the default effect coding scheme is used the simple effects are calculated for all levels of the 
#' interacting variables, as specified in the data. If a user specifies different contrasts for any of the interacting 
#' variables the simple effects for these variables are reported for the user-defined 
#' regressors. This distinction is reflected in the labels of the reported results: in the default case labels from the 
#' original factors are displayed; in the case of user-defined contrasts, the name of the regressor is displayed instead. 
#' 
#' The summary of the posterior distribution of each simple effect contains the mean, 
#' standard deviation, posterior interval, which by default reports a central 95\% interval, 
#' but can also be specified by the user, and a two-sided Bayesian p-value. 
#' 
#' Note that for a Multinomial model intercepts and between-subject regressors have choice specific 
#' coefficients and thus simple effects are reported for each possible choice outcome. To perform the 
#' calculation for a Multinomial model an additional argument \code{dep_var_name} with a name of the 
#' dependent variable must be specified.
#' @examples 
#' # Use the colorad data set
#' data(colorad)
#' \donttest{
#' # Build and analyze the model
#' model <- BANOVA.model('Binomial')
#' banova_model <- BANOVA.build(model)
#' res_1 <- BANOVA.run(y ~ typic, ~ color*blurfac, fit = banova_model,
#'                     data = colorad, id = 'id', num_trials = as.integer(16), 
#'                     iter = 2000, thin = 1, chains = 2)
#' # Calculate simple effects with "blurfac" as a moderating vriable
#' simple_effects <- BANOVA.simple(BANOVA_output = res_1, base = "blurfac")
#' }
#' @author Anna Kopyakova
#' @export
BANOVA.simple <- function(BANOVA_output = "NA", base = NULL, quantiles = c(0.025, 0.975), dep_var_name = NULL, 
                          return_posterior_samples = FALSE){

  check.quantiles <- function(){
    #quantiles must be real numbers
    if(!is(quantiles, "numeric")){
      stop('The "quantiles" must be of a class numeric, which mean you need to specify 
      this argument as a vector with two real numbers between 0 and 1, in an ascending order')
    }
    #check if specified quantiles are each in range betrween 0 and 1
    if (sum(quantiles < 1) != 2){
      stop('Each specified quantile value must be below 1. Please check the "quantiles" argument')
    } else if (sum(quantiles > 0) != 2){
      stop('Each specified quantile value must be above 0. Please check the "quantiles" argument')
    }
  }
  
  # update.regressor.names changes names of the regressors in the two-level models, as if they would
  # be regressors in a single-level models. It removes "(Intercept)" in the interacting regressors.
  update.regressor.names <- function(names_regressors_output){

    if (BANOVA_output$model_name == "BANOVA.Multinomial"){
      #Multinomial model has choice specific intercepts and thus generic name is not needed
      name_regressors_updated <- c()
    } else {
      name_regressors_updated <- c(intercept_name)
    }
    
    for (regressor_name in names_regressors_output){
      #split the name into interacting variables
      name_string    <- strsplit(regressor_name, ":")[[1]] 
    
      #remove spaces around the strings
      string_elements <- c()
      for (j in name_string){
        string_elements <- c(string_elements, gsub(" ", "", j))
      }
      
      #select all interacting variables except for the intercept
      non_intercept_vars <- string_elements[string_elements != intercept_name]
      
      if (length(non_intercept_vars) > 1){
        #if there are at least 2 interacting variables concatenate them via ":"
        variable <- paste0(non_intercept_vars, collapse = ":")
        name_regressors_updated <- c(name_regressors_updated, variable)
      } else {
        #if a single variable is left add it to the updated regressor names
        name_regressors_updated <- c(name_regressors_updated, non_intercept_vars)
      }
    }
    
    return(name_regressors_updated)
  }
  
  # make.formula combines regressors in the fist and second levels of the two-level models into one
  make.formula <- function(pvalue.table, dep_var){
    # select names of level one variables
    l1_variables <- rownames(pvalue.table)
    if (BANOVA_output$model_name == "BANOVA.Multinomial"){
      # For Multinomial models choice specific intercepts are skipped from a variable list
      l1_variables <- l1_variables[-c(1:(n_categories-1))]
    } else {
      if (length(l1_variables) == 1){
          l1_variables <- c() # if there is only the intercept keep an empty string
        } else {
          # if other variables are present remove the intercept
          l1_variables <- l1_variables[which(l1_variables != intercept_name)]
        }
    }

    # select names of level two variables
    l2_variables <- colnames(pvalue.table)
    # if there is only the intercept keep an empty string
    if (length(l2_variables) == 1){
      l2_variables <- c()
    } else {
      # if other variables are present remove the intercept
      l2_variables <- l2_variables[which(l2_variables != intercept_name)]
    }
    
    # combine the variables in a single string separated by "+"
    single_vars <-  paste0(c(l1_variables, l2_variables), collapse = " + ")
    
    # if there are non intercept variables in the first level create their interaction with second
    # level variables
    interaction_var_list <- c()
    if (length(l1_variables) != 0){
      for (i in l2_variables){ 
        for (j in l1_variables){
          term <- paste0(j, ":", i)
          interaction_var_list <- c(interaction_var_list, term)
        }
      }
    }
    
    # combine the interactions of first and second level variables in a single string separated by "+"
    interaction_vars <- paste0(interaction_var_list, collapse = " + ")
    
    # conbine all variables
    if(interaction_vars == ""){
      all_terms <- single_vars
    } else {
      all_terms <- paste0(c(single_vars, interaction_vars), collapse = " + ")
    }
    
    #create a final formula
    formula <- paste0(dep_var, " ~ ", all_terms)
    return(formula)
  }
  
  
  perform.calculation <- function(){
    
    # build.title creates title for the tables with results
    build.title <- function(){
      non_base_string <- ""
      and <- ""
      for (i in 1:length(non_base_vars_names)){
        if (i > 1){
          and = ":"
        }
        non_base_string <- paste0(non_base_string, and, non_base_vars_names[i])
      }
      title <- paste0("Simple effects of ",  non_base_string, " at each level of ", base, " factor" )
      return(title)
    }
    
    # create.table creates tables with results used for printing
    create.table <- function(remove_last_level = F){

      # is.effect.coded checks if the user specified contrasts are a variation of effect coding scheme
      is.effect.coded <- function(coding_matrix){
        #if a contrast with only one column is used it must be converted to a matrix
        if (inherits(coding_matrix, "numeric")){
          coding_matrix <- as.matrix(coding_matrix)
        }
        n = dim(coding_matrix)[1]
        k = dim(coding_matrix)[2]
        
        default_contrasts = contr.sum(n)
        #check if there is the same number of regressor in the coding schemes
        if (ncol(default_contrasts) == k){
          #check if the contrast is exactly the same as default
          if (all(default_contrasts == coding_matrix)){
            return(T)
          } else {
            #check if all elements in the matrices are the same
            condition2 = all(sort(default_contrasts) == sort(coding_matrix))
            #columns sum up to zero
            condition3 = all(colSums(coding_matrix) == 0)
            #the same level must be considered as a reference 
            condition4 = sum(rowSums(coding_matrix == -1) == k) == 1
            #only one row should sum to k (the reference row)
            condition5 = sum(abs(rowSums(coding_matrix)) == k) == 1
            if (condition2 && condition3 && condition4 && condition5){
              return(T)
            }
          }
        }
        return(F)
      }
      
      # fill in the results
      result_table <- matrix(NA, nrow = n_cases, ncol = ncol(table))
      result_table <- table
  
      Q_1 <-  paste0(" Quantile ",  min(quantiles))
      Q_2 <-  paste0(" Quantile ",  max(quantiles))
      colnames(result_table) <- c("    Simple effect", "        SD", Q_1, Q_2, "  p-value")
      colnames_return_table <- c("Simple effect", "SD",  paste0("Quantile ",  min(quantiles)),
                                 paste0("Quantile ",  max(quantiles)), "p-value")
      
      # fill in the level indices
      index_table_names <- list(NULL, colnames = c(base, non_base_vars_names))
      index_table       <- matrix(NA, nrow = nrow(effect_matrix), ncol = n_selected_vars,
                                  dimnames = index_table_names)

      # if user defines contrasts different labelling is used
      # the unspecified levels of the variable must be skipped from the table, as it is not meaningful
      contrasts <- BANOVA_output$contrast
      if (!is.null(contrasts)){
        variables_with_contrasts <- names(contrasts)
        index_table              <- matrix(as.factor(level_index[, index_table_names[["colnames"]]]), 
                                           ncol = n_selected_vars, dimnames = index_table_names)
        # update default index_table for the variables with user defined contrasts 
        for (var in variables_with_contrasts){
          if (var %in% index_table_names[["colnames"]]){
            if (!is.effect.coded(contrasts[[var]])){
              # level labels in the table are strins with variable name and regressor number
              index_table[, var] <- matrix(as.factor(level_index_strings[, var]), ncol = length(var),
                                           dimnames = list(NULL, var))
              remove_last_level  <- TRUE
            }
          }
        }
      } else { 
        #default index_table
        index_table           <- matrix(as.factor(level_index[, index_table_names[[2]]]), ncol = n_selected_vars,
                                        dimnames = index_table_names)
      }
      
      # combine tables
      result <- cbind(index_table, result_table)
      rownames(result) <- rep('', n_cases)
      
      # drop last level(s) if user defines contrasts
      if (remove_last_level){
        for (var in variables_with_contrasts){
          if (var %in% colnames(index_table)){
            levels_factor <- unique(index_table[, var])
            n_contrasts   <- dim(as.matrix(contrasts[[var]]))[2]
            last_levels   <- levels_factor[(n_contrasts+1):length(levels_factor)]
            for (level in last_levels){
              result <- result[result[, var] != level,]
            }
          }
        }
      }
      return(list(result = result, colnames_return_table = colnames_return_table))
    }
    
    ##### Main calculation ####
    var_names          <- attr(design_matrix, "varNames")
    var_interactions   <- attr(design_matrix, "interactions")
    var_classes        <- attr(design_matrix, "dataClasses")
    var_levels_index   <- attr(design_matrix, "assign")
    interactions_index <- attr(design_matrix, "interactions_index")
    n_regressors       <- ncol(design_matrix)
    var_values         <- attr(design_matrix, "varValues")
    var_index          <- c(1:(length(var_names)))
    
    # check if the base variable is in the model
    if (!(base %in% var_names)){
      stop("The specified base variable is not in the model. Please check the base argument.")
    }
  
    # check the classes of interacting variables 
    if (var_classes[base] != "factor"){
      stop("Base variable must be a factor")
    }
    
    # check if there are interactions between factor variables
    if (length(var_interactions) == 0){
      stop("There are no interactions between factor variables.")
    } else {
      if (!single_level_multinomial){
        var_values[[1]]    <- NULL # remove y var 
        var_index          <- var_index-1 #move the index such that counting starts from zero
        #allign var_interactions indices with var_levels_index
        for (i in 1: length(var_interactions))
          var_interactions[[i]] <- var_interactions[[i]] - 1 
      }
    }
    
    # find base variables and indices of it's levels
    base_index        <- var_index[var_names == base]
    base_levels_names <- names_regressors[var_levels_index == base_index]
    
    # find which variables base interacts with
    base_interactions <- c()
    for (i in 1:length(var_interactions)){
      interaction <- var_interactions[[i]]
      if (base %in% names(interaction)){
        base_interactions <- c(base_interactions, i)
      }
    }
    if(return_posterior_samples){
      simple_effect_return <- list()
    }
    
    # for each combination of interactions (block) with the base variable
    for(block in base_interactions){
      
      selected_vars        <- var_interactions[[block]] # variables in the interaction
      selected_vars_values <- var_values[selected_vars] # select values of relevant variables
      n_selected_vars      <- length(selected_vars)     # number of variables included in an interaction
      n_non_base_vars      <- n_selected_vars-1         # number of non-base variables
      
      # obtain effect matrix with intercept, regressors of the interacting variables, and thier interactions
      effect_matrix <- effect.matrix.interaction(interaction_factors = selected_vars_values, assign = var_levels_index, 
                                                   selected_vars, index_inter_factor = interactions_index[block], 
                                                   numeric_index = attr(design_matrix, 'numeric_index'), 
                                                   contrast = NULL) 
      
      n_cases        <- nrow(effect_matrix) # number of possible combinations of caseses
      
      # order the effect matrix
      level_index         <- attr(effect_matrix,"levels")   # combinations of levels in the effect matrix
      vars_names          <- colnames(level_index)          # names of selected variables
      non_base_vars_names <- vars_names[vars_names != base] # names of selected non-base variables 
      
      if(!is.null(BANOVA_output$contrast)){
        # if user defines contrasts then it might be that a different number of regressors are analyzed
        # remove columns of effect matrix that correspond to non existing coefficients
        keep_columns  <- intersect(colnames(coefficients), colnames(effect_matrix))
        effect_matrix <- effect_matrix[,keep_columns]
      }
  
      # create a level_index tables with string labels (as is in the BANOVA.run: var_name1, var_name2,...)
      level_index_strings <- level_index
      for (i in 1:ncol(level_index_strings)){
        factor_levels <- unique(level_index_strings[,i])
        num_levels    <- length(factor_levels)
        for (j in 1:num_levels){
          level_index_strings[,i][level_index_strings[,i] == factor_levels[j]] <-  paste0(vars_names[i], j)
        }
      }
      
      #order of the base variable (for a two way interaction)
      base_order  <- order(level_index_strings[, base])
      
      temp_index         <- level_index_strings
      level_index_temp   <- level_index
      effect_matrix_temp <- effect_matrix
      if (n_non_base_vars != 1){
        #first order in the non base variables if there are more than one of them
        for (i in 0:(n_non_base_vars-1)){
          order_index <- order(temp_index[, non_base_vars_names[n_non_base_vars-i]])
          temp_index  <- temp_index[order_index, ]
          
          effect_matrix_temp    <- effect_matrix_temp[order_index, ]
          level_index_temp      <- level_index_temp[order_index, ]
          level_index_strings <- level_index_strings[order_index, ]
        }
        base_order <- order(temp_index[ ,base])
      }
      #order the effect matrix on the base variable
      effect_matrix <- effect_matrix_temp[base_order, ]
      level_index   <- level_index_temp[base_order, ]
      level_index_strings <- level_index_strings[base_order, ]

      #intercept and the levels of the base variable should not be included in the simple effects
      effect_matrix[, intercept_name]    <- 0
      effect_matrix[, base_levels_names] <- 0
      coef_temp      <- coefficients[, colnames(effect_matrix)] #coefficients of relevant regressors

      if(return_posterior_samples){
        simple_effect_samlpes <- matrix(NA, nrow = nrow(coefficients), ncol = n_cases)
      }
      
      table <- matrix(NA, nrow = n_cases, ncol = 5)
      for (j in 1:n_cases){
        simple_effects <- coef_temp %*% (effect_matrix[j,]) 
        
        mean         <- apply(simple_effects, 2, mean)
        sd           <- apply(simple_effects, 2, sd)
        quantile_025 <- apply(simple_effects, 2, quantile, probs = min(quantiles), type = 3, na.rm = FALSE)
        quantile_975 <- apply(simple_effects, 2, quantile, probs = max(quantiles), type = 3, na.rm = FALSE)
        p_value      <- pValues(simple_effects)
        
        table[j,] <- cbind(mean, sd, pmin(quantile_025, quantile_975), pmax(quantile_025, quantile_975),
                           p_value)
        
        if(return_posterior_samples){
          simple_effect_samlpes[,j] <- simple_effects
        }
        
      }
      # Format the tables
      table <- format(round(table, 4), nsmall = 4)
      title <- build.title()
      create_table_result <- create.table()
      table               <- create_table_result$result
      
      if(is.null(nrow(table))){
        table <- t(as.matrix(table, nrow = 1))
        dimnames(table)[[1]] <- ""
      }
      return_table <- data.frame(table)
      table        <- as.table(table)
      #format pValues
      n_columns <- ncol(table)
      p_values  <- as.numeric(table[,  n_columns])
      table[, n_columns] <- ifelse(round(p_values, 4) == 0, '<0.0001', table[,  n_columns])
      
        
      # Print results 
      cat('\n')
      cat(title)
      cat('\n\n')
      print(table, right=T, digits = 4) 
      
      # Prepare values to be returned
      var_names <- names(selected_vars)
      n_vars    <- length(var_names)
      
      rownames(table) <- NULL
      colnames(return_table)[(n_vars+1):ncol(return_table)] <- create_table_result$colnames_return_table
 
      sol_tables[[title]] <- return_table
      
      #Prepare optional posterior samples
       if(return_posterior_samples){
        var_names <- c(base, var_names[var_names != base])
        
        interaction <- paste0(c(base, var_names[var_names != base]), collapse = ":")
        label       <- paste0("Simple effects for ", interaction)
        
        var_name_levels <- matrix(NA, nrow = n_cases, ncol = n_vars)
        for(i in 1:n_vars){
          var <- var_names[i]
          var_name_level <- paste(var,  table[,var], sep=":")
          var_name_level <- paste0("(", var_name_level, ")")
          var_name_levels[, i] <- var_name_level
        }
        colnames(simple_effect_samlpes) <- apply(var_name_levels, 1, paste0, collapse = ":")
        simple_effect_return[[label]] <- simple_effect_samlpes
      } 
    } 
    if(return_posterior_samples){
      return(list(results_summary = sol_tables, samples_simple_effects = simple_effect_return))
    } else {
      return(list(results_summary = sol_tables))
    }
  }
  
  #MAIN FUNCTION----
  #Perform input checks
  if (!is(BANOVA_output, "BANOVA")){
    stop('"BANOVA_output" must be of a class "BANOVA", which is an outup of BANOVA.run function')
  }
  if (!is(base, "character")){
    stop('"base" must be of a class "character", which mean you need to pass it as a string')
  }
  check.quantiles()
  
  sol_tables <- list()
  model_name <- BANOVA_output$model_name
  single_level_multinomial <- FALSE
  # Multinomial models ----
  if (model_name == "BANOVA.Multinomial"){
    intercept_name <- ("(Intercept)")
    if (BANOVA_output$single_level){
      # for a single level model design matrix is extracted from BANOVA_output
      single_level_multinomial = TRUE
      
      design_matrix    <- BANOVA_output$dMatrice$X_full[[1]]
      names_regressors <- colnames(design_matrix)
      
      coefficients           <- BANOVA_output$samples_l1_param #select semples of the lvl1 parameters
      colnames(coefficients) <- names_regressors
      colnames(coefficients)[1] <- intercept_name
      
    } else {
      
      n_categories <- BANOVA_output$n_categories
      
      l1_vars   <- BANOVA_output$dMatrice$X_original_choice # list of length n_categories
      l2_vars   <- BANOVA_output$mf2                        # matrix with between subject variables
      n_l2_regs <- ncol(BANOVA_output$dMatrice$Z_full)      # number of lel2 regressors
      
      # check and select the dependent variable
      if (is.null(dep_var_name)){
        stop("The label for the dependent variable is not specified")
      } else if (!is(dep_var_name, "character")) {
        stop('The label for the dependent variable must be of a class "character", 
             which mean you need to pass it as a string')
      }
      dep_var           <- as.data.frame(BANOVA_output$data[, dep_var_name])
      colnames(dep_var) <- dep_var_name
      
      # create a temporary data frame for used to create a design matrix
      l1_df     <- as.data.frame(l1_vars[[1]]) #needed to create a formula
      data_temp <- cbind(l1_df, l2_vars, dep_var)
      
      # create a formula
      formula        <- make.formula(BANOVA_output$pvalue.table, dep_var_name)
      design_matrics <- design.matrix(l1_formula = formula, l2_formula = 'NA', data = data_temp, 
                                      contrast = BANOVA_output$contrast)
      design_matrix    <- design_matrics$X
      names_regressors <- colnames(design_matrix)
       
      # select original coefficients and rename them
      original_coefficients <- BANOVA_output$samples_l2_param #select semples of the lvl2 parameters
      all_reg_names         <- update.regressor.names(rownames(BANOVA_output$coef.tables$coeff_table))
      colnames(original_coefficients) <- all_reg_names
      
      # select coefficients that are not choice specific
      common_coefficients <- data.frame(original_coefficients[, intersect(names_regressors, all_reg_names)])
      colnames(common_coefficients) <- intersect(names_regressors, all_reg_names)
      
      for (k in 1:n_categories){
        sol_tables <- list()
        # select coefficients relevant for this choice
        if (k > 1){
          index <- c(1:n_l2_regs) + n_l2_regs*(k-2)
          coefficients_choice_specific <- original_coefficients[,index]
        } else {
          coefficients_choice_specific <- original_coefficients[,c(1:n_l2_regs)]
          coefficients_choice_specific[,] <- 0
        }
        colnames(coefficients_choice_specific) <- setdiff(names_regressors, all_reg_names) 
        coefficients <- as.matrix(cbind(coefficients_choice_specific, common_coefficients))
        
        label <- paste0("For choice ", k, " simple effects are:")
        cat(label)
        cat('\n')
        perform.calculation()
        cat('\n')
      }
    }
    
  } else if (model_name == "BANOVA.multiNormal"){
    intercept_name  <- colnames(BANOVA_output$pvalue.table)[1]  #how intercept is labeled
    results <- list()
    names_dv <- BANOVA_output$names_of_dependent_variables
    if (BANOVA_output$single_level){
      design_matrix    <- BANOVA_output$dMatrice$X
      names_regressors <- colnames(design_matrix)
      for (i in 1:BANOVA_output$num_depenent_variables){
        name <- names_dv[i]
        
        coefficients           <- BANOVA_output$samples_l1.list[[i]] #select semples of the lvl1 parameters
        colnames(coefficients) <- names_regressors

        title <- paste0("\nSimple effects for ", name,"\n")
        cat(title)
        results[[name]] <- perform.calculation()
      }
    } else{
      dep_var_label  <- colnames(BANOVA_output$mf1)[1]            #name of the matrix with dep vars
      dep_var_matrix <- BANOVA_output$data[, dep_var_label]       #matrix with dep vars
      combined_data  <- cbind(BANOVA_output$data, dep_var_matrix) #data set with y as a matrix and columns
      for (i in 1:BANOVA_output$num_depenent_variables){
        name <- names_dv[i]
        # for a two level model design matrix is created by "rewriting" two equations into one
        formula <- make.formula(BANOVA_output$pvalue.tables.list[[i]],
                                BANOVA_output$names_of_dependent_variables[[i]])
        
        design_matrics <- design.matrix(l1_formula = formula, l2_formula = 'NA', data = combined_data,
                                        contrast = BANOVA_output$contrast)
        design_matrix    <- design_matrics$X
        names_regressors <- colnames(design_matrix)
        coefficients     <- BANOVA_output$samples_l2.list[[i]] #select semples of the lvl2 parameters
        colnames(coefficients) <- update.regressor.names(rownames(BANOVA_output$coef.tables.list[[i]]$coeff_table))
        
        title <- paste0("\nSimple effects for ", name,"\n")
        cat(title)
        results[[name]] <- perform.calculation()
      }
    }
    return(results)
  }
  # All other models ----
   else { 
    intercept_name  <- colnames(BANOVA_output$pvalue.table)[1] #how intercept is labeled
    if (BANOVA_output$single_level){
      # for a single level model design matrix is extracted from BANOVA_output
      design_matrix    <- BANOVA_output$dMatrice$X
      names_regressors <- colnames(design_matrix)
      
      coefficients           <- BANOVA_output$samples_l1_param #select semples of the lvl1 parameters
      colnames(coefficients) <- names_regressors
    } else {
      # for a two level model design matrix is created by "rewriting" two equations into one
      formula        <- make.formula(pvalue.table = BANOVA_output$pvalue.table, 
                                     dep_var = colnames(BANOVA_output$mf1)[1])
      design_matrics <- design.matrix(l1_formula = formula, l2_formula = 'NA', data = BANOVA_output$data, 
                                      contrast = BANOVA_output$contrast)
      design_matrix   <- design_matrics$X
      names_regressors <- colnames(design_matrix)
      
      coefficients <- BANOVA_output$samples_l2_param #select semples of the lvl2 parameters
      colnames(coefficients) <- update.regressor.names(rownames(BANOVA_output$coef.tables$coeff_table))
    }
    perform.calculation()
  }
  
} 


