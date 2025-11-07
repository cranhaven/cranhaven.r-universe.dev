#' Mediation analysis with multiple possibly correlated mediators
#' 
#' \code{BANOVA.multi.mediation} is a function for analysis of multiple possibly correlated mediators.
#' These mediators are assumed to have no causal influence on each other. 
#' Both single-level and multi-level models can be analyzed. 
#' @usage BANOVA.multi.mediation(sol_1, sol_2, xvar, mediators, individual = FALSE)
#' @param sol_1 an  object  of  class  "BANOVA"  returned  by  BANOVA.run  function with  a fitted 
#' model for an outcome variable regressed on a causal variable, a mediator, and, possibly, 
#' moderators and control variables. The outcome variable can follow Normal, T, Poisson, Bernoulli, 
#' Binomial, and ordered Multinomial distributions.
#' @param sol_2 an object of class "BANOVA" returned by BANOVA.run function, which contains an 
#' outcome of the analysis for multiple Multivariate Normal mediators regressed on a casual variable
#' and other possible moderators and control variables.
#' @param xvar a character string that specifies the name of the causal variable used in both models.
#' @param mediators a vector with character strings, which specifies the names of the mediator 
#' variables used in the models.
#' @param individual logical  indicator  of  whether to output effects for individual units in the 
#' analysis (TRUE or FALSE). This analysis requires a multilevel \code{sol_1}.
#' @return Returns an object of class \code{"BANOVA.multi.mediation"}. The returned object is a list 
#' containing:
#
#' \item{\code{dir_effects}}{table or tables with the direct effect.}
#' \item{\code{individual_direct}}{is returned if \code{individual} is set to \code{TRUE} and the 
#' causal variable is a within-subject variable. Contains a table or tables of the direct effect at 
#' the individual levels of the analysis}
#' \item{\code{m1_effects}}{a list with tables of the effects of the mediator on the outcome}
#' \item{\code{m2_effects}}{a list with tables of the effect of the causal variable on the mediator}
#' \item{indir_effects}{tables of the indirect effect}
#' \item{individual_indirect}{is returned if \code{individual} is set to \code{TRUE} and the
#'  mediator is a within-subject variable. Contains the table or tables with the indirect effect}
#' \item{effect_sizes}{a list with effect sizes on individual mediators}
#' \item{total_indir_effects}{table or tables with the total indirect effect of the causal variable}
#' \item{xvar}{the name of the causal variable}
#' \item{mediators}{the names of the mediating variables}
#' \item{individual}{the value of the argument individual (TRUE or FALSE)}
#' @details The function extends \code{BANOVA.mediation} to the case with multiple possibly 
#' correlated mediators. For details about mediation analysis performed in BANOVA see
#' the help page for the \link[BANOVA]{BANOVA.mediation}.
#' 
#' \code{BANOVA.multi.mediation} estimates and tests specific indirect effects of the causal 
#' variable conveyed through each mediator. Furthermore, the total indirect effect of the causal
#' variables are computed as a sum of the specific indirect effects.
#' 
#' The function prints multiple tables with mediated effects. Tables with direct effects of the 
#' causal variable and mediators on the outcome variable, as well as direct effects of the causal
#' variable on the mediators include a posterior mean and 95\% credible intervals of the effects. 
#' Next, the function displays on the console tables with specific indirect effects and effect sizes 
#' of the mediators, followed by the TIE of the causal variable. These tables include the mean, 
#' 95\% credible intervals, and two-sided Bayesian p-values.
#' @examples 
#' # Use the colorad data set
#' data(colorad)
#' # Add a second mediator to the data set
#' colorad$blur_squared <- (colorad$blur)^2
#' # Prepare mediators to be analyzed in the Multivariate Normal model
#' mediators <- cbind(colorad$blur, colorad$blur_squared)
#' colnames(mediators) <- c("blur", "blur_squared")
#' colorad$mediators <- mediators
#' \donttest{
#' # Build and analyze the model for the outcome variable
#' model <- BANOVA.model('Binomial')
#' banova_binom_model <- BANOVA.build(model)
#' res_1 <- BANOVA.run(y ~ typic, ~ color + blur + blur_squared, fit = banova_binom_model,
#'                     data = colorad, id = 'id', num_trials = as.integer(16), 
#'                     iter = 2000, thin = 1, chains = 2)
#' # Build and analyze the model for the mediators
#' model <- BANOVA.model('multiNormal')
#' banova_multi_norm_model <- BANOVA.build(model)
#' res_2 <- BANOVA.run(mediators ~ typic, ~ color, fit = banova_multi_norm_model,
#'                     data = colorad, id = 'id', iter = 2000, thin = 1, chains = 2)
#'                     
#' # Calculate (moderated) effects of "typic" mediated by "blur" and "blur_squared"
#' results <- BANOVA.multi.mediation(res_1, res_2, xvar='typic', mediators=c("blur", "blur_squared"))
#' }
#' @author Anna Kopyakova
#' @export

#results <- BANOVA.multi.mediation(res_1, res_2, xvar='typic', mediators=c("blur", "blur_squared"))
                              
BANOVA.multi.mediation <- function(sol_1, sol_2, xvar, mediators, individual = FALSE){
  #adapts the design matrix of a multivariate sol_2 to work with BANOVA.mediaation
  adapt.design.matrix <- function(dMatrice, mediator){
    d_temp <- dMatrice
    #X
    attributes(d_temp$X)$varValues[[1]] <- attributes(dMatrice$X)$varValues[[1]][, mediator]
    attr(attributes(d_temp$X)$varValues[[1]], "var_names") <- mediator
    #y
    d_temp$y <- dMatrice$y[,mediator]
    attr(d_temp$y, "names") <- attr(dMatrice$y, "dimnames")[[1]] 
    return(d_temp)
  }
  
  #adapts the mf1 of a multivariate sol_2 to work with BANOVA.mediaation
  adapt.mf1 <- function(mf, mediator){
    temp_mf <- mf
    mediator_of_interest <- mf[,1][, mediator, drop = F]
    
    #data frame
    temp_mf[, 1] <- mediator_of_interest
    colnames(temp_mf)[1] <- mediator
    
    #terms attribute
    if (!is.null(rownames(attr(attr(temp_mf, "terms"),'factors'))[1])){
      rownames(attr(attr(temp_mf, "terms"),'factors'))[1] <- mediator
    }
    attr(attr(temp_mf, 'terms'),'dataClasses')[1] <- "numeric"
    names(attr(attr(temp_mf, 'terms'),'dataClasses'))[1] <- mediator
    return(temp_mf)
  }
  
  #print results to the console
  print.result <- function(list_with_results, extra_title = NULL, final_results, list_name,
                           extra_list_name = NULL, skip_n_last_cols = 0, return_table_index = F,
                           print_effect_sizes = F){
    check.if.tables.are.identical <- function(table1, table2){
      rownames(table1) <- NULL
      rownames(table2) <- NULL
      return(identical(table1, table2))
    }
    print.table <- function(new_table, skip_n_last_cols, extra_title, prev_table = 0, i){
      #BANOVA.mediation reports multiple tables for cases with multiple interacting factors. 
      #When a factor interacts with a continious variable the table reports results centered at 
      #zero of the continious variable, it is not relevant here
      check.table.columns <- function(new_table, skip_n_last_cols){
        remove_table <- F
        n_columns <- ncol(new_table)
        if (is.null(ncol(new_table))){
          #if the table is a vector
          n_cols_to_check <- length(new_table) - skip_n_last_cols
          if (n_cols_to_check != 0){
            for (j in 1:n_cols_to_check){
              if (all(new_table[j] == '0') || all(new_table[j] == 0))
                remove_table <- T
            }
          }
        } else {
          #otherwise 
          n_cols_to_check <- n_columns - skip_n_last_cols
          for (j in 1:n_cols_to_check){
            if (all(new_table[, j] == '0') || all(new_table[, j] == 0))
              remove_table <- T
          }
        }
        return(remove_table)
      }
      remove_table <- F
      if (i > 1){
        remove_table <- check.table.columns(new_table, skip_n_last_cols)
      }
      #if the table is not removed print it
      if (!remove_table){
        if (!check.if.tables.are.identical(prev_table, new_table)){
          if(!is.null(extra_title)){
            #if multiple tables are printed for one subsection (i.e. indirect effects)
            # extra title is printed
            cat(extra_title)
          }
          print(noquote(new_table), row.names = F, right=T)
          if(!print_effect_sizes){
          cat("\n")
          }
        }
      }
      return(list(remove_table = remove_table, prev_table = new_table))
    }
    
    results_list <- list()
    num_tables   <- length(list_with_results) #number of tables reported by BANOVA.mediation
    prev_table   <- 0 
    counter      <- 1
    used_tables_index <- c()
    for (i in 1:num_tables){
      #extract a table
      new_table <- list_with_results[[i]] 
      
      table_name <- names(list_with_results)[i]
      if (!is.null(table_name)){
        table_name <- gsub("_", " ", table_name)
        extra_title <- paste0(table_name, "\n")
      } else {
        extra_title <- NULL
      }
      
      #print a table
      temp_result  <- print.table(new_table, skip_n_last_cols, extra_title, prev_table, i)
      #extract an indicator of whether the table was skipped or not
      remove_table <- temp_result$remove_table
      if (!remove_table){
        #if the table is not skipped prepared it to be returned
        if (!check.if.tables.are.identical(prev_table, new_table)){
          if (print_effect_sizes){
            cat("effect size:", intermediate_results[[mediator]]$effect_size[[i]])
            cat("\n\n")
          }
          results_list[[counter]] <- new_table
          used_tables_index       <- c(used_tables_index, i)
          if (num_tables > 1){
            counter      <- counter + 1
            prev_table   <- temp_result$prev_table
          }
        }
      }
    }
    #Prepare final results
    results_list_length <- length(results_list)
    counter <- 1
    for (i in 1:results_list_length){
      if(!is.null(extra_list_name)){
        if (results_list_length > 1){
          final_results[[list_name]][[extra_list_name]][[counter]] <- results_list[[i]]
        } else {
          final_results[[list_name]][[extra_list_name]] <- results_list[[1]]
        }
      } else {
        if (results_list_length > 1){
          final_results[[list_name]][[counter]] <- results_list[[i]]
        } else {
          final_results[[list_name]] <- results_list[[1]]
        }
      }
      counter <- counter + 1
    }
    if (return_table_index){
      return(list(final_results = final_results, used_tables_index = used_tables_index))
    } else {
      return(final_results)
    }
  }
  
  calculate.total.indirect.effects <- function(used_tables_index){
    ind_eff_samples <- list()
    #Exract relevant samples of indirect effects
    for (mediator in mediators){
      used_tables <- used_tables_index[[mediator]]
      ind_eff_samples[[mediator]] <- intermediate_results[[mediator]]$indirect_effects_samples[used_tables]
    }
    num_tables <- length(used_tables_index[[1]])
    total_indir_eff_samples_list <- list()
    total_indir_eff_table_list   <- list()
    if (individual){
      #for individual effects the samples must be extracted in a special way
      for (i in 1:num_tables){
        for (mediator in mediators){
          temp        <- ind_eff_samples[[mediator]][[i]]
          temp_dim    <- dim(temp)
          row_counter <- c(1:temp_dim[1])
          num_rows    <- length(row_counter)
          temp_smpl_indicator <- startsWith(colnames(temp), "s_")    
          ind_eff_samples_reshaped <- data.frame(matrix(NA, nrow = temp_dim[1]*temp_dim[3], 
                                                        ncol = temp_dim[2]), 
                                                 stringsAsFactors = F)
          colnames(ind_eff_samples_reshaped) <- colnames(temp)
          id <- c()
          for(j in 1:temp_dim[3]){
            selected_rows <- data.frame(temp[, , j, drop = F], stringsAsFactors = F)
            colnames(selected_rows) <-colnames(temp)
            #covert factors to numeric values
            selected_rows[, temp_smpl_indicator] <- (sapply(selected_rows[, temp_smpl_indicator], as.numeric))
            ind_eff_samples_reshaped[row_counter+num_rows*(j-1), ] <- selected_rows
            id <- c(id, rep(j, num_rows))
          }
          ind_eff_samples[[mediator]][[i]] <- data.frame(id, ind_eff_samples_reshaped)
        }
      }
    } 
    for (i in 1:num_tables){
      ind_eff_num_samples <- c()
      #Find common samples
      for (mediator in mediators){
        num_samples <- dim(ind_eff_samples[[mediator]][[i]])[2]
        ind_eff_num_samples <- c(ind_eff_num_samples, num_samples)
      }
      common_samples <- min(ind_eff_num_samples)
      
      #Prepare information to combine tables
      #extract samples for first mediator
      combined_table <- ind_eff_samples[[1]][[i]][, 1:common_samples] 
      #find column names of the columns with samples
      smpl_indicator <- startsWith(colnames(combined_table), "s_")    
      smpl_columns   <- colnames(combined_table)[smpl_indicator]
      num_non_smpl_columns <- sum(!smpl_indicator) + 1           
      #find common columns
      common_columns    <- colnames(combined_table)[!smpl_indicator]
      if ("(Intercept)" %in% common_columns){
        common_columns <- common_columns[!common_columns %in% "(Intercept)"] #drop the intercept
      }
      for (n in 2:num_mediators){
        common_columns <- intersect(common_columns, colnames(ind_eff_samples[[n]][[i]])[!smpl_indicator])
      }
    
      #Combine tables
      for (n in 2:num_mediators){
        temp_table1 <- combined_table[, 1:common_samples]
        temp_table2 <- ind_eff_samples[[n]][[i]][, 1:common_samples]
        combined_samples <- merge(temp_table1[, c(common_columns, smpl_columns)], 
                                  temp_table2[, c(common_columns, smpl_columns)], 
                                  by = common_columns)
        temp1 <- combined_samples[, paste0(smpl_columns, ".x")]
        temp2 <- combined_samples[, paste0(smpl_columns, ".y")]
        combined_table[, num_non_smpl_columns:common_samples] <-  temp1 + temp2
      }
      total_indirect_effects_samples <- combined_table[, c(smpl_columns)]
      
      if(is.numeric(sol_1$data[, xvar])){
        common_columns <- common_columns[common_columns != xvar]
      }
      num_common_columns <- length(common_columns)
      
      #Prepare final results
      total_indirect_effects <- data.frame(matrix(NA, nrow = nrow(combined_table), ncol = num_common_columns+4))
      colnames(total_indirect_effects) <- c(common_columns, "mean","2.5%","97.5%","p.value")
      total_indirect_effects[, 1:num_common_columns] <- combined_table[, common_columns]
      total_indirect_effects[, "mean"] <- round(apply(total_indirect_effects_samples, 1, mean), 4)
      quantiles <- round(apply(total_indirect_effects_samples, 1, quantile, 
                              probs = c(0.025, 0.975), type = 3, na.rm = FALSE), 4)
      total_indirect_effects[, "2.5%"]  <- quantiles["2.5%",]
      total_indirect_effects[, "97.5%"] <- quantiles["97.5%",]
      p_values <- round(pValues(t(total_indirect_effects_samples)), 4)
      total_indirect_effects[, "p.value"] <-  apply(p_values, 1,
                                                    FUN = function(x) ifelse(x == 0, '<0.0001', x))
      
      #Prepare the total indirect samples and results to be returned
      temp_effects <- data.frame(combined_table[, common_columns], total_indirect_effects_samples)
      colnames(temp_effects) <- c(common_columns, colnames(total_indirect_effects_samples))
      if (individual){
        temp_effects <- temp_effects[order(temp_effects$id),]
        rownames(temp_effects) <- NULL
      }
      total_indir_eff_samples_list[[i]] <- temp_effects
      total_indir_eff_table_list[[i]]   <- total_indirect_effects
    }
    return(list(total_indirect_effects = total_indir_eff_table_list,
                total_indirect_effects_samples = total_indir_eff_samples_list))
  }
  
  #arrange the direct effects outputted by BANOVA.medition into a data frame
  prepare.direct.effects.df <- function(){
    #Extract infromation about direct effects
    dir_eff_list        <- intermediate_results[[1]]$direct_effects_samples
    dir_eff_list        <- dir_eff_list[[length(dir_eff_list)]]
    dir_effect_names    <- dir_eff_list$index_name
    dir_eff_samples     <- dir_eff_list$samples
    dim_dir_eff_samples <- dim(dir_eff_samples)
    if(individual){
      row_counter <- c(1:dim_dir_eff_samples[1])
      num_rows    <- length(row_counter)
      dir_eff_samples_reshaped <- data.frame(matrix(NA, nrow = dim_dir_eff_samples[1]*dim_dir_eff_samples[3], 
                                                    ncol = dim_dir_eff_samples[2]))
      id <- c()
      for(j in 1:dim_dir_eff_samples[3]){
        selected_rows <- data.frame(dir_eff_samples[, , j])
        #covert factors to numeric values
        #selected_rows[, temp_smpl_indicator] <- lapply(selected_rows[, temp_smpl_indicator], as.numeric.factor)
        #selected_rows[, temp_smpl_indicator] <-  as.data.frame(sapply(selected_rows[, temp_smpl_indicator], as.numeric))
        dir_eff_samples_reshaped[row_counter+num_rows*(j-1), ] <- selected_rows
        id <- c(id, rep(j, num_rows))
      }
      dir_eff_samples_df <- data.frame(id, dir_eff_samples_reshaped)
    } else {
      dir_eff_samples_df  <- data.frame(matrix(dir_eff_samples, 
                                               dim_dir_eff_samples[1]*dim_dir_eff_samples[2],
                                               dim_dir_eff_samples[3]))
    } 
    #Drop the intercept name
    temp_colnames <- colnames(dir_effect_names)
    if ("(Intercept)" %in% colnames(dir_effect_names)){
      dir_effect_names <- dir_effect_names[,!colnames(dir_effect_names) %in% "(Intercept)", drop = F]
      if (is.null(dim(dir_effect_names))){
        dir_effect_names <- as.data.frame(as.matrix(dir_effect_names))
        colnames(dir_effect_names) <- temp_colnames[!temp_colnames %in% "(Intercept)"]
      }
    }
    dir_eff_samples_df <- data.frame(dir_effect_names, dir_eff_samples_df, stringsAsFactors = F)
    return(dir_eff_samples_df)
  }
  
  #calculate total effects of a causal variable - not used. 
  #This function is correct only for sol_1 with Normal dependent variables
  combine_direct_and_indirect_effects <- function(dir_eff_samples_df, total_indir_eff_samples_list){
    total_eff_table_list <- list() 
    num_indir_eff_tables <- length(total_indir_eff_samples_list)
    for (i in 1:num_indir_eff_tables){
      indir_eff_samples_df <- data.frame(total_indir_eff_samples_list[[i]])
      #find column names of the columns with samples based on indirect effects
      smpl_indicator <- startsWith(colnames(indir_eff_samples_df), "s_")
      smpl_col_names <- colnames(indir_eff_samples_df)[smpl_indicator]
      #Find common samples
      common_samples       <- min(ncol(dir_eff_samples_df), ncol(indir_eff_samples_df))
      dir_eff_samples_df   <- dir_eff_samples_df[, 1:common_samples]
      indir_eff_samples_df <- indir_eff_samples_df[, 1:common_samples]
      #Prepare information to combine tables
      colnames_dir_eff   <- colnames(dir_eff_samples_df)
      colnames_indir_eff <- colnames(indir_eff_samples_df)
      common_columns     <- intersect(colnames_dir_eff, colnames_indir_eff)
      num_common_columns <- length(common_columns)
      smpl_index         <- (num_common_columns+1):common_samples
      colnames(dir_eff_samples_df)[!colnames_dir_eff%in%common_columns] <- 
        colnames_indir_eff[!colnames_dir_eff%in%common_columns]
      #Combine tables
      total_effect_samples <- merge(dir_eff_samples_df, indir_eff_samples_df, 
                                    by = common_columns)
      temp1 <- total_effect_samples[, paste0(smpl_col_names, ".x")]
      temp2 <- total_effect_samples[, paste0(smpl_col_names, ".y")]
      
      total_effect_samples[, smpl_index] <-  temp1 + temp2
      #Prepare final results
      total_effects <- data.frame(matrix(NA, nrow = nrow(total_effect_samples), ncol = num_common_columns+4))
      colnames(total_effects) <- c(common_columns, "mean","2.5%","97.5%","p.value")
      
      total_effects[, 1:num_common_columns] <- total_effect_samples[, common_columns]
      total_effects[, "mean"] <- round(apply(total_effect_samples[, smpl_index], 1, mean), 4)
      quantiles    <- round(apply(total_effect_samples[, smpl_index], 1, quantile, 
                                  probs = c(0.025, 0.975), type = 3, na.rm = FALSE), 4)
      total_effects[, "2.5%"]  <- quantiles["2.5%",]
      total_effects[, "97.5%"] <- quantiles["97.5%",]
      p_values <- round(pValues(t(total_effect_samples[, smpl_index])), 4)
      total_effects[, "p.value"] <-  apply(p_values, 1,
                                           FUN = function(x) ifelse(x == 0, '<0.0001', x))
      if (individual){
        total_effects <- total_effects[order(total_effects$id),]
        rownames(total_effects) <- NULL
      }
      total_eff_table_list[[i]]   <- total_effects
    }
    return(total_eff_table_list)
  }
  
  #print tables with total (indirect) effects
  print.tables.with.total.effects <- function(list_with_tables, num_tables){
    for (i in 1:num_tables){
      table <- list_with_tables[[i]]
      print(noquote(table), row.names = F, right=T)
      cat('\n')
    }
  }
  
  #add tables with total (indirect) effects to the final return list
  save.tables.with.total.effects <- function(list_with_tables, num_tables, final_list_name){
    if (num_tables > 1){
      final_results[[final_list_name]] <- list_with_tables
    } else{
      final_results[[final_list_name]] <- list_with_tables[[1]]
    }
    return(final_results)
  }
  
  #for tables with individual total (indirect) effects add an original id as a column
  add.an.original.id <- function(list_with_tables, id_map, id_last = F){
    for (i in 1:length(list_with_tables)){
      table <- list_with_tables[[i]] 
      original_id <- id_map[table$id] 
      if(id_last){
        table[, ncol(table)+1] <- original_id
        table <- table[, !colnames(table) %in% "id"] #drop the first id
        colnames(table)[ncol(table)] <- "id" #rename the last column
      } else {
        table$id <- original_id
      }
      list_with_tables[[i]] <- table
    }
    return(list_with_tables)
  }
  
  #####MAIN FUNCTION######
  #Check the class of the model with multiple dependent variables
  if(sol_2$model_name != "BANOVA.multiNormal") 
    stop('The mediator must follow the multivariate Normal distribution, use BANOVA multiNormal models instead.')
  
  #Initialize variables
  num_mediators        <- length(mediators)
  intermediate_results <- list()
  final_results        <- list()

  #Adapt the output of BANOVA.multiNormal to fit BANOVA.mediation
  temp_solution <- list()
  temp_solution$samples_cutp_param     <- sol_2$samples_cutp_param
  temp_solution$data                   <- sol_2$data
  temp_solution$num_trials             <- sol_2$num_trials
  temp_solution$model_code             <- sol_2$model_code
  temp_solution$single_level           <- sol_2$single_level
  temp_solution$stan_fit               <- sol_2$stan_fit
  temp_solution$contrast               <- sol_2$contrast
  temp_solution$new_id                 <- sol_2$new_id
  temp_solution$old_id                 <- sol_2$old_id
  temp_solution$call                   <- sol_2$call 
  temp_solution$model_name             <- 'BANOVA.Normal'
  temp_solution$samples_l2_sigma_param <- sol_2$samples_l2_sigma_param
  class(temp_solution) <- "BANOVA"
  names(sol_2$R2)      <- names(sol_2$anova.tables.list)
  names(sol_2$tau_ySq) <- names(sol_2$anova.tables.list)
  for (mediator in mediators){
    temp_solution$anova.table      <- sol_2$anova.tables.list[[mediator]]
    temp_solution$coef.tables      <- sol_2$coef.tables.list[[mediator]]
    temp_solution$pvalue.table     <- sol_2$pvalue.tables.list[[mediator]]
    temp_solution$conv             <- sol_2$conv.list[[mediator]]
    temp_solution$samples_l1_param <- sol_2$samples_l1.list[[mediator]]
    temp_solution$samples_l2_param <- sol_2$samples_l2.list[[mediator]]
    temp_solution$R2               <- sol_2$R2[[mediator]]
    temp_solution$tau_ySq          <- sol_2$tau_ySq[[mediator]]
    temp_solution$dMatrice         <- adapt.design.matrix(sol_2$dMatrice, mediator)
    temp_solution$mf1              <- adapt.mf1(sol_2$mf1, mediator)
    temp_solution$mf2              <- sol_2$mf2 
    if(individual){
      stan_fit      <- rstan::extract(sol_2$stan_fit, permuted = T)
      if (sol_1$single_level || sol_2$single_level){
         stop("It seems to be a between-subject design, set individual = FALSE instead.")
      } else {
        if (sol_2$single_level){
          samples_beta1 <- stan_fit$beta1
        } else {
          samples_beta1 <- stan_fit$beta1[, , which(sol_2$names_of_dependent_variables == mediator), ]
        }
        dim_beta1 = dim(samples_beta1)
        if (length(dim_beta1) == 2){
          dim(samples_beta1) <- c(dim_beta1[1],dim_beta1[2],1)
        }
        sol <- BANOVA.mediation(sol_1, sol_2 = temp_solution, xvar=xvar, mediator=mediator,
                                individual = individual, return_posterior_samples = T,
                                multi_samples_beta1_raw_m = samples_beta1)
      }
    } else {
      sol <- BANOVA.mediation(sol_1, sol_2 = temp_solution, xvar=xvar, mediator=mediator,
                              individual = individual, return_posterior_samples = T)
    }
   intermediate_results[[mediator]] <- sol
  }
  #####Individual effects - only for multilevel models#####
  if(individual){
    #calculate id map
    id_map <- idmap(sol_1$old_id, sol_1$new_id)
    used_tables_index <- list()
    
    #######Prepare and save direct and indirect effects#######
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Indirect effects of the causal variable", xvar, "on the outcome variables\n\n"))
    for (mediator in mediators){
      #Individual direct effects of the causal variable on the outcome
      final_results[["dir_effects"]][[mediator]] <- intermediate_results[[mediator]][["dir_effects"]]
      final_results[["individual_direct"]][[mediator]] <- intermediate_results[[mediator]][["individual_direct"]]
      
      #Individual direct effects of the mediator variables on the outcome
      final_results[["m1_effects"]][[mediator]] <- intermediate_results[[mediator]][["m1_effects"]]
      
      #Individual direct effects of the causal variable on mediator variables
      final_results[["m2_effects"]][[mediator]] <- intermediate_results[[mediator]][["m2_effects"]]
      
      #######Individual indirect effects of the causal variable#######
      final_results[["indir_effects"]][[mediator]] <- intermediate_results[[mediator]][["indir_effects"]]
      final_results[["individual_indirect"]][[mediator]] <- intermediate_results[[mediator]][["individual_indirect"]]
      
      #######Report individual indirect effects of the causal variable#######
      table_name <- names(intermediate_results[[mediator]]$indir_effects)
      if (!is.null(table_name)){
        string1 <- strsplit(table_name, xvar, fixed = F)[[1]][1]
        string1 <- gsub("_", " ", string1)
        string2 <- strsplit(table_name, mediator, fixed = F)[[1]][2]
        string2 <- gsub("_", " ", string2)
        table_name <- paste0(string1, xvar, " through ", mediator, string2, "\n")
      }
      temp_result <- print.result(list_with_results = intermediate_results[[mediator]]$individual_indirect,
                                  final_results = final_results, 
                                  extra_title = table_name,
                                  list_name = "individual_indirect", extra_list_name = mediator,
                                  skip_n_last_cols = 6, return_table_index = T)
      final_results[["individual_indirect"]][[mediator]] <- temp_result[[1]]$individual_indirect[[mediator]]
      used_tables_index[[mediator]] <- temp_result$used_tables_index
      final_results$effect_sizes[[mediator]] <- intermediate_results[[mediator]]$effect_size
    }
    
    #######Report total individual indirect effects of the causal variable#######
    total_indirect_effects_results   <- calculate.total.indirect.effects(used_tables_index)
    num_tables_with_indirect_effects <- length(used_tables_index[[1]])
    #add an original id to the table
    total_indirect_effects_results$total_indirect_effects <-
      add.an.original.id(total_indirect_effects_results$total_indirect_effects, id_map, T)
    #print and save total individual indirect effects
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Total individual indirect effects of the causal variable", xvar, 
                     "on the outcome variables\n\n"))
    print.tables.with.total.effects(total_indirect_effects_results$total_indirect_effects, 
                                    num_tables_with_indirect_effects)
    final_results <- save.tables.with.total.effects(total_indirect_effects_results$total_indirect_effects, 
                                   num_tables_with_indirect_effects,
                                   "total_indir_effects")
    
  } ######Genral effects#####
  else{
    #######Report direct effects of the causal variable on the outcome#######
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Direct effects of the causal variable", xvar, "on the outcome variable\n\n"))
    final_results <- print.result(list_with_results = intermediate_results[[1]]$dir_effect, 
                                  final_results = final_results, 
                                  list_name = "dir_effects", skip_n_last_cols = 3)
    
    #######Report direct effects of the mediator variables on the outcome#######
    mediator_names <- paste(mediators,  collapse=" and ")
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Direct effects of mediators", mediator_names, "on the outcome variable\n\n"))
    for (mediator in mediators){
      final_results <- print.result(list_with_results = intermediate_results[[mediator]]$m1_effects, 
                                    final_results = final_results, 
                                    list_name = "m1_effects", extra_list_name = mediator,
                                    skip_n_last_cols = 3)
    }
    
    #######Report direct effects of the causal variable on mediator variables#######
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Direct effects of the causal variable", xvar, "on the mediator variables\n\n"))
    for (mediator in mediators){
      final_results <- print.result(list_with_results = intermediate_results[[mediator]]$m2_effects, 
                                    final_results = final_results, 
                                    list_name = "m2_effects", extra_list_name = mediator,
                                    skip_n_last_cols = 3)
    }
    
    #######Report indirect effects of the causal variable and effect size#######
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Indirect effects of the causal variable", xvar, "on the outcome variables\n\n"))
    used_tables_index <- list()
    for (mediator in mediators){
      temp_result <- print.result(list_with_results = intermediate_results[[mediator]]$indir_effects, 
                                  final_results = final_results, 
                                  list_name = "indir_effects", extra_list_name = mediator,
                                  skip_n_last_cols = 3, return_table_index = T,
                                  print_effect_sizes = T)
      final_results <- temp_result$final_results
      used_tables_index[[mediator]] <- temp_result$used_tables_index
      final_results$effect_sizes[[mediator]] <- 
        intermediate_results[[mediator]]$effect_size[temp_result$used_tables_index]
    }
    
    #######Report total indirect effects of the causal variable#######
    total_indirect_effects_results   <- calculate.total.indirect.effects(used_tables_index)
    num_tables_with_indirect_effects <- length(used_tables_index[[1]])
    cat(paste(strrep("-", 100), '\n'))
    cat(paste("Total indirect effects of the causal variable", xvar, 
                     "on the outcome variables\n\n"))
    print.tables.with.total.effects(total_indirect_effects_results$total_indirect_effects, 
                                    num_tables_with_indirect_effects)
    final_results <- save.tables.with.total.effects(total_indirect_effects_results$total_indirect_effects, 
                                   num_tables_with_indirect_effects,
                                   "total_indir_effects")
  }
  final_results$xvar <- xvar
  final_results$mediators <- mediators
  final_results$individual <- individual
  class(final_results) <- 'BANOVA.multi.mediation'
  return(final_results)
}
