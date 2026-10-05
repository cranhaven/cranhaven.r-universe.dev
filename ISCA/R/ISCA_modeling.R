#' @importFrom stringr str_remove
NULL

#' ISCA Modeling
#'
#' Function to compute an OLS regression across all clusters and iterations.
#' @param data The dataset including all relevant variables and the random assignments from the first ISCA_random_assignments()-function.
#' @param model_spec A model specification similar to the lm()-function.
#' @param weights A vector specifying the variable in which the weights are stored. The default is NONE.
#' @param n_clusters Specification of the number of clusters. This value should be equal to the number of clusters specified in the first and second step.
#' @param draws Specification of the number of probabilistic draws. The number of draws should be equal to the number of draws specified in the first and second step. If not specified, the default is 500.
#' @return The output is a table containing the regression coefficients, standard error and p-value for each regression term and cluster across all iterations. It also contains the regression coefficient, standard error and p-value for a pooled model, that is a model with all clusters combined.
#' @examples 
#' data(sim_data)
#' ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, 
#' majority_group=1, minority_group=c(0), fuzzifier = 1.5, n_clusters=4, 
#' draws=5, cluster_vars= c("female", "age", "education", "income"))
#' ISCA_modeling_res <- ISCA_modeling(data= ISCA_step1, 
#' model_spec="religiosity ~ native + female + age + education + discrimination", 
#' draws = 5, n_clusters = 4);
#' @export
ISCA_modeling <- function(data, model_spec, weights = NULL, n_clusters, draws=500) {
  
   term <- cluster <- estimate <- p.value <- cluster_assignment <- NULL 
  rm(list = c("term", "cluster_assignment", "cluster", "estimate", "p.value"))
  
  model_fit_list_across_iterations <- NULL
  results_iter <- list()
  
  for (d in 1:draws) {
    
    variable_draw <- paste0("A",d)
    variable_draw <- eval(parse(text = paste0("data$",variable_draw)))
    data <- data %>% dplyr::mutate(cluster_assignment=variable_draw) # variable draw = the probabilistic cluster assignments 
    
    # A tidy object with the coefficients should be created for each model, with a column containing each cluster, and another column
    # containing each iteration. 
    # Then each coefficient can be averaged across iteration, and presented across clusters. So first a "draws" column then a "variable_draw" column.
    
    # E.g. starting with first draw (A1), Cluster 1, Cluster 2, Cluster 3, Cluster 4, then
    # second draw (A2), , Cluster 1, Cluster 2, Cluster 3, Cluster 4, etc.
    
    # Model 99: Model according to model specification in function. 
    # All modeling uses OLS for comparability across groups, interpretability, simplicity.
    
    model_fit_list <- NULL
    
    all_coefs_list <- list()
    
    for (c in 1:n_clusters) {
      
      subsample_data <- data %>% dplyr::filter(cluster_assignment==c) # cluster assignment = A[d]
      
      filter_expr <- substitute(weights)
      model_99 <- stats::lm(data=subsample_data, stats::as.formula(model_spec), weights = eval(filter_expr)) # subsample_data aka containing draw A[d], cluster c
      
      summary(model_99) # contains coefficient only for draw A[d], cluster c 
      
      model_fit <- summary(model_99)$adj.r.squared %>% as.data.frame()
      beta  <- summary(model_99)$coefficients[2,1]
      sigma <- summary(model_99)$coefficients[2,2] # aka standard error
      
      # Put together all model estimates by cluster. 
      # Next step: creating another column for iteration number, and creating separate objects by cluster. 
      
      results <- list(model_99) # save the result of a single model as the first entry of a list
      names(results) <- paste0("", 1:1) # output: "1"
      
      all_coefs <- plyr::ldply(results, broom::tidy, .id = "model") # adds a column on the left called model, all values are 1
      
      all_coefs_cluster_X <- paste("all_coefs_cluster", c, sep = "")
      all_coefs_cluster_X <- assign(all_coefs_cluster_X, all_coefs)
      
      all_coefs_list[[c]] <- all_coefs_cluster_X
      
      model_fit_list[[c]] <- model_fit
      
    }
    
    # This part is where the results from all draws and clusters are collected
   results_across_iterations <-  all_coefs_list
   names(results_across_iterations) <- paste0("C", 1:n_clusters)
   
   results_iter_X <- paste("results_iter_", d, sep = "")
   results_iter_X <- assign(results_iter_X, results_across_iterations)
   
   results_iter[[d]] <- results_iter_X
   
   names(results_iter) <- paste0("", 1:d)
   
   model_fit_list <- dplyr::bind_rows(model_fit_list)
   model_fit_list_across_iterations[d] <- model_fit_list
    
  }
  
  # Post Model Loop Processing 
  
   model_fit_list_across_iterations <- do.call(rbind, model_fit_list_across_iterations)  # Model fit, each column is a cluster, each row is an iteration.
   
   mean.adj.R2.impvie <- model_fit_list_across_iterations %>% # returns one mean value per cluster, new variable names are mean.fit.cluster1, mean.fit.cluster2, etc.
     as.data.frame() %>%
     dplyr::summarise(dplyr::across(tidyselect::starts_with("V"), ~ mean(.x, na.rm = TRUE), .names = "mean.fit.cluster{str_remove(.col, 'V')}")) %>% 
     dplyr::ungroup()
   
   for (i in 1:length(results_iter)) { # store results for each cluster separately, e.g., cluster 1 with its X iterations
     results_cluster <- lapply(results_iter, `[[`, paste0('C', i))
     results_cluster <- plyr::ldply(results_cluster, data.frame, .id = "iteration")
     
     # Create object names and assign results
     object_name <- paste("results_cluster_", i, sep = "")
     assign(object_name, results_cluster)
   }
   
   #aggregate_list_of_results <- list(results_cluster_1, results_cluster_2, results_cluster_3, results_cluster_4) # store results of these clusters in list
   aggregate_list_of_results <- mget(ls(pattern = "results_cluster_\\d+"))
   names(aggregate_list_of_results) <- paste0("", 1:length(aggregate_list_of_results)) 
   
   # this will produce a combined table, with number of terms * number of itertations * number of cluster = X rows, 
   aggregate_list_of_results <- plyr::ldply(aggregate_list_of_results, data.frame, .id = "cluster" ) 
   
   
   
   # Pooled Model (regardless of clusters)
   
   filter_expr <- substitute(weights)
   summary(model_100 <- stats::lm(formula = stats::as.formula(model_spec), data=data, weights= eval(filter_expr)))
   
   model_fit100 <- summary(model_100)$adj.r.squared %>% as.data.frame()
   beta  <- summary(model_100)$coefficients[2,1]
   sigma <- summary(model_100)$coefficients[2,2]
   
   mean.adj.R2.impvie <- cbind(mean.adj.R2.impvie, model_fit100) 
   colnames(mean.adj.R2.impvie)[c+1] = "pooled.model"	
   
   
   pooled_coefficients <- c(summary(model_100)$coefficients[,1]) 
   pooled_standard_errors <-c(summary(model_100)$coefficients[,2])
   pooled_significance <- c(summary(model_100)$coefficients[,4])
   
   coefficients_pooled_model <- data.frame(pooled_coefficients, pooled_standard_errors, pooled_significance, stringsAsFactors = TRUE) ## Now the id column needs to be shifted to the right. 
   coefficients_pooled_model <- tibble::rownames_to_column(coefficients_pooled_model, "term") 
   coefficients_pooled_model <- coefficients_pooled_model %>% dplyr::mutate(cluster=0) # 0 means the pooled model here. 
   coefficients_pooled_model <- coefficients_pooled_model %>% dplyr::rename(mean_p_value=pooled_significance,
                                                                            mean_std.error=pooled_standard_errors, 
                                                                            mean_coefficients=pooled_coefficients) # renaming the same way as below for binding.
   
   
   coefficients_pooled_model$cluster <- factor(coefficients_pooled_model$cluster)
   coefficients_pooled_model <- coefficients_pooled_model[c(5,1,2,3,4)]
   
   # Atm aggregate_list_of_results is a dataframe with results for each cluster and iteration, so T terms * C clusters * I iteration = number of rows
   
   coefficients <- aggregate_list_of_results  %>% 
     dplyr::group_by(cluster, term) %>% # group by cluster and term, so summarise iterations..
     dplyr::summarise(mean_coefficients=mean(estimate), # ..and calculate mean coefficients..
                      mean_std.error=stats::sd(estimate), # ..mean std error..
                      mean_p_value=mean(p.value)) # ..and mean p-value
   
   coefficients <- dplyr::bind_rows(coefficients, coefficients_pooled_model) # combines results for each cluster and pooled model
   coefficients <- coefficients %>% dplyr::mutate_if(is.numeric, round, digits = 4) # rounds to 4 digits for readability
   
   return(list(coefficients, mean.adj.R2.impvie))
}


