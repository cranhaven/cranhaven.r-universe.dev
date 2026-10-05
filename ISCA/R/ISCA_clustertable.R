#' ISCA Cluster Tables
#'
#' Function to create a cluster or descriptive table across iterations.
#' @param data The dataset including all relevant variables and the random assignments from the first ISCA_random_assignments()-function.
#' @param cluster_vars A vector specifying the variables of interest.
#' @param draws Specification of the number of probabilistic draws. The number of draws should be equal to the number of draws specified in the first step. If not specified, the default is 500.
#' @return The output is a table containing the grand mean, grand standard deviation, and cluster error for each variable and cluster. No cluster error is calculated for dichotomous variables. 
#' @examples
#' data(sim_data)
#' ISCA_step1 <- ISCA_random_assignments(data=sim_data, filter=native, 
#' majority_group=1, minority_group=c(0), fuzzifier = 1.5, n_clusters=4, 
#' draws=5, cluster_vars= c("female", "age", "education", "income"))
#' result_ISCA_clustertable <- ISCA_clustertable(data = ISCA_step1, 
#' cluster_vars = c("native", "education", "age", "female", 
#' "discrimination", "religiosity"), draws = 5);
#' @export
ISCA_clustertable <- function(data, cluster_vars, draws=500) {
  
  n <- variable <- NULL
  clustering_var_means_list <- list() # creates list wherein means and stds per draw are stored
  
  for (d in 1:draws) {
    variable_draw <- paste0("A", d)
    assignment <- data[[variable_draw]] # select draw X
    
    temp_data <- data.frame(data[cluster_vars], assignment) # create dataframe with cluster and draw variables
    
    clustering_var_means <- temp_data %>% 
      dplyr::group_by(assignment) %>% # group by cluster assignments
      dplyr::summarise(dplyr::across(tidyselect::all_of(cluster_vars), # per variable...
                              list(mean = ~mean(., na.rm = TRUE), sd = ~stats::sd(., na.rm = TRUE))), # ...calculate mean and std, ignore missing vlaues
                       count = dplyr::n()) %>% # create variable counts cases per cluster
      dplyr::mutate(iteration = d) # variable for iteration, might be deleted, seems unnecessary
    
    clustering_var_means_list[[d]] <- clustering_var_means # store the resulting data.frame in a list
  }
  
  clustering_var_means_list <- do.call(rbind, clustering_var_means_list) # bind the data.frames stored in the list as a single dataframe
  
  clustering_var_grand_means_list <- list() # create new empty list to stor grand means / stds and cluster errors
  
  cluster_vars <- append(cluster_vars, "count") # add the newly created count variable to the cluster_vars vector
  
  for (i in 1:length(cluster_vars)){
    
    if (i ==1) { # for the first variable in cluster_vars...
      pattern <- cluster_vars[i]
      
      temp_data2 <- clustering_var_means_list  %>% dplyr::select(assignment, dplyr::matches(pattern)) # ... select assignment and
                                                                                                  # all variables that match / are related to the first cluster variable
                                                                                                  # aka cluster_var_mean, cluster_var_sd
      
      clustering_var_grand_means <- temp_data2 %>% 
        dplyr::group_by(assignment) %>% # ... group by cluster... 
        dplyr::summarise(dplyr::across(tidyselect::all_of(names(temp_data2)[-1]), # ...for all except the assignment variable... 
                                list(mean = ~mean(., na.rm = TRUE), sd = ~stats::sd(., na.rm = TRUE)))) # ..calculate mean and std for all the variables..
      
      clustering_var_grand_means <- clustering_var_grand_means[,1:4] # ..exclude column 5 (sd of the sd)..
      
      colnames(clustering_var_grand_means)[2] <- paste0("grand_mean_", pattern) # rename appropriately, mean of mean is grand_mean..
      colnames(clustering_var_grand_means)[3] <- paste0("cluster_error_", pattern) # ..sd of mean is cluster_error_..
      colnames(clustering_var_grand_means)[4] <- paste0("grand_sd_", pattern) # ..mean of sd is grand_sd..
      
      clustering_var_grand_means_list[[i]] <- clustering_var_grand_means # ..store results for each variable in list..
    } else if (i!=length(cluster_vars)){ # .. repeat the same for all except the last variable in cluster_vars (aka except the count variable)..
      pattern <- cluster_vars[i]
      
      temp_data2 <- clustering_var_means_list  %>% dplyr::select(assignment, dplyr::matches(pattern))
      
      clustering_var_grand_means <- temp_data2 %>%
        dplyr::group_by(assignment) %>%
        dplyr::summarise(dplyr::across(tidyselect::all_of(names(temp_data2)[-1]), 
                                list(mean = mean, sd = stats::sd)))
      
      clustering_var_grand_means <- clustering_var_grand_means[,2:4] # .. but exclude column 5 (sd of the sd) and 
                                #the assignment variable bc it is only needed once and is already added in previous loop condition
      
      colnames(clustering_var_grand_means)[1] <- paste0("grand_mean_", pattern)
      colnames(clustering_var_grand_means)[2] <- paste0("cluster_error_", pattern)
      colnames(clustering_var_grand_means)[3] <- paste0("grand_sd_", pattern)
      
      clustering_var_grand_means_list[[i]] <- clustering_var_grand_means
    } else { # ..create grand_mean and cluster_error here only for the "count" variable..
      pattern <- cluster_vars[i]
      
      temp_data2 <- clustering_var_means_list  %>% dplyr::select(assignment, dplyr::matches(pattern))
      
      clustering_var_grand_means <- temp_data2 %>%
        dplyr::group_by(assignment) %>%
        dplyr::summarise(dplyr::across(tidyselect::all_of(names(temp_data2)[-1]), 
                                list(mean = mean, sd = stats::sd)))
      
      clustering_var_grand_means <- clustering_var_grand_means[,2:3] # exclude column 1
      
      colnames(clustering_var_grand_means)[1] <- paste0("grand_mean_", pattern)
      colnames(clustering_var_grand_means)[2] <- paste0("cluster_error_", pattern)
      
      clustering_var_grand_means_list[[i]] <- clustering_var_grand_means
    }
   
  }
  
  clustering_var_grand_means_list <- do.call(cbind, clustering_var_grand_means_list) # combine all variables / columns that were stored in the list as one data.frame
  
  clustering_var_grand_means_list2 <- as.data.frame(t(clustering_var_grand_means_list)) # transpose data.frame so that column=clusters and rows=variables
  
  colnames(clustering_var_grand_means_list2) <- rownames(clustering_var_grand_means_list)
  clustering_var_grand_means_list2 <- tibble::rownames_to_column(clustering_var_grand_means_list2, "variable") # variable names were previously stored as row names
                                                                      # change row_names to variable cloumn so that new row names is normal running index
  
  clustering_var_grand_means_list2[, -1] <- lapply(clustering_var_grand_means_list2[, -1], as.numeric) # cluster column must be numeric
  
  clustering_var_grand_means_list2[,-1] <- round(clustering_var_grand_means_list2[,-1],4) # round to 4 decimals
  
  for (i in cluster_vars) { # this loop drops the mean standard deviation (aka. cluster_error) for dichotomous variables
    
    if (length(unique(temp_data[[i]])) == 2) { # if variable has only 2 levels aka. is dichotomous
      
      clustering_var_grand_means_list2 <- clustering_var_grand_means_list2 %>%
        dplyr::filter(!(variable == paste0("grand_sd_", i)))
    } 
  }
  return(clustering_var_grand_means_list2) # return final dataframe
  
}