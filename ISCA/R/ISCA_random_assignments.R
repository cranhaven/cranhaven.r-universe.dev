#' ISCA Random Assignments per Subgroup
#'
#' Function that calculates membership scores for each subgroup and assigns a cluster for a number of random draws.
#' @param data A dataset containing all relevant variables
#' @param filter Specification of the variable name that contains information on majority / minority status.
#' @param majority_group Specification of the value within the variable specified in the previous filter-argument indicating majority status. This could be either a numeric value or a character string.
#' @param minority_group specification of the value(s) indicating minority status in the filter variable. This could be either a numeric value or a character string. It can be one single minority group or a vector of several minority groups.
#' @param cluster_vars Vector specifying the variables that should be used to create the clusters.
#' @param fuzzifier The fuzzifier is a value larger than 1 determining the extent of overlap between clusters. A value of 1 effectively makes fuzzy c-means equivalent to hard k-means. The default is 1.5.
#' @param n_clusters Specification of the number of clusters to be created.
#' @param draws Specification of the number of probabilistic draws. If not specified, the default is 500.
#' @return The output is a dataframe with all original variables and a new column for every draw, each containing one random assignment. This dataframe is the foundation of the subsequent functions in the ISCA package.
#' @examples 
#' data(sim_data)
#' ISCA_step1 <- ISCA_random_assignments(data=sim_data,
#' filter=native, majority_group=1, minority_group=c(0), 
#' fuzzifier = 1.5, n_clusters=4, draws=5, 
#' cluster_vars= c("female", "age", "education", "income"));
#' @export
ISCA_random_assignments <- function(data, filter, majority_group, minority_group, cluster_vars, fuzzifier = 1.5, n_clusters, draws=500){
  
  # Start with Reference Group
  
  filter_expr <- substitute(filter) #  needed for unevaluated expression aka parameter in function (bc giving the parameter native in "" doesn't work)
  data_ref <- subset(data, eval(filter_expr) == majority_group) # filters for reference group
  
  cluster.vars <- data_ref[ , cluster_vars] # selects variables relevant for clusters (i.e., excludes ID, etc.)
  
  my.fuzzy.cmeans <-  e1071::cmeans(cluster.vars, centers=n_clusters, iter.max=100, 
                              verbose=TRUE, dist="manhattan",method="cmeans", 
                              m=fuzzifier, rate.par = NULL) # Creates fuzzy cmeans clusters
  
  cluster_centers <- my.fuzzy.cmeans$centers # take the centers for the 4 fuzzy clusters -> needed for the subgroups later in the loop below. In case the
  # the cmeans() function will not be a part of this function later it'd be important to add it as a parameter in the function
  
  probs.cluster <- my.fuzzy.cmeans$membership # probability for each (ref group) individual to belong to each cluster 
  
  ncols <- ncol(data_ref)
  data_ref <- cbind(data_ref, Hmisc::rMultinom(probs.cluster,draws)) # creates one extra column with assigned cluster for each draw
  
  data.table::setnames(data_ref, old = seq(ncols+1, ncols+draws, 1), new = paste0("A",seq(1:draws))) # Adjust names of draw variables to A1, A2, A3...
  
  # Continue with Subgroups
  
  data_subs <- list() # store results for each subgroup in list
  
  for (i in minority_group){ # loop through each subgroup
    
    filter_expr <- substitute(filter) 
    data_sub <- subset(data, eval(filter_expr) == i) # filter for subgroup
    
    cluster.vars <- data_sub[,cluster_vars] # keep only relevant cluster variables
    
    dm <- sapply(seq_len(nrow(cluster.vars)),
                 function(i) apply(cluster_centers, 1, function(v) sqrt(sum((cluster.vars[i, ]-v)^2)))) ## Calculate the distance for each subgroup respondent to the centers of the X clusters, using Euclidean distance.
    # dm stores the clusters as rows and the distance to each cluster for each individual as a separate column
    
    ## Now, compute membership values. 
    
    probs.cluster.subgroup <- t(apply(dm, 2, # 2 is for column-wise, 1 would be row-wise
                                      function(x) {
                                        tmp <- 1/((x/sum(x))^(2/(fuzzifier-1)))  # formula from above
                                        tmp/sum(tmp)  # normalization
                                      }))
    
    probs.cluster.subgroup[is.nan(probs.cluster.subgroup)] <- 1 # replace NaNs by 1
    
    #### Assign cluster membership probabilistically to each individual in subgroup sample
    
    ncols <- ncol(data_sub)
    data_sub <- cbind(data_sub,Hmisc::rMultinom(probs.cluster.subgroup,draws)) # creates one extra column with assigned cluster for each draw (same as above)
    
    data.table::setnames(data_sub, old = seq(ncols+1, ncols+draws, 1), new = paste0("A",seq(1:draws))) # Adjust names of draw variables to A1, A2, A3...
    
    data_subs[[as.character(i)]] <- data_sub # Store results in list
  }
  
  length_list <- length(minority_group) 
  
  data_subs <- append(list(data_ref), data_subs)
  
  combined_dfs <- do.call(rbind, data_subs) # combine list entries into a single dataframe 
  
  rownames(combined_dfs) <- seq_len(nrow(combined_dfs))
  
  return(combined_dfs) # that's the result / output of the function
}