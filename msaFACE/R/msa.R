moving_subset_analysis <- function(x, treatment_var, response_var, window_size, group = 1:nrow(x), ...)
{
  if (!is.factor(group))
    group <- as.factor(group)
  group_freq <- c(table(group))
  if (any(sapply(group_freq, function (i, other) any(i != other), group_freq)))
    stop("Frequency of groups must be identical")
  group_freq <- group_freq[1]
  
  if (round(window_size/group_freq, 0) !=  window_size/group_freq)
    stop("Window size must be a muliple of number of observations within one group")

    subsets <- .get_subsets(nrow(x), window_size, group_freq)
  if(!treatment_var %in% names(x)) stop("treatment_var not found in dataset")
  if(!response_var %in% names(x)) stop("response_var not found in dataset")
  is_envir_data <- c(1:ncol(x))[-1*which(names(x) %in% c(treatment_var, response_var))]
  
  x_rearr <- cbind(data.frame(response_var  = x[, which(names(x) == response_var)],
                              treatment_var = x[, which(names(x) == treatment_var)]),
                   x[,is_envir_data])
  
  msa_data <- NULL
  for (i_env_var in c(3:ncol(x_rearr)))
  {
    x_rearr_ordered <- x_rearr[order(x_rearr[,i_env_var]),]
    msa_data_var <- apply(subsets, 2, 
                          function (index_subset, data, i)
                          {
                            tmp_subset <- data[index_subset,]
                            lm <- lm(log(tmp_subset$response_var) ~ log(tmp_subset$treatment_var), na.action = "na.omit")
                            
                            ## Extract coefficients within subset
                            subset_coefficients <- c(summary(lm)$coefficients[2,1],   ## extract CO2 fertilization effect
                                                     summary(lm)$coefficients[2,4],   ## extract significance of CO2 fertilization effect
                                                     mean(tmp_subset$response_var),   ## extract average biomass in subset
                                                     colMeans(tmp_subset[,3:ncol(tmp_subset)]),   ## extract the means of all environmental variables in the subset
                                                     min (tmp_subset[,i], na.rm = T),             ## extract minimum of ordering environmental variable in the subset
                                                     max (tmp_subset[,i], na.rm = T)              ## extract maximum of ordering environmental variable in the subset
                                                     )
                            return(subset_coefficients)
                          }, x_rearr_ordered, i_env_var)
    msa_data_var <- as.data.frame(t(msa_data_var))
    names(msa_data_var) <- c("CFE", "Pval",
                             paste("Mean_",response_var, sep = ""),
                             paste("Mean_", names(x_rearr)[-c(1,2)], sep = ""),
                             paste("Min_", names(x_rearr)[i_env_var], sep =""),
                             paste("Max_", names(x_rearr)[i_env_var], sep ="")
                             )
    msa_data[[i_env_var - 2]] <- msa_data_var
  }
  names(msa_data) <- names(x_rearr)[-c(1,2)]
  class(msa_data) <- "MSA_coef"
  attr(msa_data, "allData") <- list(data = x_rearr, treatment_var = treatment_var, 
                                    response_var = response_var, 
                                    window_size = window_size, group = group)
  ### make summary function for class msa_data
  ### range of the CFE values for each i_var
  return(msa_data)
}