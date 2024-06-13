



CItoString <- function(CI, digit){
  format_string <- paste('%.',digit,'f',sep="")
  return(paste('[', sprintf(format_string,round(CI[1], digit)), ',', 
               sprintf(format_string,round(CI[2], digit)), ']', sep=''))
  
}

forstCIString <- function(specific_summary, digit){
  nrow <- nrow(specific_summary)
  cishow <- rep(' ', nrow)
  format_string <- paste('%.',digit,'f',sep="")
  for(i in 1:nrow){
    cishow[i] <- paste(sprintf(format_string,round(specific_summary$mean[i], digit))," (",
                       sprintf(format_string,round(specific_summary$ET_lower[i], digits= digit)),
                       ", ",sprintf(format_string,round(specific_summary$ET_upper[i],digit)),")",sep="")
  }
  return(cishow)
}

samplesTrucation <- function(samples, lower = lower, upper = upper, n_samples = n_samples){
  samples <- samples[(samples > lower) & (samples < upper)]
  if(length(samples) > n_samples)
    samples <- samples[1:n_samples]
  return(samples)
}


vectorToList <- function(parms_vector){
  if(length(parms_vector) == NUM_PARAMETERS_SARMANOV){
    parms_list <- list(
      a1 = parms_vector[1],
      b1 = parms_vector[2],
      a2 = parms_vector[3],
      b2 = parms_vector[4],
      rho = parms_vector[5]
    )
  } else {
    parms_list <- list(
      a1 = parms_vector[1],
      b1 = parms_vector[2],
      a2 = parms_vector[3],
      b2 = parms_vector[4]
    )
  }
  return(parms_list)
}

densityListToDf <- function(density_list, attribute_names){
  
  density_df <- NULL
  for(i in 1:length(attribute_names)){
    name <- attribute_names[i]
    new_density_df <- data.frame(x = density_list[[name]]$x,
                                 y = density_list[[name]]$y,
                                 group = rep(name, length(x = density_list[[name]]$x))) 
    density_df <- rbind(density_df, new_density_df)
  }
  
  return(density_df)
  
}

