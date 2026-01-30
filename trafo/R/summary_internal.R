summary_internal <- function(modOne, modTwo, compare, std) {
  
  formula <- NULL
  
  if (inherits(modOne, "lm") && std == TRUE) {
    
    # Summary of transformed model
    modTwo_sum <- summary(modTwo)
    modOne_sum <- summary(modOne)
    # Kommentierung muss weggenommen werden, wenn Standardfehler wieder
    # gelöscht werden sollen
    # modTwo_tmp <- summary(modTwo)
    # modTwo$call <- as.symbol((paste(paste(deparse(modTwo$call), sep = "\n", 
    #                                          collapse = "\n"), 
    #                                    paste0("formula = ",modTwo$formula), 
    #                                    sep = "\n")))
    # modTwo_sum$coefficients <- matrix(modTwo_sum$coefficients[, 1])
    # colnames(modTwo_sum$coefficients) <- c("Estimate")
    # rownames(modTwo_sum$coefficients) <- rownames(modTwo_tmp$coefficients)
    # modTwo_sum$formula <- modTwo$formula
    # modOne_sum <- summary(modOne)
    
    if (compare == TRUE) {
      #modOne_sum <- summary(modOne)
      # modOne_tmp <- summary(modOne)
      # modOne$call <- as.symbol((paste(paste(deparse(modOne$call), sep = "\n", 
      #                                       collapse = "\n"), 
      #                                 paste0("formula = ",modOne$formula), 
      #                                 sep = "\n")))
      # modOne_sum$coefficients <- matrix(modOne_sum$coefficients[, 1])
      # colnames(modOne_sum$coefficients) <- c("Estimate")
      # rownames(modOne_sum$coefficients) <- rownames(modOne_tmp$coefficients)
      # modOne_sum$formula <- modOne$formula
      
    }
    
  } else if (inherits(modOne, "lm") && std == FALSE) {
  
    # Normal summary
    modOne_sum <- summary(modOne)
    modTwo_sum <- summary(modTwo)
    modTwo_sum$formula <- modTwo$formula
    
  }   
    
  if (inherits(modOne, "lme")) {
    
    # Normal summary
    modOne_sum <- summary(modOne)
    modTwo_sum <- summary(modTwo)
    
  }
  
  
  return(list(modOne_sum = modOne_sum, 
              modTwo_sum = modTwo_sum))
  
  
  
  }