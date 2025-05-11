print.distfreereg <-
  function(x, ..., digits = 3, col_sep = 2){
    stopifnot(!is.null(x[["observed_stats"]]), !is.null(names(x[["observed_stats"]])),
              !is.null(x[["p"]][["value"]]), !is.null(x[["data"]][["Y"]]),
              !is.null(x[["mcsim_stats"]][[1]]))

    # Parameter_estimates
    parameter_names <- if(is.null(names(x[["theta_hat"]]))){
      paste0("theta", seq_along(x[["theta_hat"]]))
    } else {
      names(x[["theta_hat"]])
    }
    
    parameters <- sprintf(paste0("%1.", digits, "e"), x[["theta_hat"]])
    parameter_matrix <- rbind(parameter_names, parameters)
    buffered_parameter_matrix <- buffer_matrix(parameter_matrix)
    
    # Stats
    B <- length(x[["mcsim_stats"]][[1]])
    j <- length(names(x[["observed_stats"]]))
    col_names <- c("Stat", "Value", "Pr(>Value)", "MCSE")
    stat_matrix <- matrix(NA_character_, nrow = 1 + j, ncol = 4)
    stat_matrix[1,] <- col_names
    for(i in seq_len(j)){
      if(identical(x[["p"]][["value"]][[i]], 0)){
        prepend <- "<"
        x[["p"]][["value"]][[i]] <- 1/B
      } else {
        if(identical(x[["p"]][["value"]][[i]], 1)){
          prepend <- ">"
          x[["p"]][["value"]][[i]] <- 1-1/B
        } else {
          prepend <- ""
        }
      }
      stat_matrix[i+1, 1] <- names(x[["observed_stats"]])[i]
      stat_matrix[i+1, 2:4] <- sprintf(paste0("%1.", digits, "e"),
                                       c(x[["observed_stats"]][[i]],
                                         x[["p"]][["value"]][[i]],
                                         x[["p"]][["mcse"]][[i]]))
      stat_matrix[i+1, 3] <- paste0(prepend, stat_matrix[i+1, 3])
    }
    
    buffered_stat_matrix <- buffer_matrix(stat_matrix)
    
    # Output
    cat("\nNumber of observations:", length(x[["fitted_values"]]), "\n")
    cat("Monte Carlo simulations:", B, "\n")
    if(!is.null(x[["theta_hat"]])){
      cat("\nEstimated parameter values:\n")
      for(i in seq_len(nrow(parameter_matrix))){
        cat(paste(buffered_parameter_matrix[i,],
                  collapse = paste0(rep(" ", times = col_sep), collapse = "")), "\n")
      }
    }
    cat("\nObserved statistics:\n")
    for(i in seq_len(nrow(stat_matrix)))
      cat(paste(buffered_stat_matrix[i,],
                collapse = paste0(rep(" ", times = col_sep), collapse = "")), "\n")
    cat("---\n`MCSE' is the Monte Carlo standard error of the estimated p-value.\n\n")
  }
