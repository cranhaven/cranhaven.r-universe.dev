print.BANOVA.floodlight <- 
  function(x, ...){
    floodlight.printing <- function(x){
      for(i in 1:length(x$sol)){
        flood_table <- x$sol[[i]]
        within_range <- array(0, dim = c(nrow(flood_table), ncol(flood_table)))
        for (i in 1:nrow(flood_table)){
          for (j in ncol(flood_table):(ncol(flood_table)-2)){
            if (as.numeric(flood_table[i, j]) > x$num_range[1] & as.numeric(flood_table[i, j]) < x$num_range[2])
              within_range[i,j] <- 1
          }
        }
        #x$sol <- format(flood_table, trim = T, nsmall = 4)
        
        for (i in 1:nrow(flood_table)){
          for (j in ncol(flood_table):(ncol(flood_table)-2)){
            if (within_range[i,j])
              flood_table[i, j] <- paste(flood_table[i, j], '*', sep="")
          }
        }
        print(noquote(flood_table))
      }
    }
    if (is.null(x$sol)){
      #this is the case for multivariate models
      dep_var_names <- names(x)
      for (name in dep_var_names){
        x_temp <- x[[name]]
        title <- paste0("\nFloodlight analysis for ", name,"\n")
        cat(title)
        floodlight.printing(x_temp)
      }
    } else{
      floodlight.printing(x)
    }
  }