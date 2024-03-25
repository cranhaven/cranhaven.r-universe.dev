#' Outputs a synthetic survey using a simple model
#'
#' @description
#' `make_synthetic_data()` outputs a synthetic survey, generated using a simple, stochastic
#'   model of polarisation.
#' 
#' @return
#' A data frame corresponding to a survey.
#' 
#' @param nrow The number of rows in the survey
#' @param ncol The number of columns in the survey
#' @param minority The fraction of nodes in the smaller of the two polarised groups
#' @param polarisation The degree of polarisation among the system's agents
#' @param correlation Probability that group item corresponds to polarisation
#' @param scale Range of the Likert scale
#' 
#' @export
#' @examples
#' S <- make_synthetic_data(200, 8)
#' @export
make_synthetic_data <- function(nrow, 
                                ncol,
                                minority = 0.5,
                                polarisation = 0,
                                correlation = 0.85,
                                scale = 10){

  minority <- clean_minority(minority)
  polarisation <- clean_polarisation(polarisation)
  correlation <- clean_correlation(correlation)
  scale <- clean_scale(scale)

  data <- data.frame(matrix(NA, nrow = nrow, ncol = ncol + 1))

  avg <- scale / 2 + (1 - scale / 2) * polarisation
  avgflag <- 0

  response_hi <- (scale + (scale - 1) * polarisation) / 2
  response_lo <- (scale - (scale - 1) * polarisation) / 2

  for(i in 1:nrow){
    average_response <- scale / 2

    if(i < minority * nrow){
      if(runif(1) < correlation){
        data[i,1] <- as.character(0)
      }else{
        data[i,1] <- as.character(1)
      }
    }else{
      if(runif(1) < correlation){
        data[i,1] <- as.character(1)
      }else{
        data[i,1] <- as.character(0)
      }
    }

    minoritycol <- 0.5
    for(j in 1:ncol){
      if(i < minority * nrow){
        if(j < minoritycol * ncol){
          avgflag <- as.integer(1)
        }else{
          avgflag <- as.integer(0)
        }
      }else{
        if(j < minoritycol * ncol){
          avgflag <- as.integer(0)
        }else{
          avgflag <- as.integer(1)
        }
      }

      data[i,j+1] <- as.numeric(rpois(1, avg))
      while(data[i,j+1] < 1 | data[i,j+1] > scale){
        data[i,j+1] <- as.numeric(rpois(1, avg))
      }

      if(avgflag == 1){
        data[i,j+1] <- scale + 1 - data[i,j+1]
      }
    }
  }

  # colnames "group" "item_1" "item_2" ... 
  n <- c("group")
  for(i in 1:ncol){
    dummy <- paste(c("item_", i), collapse="")
    n <- append(n, dummy) 
  }
  colnames(data) <- n

  return(data)
}

clean_minority <- function(m){
  if(m < 0.0 | m > 1.0){
    warning("'minority' must be a fraction between 0 and 0.5, setting to 0.5")
    m <- 0.5
  }else if(m > 0.5){
    warning("'minority' must be between 0 and 0.5, taking 1 - minority")
    m <- 1 - m
  }
  return(m)
}

clean_polarisation <- function(p){
  if(p < 0){
    warning("'polarisation' has been rounded up to 0")
    p <- 0
  }else if(p > 1){
    warning("'polarisation' has been rounded down to 1")
    p <- 1
  }
  return(p)
}

clean_correlation <- function(c){
  if(c < 0){
    warning("'correlation' has been rounded up to 0")
    c <- 0
  }else if(c > 1){
    warning("'correlation' has been rounded down to 1")
    c <- 1
  }
  return(c)
}

clean_scale <- function(s){
  if(s < 2){
    warning("'scale' must be integer and greater than two, setting to 5")
    s <- 5
  }
  return(s)
}

#  for(i in 1:nrow){
#    average_response <- scale / 2
#
#    if(i < minority * nrow){
#      if(runif(1) < correlation){
#        groupinfo[i] <- 0
#      }else{
#        groupinfo[i] <- 1
#      }
#      average_response <- response_hi
#    }else{
#      if(runif(1) < correlation){
#        groupinfo[i] <- 1
#      }else{
#        groupinfo[i] <- 0
#      }
#      average_response <- response_lo
#    }
#
#    data[i,1] <- as.character(groupinfo[i])
#    for(j in 2:(ncol+1)){
#      data[i,j] <- as.numeric(rpois(1, average_response))
#      while(data[i,j] < 1 | data[i,j] > scale){
#        data[i,j] <- as.numeric(rpois(1, average_response))
#      }
#    }
#  }
