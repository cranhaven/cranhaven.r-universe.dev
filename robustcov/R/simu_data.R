#' @title Sample contaminated normal 
#' @description This function samples normal distribution with normal contamination
#' @param n samplesize
#' @param Omega precision matrix of the normal 
#' @param byrow whether the contamination happened by row? FALSE stand for cellwise contamination
#' @param cont_rate how many cells/rows are contaminated?
#' @param mu mean of the contamination
#' @param sd standard deviation of the contamination 
#' @return a matrix of contaminated (multivariate) normal distributed data, row as sample 
#' @export
conta_normal <- function(n, Omega, byrow = FALSE,
                        cont_rate = 0.05, 
                        mu = 10, sd = sqrt(0.2)){

    res <- rmvnorm(n, Omega)
    if(byrow){
        p <- nrow(Omega)
        contind <- sample.int(n , size = floor(cont_rate * n))
        res[contind,] <- rnorm(length(contind)*p, mu, sd)
    }
    else{
        nn <- length(res)
        contind <- sample.int(nn , size = floor(cont_rate * nn))
        res[contind] <- rnorm(length(contind), mu, sd)
    }
    return(res)
}

