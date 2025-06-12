"checkpd" <-
function(sigma, tol = 1e-06){
 eS <- eigen(sigma, symmetric = TRUE)
 ev <- eS$values
 if (all(ev >= -tol * abs(ev[1]))) answer <- TRUE else answer <- FALSE
 return(answer)
}

