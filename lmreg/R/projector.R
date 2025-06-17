projector <-
function(M, tol = sqrt(.Machine$double.eps)) return(M%*%ginv(M, tol = tol))
