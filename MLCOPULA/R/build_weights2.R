build.weights2 <- function(U,weights,cop){
  l <- list(frank = list(cop.est = estimation.frank, cop = "frank"),
            gaussian = list(cop.est = estimation.normal, cop = "gaussian"),
            joe = list(cop.est = estimation.joe, cop = "joe"),
            clayton = list(cop.est = estimation.clayton, cop = "clayton"),
            gumbel = list(cop.est = estimation.gumbel, cop = "gumbel"),
            amh = list(cop.est = estimation.amh, cop = "amh")
  )

  l <- l[cop]
  
  ws <- lapply(l, function (cop)
    build.weights(U = U, cop.est = cop$cop.est,weights = weights,
                  cop = cop$cop)
  )
  w_list <- lapply(ws, function(x) x$w)
  w <- do.call(pmax, w_list)
  
  name_cop <- matrix(ncol = dim(ws[[1]]$w)[1],
                     nrow = dim(ws[[1]]$w)[1])
  colnames(name_cop) <- colnames(ws[[1]]$w)
  row.names(name_cop) <- colnames(ws[[1]]$w)
  
  theta <- matrix(ncol = dim(ws[[1]]$w)[1],
                  nrow = dim(ws[[1]]$w)[1])
  colnames(theta) <- colnames(ws[[1]]$w)
  row.names(theta) <- colnames(ws[[1]]$w)
  
  for (i in 1:length(ws)) {
    filter <- w == ws[[i]]$w
    
    name_cop[filter] = ws[[i]]$cop
    theta[filter] = ws[[i]]$theta[filter]
  }
  
  res <- list(w = w, theta = theta,
              cop = name_cop, ws = ws)
  return(res)
}
