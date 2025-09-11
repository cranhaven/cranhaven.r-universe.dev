profmatch = function(t_ind, mom, solver = NULL) {
  
  mom_covs = mom$covs
  mom_tols = mom$tols
  mom_targets = mom$targets
  
  levels <- unique(t_ind)
  nlevels <- length(levels)
  
  objlist <- vector("list", nlevels)
  idlist <- vector("list", nlevels)
  timelist <- vector("list", nlevels)
  
  ids <- 1:nrow(mom$covs)
  for (j in 1:nlevels){
    level <- levels[j]
    .ids <- ids[which(t_ind == level)]
    out <- .oneprob_profmatch(level, t_ind, mom, solver)
    objlist[[j]] <- out$obj_total
    idlist[[j]] <- .ids[out$id]
    timelist[[j]] <- out$time
  }
  ids <- NULL
  obj_totals <- vector("numeric", nlevels)
  times <- vector("numeric", nlevels)
  for (j in 1:nlevels){
    ids <- c(ids, idlist[[j]])
    obj_totals[j] <- objlist[[j]]
    times[j] <- timelist[[j]]
  }
  
  out <- list(obj_totals = obj_totals, id = ids, times = times, t_ind = t_ind, mom = mom, solver = solver)

  #! Output
  return(out)
}
