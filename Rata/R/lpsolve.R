#' @rdname helpers
#' @description \code{ata_lpsolve} solves the the MIP model using lp_solve
#' @param x an ATA object
#' @param time_limit the time limit in seconds passed along to the solver
#' @param message \code{TRUE} to print messages from the solver
#' @param ... additional control parameters for the solver
#' @import lpSolveAPI
#' @keywords internal
ata_lpsolve <- function(x, time_limit, message, ...) {
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  lp <- make.lp(0, x$n_lpvar)

  # (max): direction
  lp.control(lp, sense=ifelse(x$max, "max", "min"))

  # set bound for y: positive = (lb=0); negative = (ub = 0)
  if(x$negative)
    set.bounds(lp, lower=-Inf, upper=0, x$n_lpvar - 1)

  # (obj): objective function
  set.objfn(lp, x$obj, seq_along(x$obj))

  # (type): x's = binary, y = continuous
  types <- sapply(x$types, function(x) switch(x, "B"="binary", "I"="integer", "C"="real"))
  for(i in seq_along(types))
    set.type(lp, i, types[i])

  # (bounds): column bounds
  if(!is.null(x$bounds$idx))
    with(x$bounds, for(i in 1:length(idx)) {
      set.bounds(lp, if(!is.na(lb[i])) lower=lb[i], if(!is.na(ub[i])) upper=ub[i], columns=idx[i])
    })

  # (mat): constraints
  for(i in 1:nrow(x$mat))
    add.constraint(lp, x$mat[i,], x$dir[i], x$rhs[i])

  # solve
  lp.control(lp, mip.gap=c(1e-2, 1e-2), epsint=1e-3, epsb=1e-3, epsd=1e-3)
  lp.control(lp, presolve=c("lindep", "probefix"), timeout=time_limit)
  lp.control(lp, verbose=ifelse(message, 'normal', 'neutral'))
  lp.control(lp, ...)

  start_time <- Sys.time()
  code <- solve(lp)
  solve_time <- Sys.time() - start_time
  status <- switch(as.character(code),
                   '0'="optimal solution found",
                   '1'="the model is sub-optimal",
                   '2'="the model is infeasible",
                   '3'="the model is unbounded",
                   '4'="the model is degenerate",
                   '5'="numerical failure encountered",
                   '6'="process aborted",
                   '7'="timeout",
                   '9'="the model was solved by presolve",
                   '10'="the branch and bound routine failed",
                   '11'="the branch and bound was stopped because of a break-at-first or break-at-value",
                   '12'="a feasible branch and bound solution was found",
                   '13'="no feasible branch and bound solution was found")
  optimum <- get.objective(lp)
  result <- matrix(get.variables(lp)[1:(x$n_lpvar-2)], ncol=x$n_forms, byrow=FALSE)
  obj_vars <- get.variables(lp)[x$n_lpvar - 1:0]
  if(!code %in% c(0, 1, 9))
    result <- matrix(0, nrow=nrow(result), ncol=ncol(result))

  list(code=code, status=status, optimum=optimum, result=result, obj_vars=obj_vars, start_time=start_time, solve_time=solve_time)
}

