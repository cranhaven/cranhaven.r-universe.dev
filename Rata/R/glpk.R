#' @rdname helpers
#' @description \code{ata_glpk} solves the the MIP model using GLPK
#' @import glpkAPI
#' @keywords internal
ata_glpk <- function(x, time_limit, message, ...) {
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  opts <- list(...)

  # set up the problem
  lp <- initProbGLPK()
  addRowsGLPK(lp, nrow(x$mat))
  addColsGLPK(lp, ncol(x$mat))

  # (max): optimization direction
  setObjDirGLPK(lp, ifelse(x$max, GLP_MAX, GLP_MIN))

  # (obj): obj functions
  setObjCoefsGLPK(lp, seq(x$n_lpvar), x$obj)

  # (types): x's = binary, y's = continuous
  for(j in seq(x$n_lpvar)[x$types == 'B'])
    setColKindGLPK(lp, j, GLP_BV)
  for(j in seq(x$n_lpvar)[x$types == 'C'])
    setColBndGLPK(lp, j, GLP_LO, 0, 0)

  # negative objective value
  if(x$negative)
    setColBndGLPK(lp, x$n_lpvar-1, GLP_UP, 0, 0)

  # fixed values
  if(!is.null(x$bounds$idx))
    for(j in 1:length(x$bounds$idx))
      if(is.na(x$bound$lb[j])){
        setColBndGLPK(lp, x$bounds$idx[j], GLP_UP, 0, x$bounds$ub[j])
      } else if(is.na(x$bound$ub[j])){
        setColBndGLPK(lp, x$bounds$idx[j], GLP_LO, x$bounds$lb[j], 0)
      } else {
        setColBndGLPK(lp, x$bounds$idx[j], GLP_DB, x$bounds$lb[j], x$bounds$ub[j])
      }
  # # check column bounds
  # cbind(getColsLowBndsGLPK(lp, 1:x$num_lpvar), getColsUppBndsGLPK(lp, 1:x$num_lpvar))

  # (mat)
  ind <- x$mat != 0
  ia <- rep(1:nrow(x$mat), ncol(x$mat))[ind]
  ja <- rep(1:ncol(x$mat), each=nrow(x$mat))[ind]
  ar <- x$mat[ind]
  loadMatrixGLPK(lp, length(ar), ia, ja, ar)

  # (dir & rhs): row bounds
  dir <- sapply(x$dir, function(x) switch(x, '>='=GLP_LO, '<='=GLP_UP, '='=GLP_FX))
  setRowsBndsGLPK(lp, 1:nrow(x$mat), x$rhs, x$rhs, dir)
  # # check row bounds
  # cbind(getRowsLowBndsGLPK(lp, 1:nrow(x$mat)), getRowsUppBndsGLPK(lp, 1:nrow(x$mat)))

  # solve
  setMIPParmGLPK(PRESOLVE, 1)
  setMIPParmGLPK(GMI_CUTS, 1)
  setMIPParmGLPK(MIR_CUTS, 1)
  setMIPParmGLPK(CLQ_CUTS, 1)
  setMIPParmGLPK(MIP_GAP, 1e-2)
  setMIPParmGLPK(TM_LIM, 1000 * time_limit)
  setMIPParmGLPK(MSG_LEV, ifelse(message, GLP_MSG_ON, GLP_MSG_OFF))
  for(i in seq_along(opts))
    setMIPParmGLPK(get(names(opts)[i]), opts[[i]])

  start_time <- Sys.time()
  code <- solveMIPGLPK(lp)
  solve_time <- Sys.time() - start_time
  status <- switch(as.character(code),
                   '0'="optimal solution found",
                   '1'='invalid basis',
                   '2'='singular matrix',
                   '3'='ill-conditioned matrix',
                   '4'='invalid bounds',
                   '5'='solver failed',
                   '6'='objective lower limit reached',
                   '7'='objective upper limit reached',
                   '8'='iteration limit exceeded',
                   '9'='time limit exceeded',
                   '10'='no primal feasible solution',
                   '11'='no dual feasible solution',
                   '12'='root LP optimum not provided',
                   '13'='search terminated by application',
                   '14'='relative mip gap tolerance reached',
                   '15'='no primal/dual feasible solution',
                   '16'='no convergence',
                   '17'='numerical instability',
                   '18'='invalid data',
                   '19'='result out of range')
  optimum <- mipObjValGLPK(lp)
  result <- matrix(mipColsValGLPK(lp)[1:(x$n_lpvar-2)], ncol=x$n_forms, byrow=FALSE)
  obj_vars <- mipColsValGLPK(lp)[x$n_lpvar - 1:0]

  list(code=code, status=status, optimum=optimum, result=result, obj_vars=obj_vars, start_time=start_time, solve_time=solve_time)
}
