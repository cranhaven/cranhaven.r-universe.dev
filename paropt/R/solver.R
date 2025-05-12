solve <- function(ode, parameter,
                  reltol = 1e-06, abstol = 1e-08,
                  states,
                  solvertype = "bdf",
                  own_error_fct,
                  own_spline_fct,
                  own_jac_fct,
                  verbose = FALSE) {

  stopifnot(!missing(ode))
  stopifnot(is.function(ode))
  stopifnot(is.data.frame(parameter))
  stopifnot(is.data.frame(states))
  stopifnot(is.numeric(reltol))
  stopifnot(is.numeric(abstol))
  stopifnot("time has to be the first column in parameter" = names(parameter)[1] == "time")
  stopifnot("time has to be the first column in states" = names(states)[1] == "time")
  integration_times <- states[, 1]
  stopifnot(is.logical(verbose))
  name_f <- as.character(substitute(f))

  this_is_returned <- check_fct(ode, optimizer = FALSE)
  args <- formalArgs(ode)
  stopifnot("Four arguments have to be passed to ode-function!"=length(args)==4)

  fct_ret <- ast2ast::translate(ode, verbose = verbose, output = "XPtr", reference = TRUE,
                                    types_of_args = c("double", rep("sexp", 3)),
                                    return_type = "sexp")

  ecf <- NULL
  if(missing(own_error_fct)) {
    ecf <- get_default_error_fct()
  } else {
    this_is_returned <- check_fct(own_error_fct, optimizer = FALSE)
    args <- formalArgs(own_error_fct)
    stopifnot("Three arguments have to be passed to error-function!"=length(args)==3)
    ecf <- ast2ast::translate(own_error_fct, verbose = verbose, output = "XPtr",
                              reference = FALSE,
                              types_of_args = c("double", "double", "double"),
                              return_type = "sexp")
  }

  sf <- NULL
  if(missing(own_spline_fct)) {
    sf <- get_default_spline_fct()
  } else {
    this_is_returned <- check_fct(own_spline_fct, optimizer = FALSE)
    args <- formalArgs(own_spline_fct)
    stopifnot("Three arguments have to be passed to spline-function!"=length(args)==3)
    sf <- ast2ast::translate(own_spline_fct, verbose = verbose, output = "XPtr",
                             reference = TRUE,
                             types_of_args = c("double", "sexp", "sexp"),
                             return_type = "sexp")
  }

  # own jac function
  stype <- NULL
  if(solvertype == "bdf") {
    stype <- 1
  } else if(solvertype == "adams") {
    stype <- 2
  }

  stype <- NULL
  if(solvertype == "bdf") {
    stype <- 1
  } else if(solvertype == "adams") {
    stype <- 2
  }

  jf <- get_mock_jac_fct()
  if(!missing(own_jac_fct)) {
    if(stype == 2) {
      warning("own jacobian function cannot be used by solver adams. The function is ignored")
    } else if(is.function(own_jac_fct)){
      stype <- 3
      this_is_returned <- check_fct(own_jac_fct, optimizer = FALSE)
      args <- formalArgs(own_jac_fct)
      stopifnot("Three arguments have to be passed to spline-function!"=length(args)==5)
      jf <- ast2ast::translate(own_jac_fct, verbose = verbose, output = "XPtr",
                               reference = TRUE,
                               types_of_args = c("double", "sexp", "sexp", "sexp", "sexp"),
                               return_type = "sexp")
    } else if(own_jac_fct == "dfdr") {
      stype <- 3
      l <- dfdr::fcts()
      cmr <- function(a, b, c) 1
      l <- dfdr::fcts_add_fct(l, cmr, cmr, keep = TRUE)
      args <- formalArgs(ode)
      y_arg <- rlang::as_string(args[[2]])
      ydot_arg <- rlang::as_string(args[[3]])
      y_arg <- rlang::ensym(y_arg)
      ydot_arg <- rlang::ensym(ydot_arg)
      jac <- dfdr::jacobian(ode, !!ydot_arg, !!y_arg, derivs = l)
      jac_body <- body(jac)
      jac_body <- jac_body[-2]
      jac_fct <- function(t, y, ydot, jac_mat, parameter) {}
      body(jac_fct) <- jac_body
      jf <- ast2ast::translate(jac_fct, verbose = verbose, output = "XPtr",
                               reference = TRUE,
                               types_of_args = c("double", "sexp", "sexp", "sexp", "sexp"),
                               return_type = "sexp")
    }
  }


  # boundaries
  par_time <- c()
  par_cut_idx <- c()
  parb <- c()
  for(i in 2:dim(parameter)[2]) {
    temp_par <- parameter[, i]
    idx_par <- !is.na(temp_par)
    temp_par <- temp_par[idx_par]
    parb <- c(parb, temp_par)
    par_cut_idx <- c(par_cut_idx, length(temp_par))

    param_time <- parameter[, 1]
    param_time <- param_time[idx_par]
    par_time <- c(par_time, param_time)
  }

  # states
  st <- c()
  for(i in 2:dim(states)[2]) {
    st <- c(st, states[, i])
  }
  state_idx_cuts <- rep(dim(states)[1], dim(states)[2] -1)

  # tolerances
  atol <- NULL
  if(missing(abstol)) {
    atol <- rep(1e-08, dim(states)[2] - 1)
  } else {
    stopifnot("Wrong number of absolute tolerances" =
                (dim(states)[2] - 1) == length(abstol) )
    atol = abstol
  }

  par_time <- as.vector(par_time)
  par_cut_idx <- as.integer(par_cut_idx)
  istate <- unlist(states[1, 2:dim(states)[2]])

  ret <- wrapper_solver(
    init_state = istate,
    par_times = par_time,
    param_idx_cuts = par_cut_idx,
    parameter_vec = parb,
    state_measured = st, state_idx_cuts = state_idx_cuts,
    integration_times = integration_times,
    reltol, atol, fct_ret, stype, ecf, sf, jf)

  # states
  is_states <- data.frame(states$time, ret[[2]])
  names(is_states) <- names(states)

  res <- list(error = ret[[1]], insilico_states = is_states)
  return(res)
}
