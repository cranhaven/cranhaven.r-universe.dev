variables <- function(expr) {
  all.vars(parse(text = expr))
}

allNA <- function(x) all(is.na(x))

set_class <- function(x, class) {
  class(x) <- class
  x
}

add_new_variables <- function(values, object, expr, silent) {
  names_values <- names(values)
  parameters <- pars(object)
  variables <- variables(expr)

  variables <- setdiff(variables, union(parameters, names_values))

  if (!length(variables)) {
    err("`expr` must include at least one variable that is not in object or values")
  }

  values[variables] <- NA
  values
}

drop_overridden_parameters <- function(object, values, silent) {
  parameters <- pars(object)
  drop <- parameters[parameters %in% names(values)]
  if (length(drop)) {
    if (all(parameters %in% drop)) {
      err("all the parameters in object are also in values")
    }

    if (!silent) {
      wrn(
        "the following parameters were also in values and so were dropped from object: ",
        cc(drop, " and ")
      )
    }
    object <- subset(object, pars = setdiff(parameters, drop))
  }
  object
}

drop_absent_values <- function(values, expr, silent) {
  variables <- variables(expr)
  name_values <- names(values)
  drop <- name_values[!name_values %in% variables]
  if (length(drop)) {
    if (all(name_values %in% drop)) {
      if (!silent) wrn("none of the variables in values are in expr")
      return(list())
    }
    if (!silent) {
      wrn(
        "the following variables were not in expr and so were dropped from values: ",
        cc(drop, " and ")
      )
    }
    values <- values[setdiff(name_values, drop)]
  }
  values
}

drop_absent_parameters <- function(object, expr, silent) {
  variables <- variables(expr)
  parameters <- pars(object)
  drop <- parameters[!parameters %in% variables]
  if (length(drop)) {
    if (all(parameters %in% drop)) err("none of the parameters in object are in expr")
    if (!silent) {
      wrn("the following parameters were not in expr and so were dropped from object: ", cc(drop))
    }
    object <- subset(object, pars = setdiff(parameters, drop))
  }
  object
}

subset_mcmcarray_chains <- function(x, chains) {
  x <- abind::asub(x, chains, 1L, drop = FALSE)
  set_class(x, "mcmcarray")
}

subset_mcmcarray_iterations <- function(x, iterations) {
  x <- abind::asub(x, iterations, 2L, drop = FALSE)
  dim <- dim(x)[-c(1, 2)]
  if (length(dim) == 1) dim <- NULL
  dim(x) <- dim
  x
}

subset_mcmcr_iterations <- function(x, iterations) {
  lapply(x, subset_mcmcarray_iterations, iterations = iterations)
}

subset_mcmcr_chains <- function(x, chains) {
  x <- lapply(x, subset_mcmcarray_chains, chains = chains)
  set_class(x, "mcmcr")
}

bind_iterations_mcmcarrays <- function(x) {
  x <- lapply(x, unclass)
  niters <- length(x)
  dim <- dims(x[[1]])
  dim <- c(dim, niters, 1L)
  x <- do.call("c", x)
  dim(x) <- dim
  x <- set_class(x, "mcarray")
  as.mcmcarray(x)
}

bind_iterations_mcmcrs <- function(x) {
  x <- purrr::transpose(x)
  x <- lapply(x, bind_iterations_mcmcarrays)
  x <- set_class(x, "mcmcr")
  x
}

monitor_variables <- function(monitor, values) {
  variables <- names(values[vapply(values, allNA, TRUE)])

  match <- variables[grepl(monitor, variables)]
  if (!length(match)) {
    err(
      "`monitor` '", monitor,
      "' must match at least one of the following variables in expr: ",
      cc(variables, " or ")
    )
  }
  sort(match)
}

split_apply_combine_sample <- function(i, object, expr, values, monitor) {
  object <- subset_mcmcr_iterations(object, iterations = i)
  object <- c(object, values)
  object <- within(object, eval(expr))
  object <- object[monitor]

  object
}

split_apply_combine_chain <- function(i, object, expr, values, monitor) {
  object <- subset_mcmcr_chains(object, chains = i)

  object <- lapply(1:niters(object),
    FUN = split_apply_combine_sample, object = object,
    expr = expr, values = values, monitor = monitor
  )
  object <- bind_iterations_mcmcrs(object)
  object
}

split_apply_combine <- function(object, expr, values, monitor, parallel) {
  expr <- parse(text = expr)

  if (parallel) {
    if (!requireNamespace("plyr", quietly = TRUE)) {
      err("plyr is required to run mcmc_derive on chains in parallel")
    }
    object <- plyr::llply(1:nchains(object), split_apply_combine_chain,
      object = object,
      .parallel = TRUE, expr = expr,
      values = values, monitor = monitor
    )
  } else {
    object <- lapply(1:nchains(object), split_apply_combine_chain,
      object = object, expr = expr,
      values = values, monitor = monitor
    )
  }

  Reduce(bind_chains, object)
}
