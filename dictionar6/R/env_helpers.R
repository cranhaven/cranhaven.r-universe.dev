ls_env <- function(env) {
  names(as_list_env(env))
}

length_env <- function(env) {
  length(as_list_env(env))
}

as_list_env <- function(env) {
  c(
    as.list(env),
    tryCatch(as_list_env(parent.env(env)), error = function(e) NULL)
  )
}

where_env <- function(nm, env) {
  if (!exists(nm, env)) {
    stop(sprintf("'%s' does not exist in 'env' or its parents", nm))
  }

  if (exists(nm, env, inherits = FALSE)) {
    env
  } else {
    where_env(nm, parent.env(env))
  }
}


rm_env <- function(nm, env) {
  rm(list = nm, envir = where_env(nm, env))
  invisible(env)
}

update_env_value <- function(nm, value, env) {
  env <- where_env(nm, env)
  env[[nm]] <- value
  invisible(env)
}

rename_env_item <- function(nm, new_nm, env) {
  if (exists(new_nm, env)) {
    stop(sprintf("'%s' already exists in 'env'", new_nm))
  }

  which_env <- where_env(nm, env)

  which_env[[new_nm]] <- which_env[[nm]]
  rm(list = nm, envir = which_env)

  invisible(env)
}
