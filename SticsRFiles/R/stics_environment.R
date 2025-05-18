

#' STICS env name
#'
#' Get the default STICS environment name
#'
#' @return The default STICS env name
#' @keywords internal
#' @noRd
#'
sticsenv_name <- function() {
  return(".stics")
}

#' Don't know what purpose this function serve
#'
#' Too complicated, does so many things I don't know what to tell
#' about this function. Patrice, can you write the doc, or simplify
#' the interface ?
#'
#' @param name     Name of an object in an env OR env name ??? (optional)
#' @param env_name The environment name
#' @param create   Create the environment name
#'
#' @keywords internal
#' @noRd
#'
stics_env <- function(name = NULL, env_name = globalenv(), create = TRUE) {
  exists_env <- stics_exists()

  # Can't create env
  if (!exists_env) {
    if (!create) {
      # warning(sticsenv_name(),
      #      " environment does not exist, set create to TRUE to create it.")
      return(NULL)
    }

    # creating .stics env attached to .GlobalEnv
    local_stics <-
      sticsenv_create(name = sticsenv_name(), env_name = ".GlobalEnv")
  } else {
    # getting existing .stics env
    local_stics <-
      get(x = sticsenv_name(), envir = globalenv(), inherits = FALSE)
  }

  # Returning .stics env object
  if (base::is.null(name) || name == sticsenv_name()) {
    return(local_stics)
  }

  # Checking a sub-env
  exists_name <- exists(name, envir = local_stics, inherits = FALSE)

  if (exists_name) {  # Returning .stics env object

    return(local_stics[[name]])
  }


  if (!create) {
    # warning(name,
    #      " environment does not exist, set create to TRUE to create it.")
    return(NULL)
  }

  # Creating a new .stics sub-env
  local_env <- sticsenv_create(name = name, env_name = sticsenv_name())

  return(local_env)
}


#' Create a new STICS environment
#'
#' @param name The name of the STICS environment
#' @param env_name The name of the parent environment
#'
#' @return The new environment
#' @keywords internal
#' @noRd
#'
sticsenv_create <- function(name, env_name = ".GlobalEnv") {
  parent <- eval(parse(text = env_name))

  new_env <- new.env(parent = parent)

  assign(
    x = name,
    value = new_env,
    pos = parent
  )

  new_env <- sticsenv_set_name(name = name, env_name = env_name)

  return(new_env)
}

#' Replace STICS env name
#'
#' Set the name of the environment where an object is found
#'
#' @param name The name of an object potentially found in a STICS environment
#' @param env_name The name of the STICS environment
#' @param fix_name Fix the name of the environment to `fix_name`,
#' or to `paste0("R_", name)` if `NULL` (default).
#'
#' @return The new environment
#' @keywords internal
#' @noRd
#'
sticsenv_set_name <- function(name,
                              env_name = sticsenv_name(),
                              fix_name = NULL) {
  envir <- suppressWarnings(stics_get(name = name, env_name = env_name))

  if (base::is.null(envir)) {
    return()
  }

  if (base::is.null(fix_name)) {
    env_name <- paste0("R_", name)
  } else {
    env_name <- fix_name
  }

  attr(envir, "name") <- env_name

  return(envir)
}


#' Get STICS env name from object
#'
#' Get the name of the environment where an object is found
#'
#' @param name The name of an object potentially found in a STICS environment
#' @param env_name The name of the STICS environment
#'
#' @return A vector of environment names
#' @keywords internal
#' @noRd
sticsenv_get_name <- function(name = NULL, env_name = sticsenv_name()) {
  if (!stics_exists(name = name, env_name = env_name)) {
    return(invisible())
  }

  return(environmentName(env = stics_get(name)))
}


#' List STICS env
#'
#' List all objects in a STICS environment
#'
#' @param name Optionally, the name of an object in the STICS environment
#' @param env_name The name of the STICS environment
#' @param detail Return all details ? (use `ls.str()`)
#'
#' @return A vector of all object names in the environment
#' @keywords internal
#' @noRd
sticsenv_ls <- function(name = NULL,
                        env_name = sticsenv_name(),
                        detail = FALSE) {
  envir <- stics_get(name, env_name = env_name)

  if (base::is.null(envir)) {
    return()
  }

  if (detail) {
    return(utils::ls.str(envir))
  }

  return(ls(envir))
}

#' Is object part of STICS env
#'
#' Test if an object is in a STICS environment
#'
#' @param name Name of the object in the STICS environment
#' @param env_name The name of the STICS environment
#'
#' @return A boolean, `TRUE` if the object is in the env, `FALSE` otherwise.
#' @keywords internal
#' @noRd
stics_exists <- function(name = NULL, env_name = sticsenv_name()) {
  exists_env <- exists(
    x = sticsenv_name(),
    envir = globalenv(), inherits = FALSE
  )

  if (!exists_env) {
    return(FALSE)
  }

  if (base::is.null(name) || name == sticsenv_name()) {
    return(exists_env)
  }



  # name contains a env name
  val <- eval(parse(text = paste0(sticsenv_name(), "$", name)))
  if (!base::is.null(val)) {
    return(TRUE)
  }

  # name contains a variable name
  val <- eval(parse(text = paste0(sticsenv_name(), "$", env_name, "$", name)))
  if (!base::is.null(val)) {
    return(TRUE)
  }

  # For a variable containing a list
  name <- stics_split_list(name)[1]
  val <- eval(parse(text = paste0(sticsenv_name(), "$", env_name, "$", name)))
  if (!base::is.null(val)) {
    return(TRUE)
  }

  return(FALSE)
}

#' Get object from STICS env
#'
#' Get an object from the STICS environment
#'
#' @param name Name of the object in the STICS environment
#' @param env_name The name of the STICS environment
#'
#' @return The object value.
#' @keywords internal
#' @noRd
stics_get <- function(name = NULL, env_name = sticsenv_name()) {
  if (base::is.null(name) || name == sticsenv_name()) {
    return(stics_env(create = FALSE))
  }

  if (length(name) > 1) {
    out <- lapply(name, function(x) stics_get(name = x, env_name = env_name))

    names(out) <- name
    return(out)
  }


  envir <- stics_env(name = env_name, create = FALSE)

  elts <- stics_split_list(name)

  if (!stics_exists(name = elts[1], env_name = env_name)) {
    warning(
      elts[1],
      " does not exist in environment ",
      environmentName(envir)
    )
    return(NA)
  }



  loc_var <- get(elts[1], envir = envir, inherits = FALSE)

  if (length(elts) == 1) {
    return(loc_var)
  }

  # for a list elt extraction
  xpression <- paste0("loc_var$", paste(elts[2:length(elts)], collapse = "$"))

  eval(parse(text = xpression))

  return(loc_var)
}

#' Add to STICS env
#'
#' Add an object to the STICS environment
#'
#' @param name Name of the object used in the STICS environment
#' @param value Value to associate to the object
#' @param env_name The name of the STICS environment
#'
#' @return Nothing. Just adds an object to the given environment.
#' @keywords internal
#' @noRd
#'
stics_set <- function(name, value, env_name = sticsenv_name()) {
  envir <- stics_env(name = env_name)

  # Creating a copy in the target environment
  envir[["value"]] <- value

  elts <- stics_split_list(name)


  # Simple variable, existing or not
  if (length(elts) == 1) {
    eval(parse(text = paste0(name, " <- value")), envir = envir)
    stics_remove(name = "value", env_name = env_name)
    return(invisible(TRUE))
  }

  # A named list variable
  # testing if exists
  if (!stics_exists(elts[1], env_name = env_name)) {
    warning(
      elts[1],
      paste0(
        ": unknown list variable in ",
        sticsenv_name(),
        " environment !"
      )
    )

    return(invisible(FALSE))
  }

  xpression <- paste0(paste(elts, collapse = "$"), "<- value")

  eval(parse(text = xpression), envir = envir)

  # deleting temporary value variable
  stics_remove(name = "value", env_name = env_name)

  return(invisible(TRUE))
}


#' Get the class of a STICS object
#'
#' @return A vector of classes
#' @keywords internal
#' @noRd
#'
stics_class <- function(name, env_name = sticsenv_name()) {
  return(class(stics_get(name = name, env_name = env_name)))
}


#' Split STICS names
#'
#' Split STICS names by "$"
#'
#' @return A vector of names
#' @keywords internal
#' @noRd
#'
stics_split_list <- function(name) {
  return(unlist(strsplit(x = name, split = "\\$")))
}

#' Remove objects from STICS environment
#'
#' Remove objects listed in a STICS environment.
#'
#' @return Nothing. Just removes an object from the STICS environment.
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' stics_remove()
#' }
stics_remove <- function(name = NULL, env_name = sticsenv_name()) {

  envir <- stics_get(name = env_name)

  if (base::is.null(name)) {
    envir <- parent.env(envir)
    name <- sticsenv_name()
  }

  if (!stics_exists(name = name, env_name = env_name)) {
    warning(name, " does not exist !")
    return(FALSE)
  }

  # TODO: add if name is a part of a list variable
  # but not an env
  # a$b
  # splitter pour voir si name est une liste
  # si oui alors par rm mais evaluer a$b <- NULL
  # if ()


  # getting variable names
  splitted_names <- strsplit(name, split = "$", fixed = TRUE)
  list_idx <- unlist(lapply(splitted_names, function(x) length(x) > 1))

  list_var_names <- NULL
  var_names <- NULL

  if (any(list_idx)) list_var_names <- name[list_idx]

  if (any(!list_idx)) var_names <- name[!list_idx]


  ret <- FALSE
  if (length(list_var_names)) {
    # suppression elts de liste
    ret <- unlist(lapply(
      list_var_names,
      function(x) {
        eval(parse(text = paste0("base::is.null(", x, " <- NULL)")),
             envir = envir)
      }
    ))
    ret <- all(ret)
  }

  if (!length(var_names)) {
    return(ret)
  }

  # removing other variables or env
  if (base::is.null(rm(list = var_names, envir = envir))) {
    return(ret & TRUE)
  }
  # }
}


#' Clean STICS environment
#'
#' @return Nothing. Just cleans the STICS environment.
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' stics_clean()
#' }
stics_clean <- function() {
  names <- sticsenv_ls()

  if (base::is.null(names) || !length(names)) {
    return()
  }

  stics_remove(name = names)
}
