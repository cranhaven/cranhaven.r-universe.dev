
#' "Variable generating" functions
#'
#' A pair of functions that allows a "variable generating" function
#' and read this function's local vars into the environment of the caller.
#'
#' @param env Parent environment
#'
#' @return Named vector of created local variables
#' @export
#'
#' @examples
#' myVariableGeneratingFunction <- function()
#' {
#'   x <- 1
#'   y <- 2
#'   local_variables()
#' }
#' myMainFunction <- function()
#' {
#'   source_variables(myVariableGeneratingFunction())
#'   print(c(x, y))
#' }
local_variables <- function(env = parent.frame())
{
  # The difference to as.list(env) is that we dont inherit from parent envs!
  varNames <- ls(env)
  set_names(
    map(varNames,
        function(var) { get(var, envir=env, inherits = F) }
        ),
    varNames)
}


#' @export
#' @rdname local_variables
localVariables <- local_variables

#' @rdname local_variables
#' @param localVars Result of function call exporting an environment
#'
#' @return The updated environment
#' @export
source_variables <- function(localVars)
{
  list2env(localVars, envir = parent.frame())
}

#' @export
#' @rdname local_variables
sourceVariables <- source_variables



# ln with increment
logxp <- function(data, increment = 1)
{
  if (increment == 0)
  {
    return(log(data))
  }
  else if (increment == 1)
  {
    return(log1p(data))
  }
  else
  {
    return(log(data + increment))
  }
}

