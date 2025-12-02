#' Read environment variables from a file
#'
#' Reads a \code{.env} file containing environment variables in the format \code{KEY=VALUE}, and returns them as a named list.
#' Lines starting with \code{#} are considered comments and ignored.
#'
#' @param path A string specifying the path to the \code{.env} file. If not provided, defaults to \code{.env} in the current working directory.
#'
#' @return A named list of environment variables. Each element is a key-value pair extracted from the file. If no variables are found, \code{NULL} is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming an `.env` file with the following content:
#' # DB_HOST=localhost
#' # DB_USER=root
#' # DB_PASS="secret"
#'
#' env_vars <- read_env(".env")
#' print(env_vars)
#' # Should output something like:
#' # $DB_HOST
#' # [1] "localhost"
#'
#' # If no path is given, it defaults to `.env` in the current directory.
#' env_vars <- read_env()
#' }

read_env <- function(path) {

  if(methods::missingArg(path)) { path <- '.env' }

  if(!file.exists(path)) {
    stop('Path to environment file is required.')
  }

  env_file <- readLines(path, warn = FALSE)
  env_values <- unlist(stringr::str_split(env_file, '\n'))
  env_values <- env_values[env_values != '']
  env_values <- env_values[!grepl('^#', env_values)]

  if (length(env_values) == 0) {
    return(NULL)
  }

  env <- list()
  for (i in seq_along(env_values)) {
    env_value <- unlist(stringr::str_split(env_values[i], "=")[[1]])
    key <- stringr::str_trim(env_value[1])
    value <- stringr::str_trim(env_value[2])

    env[[key]] <- stringr::str_remove_all(value, '\\"')
  }

  return(env)
}
