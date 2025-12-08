#' Start a TGVE instance server
#'
#' The function accepts a `path` to get a directory containing an instance of
#' TGVE, by default this is done via `TEMP_DIR_ENV` env variable. If neither
#' is given then the function copies a clean copy of the bundled
#' TGVE version into a temporary directory.
#'
#' @param path location of TGVE path to be served by plumber.
#' @param port to serve from.
#' @param host host to pass to plumber default `http://127.0.0.1`
#' to `FALSE`.
#' @param background run the R process in the background using `callr`,
#' defaults to `TRUE`.
#' @param run whether to start the server, defaults to `TRUE`. If
#' not, then the created server will be returned.
#' @return the valude returned depends on: (1) `run`, if it is FALSE then
#' an instance of `plumber::pr`, (2) if `run` is true and `background` is TRUE
#' the `plumber::pr` instance is started and its process is returned, and
#' (3) if `run` is TRUE and `background` is FALSE then a message is displayed
#' showing the blocking `plumber::pr` instance's `path`, `port` and `host`.
#'
#' @export
#' @examples {
#' # This will run in the background using `callr`
#' ps = tgve_server(background = TRUE)
#' Sys.sleep(2)
#' ps$kill()
#' }
tgve_server = function(path = Sys.getenv("TEMP_path_ENV"),
                       port = 8000,
                       host = "127.0.0.1",
                       background = FALSE,
                       run = TRUE) {
  if(!dir.exists(path)) {
    path = tempInstance()
  }

  server = plumber::pr()
  server = plumber::pr_static(server, "/", path)

  if(!run) {
    return(server)
  }

  message("Attempting to serve TGVE instance from: ", path)
  if(background) {
    return(background_run(server))
  }
  openURL(host = host, port = port)
  server$run(port = port, host = host, docs = FALSE)
}

#' Internal helper function to run a `plumber` instance on specific
#' host and port.
#'
#' @param server an instance of `plumber` class
#' @param port numeric port to pass to `server` instance
#' @param host character host value for `server` instance
background_run = function(server, port = 8000, host = "127.0.0.1") {
  f <- function(s, p, h) {s$setDocs(FALSE); s$run(port = p, host = h)}
  # TODO: try killing if process is running
  ps <- callr::r_bg(f, list(s = server, p = port, h = host))
  message("Running plumber at: ", "http://", host, "/", port)
  return(ps)
}
