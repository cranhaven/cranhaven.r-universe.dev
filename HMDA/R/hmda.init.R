#' @title Initialize or Restart H2O Cluster for HMDA
#'        Analysis
#' @description Initializes or restarts an H2O cluster configured for
#'        Holistic Multimodel Domain Analysis. It sets up the cluster
#'        with specified CPU threads, memory, and connection settings.
#'        It first checks for an existing cluster, shuts it down if
#'        found, and then repeatedly attempts to establish a new
#'        connection, retrying up to 10 times if necessary.
#'
#' @param cpu   integer. The number of CPU threads to use.
#'              -1 indicates all available threads. Default is -1.
#' @param ram   numeric. Minimum memory (in GB) for the cluster.
#'              If NULL, all available memory is used.
#' @param java  character. Path to the Java JDK. If provided, sets
#'              JAVA_HOME accordingly.
#' @param ip    character. The IP address for the H2O server.
#'              Default is "localhost".
#' @param port  integer. The port for the H2O server.
#'              Default is 54321.
#' @param restart  logical. if TRUE, the server is erased and restarted
#' @param shutdown logical. if TRUE, the server is closed
#' @param verbatim logical. If TRUE, prints detailed cluster info.
#'              Default is FALSE.
#' @param ignore_config logical. If TRUE, ignores any existing H2O
#'              configuration. Default is TRUE.
#' @param bind_to_localhost logical. If TRUE, restricts access to the
#'              cluster to the local machine. Default is FALSE.
#' @param ...   Additional arguments passed to h2o.init().
#'
#' @return An object representing the connection to the H2O
#'         cluster.
#'
#' @details The function sets JAVA_HOME if a Java path is provided.
#'         It checks for an existing cluster via h2o.clusterInfo().
#'         If found, the cluster is shut down and the function waits
#'         5 seconds. It then attempts to initialize a new cluster using
#'         h2o.init() with the specified settings. On failure, it retries
#'         every 3 seconds, up to 10 attempts. If all attempts fail, an
#'         error is thrown.
#'
#' @importFrom utils capture.output
#' @importFrom h2o h2o.clusterInfo h2o.shutdown
#' @examples
#' \dontrun{
#'   # Example 1: Initialize the H2O cluster with default settings.
#'   library(hmda)
#'   hmda.init()
#'
#'   # Example 2: Initialize with specific settings such as Java path.
#'   conn <- hmda.init(
#'       cpu = 4,
#'       ram = 8,
#'       java = "/path/to/java",     #e.g., "C:/Program Files/Java/jdk1.8.0_241"
#'       ip = "localhost",
#'       port = 54321,
#'       verbatim = TRUE
#'   )
#'
#'   # check the status of the h2o connection
#'   h2o::h2o.clusterInfo(conn) #you can use h2o functions to interact with the server
#' }
#'
#' @export
#' @author E. F. Haghish

hmda.init <- function(cpu = -1,
                      ram = NULL,
                      java = NULL,
                      ip = "localhost",
                      port = 54321,
                      verbatim = FALSE,
                      restart = TRUE,
                      shutdown = FALSE,
                      ignore_config=TRUE,
                      bind_to_localhost=FALSE,
                      ...) {

  # If JAVA is provided, add it to the environment
  # ============================================================
  if (!is.null(java)) {
    Sys.setenv(JAVA_HOME = java)
  }

  # Check the h2o cloud status
  # ============================================================
  # Attempt to get current connection
  connection_exists <- TRUE
  tryCatch(
    expr = {
      if (verbatim) suppressWarnings(h2o.clusterInfo())
      else capture.output(suppressWarnings(h2o.clusterInfo()) , file = nullfile())
    },
    error = function(e) {
      connection_exists <<- FALSE
    }
  )

  # Restart the server
  # ============================================================
  if (connection_exists) {
    if (restart | shutdown) {
      try(h2o.shutdown(FALSE), silent = TRUE)
      Sys.sleep(5)
    }
  }

  if (!shutdown) {
    # Run H2O on the statistics server
    # ============================================================
    keepTrying <- TRUE
    connection <- NULL
    test       <- 1
    while (keepTrying & is.null(connection)) {
      # h2o.init(jvm_custom_args = c("-help"))
      # h2o.init(jvm_custom_args = c("-session_timeout=100"))
      # bind_to_localhost = FALSE
      # h2o.init(jvm_custom_args=c("-Dsys.ai.h2o.heartbeat.benchmark.enabled=true"))
      tryCatch(suppressWarnings(connection <- h2o::h2o.init(nthreads = cpu,
                                                            startH2O = TRUE,
                                                            name = "HMDA",
                                                            min_mem_size = ram,
                                                            ip = ip, port = port,
                                                            #max_mem_size = max_mem_size,
                                                            ignore_config = ignore_config,
                                                            insecure = TRUE,
                                                            https = FALSE,
                                                            #log_level = if (debug) "DEBUG" else "FATA",
                                                            bind_to_localhost = bind_to_localhost,
                                                            ...)),
               error = function(cond) {
                 message("connection to JAVA server failed...\n");
                 return()})
      if (!is.null(connection)) {
        keepTrying <- FALSE
      }
      else {
        test <- test + 1
        message("The Java server could not be initiated. It will retry in 3 seconds...")
        Sys.sleep(3)
      }

      if (test > 10) stop("The attempt to start the H2O server was unsuccessful \ndue to an issue within your system...\n")
    }

    return(connection)
  }
}

