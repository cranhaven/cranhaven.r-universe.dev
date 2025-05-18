#' @title Get JavaSTICS command line strings
#'
#' @description From a JavaSTICS path, constructing command lines for
#' files generation from XML and/or running simulation through
#' a system2 command execution
#'
#' @param javastics JavaSTICS installation root folder
#' @param java_cmd Name or path of the java virtual machine executable
#' (optional)
#' @param type keywords for filtering in the returned list what kind of command
#' to be generated (default both: "generate" and "run")
#' @param workspace A JavaSTICS workspace path (optional)
#' @param verbose Logical value (optional), TRUE to display run information,
#' FALSE otherwise (default)
#'
#' @details According to a JavaSTICS version (i.e. javastics path content) and
#' the OS type, the function builds command line strings used from system2
#' calls. Commands may be filtered using type argument.
#'
#' @return A named list with fields : command (java command/path), cmd_generate
#' and/or cmd_run that are to be used as args in the system2 function call.
#'
#' @examples
#' \dontrun{
#' get_javastics_cmd(
#'   javastics = "/path/to/JavaSTICS/folder", ,
#'   workspace = "path/to/workspace"
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_javastics_cmd <- function(javastics,
                              java_cmd = "java",
                              type = c("generate", "run"),
                              workspace = NULL,
                              verbose = TRUE) {

  # detecting JavaSTICS command exe name from javastics path
  javastics_cmd <- file.path(javastics, "JavaSticsCmd.exe")
  cmd <- check_javastics_cmd(
    javastics_cmd = javastics_cmd,
    java_cmd = java_cmd,
    verbose = TRUE
  )
  javastics_cmd <- cmd$javastics_cmd
  verbose_cmd <- cmd$verbose_cmd

  # Base command string, without workspace
  if (!is_windows()) {
    command <- java_cmd
    generate <- paste0("-jar ", javastics_cmd, verbose_cmd, " --generate-txt")
    run <- paste0("-jar ", javastics_cmd, verbose_cmd, " --run")
  } else {
    command <- javastics_cmd
    generate <- paste0(verbose_cmd, " --generate-txt")
    run <- paste0(verbose_cmd, " --run")
  }

  # adding workspace name if provided
  if (!is.null(workspace)) {
    workspace <- paste0('"', workspace, '"')
    generate <- paste(generate, workspace)
    run <- paste(run, workspace)
  }

  # All strings in a named list
  # content returned conditionally to type content
  list(
    command = command,
    cmd_generate = generate,
    cmd_run = run
  )[c("command", paste("cmd", type, sep = "_"))]
}

#' @title Checking if the java virtual machine is compatible with the given
#' JavaSTICS command executable, and command line options
#'
#' @description For Unix like systems, the system java virtual machine is
#' detected and consistency for JavaSTICS executable used is checked.
#' Command line verbosity option is detected for JavaSTICS command line for all
#' systems.
#'
#' @param javastics_cmd path to JavaSTICS command line executable
#' @param java_cmd Name or path of the java virtual machine executable
#'
#' @details Errors are raised if the JavaSTICS executable cannot be run with the
#' given java virtual machine for unix like systems.
#'
#' @return A named list with the JavaSTICS command line path,
#' the java path and the verbose string option (according to JavaSTICS command
#'  line version, and the "verbose" argument value)
#'
#' @examples
#' \dontrun{
#' check_javastics_cmd(javastics_cmd = "/path/to/JavaSticsCmd.exe")
#' check_javastics_cmd(
#'   javastics_cmd = "/path/to/JavaSticsCmd.exe",
#'   java_cmd = "/path/to/java"
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
check_javastics_cmd <- function(javastics_cmd = "JavaSticsCmd.exe",
                                java_cmd = "java",
                                verbose = TRUE) {
  if (is_windows()) {
    help_test <- system2(javastics_cmd,
                         c("--help"),
                         stdout = TRUE, stderr = TRUE
    )
  } else {
    help_test <- system2(java_cmd,
                         c("-jar", javastics_cmd, "--help"),
                         stdout = TRUE, stderr = TRUE
    )
  }

  # detecting invalid option for given JavaSTICS command line
  help_status <- !length(
    grep(pattern = "Invalid option entered",
         help_test)) > 0

  if (!is_windows()) {
    ver <- get_java_version(java_cmd = java_cmd)

    status <- attr(help_test, "status")

    if (!is.null(status) && status > 0) {
      stop("The given or default java version ",
           ver,
           " is not usable with that version of",
           " JavaSTICS, use at least java version 11 !")
    }

    if (is.null(status) &&
        !help_status &&
        ver > 1.8) {
      stop("The given or default java version ",
           ver,
           " is not usable with that version of",
           " JavaSTICS, use at most java version 1.8 !")
    }
  }

  verbose_cmd <- ""
  if (help_status && verbose) verbose_cmd <- " --verbose"

  list(
    javastics_cmd = javastics_cmd,
    java_cmd = java_cmd,
    verbose_cmd = verbose_cmd
  )
}



#' @title Getting the java virtual machine version
#'
#' @description For Unix like systems, the system java virtual machine needs to
#' be detected for compatibility checking
#'
#' @param java_cmd Name or path of the java virtual machine executable
#'
#' @return A numerical version, with a string version as "version" attribute
#'
#' @examples
#' \dontrun{
#' get_java_version(java_cmd = "java")
#' get_java_version(java_cmd = "/path/to/java")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
get_java_version <- function(java_cmd = "java") {

  # java_cmd must contain a java executable path, if not known in the system
  # environment
  if (!is_windows()) {
    java_path <- system2("which", java_cmd, stdout = TRUE, stderr = TRUE)
  } else {
    # for Windows: splitting command if java_cmd is a full path
    if (!basename(java_cmd) == java_cmd)
      java_cmd <- c("/R", dirname(java_cmd), basename(java_cmd))

    java_path <- system2("where", java_cmd, stdout = TRUE, stderr = TRUE)
  }

  if (!length(java_path)) {
    stop(paste(java_cmd, "not found !"))
  }

  # Getting version string
  java_version <- system2(java_cmd, "-version", stdout = TRUE, stderr = TRUE)

  version_str <- gsub("[\"a-z\ ]", x = java_version[1], replacement = "")

  version_num <- as.numeric(substr(1, 3, x = version_str))

  attr(x = version_num, which = "version") <- version_str

  return(version_num)
}
