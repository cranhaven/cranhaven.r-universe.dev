#' Import a ROCK project from a ROCKproject file
#'
#' @param input The path to the ROCK project file (typically with the extension `.ROCKproject`)
#' @param path The path where to store the ROCK project
#' @param createDirs Whether to, if the `path` does not exist, create it
#' @param preventOverwriting If the path already contains files, whether to
#' prevent them from being overwritten (`TRUE`) or not (`FALSE`).
#' @param forceBaseZip Whether to force using the `zip()` function included in
#' R even if the `zip` package is installed.
#' @param silent Whether to be chatty or silent
#'
#' @returns Invisibly, `output`.
#' @export
#'
#' @examples ### Get path to example project
#' exampleProjectFile <-
#'   system.file(
#'     "ROCKprojects",
#'     "example1.ROCKproject",
#'     package="rock"
#'   );
#'
#' ### Get a temporary directory to write to
#' temporaryDir <-
#'   tempdir();
#'
#' ### Import the project
#' rock::import_ROCKproject(
#'   input = exampleProjectFile,
#'   path = temporaryDir,
#'   silent = FALSE
#' );
import_ROCKproject <- function(input,
                               path = ".",
                               createDirs = FALSE,
                               preventOverwriting = TRUE,
                               forceBaseZip = FALSE,
                               silent = rock::opts$get(silent)) {

  if (!dir.exists(dirname(path))) {
    if (createDirs) {
      dir.create(path, recursive = TRUE);
    } else {
      stop("The directory where you wanted to open the ROCK project, `",
           path,
           "`, does not exist!");
    }
  }

  if (!file.exists(input)) {

    stop("The file you specified to import the ROCKproject from, `", input,
         "`, does not exist!");

  }

  oldWorkingDirectory <- getwd();
  on.exit(setwd(oldWorkingDirectory));
  setwd(path);

  if (requireNamespace("zip", quietly = TRUE) && (!forceBaseZip)) {
    zip::unzip(
      zipfile = input,
      overwrite = !preventOverwriting,
      exdir = path
    );
  } else {
    tryCatch(
      {
        zipResults <-
          utils::capture.output(
            utils::zip(
              zipfile = input,
              overwrite = !preventOverwriting,
              exdir = path
            )
          )
      },
      error = function(e) {
        stop("ROCK project files are ZIP archives. I tried to unZIP the file ",
             "you specified using R's native `zip()` function, but it ",
             "returned this error:\n\n  ", e$message, "\n\nYou can install ",
             "the R package {zip}, which may resolve the error. To do that, ",
             "run:\n\n  install.packages('zip');\n\n");
      }
    );
  }

  return(invisible(path));

}
