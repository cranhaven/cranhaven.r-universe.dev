#' Export a ROCK project to a single ROCKproject file
#'
#' @param output The file to write to; should have the extension `.ROCKproject`
#' @param path The path with the ROCK project
#' @param config Optionally, a named list with configuration options to
#' override. For supported options, see \code{vignette("ROCKproject-format", package = "rock");}
#' @param includeRegex A regular expression used to select files to include in
#' the project file
#' @param excludeRegex A regular expression used to omit files from the
#' project file; selection takes place after the selection by `includeRegex`
#' @param createDirs Whether to, if any directories in the `output` path does
#' not exist, create these
#' @param preventOverwriting If the output file already exists, whether to
#' prevent it from being overwritten (`TRUE`) or not (`FALSE`).
#' @param forceBaseZip Whether to force using the `zip()` function included in
#' R even if the `zip` package is installed.
#' @param silent Whether to be chatty or silent
#'
#' @returns Invisibly, `output`.
#' @export
#'
#' @examples ### Get path to example project
#' examplePath <-
#'   system.file(
#'     "ROCKprojects",
#'     "exportable-ROCKproject-1",
#'     package="rock"
#'   );
#'
#' ### Get a temporary filename to write to
#' projectFilename <-
#'   tempfile(
#'     fileext = ".ROCKproject"
#'   );
#'
#' ### Export it
#' rock::export_ROCKproject(
#'   path = examplePath,
#'   output = projectFilename,
#'   silent = FALSE
#' );
export_ROCKproject <- function(output,
                               path = ".",
                               config = NULL,
                               includeRegex = NULL,
                               excludeRegex = NULL,
                               createDirs = FALSE,
                               preventOverwriting = TRUE,
                               forceBaseZip = FALSE,
                               silent = rock::opts$get("silent")) {

  outputDir <- dirname(output);

  if (!dir.exists(dirname(output))) {
    if (createDirs) {
      dir.create(outputDir, recursive = TRUE);
    } else {
      stop("The directory where you wanted to create the ROCKproject file, `",
           outputDir,
           "`, does not exist!");
    }
  }

  if (!dir.exists(path)) {

    stop("The directory you specified to export the ROCKproject from, `", path,
         "`, does not exist!");

  }

  oldWorkingDirectory <- getwd();
  on.exit(setwd(oldWorkingDirectory));
  setwd(path);

  if (!file.exists(file.path(path, "_ROCKproject.yml"))) {

    rock::make_ROCKproject_config(
      path = path
    );

    warning("No file with project settings was found yet! This file ",
            "should normally be called `_ROCKproject.yml` and be stored ",
            "in the path you specified (", path, "). I'm now creating it, ",
            "using the default settings.\n\n",
            "You can edit this file and then export the project again (repeating ",
            "this same command).");
  }

  fullFileList <-
    list.files(
      path,
      recursive = TRUE,
      full.names = FALSE
    );

  msg(
    "The full list of files is\n\n",
    paste0(paste0("  - ", fullFileList, "\n")),
    "\n",
    silent = silent
  );

  if (!is.null(includeRegex)) {

    includedFiles <-
      grep(
        includeRegex,
        fullFileList,
        value = TRUE
      );

  } else {

    includedFiles <- fullFileList;

  }

  if (!is.null(excludeRegex)) {

    excludedFiles <-
      grep(
        excludeRegex,
        includedFiles,
        value = TRUE
      );

    selectedFiles <-
      setdiff(
        includedFiles,
        excludedFiles
      );

  } else {

    excludedFiles <- NULL;
    selectedFiles <- includedFiles;

  }

  if (file.exists(output)) {
    if (preventOverwriting) {
      stop("The file you specified to write to, '", output,
           "', already exists, and ",
           "`preventOverwriting` is set to TRUE, so I'm aborting.");
    } else {

      unlink(output);

      msg("The file you specified to write to, '", output,
          "', already existed, and `preventOverwriting` is set to FALSE, ",
          "so I deleted it.\n\n",
          silent = silent);

    }
  }

  if (requireNamespace("zip", quietly = TRUE) && (!forceBaseZip)) {
    zip::zip(
      zipfile = output,
      files = selectedFiles
    );
  } else {
    tryCatch(
      {
        zipResults <-
          utils::capture.output(
            utils::zip(
              zipfile = output,
              files = selectedFiles
            )
          )
      },
      error = function(e) {
        stop("ROCK project files are ZIP archives. I tried to ZIP the files ",
             "you specified using R's native `zip()` function, but it ",
             "returned this error:\n\n  ", e$message, "\n\nYou can install ",
             "the R package {zip}, which may resolve the error. To do that, ",
             "run:\n\n  install.packages('zip');\n\n");
      }
    );
  }

  return(invisible(output));

}
