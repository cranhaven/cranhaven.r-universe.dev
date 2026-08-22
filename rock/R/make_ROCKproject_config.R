#' Make a ROCK project configuration file
#'
#' This function creates a ROCK project configuration file in the YAML
#' format. See the Details section for more information.
#'
#' For more information about the ROCK project configuration file format,
#' see the `ROCKproject-format` vignette. You can view this from R using:
#'
#' `vignette("ROCKproject-format", package="rock");`
#'
#' You can also visit it at <https://rock.opens.science/articles/ROCKproject-format.html>.
#'
#' @param project A named list with the project metadata.
#' @param codebook  A named list with the project's codebook.
#' @param sources A named list with the project's source import settings.
#' @param workflow A named list with the project's workflow settings.
#' @param extra A named list with any additional things you want to store in the
#' project's configuration file. One never knows whether being able to do that
#' comes in handy at some point.
#' @param path If not `NULL`, the ROCK project configuration will be written to
#' the file `_ROCKproject.yml` in this directory.
#'
#' @returns A list with the passed configuration in a list called
#' `$input`, and as YAML in a character vector called `$yaml`.
#' @export
#'
#' @examples ### To get the default configuration,
#' ### just pass no arguments:
#' defaultConfig <-
#'   rock::make_ROCKproject_config();
#'
#' ### You can then extract the object with settings
#' config <- defaultConfig$input;
#'
#' ### Edit some of them
#' config$project$title <- "Some new title";
#'
#' ### Call the function again with the new arguments
#' myConfig <-
#'   rock::make_ROCKproject_config(
#'     project = config$project,
#'     codebook = config$codebook,
#'     sources = config$sources,
#'     workflow = config$workflow,
#'     extra = config$extra
#'   );
#'
#' ### View the result
#' cat(myConfig$yaml);
#'
make_ROCKproject_config <- function(project = list(
                                      title = "Default title",
                                      authors = "Authors",
                                      authorIds = NULL,
                                      version = "1.0",
                                      ROCK_version = "1.0",
                                      ROCK_project_version = "1.0",
                                      date_created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                                      date_modified = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
                                    ),
                                    codebook = list(
                                      urcid = "",
                                      embedded = NULL,
                                      local = ""
                                    ),
                                    sources = list(
                                      extension =  ".rock",
                                      regex = NULL,
                                      dirsToIncludeRegex = "data/",
                                      recursive = TRUE,
                                      dirsToExcludeRegex = NULL,
                                      filesToIncludeRegex = NULL,
                                      filesToExcludeRegex = NULL
                                    ),
                                    workflow = list(
                                      pipeline = NULL,
                                      actions = NULL
                                    ),
                                    extra = NULL,
                                    path = NULL) {

  res <-
    list(
      input =
        list(
          project = project,
          codebook = codebook,
          sources = sources,
          workflow = workflow,
          extra = extra
        )
    );

  res$yaml =
    yaml::as.yaml(
      list(
        `_ROCKproject` = res$input
      )
    );

  if (is.null(path)) {
    return(res);
  } else {
    if (dir.exists(path)) {
      yaml::write_yaml(
        res$yaml,
        file.path(path, "_ROCKproject.yml")
      );
    } else {
      stop("The path you passed as `path` ('", path, "') does not exist!");
    }
  }

}
