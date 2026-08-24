#' Quietly update a package from a remote repository
#'
#' Simple wrapper for `remotes` functions that fail gracefully (well, don't
#' fail at all, just don't do what they're supposed to do) when there's no
#' internet connection).
#'
#' @param x The repository name (e.g. "`r-packages/ufs`")
#' @param func The `remotes` function to use
#' @param unloadNamespace Whether to first unload the relevant namespace
#' @param dependencies,upgrade Whether to install dependencies or upgrade
#' @param quiet Whether to suppress messages and warnings
#' @param errorInvisible Whether to suppress errors
#' @param ... Additional arguments are passed on to the `remotes` function
#'
#' @return The result of the call to the `remotes` function
#' @rdname quietRemotesInstall
#' @export
quietRemotesInstall <- function(x,
                                func,
                                unloadNamespace = TRUE,
                                dependencies = FALSE,
                                upgrade = FALSE,
                                quiet = TRUE,
                                errorInvisible = TRUE,
                                ...) {

  if (!requireNamespace("remotes", quietly = TRUE)) {
    message("This is a wrapper for `remotes` functions; ",
            "you need to have `remotes` installed to use it.");
    return(invisible(FALSE));
  }

  if (unloadNamespace) {
    unloadNamespace(gsub(".*/", "", x));
  }

  if (quiet) {
    silencefunc <- function(x) {
      invisible(
        suppressMessages(
          suppressWarnings(
            x
          )
        )
      );
    }
  } else {
    silencefunc <- function(x) {
      x
    };
  }

  remotesFunction <- utils::getFromNamespace(x = func,
                                             ns = "remotes");

  if (errorInvisible) {
    silencefunc(tryCatch(
      remotesFunction(x,
                      dependencies=dependencies,
                      quiet=quiet,
                      upgrade=upgrade,
                      ...),
      error=invisible));
  } else {
    silencefunc(
      remotesFunction(x,
                      dependencies=dependencies,
                      quiet=quiet,
                      upgrade=upgrade,
                      ...)
      );
  }


}

#' @rdname quietRemotesInstall
#' @export
quietGitLabUpdate <- function(x,
                              unloadNamespace = TRUE,
                              dependencies = FALSE,
                              upgrade = FALSE,
                              quiet = TRUE,
                              errorInvisible = TRUE,
                              ...) {

  if (!requireNamespace("remotes", quietly = TRUE)) {
    message("This is a wrapper for `remotes` functions; ",
            "you need to have `remotes` installed to use it.");
    return(invisible(FALSE));
  }

  quietRemotesInstall(x,
                      func = "install_gitlab",
                      unloadNamespace = unloadNamespace,
                      dependencies=dependencies,
                      quiet=quiet,
                      upgrade=upgrade,
                      errorInvisible=errorInvisible,
                      ...);
}
