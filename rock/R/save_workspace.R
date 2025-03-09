#' Save your justifications to a file
#'
#' When conducting analyses, you make many choices that ideally, you
#' document and justify. This function saves stored justifications to a
#' file.
#'
#' @param file If specified, the file to export the justification to.
#' @param encoding The encoding to use when writing the file.
#' @param append Whether to append to the file, or replace its contents.
#' @param preventOverwriting Whether to prevent overwriting an existing file.
#' @param silent Whether to be silent or chatty.
#'
#' @return The result of a call to [justifier::export_justification()].
#' @export
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Load example source
#' loadedExample <- rock::load_source(exampleFile);
#'
#' ### Split a code into two codes, showing progress (the backticks are
#' ### used to be able to specify a name that starts with an underscore)
#' recoded_source <-
#'   rock::recode_split(
#'     loadedExample,
#'     codes="childCode1",
#'     splitToCodes = list(
#'       `_and_` = " and ",
#'       `_book_` = "book",
#'       `_else_` = TRUE
#'     ),
#'     silent=FALSE,
#'     justification = "Because this seems like a good idea"
#'   );
#'
#' ### Save this workspace to a file
#' temporaryFilename <- tempfile();
#' rock::save_workspace(file = temporaryFilename);
save_workspace <- function(file = rock::opts$get('justificationFile'),
                           encoding = rock::opts$get("encoding"),
                           append = FALSE,
                           preventOverwriting = rock::opts$get('preventOverwriting'),
                           silent = rock::opts$get("silent")) {

  if (requireNamespace("justifier", quietly = TRUE)) {

    justifier::save_workspace(
      file = file,
      encoding = encoding,
      preventOverwriting = preventOverwriting,
      append = append,
      silent = silent
    );

  } else {

    stop("You specified that you wanted to save your `justifier` workspace, ",
         "but the `justifier` package ",
         "isn't installed. You can install the most recent version ",
         "using the `remotes` package with:\n\n",
         "remotes::install_gitlab('r-packages/justifier');\n\n",
         "You can also install the version on CRAN with:\n\n",
         "install.packages('rock');\n");

  }

}
