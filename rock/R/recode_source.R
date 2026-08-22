#' #' Change a code into another code
#' #'
#' #' These functions change a code into another code, and make it easy to
#' #' justify that decision.
#' #'
#' #' @param input A file with a source (for `recode_source`), or a directory
#' #' with sources (for `recode_sources`), or an object with one source or
#' #' multiple sources as produced by one of the `loading_sources` functions.
#' #' @param codes A named list of character vectors, where each character vector
#' #' lists the codes to replace, and the name of that character vector is the
#' #' code to replace them with.
#' #' @param output If specified, the coded source will be written here.
#' #' @param justification The justification for this action.
#' #' @param justificationFile If specified, the justification is appended to
#' #' this file.
#' #' @param preventOverwriting Whether to prevent overwriting existing files.
#' #' @param encoding The encoding to use.
#' #' @param silent Whether to be chatty or quiet.
#' #'
#' #' @return Invisibly, the recoded source(s) or source(s) object.
#' #' @rdname recoding_sources
#' #' @examples ### Get path to example source
#' #' examplePath <-
#' #'   system.file("extdata", package="rock");
#' #'
#' #' ### Get a path to one example file
#' #' exampleFile <-
#' #'   file.path(examplePath, "example-1.rock");
#' #'
#' #' ### Parse single example source
#' #' loadedExample <- rock::load_source(exampleFile);
#' #'
#' #' @export
#' recode_source <- function(input,
#'                           codes,
#'                           output = NULL,
#'                           justification = NULL,
#'                           justificationFile = NULL,
#'                           preventOverwriting = rock::opts$get('preventOverwriting'),
#'                           encoding = rock::opts$get('encoding'),
#'                           silent = rock::opts$get('silent')) {
#'
#'   ### Read input, if it's a file
#'   if ((length(input) == 1) && (file.exists(input))) {
#'     input <- readLines(input,
#'                        encoding=encoding);
#'     input <- cleaned_source_to_utterance_vector(input);
#'   } else if (class(input) == "character") {
#'     input <- cleaned_source_to_utterance_vector(input);
#'   } else if (!("rock_source" %in% class(input))) {
#'     stop("With the `input` argument you must pass either the ",
#'          "path to a file with a source, a character vector ",
#'          "containing the source, or a ROCK source ",
#'          "as loaded with load_source or load_sources.\n");
#'   }
#'
#'   codeDelimiters <- rock::opts$get(codeDelimiters);
#'
#'   if (exists("indices") && !is.null(indices) && (is.logical(indices) && (length(indices) == length(input)))) {
#'     ### The indices are already set as a (valid) logical vector
#'     codeToAdd <- paste0(codeDelimiters[1],
#'                         codes[1],
#'                         codeDelimiters[2]);
#'     if (!silent) {
#'       cat0("The 'indices' argument is a logical vector indicating to which utterances to apply code '",
#'                 codes[1], "' (specifically, the utterances on lines ",
#'                 vecTxt(which(indices)), ").\n");
#'     }
#'   } else if (exists("indices") && !is.null(indices) && (is.numeric(indices)) && ((min(indices) > 1) && (max(indices) <= length(input)))) {
#'     ### The indices are already set as a (valid) numeric vector
#'     codeToAdd <- paste0(codeDelimiters[1],
#'                         codes[1],
#'                         codeDelimiters[2]);
#'     if (!silent) {
#'       cat0("The first argument is a numeric vector indicating to which utterances to apply code '",
#'                 codes[1], "' (specifically, the utterances on lines ",
#'                 vecTxt(indices), ").\n");
#'     }
#'   } else {
#'
#'     ### Create regex to match codes, where we escape the character
#'     ### class specification codes (square brackets)
#'     regexMatchingCode <-
#'       paste0("^",
#'              escapeRegexCharacterClass(codeDelimiters[1]),
#'              "(.*)",
#'              escapeRegexCharacterClass(codeDelimiters[2]),
#'              "$");
#'
#'     if (!silent) {
#'       if (length(codes) > 1) {
#'         cat0("Multiple codes to check have been specified.");
#'       }
#'       cat0("Starting processing of ",
#'            vecTxtQ(names(codes)),
#'            " against the regular expression ",
#'            vecTxtQ(regexMatchingCode), ".\n");
#'     }
#'
#'     for (i in seq_along(codes)) {
#'
#'       ### Generate code to add to utterances matching this code
#'       codeToAdd <-
#'         paste0(codeDelimiters[1],
#'                codes[i],
#'                codeDelimiters[2]);
#'
#'       if (any(grepl(regexMatchingCode,
#'                     names(codes)[i],
#'                     perl=TRUE))) {
#'
#'         ### The name of this case is a code or an utterance ID;
#'         ### Extract it from between the specified delimiters
#'         escapedCodeToFind <-
#'           escapeRegexCharacterClass(names(codes)[i]);
#'
#'         ### Get indices matching it
#'         indices <-
#'           grep(escapedCodeToFind,
#'                input,
#'                perl=TRUE);
#'
#'         if (!silent) {
#'           cat0("Looking for code or utterance id '",
#'                     escapedCodeToFind, "'; found in utterances with line numbers ",
#'                     vecTxt(indices), ".\n");
#'         }
#'
#'       } else if (grepl("^[0-9]+$", names(codes)[i])) {
#'         ### It's a number, so a line number; check whether
#'         ### it's not too high and then add code
#'         if (as.numeric(names(codes)[i]) <= length(input)) {
#'           ### (Only one index, really)
#'           indices <- as.numeric(names(codes)[i]);
#'         } else {
#'           stop("You specified a line number (",
#'                names(codes)[i], ") that's higher than ",
#'                "the number of lines in the source (",
#'                length(input),
#'                ")!\n");
#'         }
#'       } else {
#'         ### The name of this case is a regular expression;
#'         ### get indices matching it
#'         indices <- grep(names(codes)[i],
#'                         input,
#'                         perl=TRUE);
#'         if (!silent) {
#'           cat0("Looking for a text match with regular expression '",
#'                names(codes)[i], "'; found in utterances with line numbers ",
#'                vecTxt(indices), ".\n");
#'         }
#'       }
#'
#'       ### Append code
#'       input[indices] <-
#'         paste(input[indices],
#'               codeToAdd,
#'               sep=" ");
#'
#'       if (!silent) {
#'         cat0("Appending code '", codeToAdd, "' to utterances at those line numbers.\n");
#'       }
#'     }
#'
#'     if (is.null(output)) {
#'       class(input) <- c("rock_source", "character");
#'       return(input);
#'     } else {
#'
#'       if (!dir.exists(dirname(output))) {
#'         stop("The directory specified where the output file '",
#'              basename(output), "' is supposed to be written ('",
#'              dirname(output),
#'              "') does not exist.");
#'       }
#'       if (file.exists(output) && preventOverwriting) {
#'         if (!silent) {
#'           message("File '",
#'                   output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
#'                   "write the source with added codes to disk.");
#'         }
#'       } else {
#'         con <- file(description=output,
#'                     open="w",
#'                     encoding=encoding);
#'         writeLines(text=input,
#'                    con=con);
#'         close(con);
#'       }
#'       if (!silent) {
#'         message("I just wrote a source with added codes to file '",
#'                 output,
#'                 "'.");
#'       }
#'
#'       class(input) <- c("rock_source", "character");
#'       return(invisible(input));
#'
#'     }
#'
#'   }
#'
#' }
