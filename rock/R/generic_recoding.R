#' Generic underlying recoding function
#'
#' This function contains the general set of actions that are always used
#' when recoding a source (e.g. check the input, document the
#' justification, etc). Users should normally never call this function.
#'
#' @param input One of 1) a character string specifying the path to a file
#' with a source; 2) an object with a loaded source as produced by a call
#' to [rock::load_source()]; 3) a character string specifying the path to a
#' directory containing one or more sources; 4) or an object with a list of
#' loaded sources as produced by a call to [rock::load_sources()].
#' @param codes The codes to process
#' @param output If specified, the coded source will be written here.
#' @param outputPrefix,outputSuffix The prefix and suffix to add to the
#' filenames when writing the processed files to disk, in case multiple
#' sources are passed as input.
#' @param filenameRegex Only process files matching this regular expression.
#' @param decisionLabel A description of the (recoding) decision that was taken.
#' @param func The function to apply.
#' @param filter Optionally, a filter to apply to specify a subset of the
#' source(s) to process (see [get_source_filter()]).
#' @param justification The justification for this action.
#' @param justificationFile If specified, the justification is appended to
#' this file. If not, it is saved to the `justifier::workspace()`. This can
#' then be saved or displayed at the end of the R Markdown file or R script
#' using `justifier::save_workspace()`.
#' @param preventOverwriting Whether to prevent overwriting existing files
#' when writing the files to `output`.
#' @param encoding The encoding to use.
#' @param silent Whether to be chatty or quiet.
#' @param ... Other arguments to pass to `fnc`.
# #' @inheritParams loading_sources
#' @rdname generic_recoding
#'
#' @return Invisibly, the recoded source(s) or source(s) object.
#' @export
generic_recoding <- function(input,
                             codes,
                             func,
                             filenameRegex = ".*",
                             filter = TRUE,
                             output = NULL,
                             outputPrefix = "",
                             outputSuffix = "_recoded",
                             decisionLabel = NULL,
                             justification = NULL,
                             justificationFile = NULL,
                             preventOverwriting = rock::opts$get('preventOverwriting'),
                             encoding = rock::opts$get('encoding'),
                             silent = rock::opts$get('silent'),
                             ...) {

  originalFilter <- filter;

  if ("rock_loaded_sources_list" %in% class(input)) {

    if (!is.null(output)) {
      if (!dir.exists(output)) {
        stop("You passed a list of loaded sources from rock::load_sources() ",
             "as input. The value of the `output` argument then has to be ",
             "either NULL or an existing directory. However, you passed '",
             output,
             "'.");
      }
    }

    msg("A list of loaded sources was passed; processing them ",
        "sequentially.\n\n",
        silent=silent);
    ### Repeatedly call ourselves for each source.
    ###
    ### ----- NOTE ----- We may want to only document one justification!
    ###
    objNames <- names(input);  ### Store the element names and the object
    objClass <- class(input);  ### class (don't trust lapply to preserve names)
    res <-
      lapply(
        objNames,
        function(i) {
          if (!silent) {
            cat0("Processing source '", i, "'.\n");
          }
          if (is.null(output)) {
            currentOutputFilename <- NULL;
          } else {
            currentOutputFilename <-
              composeOutputFilename(
                path=output,
                filename=i,
                outputPrefix=outputPrefix,
                outputSuffix=outputSuffix
              );
          }
          return(
            generic_recoding(
              input = input[[i]],
              codes = codes,
              func = func,
              filter = filter,
              output = currentOutputFilename,
              filenameRegex = filenameRegex,
              outputPrefix = outputPrefix,
              outputSuffix = outputSuffix,
              decisionLabel = decisionLabel,
              justification = justification,
              justificationFile = justificationFile,
              preventOverwriting = preventOverwriting,
              encoding = encoding,
              silent = silent,
              ...
            )
          );
        }
      );
    names(res) <- objNames;
    class(res) <- objClass;
    return(res);
  } else if ((length(input) == 1) && dir.exists(input)) {
    ### A directoryname, read them all and then call ourselves and return the
    ### result

    if (!is.null(output)) {
      if (!dir.exists(output)) {
        stop("You passed the path to a directory which I will pass to ",
             "rock::load_sources(). The value of the `output` argument then ",
             "has to be either NULL or an existing directory. ",
             "However, you passed '", output, "'.");
      }
    }

    msg("The path to a directory was passed; loading all sources ",
        "matching regular expression '", filenameRegex, "'.\n\n",
        silent=silent);

    sourceList <- load_sources(
      input,
      filenameRegex=filenameRegex,
      silent=silent,
      encoding=encoding
    );

    if (!silent) {
      cat0("\n");
    }

    return(
      generic_recoding(
        input = sourceList,
        codes = codes,
        func = func,
        filter = filter,
        output = output,
        filenameRegex = filenameRegex,
        outputPrefix = outputPrefix,
        outputSuffix = outputSuffix,
        decisionLabel = decisionLabel,
        justification = justification,
        justificationFile = justificationFile,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent,
        ...
      )
    );
  } else {

    ### Check input
    if (!is.character(input)) {
      stop(
        "As 'input', you must pass ",
        "one of 1) a character string specifying the path to a file with a ",
        "source; 2) an object with a loaded source as produced by a call to ",
        "`rock::load_source()`; 3) a character string specifying the path to ",
        "a directory containing one or more sources; 4) or an object with a ",
        "list of loaded sources as produced by a call to ",
        "`rock::load_sources()`."
      );
    }
    ### Load input; note that `load_source` checks whether the source was
    ### already loaded and if so, just return it without doing anything
    input <- load_source(input, silent=silent, encoding=encoding);

  }

  ### Look for YAML delimiters so the YAML can be preserved
  yamlDelimiters <- yaml_delimiter_indices(input);

  if (length(yamlDelimiters) > 1) {
    delimiterMatches <-
      match_consecutive_delimiters(yamlDelimiters);

    ### Store YAML fragments so we can restore them later
    yamlFragments <-
      lapply(
        seq_along(delimiterMatches),
        function(i) {
          return(
            input[
              delimiterMatches[[i]][1]:delimiterMatches[[i]][2]
            ]
          );
        }
      );

    ### Empty the lines with the YAML fragments so they don't show up in
    ### the logs and confuse users
    for (i in seq_along(delimiterMatches)) {
      input[delimiterMatches[[i]][1]:delimiterMatches[[i]][2]] <- "";
    }
  }

  ### Process the filter
  if (!is_source_filter(filter)) {
    filter <-
      get_source_filter(
        input,
        filter
      );
  }

  ### Get the function to call
  if (is.function(func)) {
    funcName <- deparse(substitute(func));
  } else if (func %in% getNamespaceExports('rock')) {
    funcName <- func;
    func <- getExportedValue("rock", funcName);
  } else if(grepl("^(\\w+)\\:\\:(\\w+)$", "\\1", func)) {
    pkg <- gsub("^(\\w+)\\:\\:(\\w+)$", "\\1", func);
    funcName <- gsub("^(\\w+)\\:\\:(\\w+)$", "\\2", func);
    func <- getExportedValue(pkg, funcName);
  } else if (exists(func)) {
    funcName <- func;
    func <- get(funcName);
  } else {
    stop("Function `", func, "` is not an exported function from the `rock` ",
         "package, nor can I find it anywhere else!");
  }

  ### Call the function
  input <-
    func(input = input,
         codes = codes,
         filter = filter,
         silent = silent,
         ...);

  ### Restore YAML fragments
  if (length(yamlDelimiters) > 1) {
    for (i in seq_along(delimiterMatches)) {
      input[delimiterMatches[[i]][1]:delimiterMatches[[i]][2]] <-
        yamlFragments[[i]];
    }
  }

  ### Document the justification

  if (!is.null(justification)) {

    if (requireNamespace("justifier", quietly = TRUE)) {

      autoLabel <-
        paste0(
          "Decided to apply `", funcName, "` to source `",
          as.character(substitute(source)),
          "` for codes ", vecTxt(codes),
          ", with filter ", originalFilter, "."
        );

      if (is.null(decisionLabel)) {
        decisionLabel <-
          autoLabel;
      }

      msg("Saved decision and justification '",
          justification, "' using the {justifier} package.\n",
          silent = silent);

      decisionObject <-
        justifier::log_decision(
          label = decisionLabel,
          justification = justification,
          tag = c("rock", "recoding", funcName),
          rock_codes = codes,
          autoLabel = autoLabel
        );

      if (!is.null(justificationFile)) {

        justifier::export_justification(
          decisionObject,
          file = justificationFile,
          append = TRUE
        );

      }

    } else {

      warning("You specified a justification, but the `justifier` package ",
              "isn't installed. You can install the most recent version ",
              "using the `remotes` package with:\n\n",
              "remotes::install_gitlab('r-packages/justifier');\n\n",
              "You can also install the version on CRAN with:\n\n",
              "install.packages('rock');\n");

    }

  }

  ### Return, or save and return, the result.
  class(input) <- c("rock_source", "character");

  if (is.null(output)) {
    return(input);
  } else {

    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    if (file.exists(output) && preventOverwriting) {
      msg(
        "File '",
        output,
        "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
        "write the recoded source to disk.",
        silent=silent);
    } else {

      # con <- file(description=output,
      #             open="w",
      #             encoding=encoding);
      # writeLines(text=input,
      #            con=con);
      # close(con);
        writingResult <-
          writeTxtFile(
            x = input,
            output = output,
            preventOverwriting = preventOverwriting,
            encoding = encoding,
            silent = silent
          );

        if (writingResult) {
          msg("I just wrote a recoded source to file '",
              output,
              "'.",
              silent = silent);
        } else {
          warning("Could not write output file to `",
                  output, "`.");
        }

    }
    # msg(
    #   "I just wrote a recoded source to file '",
    #   output,
    #   "'.",
    #   silent=silent
    # );
  }

  return(invisible(input));

}
