#' Export codes to a plain text file
#'
#' These function can be used to convert one or more parsed sources to HTML,
#' or to convert all sources to tabbed sections in Markdown.
#'
#' @param input An object of class `rock_parsedSource` (as resulting from a call
#' to `parse_source`) or of class `rock_parsedSources` (as resulting from a call
#' to `parse_sources`.
#' @param codeTree Codes from which code tree to export the codes. Valid options
#' are `fullyMergedCodeTrees`, `extendedDeductiveCodeTrees`, `deductiveCodeTrees`,
#' and `inductiveCodeTrees`.
#' @param codingScheme With the ROCK, it's possible to use multiple coding scheme's
#' in parallel. The ROCK default is called `codes` (using the double square brackets
#' as code delimiters), but other delimiters can be used as well, and give a different
#' name. Use `codingScheme` to specify which code tree you want to export, if you
#' have multiple.
#' @param regex An optional regular expression: only codes matching this regular
#' expression will be selected.
#' @param onlyChildrenOf A character vector of one or more regular expressions that
#' specify codes within which to search. For example, if the code tree contains codes
#' `parent1` and `parent2`, and each have a number of child codes, and `parent` is
#' passed as `onlyChildrenOf`, only the codes within `parent` are selected.
#' @param leavesOnly Whether to only write the leaves (i.e. codes that don't have
#' children) or all codes in the code tree.
#' @param includePath Whether to only return the code itself (e.g. `code`) or also
#' include the path to the root (e.g. `code1>code2>code`).
#' @param output THe filename to write to.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param encoding The encoding to use when writing the exported source(s).
#' @param silent Whether to suppress messages.
#'
#' @return A character vector.
#' @aliases export_codes_to_txt
#' @rdname exporting_codes
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Parse a selection of example sources in that directory
#' parsedExamples <-
#'   rock::parse_sources(
#'     examplePath,
#'     regex = "(test|example)(.txt|.rock)"
#'   );
#'
#' ### Show results of exporting the codes
#' rock::export_codes_to_txt(parsedExamples);
#'
#' ### Only show select a narrow set of codes
#' rock::export_codes_to_txt(
#'   parsedExamples,
#'   leavesOnly=TRUE,
#'   includePath=FALSE,
#'   onlyChildrenOf = "inductFather",
#'   regex="3|5"
#' );
#'
#' @export
export_codes_to_txt<- function(input,
                               output = NULL,
                               codeTree = "fullyMergedCodeTrees",
                               codingScheme = "codes",
                               regex = ".*",
                               onlyChildrenOf = NULL,
                               leavesOnly = TRUE,
                               includePath = TRUE,
                               preventOverwriting = rock::opts$get(preventOverwriting),
                               encoding = rock::opts$get(encoding),
                               silent=rock::opts$get(silent)) {

  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);

  if (!(codeTree %in% c("fullyMergedCodeTrees", "extendedDeductiveCodeTrees",
                        "deductiveCodeTrees", "inductiveCodeTrees"))) {
    stop("The `codeTreeType` must be one of `fullyMergedCodeTrees`, ",
         "`extendedDeductiveCodeTrees`, `deductiveCodeTrees`, and ",
         "`inductiveCodeTrees`.");
  }

  if (any(c("rock_parsedSource", "rock_parsedSources") %in% class(input))) {

    if (codeTree == "inductiveCodeTrees") {
      codeTree <- input[[codeTree]][[codingScheme]];
    } else {
      codeTree <- input[[codeTree]];
    }

    ### Select parent node(s)
    if (is.null(onlyChildrenOf)) {
      res <-
        gsub("/",
             rock::opts$get(inductiveCodingHierarchyMarker),
             codeTree$Get("pathString",
                          filterFun = ifelse(leavesOnly,
                                             data.tree::isLeaf,
                                             function(x) TRUE)));
    } else {
      res <- c();
      for (i in seq_along(onlyChildrenOf)) {
        subTree <-
          data.tree::FindNode(codeTree, onlyChildrenOf[i]);
        if (length(subTree) == 0) {
          stop("You specified parent code '", onlyChildrenOf[i],
               "' in argument `onlyChildrenOf`, but this code ",
               "does not exist in the code tree you passed!");
        } else {
          res <-
            c(res,
              gsub("/",
                   rock::opts$get(inductiveCodingHierarchyMarker),
                   subTree$Get("pathString",
                               filterFun = ifelse(leavesOnly,
                                                  data.tree::isLeaf,
                                                  function(x) TRUE))));
        }
      }
    }

    if (includePath) {
      res <- unname(res);
    } else {
      res <- names(res);
    }

    ### Remove the root, if need be
    res <- gsub(paste0("^", codeTree$root$name,
                       rock::opts$get(inductiveCodingHierarchyMarker)),
                "",
                res);
    res <-
      res[(res!=codeTree$root$name)];

    ### Apply regular expression
    res <- grep(regex,
                res,
                value=TRUE);

    if (is.null(output)) {
      return(res);
    } else if (!dir.exists(dirname(output))) {
      stop("The directory specified to save the output file to ('",
           dirname(output),
           "') does not exist!");
    } else {
      if (file.exists(output) && preventOverwriting) {
        if (!silent) {
          message("File '",
                  output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                  "write the codes to disk.");
        }
      } else {
        con <- file(description=output,
                  open="w",
                  encoding=encoding);
        writeLines(text=res,
                   con=con);
        close(con);
        if (!silent) {
          message("I just wrote codes to file '",
                  output,
                  "'. Note that this file may be overwritten if this ",
                  "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
                  "Therefore, make sure to copy it to ",
                  "another directory, or rename it, if this version should be preserved for some reason.");
        }
        invisible(res);
      }
    }
  } else {
    stop("As argument 'input', only provide an object with parsed sources, ",
         "such as results from a call to `rock::parse_source()` or ",
         "`rock::parse_sources()`. You provided an object of class(es) ",
         rock::vecTxtQ(class(input)), ".");
  }

  if (is.null(output)) {
    return(res);
  } else {
    return(invisible(res));
  }

}
