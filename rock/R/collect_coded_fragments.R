#' Create an overview of coded fragments
#'
#' Collect all coded utterances and optionally add some context
#' (utterances before and utterances after) to create an overview
#' of all coded fragments per code.
#'
#' By default, the output is optimized for inclusion in an R Markdown
#' document. To optimize output for the R console or a plain text
#' file, without any HTML codes, set `add_html_tags` to FALSE, and
#' potentially set `cleanUtterances` to only return the utterances,
#' without the codes.
#'
#' @param x The parsed source(s) as provided by `rock::parse_source`
#' or `rock::parse_sources`.
#' @param codes The regular expression that matches the codes to include,
#' or a character vector with codes or regular expressions for codes (which
#' will be prepended with "`^`" and appended with "`$`", and then
#' concatenated using "`|`" as a separator, to create a regular expression
#' matching all codes).
#' @param context How many utterances before and after the target
#' utterances to include in the fragments. If two values, the first is the
#' number of utterances before, and the second, the number of utterances
#' after the target utterances.
#' @param includeDescendents Whether to also collect the fragments coded with
#' descendent codes (i.e. child codes, 'grand child codes', etc; in other
#' words, whether to collect the fragments recursively).
#' @param attributes To only select coded utterances matching one or more
#' values for one or more attributes, pass a list where every element's
#' name is a valid (i.e. occurring) attribute name, and every element is a
#' character value with a regular expression specifying all values for that
#' attribute to select.
#' @param heading Optionally, a title to include in the output. The title
#' will be prefixed with `headingLevel` hashes (`#`), and the codes with
#' `headingLevel+1` hashes. If `NULL` (the default), a heading will be
#' generated that includes the collected codes if those are five or less.
#' If a character value is specified, that will be used. To omit a heading,
#' set to anything that is not `NULL` or a character vector (e.g. `FALSE`).
#' If no heading is used, the code prefix will be `headingLevel` hashes,
#' instead of `headingLevel+1` hashes.
#' @param headingLevel The number of hashes to insert before the headings.
#' @param add_html_tags Whether to add HTML tags to the result.
#' @param rawResult Whether to return the raw result, a list of the
#' fragments, or one character value in markdown format.
#' @param includeCSS Whether to include the ROCK CSS in the returned HTML.
#' @param includeBootstrap Whether to include the default bootstrap CSS.
#' @param output Here, a path and filename can be provided where the
#' result will be written. If provided, the result will be returned
#' invisibly.
#' @param outputViewer If showing output, where to show the output: in
#' the console (`outputViewer='console'`) or in the viewer
#' (`outputViewer='viewer'`), e.g. the RStudio viewer. You'll usually want
#' the latter when outputting HTML, and otherwise the former. Set to `FALSE`
#' to not output anything to the console or the viewer.
#' @param template The template to load; either the name of one
#' of the ROCK templates (currently, only 'default' is available), or
#' the path and filename of a CSS file.
#' @param omitEmptyCodes Whether to still show the title for codes that do not
#' occur or not.
#' @param preserveSpaces Whether to preserve spaces in the output (replacing
#' double spaces with "`&nbsp;&nbsp;`").
#' @param codeHeadingFormatting,codeHeadingFormatting_html A character value of the
#' form `%s *(path: %s)*` (the default) or `\n\n### %s\n\n*path:* ``%s``\n\n`.
#' The first `%s` is replaced by the code identifier; the second `%s` by the
#' corresponding path in the code tree; for markdown/console and html output,
#' respectively.
#' @param cleanUtterances Whether to use the clean or the raw utterances
#' when constructing the fragments (the raw versions contain all codes). Note that
#' this should be set to `FALSE` to have `add_html_tags` be of the most use.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#'
#' @return Either a list of character vectors, or a single character value.
#'
#' @rdname collect_coded_fragments
# #' @inheritParams collect_coded_fragments_recursively
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(
#'     examplePath, "example-.rock"
#'   );
#'
#' ### Parse single example source
#' parsedExample <-
#'   rock::parse_source(
#'     exampleFile
#'   );
#'
#' ### Show organised coded fragments in Markdown
#' cat(
#'   rock::collect_coded_fragments(
#'     parsedExample
#'   )
#' );
#'
#' ### Only for the codes containing 'Code2', with
#' ### 2 lines of context (both ways)
#' cat(
#'   rock::collect_coded_fragments(
#'     parsedExample,
#'     'Code2',
#'     context = 2
#'   )
#' );
#'
#' ### Parse multiple example sources
#' ### Load two example sources
#' parsedExamples <- rock::parse_sources(
#'   examplePath,
#'   regex = "example-[1234].rock"
#' );
#'
#' cat(
#'   rock::collect_coded_fragments(
#'     parsedExamples,
#'     '[cC]ode2',
#'     context = 2
#'   )
#' );
#'
#'
#' @export
collect_coded_fragments <- function(x,
                                    codes = ".*",
                                    context = 0,
                                    includeDescendents = FALSE,
                                    attributes = NULL,
                                    heading = NULL,
                                    headingLevel = 3,
                                    add_html_tags = TRUE,
                                    cleanUtterances = FALSE,
                                    omitEmptyCodes = TRUE,
                                    output = NULL,
                                    outputViewer = "viewer",
                                    template = "default",
                                    rawResult = FALSE,
                                    includeCSS = TRUE,
                                    preserveSpaces = TRUE,
                                    codeHeadingFormatting = rock::opts$get("codeHeadingFormatting"),
                                    codeHeadingFormatting_html = rock::opts$get("codeHeadingFormatting_html"),
                                    includeBootstrap = rock::opts$get("includeBootstrap"),
                                    preventOverwriting = rock::opts$get("preventOverwriting"),
                                    silent=rock::opts$get("silent")) {

  sourceFormatting <- rock::opts$get("sourceFormatting");
  sourceFormatting_html <- rock::opts$get("sourceFormatting_html");

  fragmentDelimiter <- rock::opts$get("fragmentDelimiter");
  fragmentDelimiter_html <- rock::opts$get("fragmentDelimiter_html");
  fragmentDelimiter_above_html <- rock::opts$get("fragmentDelimiter_above_html");
  fragmentDelimiter_below_html <- rock::opts$get("fragmentDelimiter_below_html");

  utteranceGlue <- ifelse(add_html_tags, "\n", rock::opts$get("utteranceGlue"));

  if (is.null(context) || any(is.na(context)) || (length(context) == 0)) {
    context <- 0;
  } else if (length(context) == 1) {
    context = c(context, context);
  } else if (length(context) > 2) {
    context <- context[1:2];
  }

  if (!("rock_parsedSource" %in% class(x)) &&
      !("rock_parsedSources" %in% class(x))) {
    stop(glue::glue("The object you provided (as argument `x`) has class '{vecTxtQ(class(x))}', ",
                    "but I can only process objects obtained by parsing one or more sources (with ",
                    "`rock::parse_source` or `rock::parse_sources`), which have class 'rock_parsedSource' ",
                    "or 'rock_parsedSources'."));
  }

  if (interactive() && ("viewer" %in% outputViewer)) {
    if ((!requireNamespace("rstudioapi", quietly = TRUE)) &&
        (rstudioapi::isAvailable())) {
      viewer <- rstudioapi::viewer
    }
    else {
      viewer <- getOption("viewer", utils::browseURL)
    }
    outputToViewer <- TRUE
  } else {
    outputToViewer <- FALSE
  }

  if ("rock_parsedSource" %in% class(x)) {
    singleSource <- TRUE;
  } else {
    singleSource <- FALSE;
  }

  if (length(codes) == 0) {
    stop("Argument `codes` has a length of 0. Without any codes to collect ",
         "fragments for, I have nothing to do!");
  }

  if (length(codes) > 1) {
    codes <- paste0(paste0("^", codes, "$"),
                    collapse="|");
  }

  if (!is.null(x$convenience$inductiveSplitCodes)) {
    allCodes <- unique(unlist(x$convenience$inductiveSplitCodes));
  } else if (!is.null(x$inductiveSplitCodes)) {
    allCodes <- unique(unlist(x$inductiveSplitCodes));
  } else {
    stop("Cannot find 'inductiveSplitCodes'!");
  }

  if (!is.null(x$convenience$original_inductiveCodeTreeNames)) {
    codeTreeNames <- x$convenience$original_inductiveCodeTreeNames;
  } else if (!is.null(x$original_inductiveCodeTreeNames)) {
    codeTreeNames <- x$original_inductiveCodeTreeNames;
  } else {
    stop("Cannot find 'original_inductiveCodeTreeNames'!");
  }

  allCodes <-
    setdiff(
      allCodes,
      codeTreeNames
    );

  ### Check against used codes
  matchedCodes <- grep(codes,
                       allCodes,
                       #x$convenience$codings,
                       ### Changed at 2022-06-10: you also want to be
                       ### able to match 'ancestor codes', not only leaves
                       #x$convenience$codingLeaves,
                       value=TRUE);

  msg(
    "The regular expression passed in argument `codes` ('",
    codes, "') matches the following codings: ",
    vecTxtQ(matchedCodes), ".\n\n",
    silent = silent
  );

  if (includeDescendents) {
    matchedCodes <- unlist(
      lapply(
        matchedCodes,
        rock::get_descendentCodeIds,
        x = x,
        includeParentCode = TRUE
      )
    );

    msg(
      "After combining with the descendent codes, the current list is: ",
      vecTxtQ(matchedCodes), ".\n\n",
      silent = silent
    );

  }

  ### For convenience
  dat <- x$qdt;

  ### Remove codes that were not used on any utterances
  allCodes <- matchedCodes;
  usedCodes <-
    matchedCodes[matchedCodes %in% names(dat)];
  unusedCodes <- setdiff(matchedCodes, usedCodes);

  msg(
    "Of these, the following were not ",
    "used on any utterances: ", vecTxtQ(unusedCodes), ".\n\n",
    "This leaves the following codes: ", vecTxtQ(usedCodes), ".\n",
    silent = silent
  );

  usedCodesPaths <-
    x$convenience$codingPaths[usedCodes];

  ### Select utterances matching the specified attributes

  selectedUtterances <- rep(TRUE, nrow(dat));
  if (!is.null(attributes)) {
    if ((!is.list(attributes)) || (!all(names(attributes) %in% x$convenience$attributesVars))) {
      stop("As `attributes` argument, you must pass a list where every element's ",
           "name is a valid attribute, and every element is a character value ",
           "with a regular expression specifying all values you want to select in that attribute. ",
           "The attribute(s) in the {rock} object you passed are ",
           vecTxtQ(x$convenience$attributesVars), ", but you passed attribute(s) ",
           vecTxtQ(names(attributes)), ".");
    } else {
      ### Cycle through specified attributes and values; set to FALSE where there's no match
      for (attributeName in names(attributes)) {
        selectedUtterances <-
          selectedUtterances & grepl(attributes[attributeName], dat[, attributeName]);
      }
    }
  }

  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ###
  ### Get line numbers of the fragments to extract, get fragments, store them
  ###
  ### First for the 'raw' version, without any HTML markup
  ###
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  res <- lapply(
    usedCodes,
    function(i) {

      msg(
        "\n   - Processing code '", i, "'. ",
        silent = silent
      );

      if (i %in% names(dat)) {
        return(
          lapply(
            which(selectedUtterances & (dat[, i] == 1)),
            function(center) {

              indices <- seq(center - context[1],
                             center + context[2]);

              ### Store indices corresponding source of this utterance
              if (singleSource) {
                sourceIndices <- c(1, nrow(dat));
              } else {
                sourceIndices <-
                  which(dat[, 'originalSource'] == dat[center, 'originalSource']);
              }

              ### If this source is shorter than the number of lines requested,
              ### simply send the complete source
              if ((max(sourceIndices) - min(sourceIndices)) <= (1 + sum(context))) {
                indices <- sourceIndices;
              } else {
                ### Shift forwards or backwards to make sure early or late
                ### fragments don't exceed valid utterance (line) numbers
                indices <- indices - min(0, (min(indices) - min(sourceIndices)));
                indices <- indices - max(0, (max(indices) - max(sourceIndices)));
              }

              ### Get clean or raw utterances
              if (cleanUtterances) {
                res <- dat[indices, 'utterances_clean'];
              } else {
                res <- dat[indices, 'utterances_raw'];
              }

              if (rawResult) {
                return(res);
              } else {

                ### Collapse all utterances into one character value
                res <- paste0(res,
                              collapse=utteranceGlue);

                ### Add the sources, if necessary
                if ((!identical(sourceFormatting, FALSE)) && !singleSource) {
                  res <- paste0(
                    sprintf(
                      sourceFormatting,
                      dat[center, 'originalSource']
                    ),
                    res);
                }

                ### Return result
                return(res);
              }
            }
          )
        );
      } else {
        return(NULL);
      }
    }
  );

  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ###
  ### Get line numbers of the fragments to extract, get fragments, store them
  ###
  ### Second run, now adding HTML markup
  ###
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  res_html <- lapply(
    usedCodes,
    function(i) {

      msg(
        "\n   - Processing code '", i, "'. ",
        silent = silent
      );

      if (i %in% names(dat)) {
        return(
          lapply(
            which(selectedUtterances & (dat[, i] == 1)),
            function(center) {

              indices <- seq(center - context[1],
                             center + context[2]);

              ### Store indices corresponding source of this utterance
              if (singleSource) {
                sourceIndices <- c(1, nrow(dat));
              } else {
                sourceIndices <-
                  which(dat[, 'originalSource'] == dat[center, 'originalSource']);
              }

              ### If this source is shorter than the number of lines requested,
              ### simply send the complete source
              if ((max(sourceIndices) - min(sourceIndices)) <= (1 + sum(context))) {
                indices <- sourceIndices;
              } else {
                ### Shift forwards or backwards to make sure early or late
                ### fragments don't exceed valid utterance (line) numbers
                indices <- indices - min(0, (min(indices) - min(sourceIndices)));
                indices <- indices - max(0, (max(indices) - max(sourceIndices)));
              }

              ### Get clean or raw utterances
              if (cleanUtterances) {
                res <- dat[indices, 'utterances_clean'];
              } else {
                res <- dat[indices, 'utterances_raw'];
              }

              if (rawResult) {
                return(res);
              } else {
                ### Add html tags, if requested
                if (add_html_tags) {
                  res <- paste0(
                    rock::add_html_tags(
                      res,
                      context = setdiff(
                        seq_along(indices),
                        which(indices == center)
                      )
                    )
                  );
                }

                ### Collapse all utterances into one character value
                res <- paste0(res,
                              collapse=utteranceGlue);

                ### Add the sources, if necessary
                if ((!identical(sourceFormatting_html, FALSE)) && !singleSource) {
                  res <- paste0(
                    sprintf(
                      sourceFormatting_html,
                      dat[center, 'originalSource']
                    ),
                    res);
                }

                ### Return result
                return(res);
              }
            }
          )
        );
      } else {
        return(NULL);
      }
    }
  );

  rawRes <- res;
  names(rawRes) <- usedCodes;

  if (rawResult) {
    return(rawRes);
  } else {
    ### Set the code subheading level based on whether a heading
    ### will be included
    if (is.null(heading)) {
      if (length(usedCodes) > 5) {
        heading_markdown <-
          rock::heading(
            "Collected coded fragments with a total of ",
            sum(context), " lines of context (",
            context[1], "+", context[2], ")",
            headingLevel = headingLevel,
            output = "markdown",
            cat = FALSE
          );
        heading_html <-
          rock::heading(
            "Collected coded fragments with a total of ",
            sum(context), " lines of context (",
            context[1], "+", context[2], ")",
            headingLevel = headingLevel,
            output = "html",
            cat = FALSE
          );
      } else {
        heading_markdown <-
          rock::heading(
            "Collected coded fragments for codes ",
            vecTxtQ(usedCodes), " with a total of ",
            sum(context), " lines of context (",
            context[1], "+", context[2], ")",
            headingLevel = headingLevel,
            output = "markdown",
            cat = FALSE
          );
        heading_html <-
          rock::heading(
            "Collected coded fragments for codes ",
            vecTxtQ(usedCodes), " with a total of ",
            sum(context), " lines of context (",
            context[1], "+", context[2], ")",
            headingLevel = headingLevel,
            output = "html",
            cat = FALSE
          );
      }
      codeSubheadingLevel <- headingLevel + 1;
    } else if (is.character(heading)) {
      heading_markdown <-
        rock::heading(
          heading,
          headingLevel = headingLevel,
          output = "markdown"
        );
      heading_html <-
        rock::heading(
          heading,
          headingLevel = headingLevel,
          output = "html",
          cat = FALSE
        );
      codeSubheadingLevel <- headingLevel + 1;
    } else {
      heading <- FALSE;
      codeSubheadingLevel <- headingLevel;
    }

    ### Function to produce the code subheading at the right level
    codeSubheading_html <- function(x,
                                    hl = codeSubheadingLevel) {
                                      return(
                                        unlist(
                                          lapply(
                                            x,
                                            heading_vector,
                                            headingLevel = hl,
                                            output = "html"
                                          )
                                        )
                                      );
                                    }
    codeSubheading_markdown <- function(x,
                                        hl = codeSubheadingLevel) {
                                          return(
                                            unlist(
                                              lapply(
                                                x,
                                                heading_vector,
                                                headingLevel = hl,
                                                output = "markdown"
                                              )
                                            )
                                          );
    }

    ### Combine all fragments within each code
    res <- lapply(res,
                  paste0,
                  collapse=fragmentDelimiter);
    res_markdown <- lapply(res,
                           paste0,
                           collapse=fragmentDelimiter);
    res_html <- lapply(res_html,
                       paste0,
                       collapse=fragmentDelimiter_html);

    if (omitEmptyCodes) {
      elementsToKeep <-
        unlist(
          lapply(
            res,
            function(x) {
              if (is.vector(x) && ((length(x) == 0) || (all(x == "")))) {
                return(FALSE);
              } else {
                return(TRUE);
              }
            }
          )
        );
    } else {
      elementsToKeep <- rep(TRUE, length(res));
    }

    ### Unlist into vector
    res <- unlist(res);
    res_html <- unlist(res_html);
    res_markdown <- unlist(res_markdown);

    ### Add titles for html and markdown versions
    res_html <- paste0(codeSubheading_html(
                    sprintf(
                      codeHeadingFormatting_html,
                      usedCodes[elementsToKeep],
                      usedCodesPaths[elementsToKeep]
                    )
                  ),
                  fragmentDelimiter_above_html,
                  res_html[elementsToKeep],
                  fragmentDelimiter_below_html);

    res_markdown <- paste0(codeSubheading_markdown(
                      sprintf(
                        codeHeadingFormatting,
                        usedCodes[elementsToKeep],
                        usedCodesPaths[elementsToKeep]
                      )
                    ),
                    fragmentDelimiter,
                    res_markdown[elementsToKeep],
                    fragmentDelimiter);

    ### Collapse into one character value
    res_html <- paste0(res_html, collapse="\n");
    res_markdown <- paste0(res_markdown, collapse="\n");

    ### Add title heading
    if (!identical(heading, FALSE)) {
      res_html <- paste0(heading_html, res_html);
      res_markdown <- paste0(heading_markdown, res_markdown);
    }
  }

  ### Add CSS for html tags if requested
  if (add_html_tags) {
    res_without_css <- res_html;
    if (includeCSS) {
      res_html <-
        paste0(
          rock::css(
            template=template,
            includeBootstrap = ifelse(is.character(includeBootstrap),
                                      FALSE,
                                      includeBootstrap)
          ),
          "\n\n",
          res_html
        );
    }
  } else {
    res_without_css <- res_html;
  }

  if (preserveSpaces) {
    res_html <-
      gsub("  ", "&nbsp;&nbsp;", res_html);
  }

  res_html <- paste0("<div class='rock rock-collected-fragments-container'>",
                     res_html,
                     "</div>");

  if (is.null(output)) {
    if (isTRUE(getOption('knitr.in.progress'))) {

      ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ### Adding the CSS is missing, isn't that wrong?
      ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      return(knitr::asis_output(c("\n\n",
                                  res_html,
                                  "\n\n")));
    } else {
      if (outputToViewer) {
        #viewerHTML <- markdown::mark_html(text=res_without_css);
        #viewerHTML <- markdown::mark(text=res_without_css, template=TRUE);
        #viewerHTML <- markdown::mark(text=res_without_css);
        viewerHTML <- res_html;
        if (add_html_tags) {
          viewerHTML <- htmltools::HTML(
            rock::css(template=template,
                      includeBootstrap = ifelse(is.character(includeBootstrap),
                                                TRUE,
                                                includeBootstrap)),
            viewerHTML
          );
        } else {
          viewerHTML <- htmltools::HTML(viewerHTML);
        }
        htmltools::html_print(htmltools::HTML(viewerHTML),
                              background = "white",
                              viewer = viewer)
      }
      if ("console" %in% outputViewer) {
        cat(res_markdown)
      }
      return(invisible(res_markdown));
    }
  } else {

    if (outputToViewer) {
      #viewerHTML <- markdown::mark_html(text=res_without_css);
      #viewerHTML <- markdown::mark(text=res_without_css, template=TRUE);
      #viewerHTML <- markdown::mark(text=res_without_css);
      viewerHTML <- res_html;
      viewerHTML <- htmltools::HTML(c("<html>", viewerHTML, "</html"));
      if (add_html_tags) {
        viewerHTML <- htmltools::HTML(
          rock::css(template=template,
                    includeBootstrap = ifelse(is.character(includeBootstrap),
                                              TRUE,
                                              includeBootstrap)),
          viewerHTML
        );
      } else {
        viewerHTML <- htmltools::HTML(viewerHTML);
      }
      htmltools::html_print(htmltools::HTML(viewerHTML),
                            background = "white",
                            viewer = viewer)
    }
    if ("console" %in% outputViewer) {
      cat(res_markdown)
    }

    if (dir.exists(dirname(output))) {
      if (file.exists(output) | preventOverwriting) {

        fileHTML <- res_html;
        fileHTML <- htmltools::HTML(c("<html>", fileHTML, "</html"));
        if (add_html_tags) {
          fileHTML <- htmltools::HTML(
            rock::css(template=template,
                      includeBootstrap = ifelse(is.character(includeBootstrap),
                                                TRUE,
                                                includeBootstrap)),
            fileHTML
          );
        }

        writeLines(fileHTML,
                   con = con <- file(output,
                                     "w",
                                     encoding="UTF-8"));
        close(con);

        if (!silent) {
          cat0("Wrote output file '", output,
               "' to disk.");
        }
      } else {
        if (!silent) {
          cat0("Specified output file '", output,
               "' exists, and `preventOverwriting` is set to `TRUE`; ",
               "did not write the file!");
        }
      }
      return(invisible(res_markdown));
    } else {
      stop("You passed '", output,
           "' as output filename, but directory '", dirname(output),
           "' does not exist!");
    }
  }

}
