#' Add HTML tags to a source
#'
#' This function adds HTML tags to a source to allow pretty printing/viewing.
#'
#' @param x A character vector with the source
#' @param context Optionally, lines to pass the contextClass
#' @param codeClass,codeValueClass,idClass,sectionClass,uidClass,contextClass,utteranceClass,commentClass,networkCodeClass,rockLineClass,codingClass,yamlClass
#' The classes to use for, respectively, codes, code values,
#' class instance identifiers (such as case
#' identifiers or coder identifiers), section breaks, utterance
#' identifiers, context, full utterances, comments, network codes, source lines, codings, and YAML chunks. All `<span>` elements except
#' for the full utterances, which are placed in `<div>` elements.
#'
#' @return The character vector with the replacements made.
#' @rdname prettifying_sources
#' @export
#'
#' @examples ### Add tags to a mini example source
#' add_html_tags("[[cid=participant1]]
#' This is something this participant may have said.
#' Just like this. [[thisIsACode]]
#' ---paragraph-break---
#' And another utterance.");
add_html_tags <- function(x,
                          context = NULL,
                          codeClass = rock::opts$get("codeClass"),
                          codeValueClass = rock::opts$get("codeValueClass"),
                          networkCodeClass = rock::opts$get("networkCodeClass"),
                          idClass = rock::opts$get("idClass"),
                          sectionClass = rock::opts$get("sectionClass"),
                          uidClass = rock::opts$get("uidClass"),
                          contextClass = rock::opts$get("contextClass"),
                          rockLineClass = rock::opts$get("rockLineClass"),
                          utteranceClass = rock::opts$get("utteranceClass"),
                          codingClass = rock::opts$get("codingClass"),
                          commentClass = rock::opts$get("commentClass"),
                          yamlClass = rock::opts$get("yamlClass")) {

  codeRegexes <- rock::opts$get("codeRegexes");
  codeValueRegexes <- rock::opts$get("codeValueRegexes");
  classInstanceRegex <- rock::opts$get("classInstanceRegex");
  sectionRegexes <- rock::opts$get("sectionRegexes");
  networkCodeRegexes <- rock::opts$get("networkCodeRegexes");
  uidRegex <- rock::opts$get("uidRegex");
  ignoreRegex <- rock::opts$get("ignoreRegex");
  inductiveCodingHierarchyMarker <- rock::opts$get("inductiveCodingHierarchyMarker");

  encoding <- rock::opts$get('encoding');
  silent <- rock::opts$get('silent');
  delimiterRegEx <- rock::opts$get('delimiterRegEx');
  ignoreOddDelimiters <- FALSE;

  res <- x;

  ### First replace smaller than and bigger than symbols
  ### with the corresponding entities
  res <- gsub("<", "&lt;", res, fixed=TRUE);
  res <- gsub(">", "&gt;", res, fixed=TRUE);

  ###---------------------------------------------------------------------------
  ### Lines to ignore (comments)
  ###---------------------------------------------------------------------------

  linesToIgnore_lineNrs <-
    grep(
      ignoreRegex,
      res,
      perl = TRUE
    );

  emptyLines_lineNrs <-
    grep(
      "^\\s*$",
      res,
      perl = TRUE
    );

  linesToIgnore_lineNrs <-
    sort(
      unique(
        union(
          linesToIgnore_lineNrs,
          emptyLines_lineNrs
        )
      )
    );

  linesToIgnore_contents <-
    res[linesToIgnore_lineNrs];

  linesToIgnore_contents <-
    paste0(
      '<span class="', commentClass, '">',
      linesToIgnore_contents,
      '</span>'
    );

  ###---------------------------------------------------------------------------
  ### YAML lines
  ###---------------------------------------------------------------------------

  ### This is adapted from from yum::extract_yaml_fragments()

  yamlDelimiterLines <- grep(delimiterRegEx, res);

  if (length(yamlDelimiterLines) > 0) {

    if (!yum::is.even(length(yamlDelimiterLines))) {
      stop("Uneven number of YAML chunk delimiters found! You",
           "probably forgot or accidently deleted it. The YAML ",
           "chunk delimiter is usually '---' on its own on a line. ",
           "Specifically, I searched for all lines matching regular ",
           "expression '", delimiterRegEx, "'.");
    }

    yamlFragmentIndices <- seq_along(yamlDelimiterLines);

    if (length(yamlFragmentIndices) == 2) {
      indexSets <- list(seq(yamlDelimiterLines[1], yamlDelimiterLines[2]));
    } else {
      indexSets <- mapply(seq, yamlDelimiterLines[yum::is.odd(yamlFragmentIndices)],
                          yamlDelimiterLines[yum::is.even(yamlFragmentIndices)], SIMPLIFY = FALSE);
    }

    yamlLines <- unlist(indexSets);

    yamlLines_contents <-
      paste0(
        '<div class="', yamlClass, '">',
        res[yamlLines],
        '</div>'
      );

  } else {
    yamlLines <- NULL;
    yamlLines_contents <- NULL;
  }

  ###---------------------------------------------------------------------------
  ### Codes
  ###---------------------------------------------------------------------------

  ### Also replace <> symbols in all codeRegexes
  codeRegexes <- gsub("<", "&lt;", codeRegexes, fixed=TRUE);
  codeRegexes <- gsub(">", "&gt;", codeRegexes, fixed=TRUE);

  ### And networkRegexes
  networkCodeRegexes <- gsub("<", "&lt;", networkCodeRegexes, fixed=TRUE);
  networkCodeRegexes <- gsub(">", "&gt;", networkCodeRegexes, fixed=TRUE);

  ### Add html tags to flat codes and tree codes
  for (currentCodeRegexName in names(codeRegexes)) {
    currentCodeRegex <- codeRegexes[currentCodeRegexName];
    codeContentMatches <- grepl(currentCodeRegex, res, perl = TRUE);
    if (any(codeContentMatches)) {
      codeContent <-
        ifelse(codeContentMatches,
               gsub(paste0(".*", currentCodeRegex, ".*"),
                    "\\1",
                    res,
                    perl = TRUE),
               "");
      splitCodeContent <-
        unlist(lapply(strsplit(codeContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeContent <-
        paste0('<span class="', codingClass, " ", codeClass,
               ' ', currentCodeRegexName,
               '">');
      res <-
        gsub(paste0("(", currentCodeRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res,
             perl = TRUE);
    }
  }

  ### Add html tags to network codes
  for (currentCodeRegexName in names(networkCodeRegexes)) {
    currentCodeRegex <- networkCodeRegexes[currentCodeRegexName];
    codeContentMatches <- grepl(currentCodeRegex, res, perl = TRUE);
    if (any(codeContentMatches)) {
      codeContent <-
        ifelse(codeContentMatches,
               gsub(paste0(".*", currentCodeRegex, ".*"),
                    "\\1",
                    res,
                    perl = TRUE),
               "");
      splitCodeContent <-
        unlist(lapply(strsplit(codeContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeContent <-
        paste0('<span class="', codingClass, " ", networkCodeClass,
               ' ', currentCodeRegexName,
               '">');
      res <-
        gsub(paste0("(", currentCodeRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res,
             perl = TRUE);
    }
  }

  ###---------------------------------------------------------------------------
  ### Code values
  ###---------------------------------------------------------------------------

  ### Also replace <> symbols in all codeValueRegexes
  codeValueRegexes <- gsub("<", "&lt;", codeValueRegexes, fixed=TRUE);
  codeValueRegexes <- gsub(">", "&gt;", codeValueRegexes, fixed=TRUE);

  ### Add html tags
  if (is.null(names(codeValueRegexes)) && (!is.null(codeValueRegexes))) {
    names(codeValueRegexes) <- paste0("codeValue", seq_along(codeValueRegexes));
  }

  for (currentCodeValueRegexName in names(codeValueRegexes)) {
    currentCodeValueRegex <- codeValueRegexes[currentCodeValueRegexName];
    codeValueContentMatches <- grepl(currentCodeValueRegex, res, perl = TRUE);
    if (any(codeValueContentMatches)) {
      codeValueContent <-
        ifelse(codeValueContentMatches,
               gsub(paste0(".*", currentCodeValueRegex, ".*"),
                    "\\1",
                    res,
                    perl = TRUE),
               "");
      splitCodeValueContent <-
        unlist(lapply(strsplit(codeValueContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeValueContent <-
        paste0('<span class="', codingClass, " ", codeValueClass,
               ' ', currentCodeValueRegexName,
               '">');
      res <-
        gsub(paste0("(", currentCodeValueRegex, ")"),
             paste0(splitCodeValueContent, '\\1</span>'),
             res,
             perl = TRUE);
    }
  }

  ###---------------------------------------------------------------------------
  ### Sections
  ###---------------------------------------------------------------------------

  ### Also replace <> symbols in all sectionRegexes
  sectionRegexes <- gsub("<", "&lt;", sectionRegexes, fixed=TRUE);
  sectionRegexes <- gsub(">", "&gt;", sectionRegexes, fixed=TRUE);

  ### Add break tags
  for (currentBreakRegexName in names(sectionRegexes)) {
    currentBreakRegex <- sectionRegexes[currentBreakRegexName];
    codeContentMatches <- grepl(currentBreakRegex, res, perl = TRUE);
    if (any(codeContentMatches)) {
      codeContent <-
        ifelse(codeContentMatches,
               gsub(paste0(".*", currentBreakRegex, ".*"),
                    "\\1",
                    res,
                    perl = TRUE),
               "");
      splitCodeContent <-
        unlist(lapply(strsplit(codeContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeContent <-
        paste0('<span class="', codingClass, " ", sectionClass,
               ' ', currentBreakRegexName,
               '">');
      res <-
        gsub(paste0("(", currentBreakRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res,
             perl = TRUE);
    }
  }

  ###---------------------------------------------------------------------------
  ### Class instance identifiers
  ###---------------------------------------------------------------------------

  ### Also replace <> symbols in all idRegexes
  classInstanceRegex <- gsub("<", "&lt;", classInstanceRegex, fixed=TRUE);
  classInstanceRegex <- gsub(">", "&gt;", classInstanceRegex, fixed=TRUE);

  ### Add identifier tags
  for (currentIdRegexName in names(classInstanceRegex)) {
    currentIdRegex <- classInstanceRegex[currentIdRegexName];
    codeContentMatches <- grepl(currentIdRegex, res, perl = TRUE);
    if (any(codeContentMatches)) {
      res <-
        gsub(paste0("(", currentIdRegex, ")"),
             paste0('<span class="', idClass,
                    ' ', currentIdRegexName,
                    '">\\1</span>'),
             res,
             perl = TRUE);
    }
  }

  ###---------------------------------------------------------------------------
  ### Utterance identifiers
  ###---------------------------------------------------------------------------

  ### Add UID tags
  res <-
    gsub(paste0("(", uidRegex, ")"),
         paste0('<span class="', codingClass, " ", uidClass,
                '">\\1</span>'),
         res);

  ###---------------------------------------------------------------------------
  ### Context
  ###---------------------------------------------------------------------------

  ### Add context tags, if applicable
  if (!is.null(context)) {
    res[context] <-
      paste0('<span class="', contextClass, '">', res[context], '</span>');
  }

  ###---------------------------------------------------------------------------
  ### Utterances
  ###---------------------------------------------------------------------------

  ### Add rock-line and utterance tags
  res <- paste0('<div class="', rockLineClass, ' ', utteranceClass, '">', res, '</div>\n');

  ###---------------------------------------------------------------------------
  ### Replace YAML lines ('overwriting' any applied tags)
  ###---------------------------------------------------------------------------

  res[yamlLines] <-
    yamlLines_contents;

  # ### Add rock-line tag
  res[yamlLines] <-
    paste0('<div class="', rockLineClass, ' ">', res[yamlLines], '</div>\n');

  ###---------------------------------------------------------------------------
  ### Replace lines to ignore ('overwriting' any applied tags)
  ###---------------------------------------------------------------------------

  res[linesToIgnore_lineNrs] <-
    linesToIgnore_contents;

  ### Add rock-line tag
  res[linesToIgnore_lineNrs] <-
    paste0('<div class="', rockLineClass, ' ">', res[linesToIgnore_lineNrs], '</div>\n');

  return(res);

}
