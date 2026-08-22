#' Create a frequency histogram for codes
#'
#' @param x A parsed source(s) object.
#' @param codes A regular expression to select codes to include.
#' @param sortByFreq Whether to sort by frequency decreasingly
#' (`decreasing`, the default), increasingly (`increasing`),
#' or alphabetically (`NULL`).
#' @param forceRootStripping Force the stripping of roots, even if they are
#' different.
#' @param trimSourceIdentifiers If not `NULL`, the number of character to trim
#' the source identifiers to.
#' @param ggplot2Theme Can be used to specify theme elements for the plot.
#' @param silent Whether to be chatty or silent.
#'
#' @return a [ggplot2::ggplot()].
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
#' loadedExample <- rock::parse_source(exampleFile);
#'
#' ### Show code frequencies
#' code_freq_hist(loadedExample);
code_freq_hist <- function(x,
                           codes = ".*",
                           sortByFreq = "decreasing",
                           forceRootStripping = FALSE,
                           trimSourceIdentifiers = 20,
                           ggplot2Theme = ggplot2::theme(legend.position = "bottom"),
                           silent = rock::opts$get("silent")) {

  if (!(("rock_parsedSources" %in% class(x)) |
        ("rock_parsedSource"  %in% class(x)))) {
    stop("As `x`, you must pass either an `rock_parsedSource` or ",
         "an `rock_parsedSources` object (i.e. either the result ",
         "from a call to `rock::parseSource()` or the result from ",
         "a call to `rock::parseSources()`). However, you ",
         "provided an object of class ", vecTxtQ(x), ".");
  }

  showFullCodePaths <- rock::opts$get("showFullCodePaths");

  if ("rock_parsedSource" %in% class(x)) {

    x$countedCodings <-
      x$countedCodings[grepl(codes,
                             names(x$countedCodings))];

    tmpDf <-
      data.frame(Code = names(x$countedCodings),
                 Frequency = x$countedCodings);

  } else {

    tmpDf <-
      do.call(
        rbind,
        lapply(names(x$parsedSources),
               function(y) {
                 srcObj <- x$parsedSources[[y]];
                 if ('countedCodings' %in% names(srcObj)) {
                   res <-
                     srcObj$countedCodings[grepl(codes,
                                                 names(srcObj$countedCodings))];
                   res <- data.frame(Code = names(res),
                                     Frequency = res,
                                     Source = y);
                   return(res);
                 } else {
                   return(NULL);
                 }
               }));

  }

  if (('Source' %in% names(tmpDf)) && (!is.null(trimSourceIdentifiers))) {
    tmpDf$Source <-
      substr(
        tmpDf$Source,
        1,
        trimSourceIdentifiers
      );
  }

  if (showFullCodePaths) {

    tmpDf$Code <-
      ifelse(
        tmpDf$Code %in% names(x$convenience$codingPaths),
        x$convenience$codingPaths[tmpDf$Code],
        tmpDf$Code
      );

    if (rock::opts$get("stripRootsFromCodePaths")) {
      ### Check whether all elements have the same root
      if (forceRootStripping ||
          (length(unique(root_from_codePaths(tmpDf$Code))) == 1)) {
        tmpDf$Code <-
          stripCodePathRoot(
            tmpDf$Code
          );
      } else {
        if (!silent) {
          warning("Not stripping the root elements from the code paths: ",
                  "not all root elements are the same, and ",
                  "forceRootStripping=FALSE.");
        }
      }
    }

  }

  if ("rock_parsedSource" %in% class(x)) {

    if ((!is.null(sortByFreq)) && (sortByFreq == "increasing")) {
      sortOrderInDf <- order(tmpDf$Frequency,
                             decreasing = TRUE);
    } else if ((!is.null(sortByFreq)) && (sortByFreq == "decreasing")) {
      sortOrderInDf <- order(tmpDf$Frequency,
                             decreasing = FALSE);
    } else {
      sortOrderInDf <- order(tmpDf$Code,
                             decreasing = TRUE);
    }

    totalOccurrences <- tmpDf$Frequency[sortOrderInDf];
    names(totalOccurrences) <- tmpDf$Code[sortOrderInDf];
    totalOccurrencesSorted <- totalOccurrences;

  } else {

    totalOccurrences <-
      unclass(by(tmpDf$Frequency, tmpDf$Code, sum));
    attributes(totalOccurrences)$call <- NULL;

    if ((!is.null(sortByFreq)) && (sortByFreq == "increasing")) {
      ### We flip the order because of ggplot2's Y axis starts at the bottom
      totalOccurrencesSorted <-
        totalOccurrences[order(totalOccurrences,
                               names(totalOccurrences),
                               decreasing=c(TRUE, FALSE),
                               method = "radix")];
      sortOrderInDf <- order(tmpDf$Code[totalOccurrencesSorted],
                             decreasing = TRUE);
    } else if ((!is.null(sortByFreq)) && (sortByFreq == "decreasing")) {
      ### We flip the order because of ggplot2's Y axis starts at the bottom
      totalOccurrencesSorted <-
        totalOccurrences[order(totalOccurrences,
                               names(totalOccurrences),
                               decreasing=c(FALSE, TRUE),
                               method = "radix")];
      sortOrderInDf <- order(tmpDf$Code[totalOccurrencesSorted],
                             decreasing = TRUE);
    } else {
      totalOccurrencesSorted <-
        totalOccurrences[
          order(names(totalOccurrences),
                decreasing = TRUE)
        ];
      sortOrderInDf <- order(tmpDf$Code[totalOccurrencesSorted],
                             decreasing = FALSE);
    }
  }

  ### Create factor, apply order
  tmpDf$Code <- factor(tmpDf$Code,
                       levels = names(totalOccurrencesSorted), #tmpDf$Code[sortOrder],
                       labels = names(totalOccurrencesSorted), #tmpDf$Code[sortOrder],
                       ordered = TRUE);

  tmpDf$codeNr <- as.numeric(tmpDf$Code);

  if ("rock_parsedSource" %in% class(x)) {
    res <- ggplot2::ggplot(data=tmpDf,
                           mapping=ggplot2::aes_string(x='codeNr',
                                                       y='Frequency'));
  } else {
    res <- ggplot2::ggplot(data=tmpDf,
                           mapping=ggplot2::aes_string(x='codeNr',
                                                       y='Frequency',
                                                       fill = 'Source')) +
      ggplot2::scale_fill_viridis_d(end=.9);
  }

  res <- res +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::scale_x_continuous(name = "Code",
                                breaks=tmpDf$codeNr[sortOrderInDf],
                                labels=tmpDf$Code[sortOrderInDf],
                                sec.axis = ggplot2::dup_axis(labels = totalOccurrencesSorted,
                                                             breaks = 1:length(totalOccurrencesSorted),
                                                             name = "Number of code occurrences")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color="#eeeeee",
                                                              size=.1)) +
    ggplot2::coord_flip() +
    ggplot2Theme;

  return(res);

}
