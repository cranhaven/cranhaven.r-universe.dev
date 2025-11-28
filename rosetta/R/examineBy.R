#' @param by A variable by which to split the dataset before calling
#' \code{\link{examine}}. This can be used to show the descriptives separate by
#' levels of a factor.
#' @param x The object to print or pander.
#' @param headerPrefix,secondaryHeaderPrefix,tertairyHeaderPrefix Prefixes
#' for the primary, secondary header, and tertairy headers
#' @param headerStyle,secondaryHeaderStyle,tertairyHeaderStyle Characteers to
#' surround the primary, secondary, and tertairy headers with
#' @param separator Separator for the result blocks.
#'
#' @export
#' @rdname examine
examineBy <- function(..., by=NULL, stem=TRUE, plots=TRUE,
                      extremeValues = 5,
                      qqCI=TRUE, conf.level=.95) {

  if (is.null(by)) {
    stop("You have to specify a 'by' argument. If you don't want to ",
         "order descriptives organised by another variable, use 'examine'.");
  }

  if (length(list(...)) == 1) {
    dat <- list(...)[[1]];
    if (is.data.frame(dat)) {
      varNames <- names(dat);
    } else {
      varNames <- unlist(as.list(substitute(list(...)))[-1]);
    }
  } else {
    if (length(unique(unlist(lapply(list(...), length)))) != 1) {
      stop("The vectors that were provided has unequal lengths ",
           "(specifically, ", vecTxt(lapply(list(...), length)), ").");
    }
    dat <- list(...);
    varNames <- unlist(as.list(substitute(list(...)))[-1]);
  }

  dat <- as.data.frame(dat);
  names(dat) <- ufs::extractVarName(varNames);

  res <- plyr::dlply(
    dat, plyr::as.quoted(~by), examine,
    stem=stem, plots=plots,
    extremeValues=extremeValues,
    qqCI=qqCI, conf.level=conf.level
  );

  class(res) <- 'examineBy';

  return(res);

}

#' @rdname examine
#' @export
#' @method print examineBy
print.examineBy <- function(x, ...) {

  for (examineObjects in 1:length(x)) {
    cat0(repStr("#", 60), "\n");
    cat0(ufs::extractVarName(names(x)[examineObjects]), "\n");
    cat0(repStr("#", 60), "\n\n");
    print(x[[examineObjects]]);
  }

}

#' @method pander examineBy
#' @rdname examine
#' @importFrom pander pander
#' @export
pander.examineBy <- function(x, headerPrefix = "",
                             headerStyle = "**",
                             secondaryHeaderPrefix = "",
                             secondaryHeaderStyle="*",
                             tertairyHeaderPrefix = "--> ",
                             tertairyHeaderStyle="",
                             separator = paste0("\n\n", repStr("-", 10), "\n\n"),
                             ...) {

  for (examineObjects in 1:length(x)) {
    cat("\n");
    if (examineObjects > 1)
      cat0(separator);
    cat0(headerPrefix, headerStyle,
         ufs::extractVarName(names(x)[examineObjects]),
         headerStyle);
    pander(x[[examineObjects]],
           headerPrefix=secondaryHeaderPrefix,
           headerStyle=secondaryHeaderStyle,
           secondaryHeaderPrefix=tertairyHeaderPrefix,
           secondaryHeaderStyle=tertairyHeaderStyle);
  }

}
