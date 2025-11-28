#' @rdname frequencies
#' @export
frequencies <- function(..., digits = 1, nsmall=1, transposed=FALSE, round=1,
                    plot=FALSE, plotTheme = ggplot2::theme_bw()) {

  ### Call functions to explore the variables
  res <- lapply(list(...), function(x) {
    rsl <- list();
    rsl$freq <- freq(x, digits=digits, nsmall=nsmall,
                     transposed=transposed, round=round,
                     plot=plot, plotTheme=plotTheme);
    return(rsl);
  });

  ### Get the variable names
  names(res) <- unlist(as.list(substitute(list(...)))[-1]);

  ### Set class for correct printing and return result
  class(res) <- 'frequencies';
  return(res);
}

#' @method print frequencies
#' @rdname frequencies
#' @export
print.frequencies <- function(x, ...) {
  for (currentName in names(x)) {
    cat0("### Frequencies for '", ufs::extractVarName(currentName), "'\n\n");
    print(x[[currentName]]$freq);
    cat("\n");
  }
}

#' @method pander frequencies
#' @rdname frequencies
#' @importFrom pander pander
#' @export
pander.frequencies <- function(x, prefix="###", ...) {
  for (currentName in names(x)) {
    cat0(prefix, " Frequencies for '", ufs::extractVarName(currentName), "'\n\n");
    pander(x[[currentName]]$freq);
    cat("\n");
  }
}
