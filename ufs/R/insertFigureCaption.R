#' @rdname insertNumberedCaption
#' @export
insertFigureCaption <- function(captionText = "",
                                captionName = "fig.cap",
                                prefix = getOption(paste0(optionName,
                                                          "_prefix"),
                                                   "Figure %s: "),
                                suffix = getOption(paste0(optionName,
                                                          "_suffix"),
                                                   ""),
                                optionName = paste0("setCaptionNumbering_", captionName),
                                resetCounterTo = NULL) {
  return(
    insertNumberedCaption(
      captionText=captionText,
      captionName=captionName,
      prefix=prefix,
      suffix=suffix,
      optionName=optionName,
      resetCounterTo=resetCounterTo
    )
  );
}
