#' @rdname insertNumberedCaption
#' @export
insertTableCaption <- function(captionText = "",
                               captionName = "tab.cap",
                               prefix = getOption(paste0(optionName,
                                                         "_prefix"),
                                                  "Table %s: "),
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
