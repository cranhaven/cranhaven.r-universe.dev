#' @rdname setCaptionNumberingKnitrHook
#' @export
setTabCapNumbering <- function (captionName = "tab.cap",
                                prefix = "Table %s: ",
                                suffix = "",
                                optionName = paste0("setCaptionNumbering_",
                                                    captionName),
                                resetCounterTo = 1) {
  return(
    invisible(
      setCaptionNumberingKnitrHook(
        captionName = captionName,
        prefix = prefix,
        suffix = suffix,
        optionName = optionName,
        resetCounterTo = resetCounterTo
      )
    )
  );
}
