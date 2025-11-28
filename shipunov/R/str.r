Str <- function(df, as.factor=FALSE)
{
 if (is.data.frame(df) & sum(sapply(df, is.atomic)) == length(df)) {
 if (as.factor) df <- data.frame(as.list(df), stringsAsFactors=TRUE)
 str.tmp <- sub("^ \\$ ", "", capture.output(str(df, list.len=ncol(df))))
 nums <- prettyNum(0:length(df), width=2)
 nums[1] <- ""
 nas <- c(0, sapply(df, function(.x) sum(is.na(.x))))
 str.tmp <- ifelse(nas > 0, sub(": ", "* ", str.tmp), str.tmp)
 cat(paste(nums, str.tmp, "\n", sep=" "))
 if (!identical(as.character(seq_len(nrow(df))), row.names(df))) {
  rown.tmp <- capture.output(str(row.names(df), vec.len=5))
  rown.tmp <- sub("chr", "row.names", rown.tmp)
  cat(rown.tmp, "\n")
  }
 } else {
 str(df)
 }
 if (as.factor) invisible(df)
}

