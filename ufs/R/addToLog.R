### This function adds a message to the log
### and displays it depending on a boolean
addToLog <- function(fullLog, ..., showLog = FALSE) {
  if (showLog) {
    cat(paste0(..., collapse="\n"))
  }
  return(paste0(fullLog, "\n", paste0(..., collapse="\n")));
}
