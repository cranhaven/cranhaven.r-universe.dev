.onAttach <- function(...) {
  if (!interactive()) return()
  
  tips <- c(
    "View the Choroplethr documentation at www.Choroplethr.com",
    "Stuck? Ask a question here: https://stackoverflow.com/questions/tagged/choroplethr."
  )
  
  tip <- sample(tips, 1)
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}