dir_or_file_self <-
function(..., special = "") {
  x <- as.character2(...)
  x <- whetherencode(x)
  x <- normalizePath(x, winslash = "/", mustWork = TRUE)
  dir_pos <- dir.exists(x)
  all_dir <- x[dir_pos]
  if (length(all_dir) > 0) {
    file_1 <- unlist(lapply(all_dir, list.files, all.files = TRUE, full.names = TRUE, recursive = TRUE))
  }
  else {
    file_1 <- NULL
  }
  file_pos <- file.exists(x) & !dir.exists(x)
  file_2 <- x[file_pos]
  file_12 <- c(file_1, file_2)
  if (is_character_vector(special, len = 1) == TRUE && special != "" && length(file_12) > 0) {
    file_12 <- file_12[grepl(special, file_12)]
  }
  if (length(file_12) == 0)
    stop("There is nothing to collect.")
  file_12 <- sort(unique(file_12))
  file_12 <- stringi::stri_encode(file_12, to = "UTF-8")
  return(file_12)
}
