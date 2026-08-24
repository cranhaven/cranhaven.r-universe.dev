read_map <- function(input,
                     fileRegexes = c("^[^\\.]+.*\\.map$"),
                     select="behaviorchange_map",
                     delimiterRegEx = "^---$",
                     ignoreOddDelimiters = FALSE,
                     encoding = "UTF-8",
                     silent = TRUE) {

  res <- list();

  if (dir.exists(input)) {
    res$aspects <- yum::load_and_simplify_dir(input,
                                              select=select);
  } else if (file.exists(input)) {
    res$aspects <- yum::load_and_simplify(input,
                                          select=select);
  } else {
    stop("What you specified as `input` is neither a file nor a directory! You specified: ",
         ufs::vecTxtQ(input), ".");
  }

  res$matrix_cols <- read_matrix_cols(input=input,
                                      fileRegexes=fileRegexes,
                                      delimiterRegEx=delimiterRegEx,
                                      ignoreOddDelimiters=ignoreOddDelimiters,
                                      encoding=encoding,
                                      silent=silent);
  return(res);

}

#res <- read_map(here::here("..", "tmp", "map-test.map"));


