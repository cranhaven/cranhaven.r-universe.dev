read_matrix_cols <- function(input,
                             fileRegexes = c("^[^\\.]+.*\\.map$"),
                             select="behaviorchange_matrix_columns",
                             delimiterRegEx = "^---$",
                             ignoreOddDelimiters = FALSE,
                             encoding = "UTF-8",
                             silent = TRUE) {

  if (dir.exists(input)) {
    res <- yum::load_and_simplify_dir(input,
                                      select=select,
                                      fileRegexes=fileRegexes,
                                      delimiterRegEx=delimiterRegEx,
                                      ignoreOddDelimiters = ignoreOddDelimiters,
                                      encoding=encoding,
                                      silent=silent);
  } else if (file.exists(input)) {
    res <- yum::load_and_simplify(input,
                                  select=select,
                                  delimiterRegEx=delimiterRegEx,
                                  ignoreOddDelimiters = ignoreOddDelimiters,
                                  encoding=encoding,
                                  silent=silent);
  } else {
    stop("What you specified as `input` is neither a file nor a directory! You specified: ",
         ufs::vecTxtQ(input), ".");
  }

  return(unname(unlist(res)));

}

#res <- read_matrix_cols(here::here("..", "tmp", "map-test.map"));


