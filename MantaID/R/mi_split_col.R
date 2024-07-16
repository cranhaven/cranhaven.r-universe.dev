#' Cut the string of ID column character by character and divide it into multiple columns.
#'
#' @param data Dataframe(tibble) to be split.
#' @param cores Int.The num of cores to allocate for computing.
#' @param pad_len The length of longest id, i.e. the maxlength.
#' @importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parSapply stopCluster
#' @importFrom  dplyr bind_cols
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#' @return A tibble with pad_len+1 column.
#' @export
mi_split_col <- function(data, cores = NULL, pad_len = 10) {

  core_max <- detectCores(logical = FALSE)%/%2
  if (is.null(cores)) {
    cl <- makeCluster(core_max)
  } else {
    cl <- makeCluster(cores)
  }
  mi_split_str <- function(str, pad_len) {
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len]
  }
  clusterExport(cl, varlist = c("pad_len", "mi_split_str"), envir = environment())
  clusterEvalQ(cl, c(library(data.table), library(magrittr), library(stringr),library(dplyr)))
  output <- parSapply(cl, data[, 1][[1]], mi_split_str, pad_len)
  stopCluster(cl)
  output %>%
    unlist() %>%
    matrix(byrow = TRUE, ncol = pad_len) %>%
    bind_cols(data[, 2]) %>%
    set_names(c(str_c("pos", 1:pad_len),"class"))
}
