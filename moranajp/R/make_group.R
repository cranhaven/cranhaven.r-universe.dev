#' Make groups by splitting string length
#'
#' @inherit moranajp_all
#' @param  length                A numeric.
#' @param  n_group               A numeric.
#' @param  tmp_group,str_length  A string to use temporary.
make_groups <- function(tbl, text_col = "text", length = 8000, 
                        tmp_group = "tmp_group", str_length = "str_length") {
  len_total <- sum(stringr::str_length(tbl[[text_col]]))
  ratio <- seq(from = 0.8, to = 0.3, by = -0.1)

  for(i in seq_along(ratio)){
    len_split <- round(length * ratio[i], 0)
    n_group <- ceiling(len_total / len_split)
  # print(n_group)  # for debug
    res <- make_groups_sub(tbl, text_col, n_group, tmp_group, str_length)
    len_max <- max(res$str_length)
    if(len_max > length){
      stop(paste0("Max length of text is over the limit\n", "lim: ", length, "\n", "max: ", len_max))
    }

    len_sum_max <- max_sum_str_length(res, tmp_group, str_length)
    if(len_sum_max < length) return(dplyr::select(res, ! dplyr::all_of("str_length")))
  }
  dplyr::select(res, ! dplyr::all_of("str_length"))
}

#' @rdname make_groups
make_groups_sub <- function(tbl, text_col, n_group, tmp_group, str_length) {
  nr <- nrow(tbl)
  res <- 
    tbl |>
    dplyr::mutate({{tmp_group}} := dplyr::ntile(1:nr, n_group)) |>
    dplyr::mutate({{str_length}} := stringr::str_length(.data[[text_col]]))
}

#' @rdname make_groups
max_sum_str_length <- function(tbl, tmp_group, str_length) {
  tbl |>
    dplyr::group_by(.data[[tmp_group]]) |>
    dplyr::summarise(sum = sum(.data[[str_length]])) |>
    dplyr::summarise(max(sum))
}
