## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(moranajp)

## ----eval = FALSE-------------------------------------------------------------
#  data(neko)
#  neko <- unescape_utf(neko)
#  head(neko)
#  
#  n_match <-
#    list.files(bin_dir) |>
#    stringr::str_count("mecab") |>
#    sum()
#  
#    # if(n_match > 0){
#    #   neko |>
#    #     unescape_utf() |>
#    #     moranajp_all(text_col = "text", bin_dir = bin_dir, iconv = "CP932_UTF-8") |>
#    #     print(n=100)
#    # }

