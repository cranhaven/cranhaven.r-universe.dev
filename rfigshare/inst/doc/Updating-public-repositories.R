## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(rfigshare)
#  article_id <- 3761562
#  deposit_details <- fs_details(article_id)
#  deposit_details$title

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  "OLIdata_YYYY-MM-DD.txt"

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library(dplyr)
#  deposit_files <- unlist(deposit_details$files)
#  deposit_files <- data.frame(split(deposit_files, names(deposit_files)),stringsAsFactors = F)
#  file_id <- deposit_files %>%
#    filter(grepl("^OLIdata_", name)) %>%
#    select(id) %>%
#    .[[1]]

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  fs_delete(article_id, file_id)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  # This file does not exist in these training materials.
#  fs_upload(article_id, paste0("OLIdata_", as.Date(Sys.time())))

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  fs_make_public(article_id)

