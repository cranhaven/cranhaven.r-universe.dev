#' @importFrom readxl read_excel
NULL

#' @title read_ramlegacy
#' @noRd


read_ramlegacy <- function(vers_path = NULL, version = NULL) {

  # Get the names of all the dataframes present in the excel database
  excel_file <- grep("RLSADB.*\\.(xlsx|xls)",
    list.files(vers_path),
    value = TRUE
  )
  excel_path <- file.path(vers_path, excel_file)
  na_vec <- c("NA", "NULL", "_", "none", "N/A", "")
  sheets <- readxl::excel_sheets(excel_path)

  # for older versions read in the all the sheets from the excel file, save the
  # list of dfs as an rds object.
  if (version < 4.40) {

    # read in all the dataframes

    final_lst_dfs <- lapply(seq_along(sheets), function(i) {
      s <- sheets[i]

      # specify column types for problematic sheets
      if (s == "Timeseries") {
        col_type_vec <- c(rep("text", 5), rep("numeric", 1))
      } else if (s == "Timeseries_values_view") {
        col_type_vec <- c(rep("text", 4), rep("numeric", 8))
      } else if (s == "bioparams") {
        col_type_vec <- c(rep("text", 7))
      } else {
        col_type_vec <- NULL
      }
      readxl::read_excel(
        path = excel_path,
        sheet = s, na = na_vec, col_types = col_type_vec
      )
    })
    sheets <- tolower(sheets)
    names(final_lst_dfs) <- sheets

    # For newer version, read in the data frames from the
    # .Rdata file, restructure the list into 3 subsections
  } else {
    # construct rdata path to read in the .RData file
    rdata_file <- grep("\\.RData",
      list.files(vers_path),
      value = TRUE
    )
    rdata_path <- file.path(vers_path, rdata_file)
    dfs_names <- load(rdata_path)
    temp_lst_dfs <- mget(dfs_names)
    # construct a new list of dfs to return
    final_lst_dfs <- vector("list", 26)
    # store the regular dataframes in the first 24 positions in the final
    # list
    for (i in c(1:24)) {
      final_lst_dfs[[i]] <- temp_lst_dfs[[sheets[i]]]
    }
    #
    metadata_df <- temp_lst_dfs[["metadata"]]
    most.used.time.series <- vector("list", 31)
    # get the names of these most used time series dfs
    ts_df_names <- grep("\\.data", names(temp_lst_dfs),
      perl = TRUE,
      value = TRUE
    )
    # store them in the list MostUsedTimeSeries
    for (i in c(1:31)) {
      most.used.time.series[[i]] <- temp_lst_dfs[[ts_df_names[i]]]
    }


    # store metadata and MostUsedTimeSeries in final_lst_dfs
    final_lst_dfs[[25]] <- metadata_df
    final_lst_dfs[[26]] <- most.used.time.series

    names(final_lst_dfs[[26]]) <- tolower(ts_df_names)
    names(final_lst_dfs) <- c(tolower(sheets), "metadata", "most.used.time.series")
  }

  # write final_lst_dfs as rdata object
  write_path <- file.path(
    vers_path,
    paste0("v", version, ".rds")
  )
  saveRDS(final_lst_dfs, file = write_path)
}
