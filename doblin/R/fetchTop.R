#' Fetch Top Barcodes by Maximum Frequency
#'
#' This function retrieves the top `N_LINEAGES` barcodes from a reshaped dataframe,
#' as returned by `reshapeData()`. The top barcodes are assumed to be located in the 
#' first `N_LINEAGES` rows of the reshaped dataframe. This implies that `reshaped_df` 
#' should already be sorted by descending maximum frequency for correct behavior.
#'
#' @param reshaped_df A dataframe returned by `reshapeData()`. Only the first 
#' four columns are used.
#' @param N_LINEAGES An integer specifying the number of barcodes to retrieve.
#'
#' @return A dataframe containing the top `N_LINEAGES`. Used for selecting 
#' dominant lineages for plotting or analysis.
#' 
#' @export
#' @name fetchTop
#' 
#' @examples
#' # Load demo barcode count data (installed with the package)
#' demo_file <- system.file("extdata", "demo_input.csv", package = "doblin")
#' input_dataframe <- readr::read_csv(demo_file, show_col_types = FALSE)
#'
#' # Reshape & sort the data
#' reshaped_df <- reshapeData(input_dataframe)
#'
#' # Fetch the top 10 most abundant barcodes
#' top_barcodes <- fetchTop(reshaped_df, N_LINEAGES = 10)

fetchTop <- function(reshaped_df, N_LINEAGES) {
  
  df_top_max = unique(reshaped_df[,1:4])
  df_top_max = df_top_max[1:N_LINEAGES,]
  
  return(df_top_max)
}
