#' Extract Labels and Colors from Species Delimitation Partitions
#'
#' @description
#' `get_delim_cols()` returns a [tbl_df][tibble::tbl_df] format containing 
#' extracted and processed data from [delim_autoplot].
#' 
#' @param p Output from [delim_autoplot].
#' @param delimname A character vector of species delimitation names (optional). 
#' If provided, the function filters the data to only include rows matching such terms. Default to NULL.
#' @param hap_tbl output from [haplotype_tbl] (optional). 
#' If provided, the function will annotate color and fill data for collapsed haplotypes. Default to NULL.
#'
#' @details
#' `get_delim_cols()` is a convenience function to extract labels, species partitions,
#' color and fill data from the output of [delim_autoplot] in a [tbl_df][tibble::tbl_df] 
#' format. It is best used when combined with haplotype information from
#' [haplotype_tbl] or when combined with other metadata, such as GPS coordinates 
#' for map plotting.
#' 
#' @author
#' Pedro S. Bittencourt.
#' 
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#' 
#' @examples
#' 
#' # plot using autoplot
#' p <- delim_autoplot(geophagus_delims, geophagus_beast)
#' 
#' # view 
#' p
#' 
#' # get haplotypes
#' hap_tbl <- haplotype_tbl(geophagus)
#' 
#' # extract colors for consensus
#' get_delim_cols(p, delimname= "consensus", hap_tbl= hap_tbl)
#' 
#' 
#' @export
get_delim_cols <- function(p, delimname= NULL, hap_tbl= NULL){
  
  # get a ggplot_built object
  p_build <- ggplot2::ggplot_build(p)
  
  # extract and bind labels and cols
  if(rlang::is_empty(purrr::pluck(p_build, "data", 2))) {
    
    # get data
    p_data <- p@data
    
    # get geom_tile colors
    p_colors <- p_build |> purrr::pluck("data", 1)
    
    # bind cols and select
    p_bind <- dplyr::bind_cols(p_data, p_colors) |>
      dplyr::select(tidyselect::all_of(c("labels", "method", "spp", "fill", "colour")))
    
  } else {
    
    # get data for complete rows
    p_data <- p@data |> tidyr::drop_na("spp")
    
    p_data_NA <- p@data |> dplyr::filter(is.na(.data$spp))
    
    # get geom_tile colors for complete rows
    p_colors <- p_build |> purrr::pluck("data", 1)
    
    p_colors_NA <- p_build |> purrr::pluck("data", 2)
    
    # bind_cols of complete data with colors
    p_complete <- dplyr::bind_cols(p_data, p_colors)
    
    # bind_cols of missing data with colors
    p_missing <- dplyr::bind_cols(p_data_NA, p_colors_NA)
    
    # bind_rows of complete and missing cases and arrange labels
    p_bind <- dplyr::bind_rows(p_complete, p_missing) |>
      dplyr::select(tidyselect::all_of(c("labels", "method", "spp", "fill", "colour"))) |>
      dplyr::arrange(match(.data$labels, ggtree::get_taxa_name(p[[1]])))
    
  }
  
  if(!is.null(delimname)){
    
    p_bind <- p_bind |> 
      dplyr::filter(.data$method %in% delimname)
    
  }
  
  if(!is.null(hap_tbl)){
    
    collapsed_df <- hap_tbl |>
      dplyr::select(tidyselect::all_of(c("labels", "collapsed"))) |>
      dplyr::mutate(collapsed= stringr::str_split(.data$collapsed, pattern= ", ")) |>
      tidyr::unnest(cols= c("collapsed")) |>
      dplyr::left_join(p_bind, by= "labels", relationship = "many-to-many") |>
      tidyr::drop_na("collapsed") |>
      dplyr::select(-labels) |>
      dplyr::rename(labels= "collapsed") |>
      dplyr::mutate(status= "collapsed")
    
    p_bind <- dplyr::bind_rows(p_bind, collapsed_df) |>
      tidyr::replace_na(list(status = "haplotype"))
    
  }
  
  return(p_bind)
}