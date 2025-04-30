#' @keywords internal
#' @noRd

Excess_na_converter <- function(grouped_data,verbose=T) {
  grouped_data2 <- grouped_data %>%
    dplyr::mutate(dplyr::across(tidyselect::contains(c("NO2","NOx","NO","Ozone","CO")), ~
                                  dplyr::case_when(mean(is.na(.x)) >= 0.25 ~ NA_real_,
                                                   mean(is.na(.x)) < 0.25 ~ .x)))

  if (verbose==T) {
    cat("Before aggregation: converting to NA all the obs. belonging to a group with more than 75% missing values \n")
    for (j in 1:dim(grouped_data2)[2]) {
      cat(paste0("Number of obs. converted to NA for ",colnames(grouped_data2)[j],": ",
                 sum(is.na(grouped_data2[,j]) - is.na(grouped_data[,j])),"\n"))
    }
  }

  return(grouped_data2)
}
