#' @title Subtract timepoint T0 and remove from data
#' @description This function subtracts the values at timepoint T0 from all other timepoints
#' and removes it from the data.
#' @param input_data A data frame containing columns preferably named as 'Position', 'Value', 
#' 'Experiment','Validity', and 'Timepoint'.
#' @param grouping A character vector specifying the columns to use for grouping.
#'        Defaults to c("Experiment", "Position").
#' @param value The column containing the values to be modified. Defaults to "Value".
#' @param timepoint The column containing the timepoint information. Defaults to "Timepoint".
#' @param validity The column containing validity information. Defaults to "Validity".
#' @return A modified data frame with timepoint T0 subtracted and removed.
#' @details This function modifies the input data frame by subtracting the value at 
#' T0 timepoint from all other timepoints for each plate (i.e. experiment). It then removes the rows with 
#' this timepoint from the data frame.
#' @importFrom dplyr mutate_at group_by_at sym
#' @importFrom rlang :=
#' @export
subtract_T0 <- function(input_data,
                        grouping = c("Group", "Experiment", "Position"),
                        value = "Value",
                        timepoint = "Timepoint",
                        validity = "Validity") {
  # Modify data
  modified_data <- input_data %>%
    group_by_at(vars({{ grouping }})) %>% 
    mutate_at(vars({{ value }}), ~ . - .[.data[[timepoint]] == "T0"]) %>%
    mutate_at(vars({{ value }}), ~if_else(. < 0, 0, .)) %>% 
    dplyr::filter(!!sym(timepoint) != "T0") %>%
    dplyr::filter(!!sym(validity) == "valid") %>%
    ungroup()
  
  return(modified_data)
}
