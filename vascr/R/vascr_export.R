#' Export a vascr dataframe
#'
#' @param data.df a vascr dataset to export
#' @param filepath Path to save the dataframe in
#' @param level Level of replication to export, defaults to experiments
#'
#'
#' @importFrom stats setNames
#' @importFrom utils write.csv
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom dplyr mutate select arrange
#' @importFrom tidyr pivot_longer
#'
#' @returns A dataframe in prism format, or writes to file if filepath specified
#' @export
#'
#' @examples
#' filepath = tempfile("test_export", fileext = ".xlsx")
#' to_export = growth.df %>% vascr_subset(unit = c("R", "Rb"), frequency = c(0,4000))
#' vascr_export_prism(to_export, filepath)
#' 
#' vascr_export_prism(to_export, filepath, level = "wells")
#' 
vascr_export_prism = function(data.df, filepath = tempfile("test_export", fileext = ".xlsx"), level = "experiments"){
  
  if(!is.null(filepath)) {rlang::check_installed("writexl")}

comboes = data.df %>% select("Unit", "Frequency") %>%
  distinct() %>%
  mutate(names = paste(.data$Unit, .data$Frequency))

combo = 0

 cli_progress_bar("Exporting units", total = nrow(comboes))

output = foreach(combo = c(1:nrow(comboes)), .final = function(x) setNames(x, comboes$names)) %do%
{
  combodata = comboes[combo,]
  
  if(level == "experiments")
  {
  prismed_expt = data.df %>%
    arrange("Sample", "Experiment") %>%
    vascr_subset(unit = combodata$Unit, frequency = combodata$Frequency) %>%
    vascr_summarise(level = level) %>%
    select("Sample", "Value", "Time", "Experiment") %>%
    mutate(Experiment = paste("[",as.numeric(.data$Experiment),"]", sep = "")) %>%
    mutate(Sample = str_replace_all(.data$Sample, ",", " ")) %>%
    pivot_wider(names_from = c("Sample", "Experiment"), values_from = "Value", names_repair = "minimal", id_cols = c("Time")) %>%
    arrange(.data$Time)
  } else if(level == "wells")
  {
    prismed_expt = data.df %>%
      arrange("Sample", "Experiment") %>%
      vascr_subset(unit = combodata$Unit, frequency = combodata$Frequency) %>%
      vascr_summarise(level = level) %>%
      select("Sample", "Value", "Time", "Experiment", "Well") %>%
      mutate(Experiment = paste("[",as.numeric(.data$Experiment),"]", sep = "")) %>%
      mutate(Sample = str_replace_all(.data$Sample, ",", " ")) %>%
      pivot_wider(names_from = c("Sample", "Experiment", "Well"), values_from = "Value", names_repair = "minimal", id_cols = c("Time")) %>%
      arrange(.data$Time)
  } else
  {
    vascr_notify("error", "Wrong level, can only export to prism as experiments or wells")
  }
  
   colnames(prismed_expt) = colnames(prismed_expt) %>% str_remove("_\\[.*")
   
   cli_progress_update()
   
   prismed_expt
}

cli_process_done()

 if(!is.null(filepath))
 {
  writexl::write_xlsx(output, filepath)
 } else
 {
   return(output)
 }
}



