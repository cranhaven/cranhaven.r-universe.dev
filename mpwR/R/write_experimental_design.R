#' Create template for experimental design
#'
#' Generation of a exp_design.csv file for using the import option with load_experimental_design.
#'
#' The generated exp_design.csv file can be used as starting point for importing with the load_experimental_design option for mpwR. Example entries are provided. The template file - exp_design.csv - is generated under the specified path.
#'
#' @param path Path to folder where exp_design file is generated.
#' @author Oliver Kardell
#'
#' @return This function returns a csv-file with the following columns:
#' \itemize{
#'  \item analysis_name - name of your analysis.
#'  \item software - name of used software: DIA-NN, MaxQuant, PD, Spectronaut, Generic.
#'  \item path_to_folder - path to analysis folder.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_experimental_design(path = "DIRECTORY_WHERE_FILE_IS_GENERATED")
#' }


write_experimental_design <- function(path) {

  template <- tibble::tibble(
    analysis_name = c("A", "B", "C", "D", "E"),
    software = c("DIA-NN", "MaxQuant", "PD", "Spectronaut", "Generic"),
    path_to_folder = c("PathToAnalysisFolder/A", "PathToAnalysisFolder/B", "PathToAnalysisFolder/C", "PathToAnalysisFolder/D", "PathToAnalysisFolder/E")
  )

  utils::write.csv(template, file = paste0(path, "/exp_design.csv"), row.names = FALSE)

}
