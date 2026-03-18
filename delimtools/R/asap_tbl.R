#' A Command-Line Interface for ASAP - Assemble Species by Automatic Partitioning
#'
#' @description
#' `asap_tbl()` returns species partition hypothesis estimated by ASAP software
#' (https://bioinfo.mnhn.fr/abi/public/asap/).
#'
#' @param infile Path to fasta file.
#' @param exe Path to an ASAP executable.
#' @param haps Optional. A vector of haplotypes to keep into the [tbl_df][tibble::tbl_df].
#' @param model An integer specifying evolutionary model to be used. Available options are:
#' \itemize{
#'   \item 0: Kimura-2P
#'   \item 1: Jukes-Cantor (default)
#'   \item 2: Tamura-Nei
#'   \item 3: simple distance (p-distance)
#' }
#' @param outfolder Path to output folder. Default to NULL. If not specified, a temporary location is used.
#' @param webserver A .csv file containing ASAP results obtained from a webserver. Default to NULL.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'asap'.
#'
#' @details
#' `asap_tbl()` relies on [system][base::system] to invoke ASAP software through
#' a command-line interface. Hence, you must have the software available as an executable file on 
#' your system in order to use this function properly.
#' `asap_tbl()` saves all output files in `outfolder` and imports the first partition
#' file generated to `Environment`.
#' Alternatively, `asap_tbl()` can parse a .csv file obtained from webserver such as 
#' (https://bioinfo.mnhn.fr/abi/public/asap/asapweb.html).
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df]
#'
#' @author
#' Nicolas Puillandre, Sophie Brouillet, Guillaume Achaz.
#' 
#' @source
#' Puillandre N., Brouillet S., Achaz G. 2021. ASAP: assemble species by automatic 
#' partitioning. Molecular Ecology Resources 21:609–620.
#'
#' @examples
#' 
#' \donttest{
#' 
#' #' # get path to fasta file
#' path_to_file <- system.file("extdata/geophagus.fasta", package = "delimtools")
#' 
#' # run ASAP
#' asap_df <- asap_tbl(infile = path_to_file, exe= "/usr/local/bin/asap", model= 3)
#' 
#' # check
#' asap_df
#' 
#' }
#'
#' @export
asap_tbl <- function(infile, exe = NULL, haps = NULL, model = 3, outfolder = NULL, webserver = NULL, delimname = "asap") {
  
  # check if `readr` is installed
  rlang::check_installed("readr", reason= "to execute `ASAP` properly.")
  
  if(!is.null(webserver) && !file.exists(webserver)) {
    
    cli::cli_abort("Error: Please provide a valid path to an ASAP results file.")
    
  }
  
  if(!is.null(webserver) && file.exists(webserver)) {
    
    delim <- readr::read_csv(webserver, col_names = c("labels", delimname), col_types = "c")
    
    if(!is.null(haps)){
      
      delim <- delim |>
        dplyr::filter(labels %in% haps)
    }
    
    return(delim)
    
  }
  
  if(!file.exists(exe)){
    
    cli::cli_abort("Error: Please provide a valid path to the ASAP executable file.")
    
  }
  
  if(missing(model)){
    
    model <- 1
    
    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Evolutionary model not specified. Using p-distance as default model.")
  }
  
  if(is.null(outfolder)){
    
    outfolder <- tempdir()
    
  }
  
  if(!dir.exists(outfolder)){
    
    cli::cli_abort("Error: Please provide a valid results directory.")
    
  }
  
  # ASAP has weird issues when full path for infile is not provided.
  string_asap <- glue::glue("{exe} -d {model} -a -o {outfolder} {tools::file_path_as_absolute(infile)}")
  
  res <- system(command=string_asap, intern = TRUE)
  
  fpath <- glue::glue('{outfolder}/{basename(infile)}.Partition_1.csv')
  
  delim <- readr::read_csv(fpath, col_names = c("labels", delimname), col_types = "ci")
  
  if(!is.null(haps)){
    
    delim <- delim |>
      dplyr::filter(labels %in% haps)
  }
  
  # clean up rogue files
  
  rogue1 <- tools::file_path_as_absolute(glue::glue("{basename(infile)}.res.cvs"))
  rogue2 <- tools::file_path_as_absolute(glue::glue("{basename(infile)}_distmat.txt"))
  
  if(file.exists(rogue1)) {file.copy(rogue1,glue::glue("{tempdir()}/{basename(rogue1)}"))}
  if(file.exists(rogue2)) {file.copy(rogue2,glue::glue("{tempdir()}/{basename(rogue2)}"))}
  
  if(file.exists(rogue1)) {file.remove(rogue1)}
  if(file.exists(rogue2)) {file.remove(rogue2)}
  
  cli::cli_alert_info("ASAP files are located in directory '{outfolder}'.")
  
  return(delim)
}
