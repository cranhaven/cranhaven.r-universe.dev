#' A Command-Line Interface for ABGD - Automatic Barcode Gap Discovery
#'
#' @description
#' `abgd_tbl()` returns species partition hypothesis estimated by ABGD software
#' (https://bioinfo.mnhn.fr/abi/public/abgd/).
#'
#' @param infile Path to fasta file.
#' @param exe Path to an ABGD executable.
#' @param haps Optional. A vector of haplotypes to keep into the [`tbl_df`][tibble::tbl_df].
#' @param slope Numeric. Relative gap width (slope). Default to 1.5.
#' @param model An integer specifying evolutionary model to be used. Available options are:
#' \itemize{
#'   \item 0: Kimura-2P
#'   \item 1: Jukes-Cantor (default)
#'   \item 2: Tamura-Nei
#'   \item 3: simple distance (p-distance)
#' }
#' @param outfolder Path to output folder. Default to NULL. If not specified, a temporary location is used.
#' @param webserver A .txt file containing ABGD results obtained from a webserver. Default to NULL.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'abgd'.
#'
#' @details
#' `abgd_tbl()` relies on [system][base::system()] to invoke ABGD software through
#' a command-line interface. Hence, you must have the software available as an executable file on
#' your system in order to use this function properly.
#' `abgd_tbl()` saves all output files in `outfolder` and imports the first recursive partition
#' file generated to `Environment`.
#' Alternatively, `abgd_tbl()` can parse a .txt file obtained from a webserver such as
#' (https://bioinfo.mnhn.fr/abi/public/abgd/abgdweb.html).
#'
#' @return
#' an object of class [`tbl_df`][tibble::tbl_df]
#'
#' @author
#' N. Puillandre,  A. Lambert,  S. Brouillet,  G. Achaz
#'
#' @source
#' Puillandre N., Lambert A., Brouillet S., Achaz G. 2012. ABGD, Automatic Barcode 
#' Gap Discovery for primary species delimitation. Molecular Ecology 21(8):1864-77.
#' 
#' @examples
#' \donttest{
#'
#' #' # get path to fasta file
#' path_to_file <- system.file("extdata/geophagus.fasta", package = "delimtools")
#'
#' # run ABGD
#' abgd_df <- abgd_tbl(
#'   infile = path_to_file,
#'   exe = "/usr/local/bin/abgd",
#'   model = 3,
#'   slope = 0.5,
#'   outfolder = NULL
#' )
#'
#' # check
#' abgd_df
#' }
#'
#' @export
abgd_tbl <- function(infile, exe = NULL, haps = NULL, slope = 1.5, model = 3, outfolder = NULL, webserver = NULL, delimname = "abgd") {
  dname <- rlang::sym(delimname)
  
  # check if `readr` is installed
  rlang::check_installed("readr", reason= "to execute `ABGD` properly.")

  if (!is.null(webserver) && !file.exists(webserver)) {
    cli::cli_abort("Error: Please provide a valid path to an ABGD results file.")
  }

  if (!is.null(webserver) && file.exists(webserver)) {
    delim <- readr::read_delim(webserver, delim = ";", col_names = c(delimname, "labels"), col_types = "c")

    delim <- delim |>
      dplyr::mutate(!!dname := 1:nrow(delim)) |>
      dplyr::mutate(labels = stringr::str_replace_all(labels, "id: ", "")) |>
      tidyr::separate_longer_delim(cols = labels, delim = " ") |>
      dplyr::relocate(labels, .before = !!dname)
    
    if(!is.null(haps)){
      
      delim <- delim |>
        dplyr::filter(labels %in% haps)
    }

    return(delim)
  }

  if (!file.exists(exe)) {
    cli::cli_abort("Error: Please provide a valid path to the ABGD executable file.")
  }

  if (missing(model)) {
    model <- 1

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Evolutionary model not specified. Using p-distance as default model.")
  }

  if (is.null(outfolder)) {
    outfolder <- tempdir()
  }

  if (!dir.exists(outfolder)) {
    cli::cli_abort("Error: Please provide a valid results directory.")
  }


  string_abgd <- glue::glue("{exe} -d {model} -X {slope} -o {outfolder} {infile}")

  res <- system(command = string_abgd, intern = TRUE)

  # fpath <- glue::glue('{outfolder}/{stringr::str_split_fixed(basename(infile), "\\\\.", 2)[,1]}.part.1.txt')
  fpath <- glue::glue("{outfolder}/{tools::file_path_sans_ext(basename(infile))}.part.1.txt")
  
  delim <- readr::read_delim(fpath, delim = ";", col_names = c(delimname, "labels"), col_types = "c")

  delim <- delim |>
    dplyr::mutate(!!dname := 1:nrow(delim)) |>
    dplyr::mutate(labels = stringr::str_replace_all(labels, "id: ", "")) |>
    tidyr::separate_longer_delim(cols = labels, delim = " ") |>
    dplyr::relocate(labels, .before = !!dname)

  if (!is.null(haps)) {
    delim <- delim |> dplyr::filter(labels %in% haps)
  }

  cli::cli_alert_info("ABGD files are located in directory '{outfolder}'.")

  return(delim)
}
