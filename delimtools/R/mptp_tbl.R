#' A Command-Line Interface for mPTP - multi-rate Poisson Tree Processes
#'
#' @description
#' `mptp_tbl()` returns species partition hypothesis estimated by mPTP software
#' <https://github.com/Pas-Kapli/mptp>.
#'
#' @param infile Path to tree file in Newick format. Should be dichotomous and rooted.
#' @param exe Path to an mPTP executable.
#' @param outfolder Path to output folder. Default to NULL. If not specified, a temporary location is used.
#' @param method Which algorithm for Maximum Likelihood point-estimate to be used. Available options are:
#' \itemize{
#' \item single Single-rate PTP model. It assumes that every species evolved with the same rate.
#' \item multi Multi-rate mPTP model. It assumes that all species have different evolutionary rates.
#' }
#' @param minbrlen Numeric. Branch lengths smaller or equal to the value provided
#' are ignored from computations. Default to 0.0001. Use [min_brlen]for fine tuning.
#' @param webserver A .txt file containing mPTP results obtained from a webserver. Default to NULL.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'mptp'.
#'
#' @details
#' `mptp_tbl()` relies on [system][base::system] to invoke mPTP software through
#' a command-line interface. Hence, you must have the software available as an executable file on
#' your system in order to use this function properly. `mptp_tbl()` saves all output files in
#' `outfolder` and imports the results generated to `Environment`.
#' If an `outfolder` is not provided by the user, then a temporary location is used.
#' Alternatively, `mptp_tbl()` can parse a file obtained from webserver such as
#' <https://mptp.h-its.org/>.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df]
#'
#' @author
#' Paschalia Kapli, Sarah Lutteropp, Jiajie Zhang, Kassian Kobert, Pavlos Pavlides, Alexandros Stamatakis, Tomáš Flouri.
#'
#' @source
#' Kapli T., Lutteropp S., Zhang J., Kobert K., Pavlidis P., Stamatakis A., Flouri T. 2016.
#' Multi-rate Poisson tree processes for single-locus species delimitation under
#' maximum likelihood and Markov chain Monte Carlo. Bioinformatics 33(11):1630-1638.
#'
#' @examples
#' \donttest{
#'
#' # get path to phylogram
#' path_to_file <- system.file("extdata/geophagus_raxml.nwk", package = "delimtools")
#'
#' # run mPTP in single threshold mode (PTP)
#' ptp_df <- mptp_tbl(
#'   infile = path_to_file,
#'   exe = "/usr/local/bin/mptp",
#'   method = "single",
#'   minbrlen = 0.0001,
#'   delimname = "ptp",
#'   outfolder = NULL
#' )
#'
#' # check
#' ptp_df
#'
#' # run mPTP in multi threshold mode (mPTP)
#'
#' mptp_df <- mptp_tbl(
#'   infile = path_to_file,
#'   exe = "/usr/local/bin/mptp",
#'   method = "single",
#'   minbrlen = 0.0001,
#'   delimname = "mptp",
#'   outfolder = NULL
#' )
#'
#' # check
#' mptp_df
#' }
#'
#' @export
mptp_tbl <- function(infile, exe = NULL, outfolder = NULL, method = c("multi", "single"), minbrlen = 0.0001, webserver = NULL, delimname = "mptp") {
  dname <- rlang::sym(delimname)

  split_vec <- function(vec, sep = "") {
    is.sep <- vec == sep
    split(vec[!is.sep], cumsum(is.sep)[!is.sep] + 1)
  }

  if (!is.null(webserver) && file.exists(webserver)) {
    header <- readLines(webserver)[1]

    if (!grepl("mptp", header)) {
      cli::cli_abort("Error: Looks like your file does not contain valid mPTP results ...")
    }

    lines <- readLines(webserver)[-c(1:6)] |> sub(":", "", x = _)

    mptp.ls <- split_vec(lines)

    mptp.ls <- lapply(mptp.ls, function(x) x[-1])

    if (grepl("single", header)) {
      mptp.df <- do.call(rbind, lapply(names(mptp.ls), function(x) tibble::tibble(labels = mptp.ls[[x]], !!dname := as.integer(unlist(x)))))
    }

    if (grepl("multi", header)) {
      mptp.df <- do.call(rbind, lapply(names(mptp.ls), function(x) tibble::tibble(labels = mptp.ls[[x]], !!dname := as.integer(unlist(x)))))
    }

    return(mptp.df)
  }


  if (!file.exists(exe)) {
    cli::cli_abort("Error: Please provide a valid path to the mPTP executable file.")
  }

  if (is.null(outfolder)) {
    outfolder <- tempdir()
  }

  if (!dir.exists(outfolder)) {
    cli::cli_abort("Error: Please provide a valid results directory.")
  }

  if (missing(method)) {
    cli::cli_abort(c("Please provide a valid option for {.arg method}.",
      "i" = "Available options are {.val multi} or {.val single}."
    ))
  }

  if (method != "single" && method != "multi") {
    cli::cli_abort(c("Please provide a valid option for {.arg method}.",
      "i" = "Available options are {.val multi} or {.val single}."
    ))
  }

  if (method == "multi") {
    string.mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --minbr {minbrlen}")
    res <- system(command = string.mptp, intern = TRUE)
    writeLines(res)

    lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |> sub(":", "", x = _)

    mptp.ls <- split_vec(lines)

    mptp.ls <- lapply(mptp.ls, function(x) x[-1])

    mptp.df <- do.call(rbind, lapply(names(mptp.ls), function(x) tibble::tibble(labels = mptp.ls[[x]], !!dname := as.integer(unlist(x)))))
  }

  if (method == "single") {
    string.mptp <- glue::glue("{exe} --tree_file {infile} --output_file {outfolder}/{basename(infile)}.mptp.{method} --ml --{method} --minbr {minbrlen}")
    res <- system(command = string.mptp, intern = TRUE)
    writeLines(res)

    lines <- readLines(glue::glue("{outfolder}/{basename(infile)}.mptp.{method}.txt"))[-c(1:8)] |> sub(":", "", x = _)

    mptp.ls <- split_vec(lines)

    mptp.ls <- lapply(mptp.ls, function(x) x[-1])

    mptp.df <- do.call(rbind, lapply(names(mptp.ls), function(x) tibble::tibble(labels = mptp.ls[[x]], !!dname := as.integer(unlist(x)))))
  }

  minbrlen.tab <- delimtools::min_brlen(tree = infile, verbose = FALSE)

  minbrlen.est <- minbrlen.tab |>
    dplyr::pull(1) |>
    min() |>
    format(scientific = FALSE)

  if (minbrlen.est < format(minbrlen, scientific = FALSE)) {
    writeLines("\n")

    cli::cli_alert_info(
      "Warning: there are tip-to-tip distances smaller than the specified minimum branch length ({format(minbrlen, scientific=FALSE)}).
      Consider using `delimtools::min_brlen()` to explore branch lengths in your tree."
    )

    writeLines("\n")
  }

  tr <- ape::read.tree(infile)

  if (!ape::is.rooted(tr)) {
    cli::cli_alert_info(
      "Warning: your tree is unrooted and has been subject to default rooting by mptp. Consider rooting your tree."
    )

    writeLines("\n")
  }

  cli::cli_alert_info("mPTP files are located in '{outfolder}'.")

  writeLines("\n")

  return(mptp.df)
}
