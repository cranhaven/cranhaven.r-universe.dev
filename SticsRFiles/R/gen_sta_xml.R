#' @title Generate STICS sta xml file(s) from a template or an input file
#'
#' @param param_df A table (df, tibble) containing the values of the parameters
#' to use (see details)
#' @param file Path of a sta xml file to be used as a template. Optional,
#' if not provided, the function will use a standard template depending
#' on the STICS version.
#' @param out_dir Path of the directory where to generate the file(s).
#' @param stics_version Name of the STICS version. Optional, used if
#' the `file` argument is not provided. In this case the function uses a
#' standard template associated to the STICS version.
#' @param param_table `r lifecycle::badge("deprecated")` `param_table` is no
#'   longer supported, use `param_df` instead.
#' @param sta_in_file `r lifecycle::badge("deprecated")` `sta_in_file` is no
#'   longer supported, use `file` instead.
#' @param out_path `r lifecycle::badge("deprecated")` `out_path` is no
#'   longer supported, use `out_dir` instead.
#'
#' @details Please see `get_stics_versions_compat()` for the full list of
#' STICS versions that can be used for the argument `stics_version`.
#'
#'  `param_df` is a `data.frame` with the following format:
#'
#'  |Sta_name         |  zr| NH3ref| latitude| patm| aclim|
#'  |:----------------|---:|------:|--------:|----:|-----:|
#'  |climatex_sta.xml | 2.5|      0|       49| 1000|    20|
#'  |climatex2_sta.xml | 2.8|      0|       49| 1000|    20|
#'  |climatex3_sta.xml | 2.2|      0|       49| 1000|    20|
#'
#' The first column gives the sta file name (to be generated),
#' all following columns give the parameter value to put in the file,
#' and each line denotes a separate sta file (for e.g. several USMs).
#'
#' The first column name must contain the keyword sta or Sta or STA
#' as a prefix to be detected (as shown in the table extract above).
#'
#' If not given (the default, `NULL`), the function returns the template as is.
#'
#' @return None
#'
#' @examples
#' xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx")
#' sta_param_df <- read_params_table(file = xl_path, sheet_name = "Station")
#' gen_sta_xml(out_dir = tempdir(), param_df = sta_param_df)
#'
#' @export
#'
# TODO: refactor with gen_tec_file, gen_ini_file : same code
gen_sta_xml <- function(param_df,
                        file = NULL,
                        out_dir,
                        stics_version = "latest",
                        param_table = lifecycle::deprecated(),
                        sta_in_file = lifecycle::deprecated(),
                        out_path = lifecycle::deprecated()) {
  if (lifecycle::is_present(param_table)) {
    lifecycle::deprecate_warn("1.0.0",
                              "gen_sta_xml(param_table)",
                              "gen_sta_xml(param_df)")
  } else {
    param_table <- param_df # to remove when we update inside the function
  }
  if (lifecycle::is_present(sta_in_file)) {
    lifecycle::deprecate_warn("1.0.0",
                              "gen_sta_xml(sta_in_file)",
                              "gen_sta_xml(file)")
  } else {
    sta_in_file <- file # to remove when we update inside the function
  }
  if (lifecycle::is_present(out_path)) {
    lifecycle::deprecate_warn("1.0.0",
                              "gen_sta_xml(out_path)",
                              "gen_sta_xml(out_dir)")
  } else {
    out_path <- out_dir # to remove when we update inside the function
  }


  xml_doc_tmpl <- NULL

  if (!base::is.null(sta_in_file)) {
    xml_doc_tmpl <- xmldocument(sta_in_file)
  }


  # detecting sta names column
  param_names <- names(param_table)
  col_id <- grep("^sta", tolower(param_names))
  if (!length(col_id)) {
    stop("The column for identifying sta names has not been found !")
  }
  sta_col <- param_names[col_id]


  xml_docs <- gen_sta_doc(
    xml_doc = xml_doc_tmpl,
    param_table = param_table[, -col_id],
    stics_version = stics_version
  )


  if (!is.list(xml_docs) && methods::is(xml_docs, "xml_document")) {
    xml_docs <- list(xml_docs)
  }


  # Finding non NULL elements in xml_docs (i.e. no errors in doc generation)
  out_idx <- unlist(lapply(xml_docs, base::is.null))

  if (any(out_idx)) {
    message(paste0("\nErrors have been detected while trying to replace",
                   "parameters values in xml documents\n"),
            paste(sum(!out_idx), "files have been generated !\n"))

    # selecting available documents to produce
    xml_docs <- xml_docs[out_idx]
  }

  # No files will be generated
  if (all(out_idx)) {
    return(invisible())
  }


  # checking if out_path exists
  if (!dir.exists(out_path)) {
    stop(paste("The directory does not exist", out_path))
  }

  # defining output files paths
  out_name <- param_table[[sta_col]]
  ids <- grepl("_sta.xml$", out_name)
  if (sum(ids) < length(out_name)) {
    out_name[!ids] <- paste0(param_table[[sta_col]][!ids], "_sta.xml")
  }
  sta_out_file <- file.path(out_path, out_name)

  # checking dimensions
  if (!length(xml_docs) == length(sta_out_file)) {
    stop("Xml output files names must have the same length as table lines ! ")
  }

  # saving files
  # TODO: vectorize the saveXmlDoc method of the xml_document class
  for (f in seq_along(xml_docs)) {
    save_xml_doc(xml_docs[[f]], sta_out_file[[f]])

    delete(xml_docs[[f]])
  }

  if (!base::is.null(xml_doc_tmpl) && inherits(xml_doc_tmpl, "xml_document"))
    delete(xml_doc_tmpl)

}
