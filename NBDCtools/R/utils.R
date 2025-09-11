# BIDS sidecars ----------------------------------------------------------------

#' Create BIDS sidecar
#'
#' @description
#' Creates a Brain Imaging Data Structure (BIDS) JSON sidecar file from the
#' metadata (data dictionary and levels table). Returns the JSON object or
#' writes it to a file.
#'
#' @param data tibble. The raw data or data with labels, see
#' [transf_label()]. If the data is not labelled, the function will
#' try to label the data first.
#' @param study character. NBDC study (One of `"abcd"` or `"hbcd"`)
#' @param release character. Release version (Default: `"latest"`).
#' @param var_coding character. the variable coding, one of "values", "labels".
#' If the data is processed with [transf_value_to_label()], please use "labels".
#' @param metadata_description string, the description of the metadata
#' @param path_out character. the path to the output file.
#' If `NULL`, the function will return the json object.
#' @param pretty logical. Whether to pretty print the json.
#'
#' @return the json object or the path to the json file
#' @export
#' @examples
#' \dontrun{
#' data |> create_bids_sidecar()
#' data |> create_bids_sidecar(path_out = "data.json")
#' }
create_bids_sidecar <- function(
  data,
  study,
  release = "latest",
  var_coding = "values",
  metadata_description = "Dataset exported using NBDCtools",
  path_out = NULL,
  pretty = TRUE
) {
  on.exit(gc(), TRUE)
  chk::chk_data(data)
  var_coding <- match.arg(var_coding, c("values", "labels"))
  chk::chk_string(metadata_description)
  if (!is.null(path_out)) {
    chk::chk_string(path_out)
    if (!dir.exists(dirname(path_out))) {
      cli::cli_abort(c("The directory does not exist: ", dirname(path_out)))
    }
  }
  chk::chk_logical(pretty)

  dd <- get_dd(study = study, release = release)

  chk_res <- check_type_label(data, return_flag = TRUE)
  if (!chk_res$transform_label) {
    cli::cli_inform(
      c(i = "The data is not labelled. Trying to label the data before BIDS.")
    )
    data <- transf_label(data, study = study, release = release)
  }

  # removed columns that are not in the data dictionary
  col_names <- check_dd(
    col_names = colnames(data),
    dd = dd,
    silent = TRUE
  )
  data <- data |>
    select(all_of(col_names))

  list_bids <- bids_build_json(
    data = data,
    dd = dd,
    metadata_description = metadata_description,
    var_coding = var_coding
  )
  if (is.null(path_out)) {
    return(jsonlite::toJSON(list_bids, pretty = pretty))
  }
  cli::cli_inform(c(i = "Writing BIDS format to {.file path_out}"))
  jsonlite::write_json(list_bids, path_out, pretty = TRUE, auto_unbox = TRUE)
  path_out
}

#' @noRd
#' @return list list_bids
bids_build_json <- function(
  data,
  dd,
  metadata_description,
  var_coding = "values"
) {
  cli::cli_inform(c(i = "Building BIDS ..."))
  data <- data |>
    select(
      -any_of(union(get_id_cols_abcd(), get_id_cols_hbcd())),
    )
  length_json <- ncol(data) + 1
  # create emtpy list of the length
  list_bids <- vector("list", length_json) |>
    setNames(c(
      "MeasurementToolMetadata",
      names(data)
    ))

  # write meta
  list_bids[["MeasurementToolMetadata"]] <- list(
    "Description" = metadata_description
  )

  # get label and levels
  tbl_col_label <- sjlabelled::get_label(data)
  if (var_coding == "values") {
    tbl_col_level <- sjlabelled::get_labels(
      data,
      values = TRUE,
      attr.only = TRUE,
      drop.na = TRUE
    )
  }

  # write variables
  for (col_name in names(list_bids)[-1]) {
    list_bids[[col_name]] <- list(
      "Description" = tbl_col_label[[col_name]]
    )
    if (var_coding == "values" && length(tbl_col_level[[col_name]]) > 0) {
      col_level_order <- stringr::str_sort(
        names(tbl_col_level[[col_name]]),
        numeric = TRUE
      )
      list_bids[[col_name]]$Levels <- as.list(
        tbl_col_level[[col_name]][col_level_order]
      )
    }
    col_units <- bids_get_units(col_name, dd)
    if (!is.null(col_units) && !is.na(col_units) && col_units != "") {
      list_bids[[col_name]]$Units <- col_units
    }
    list_bids[[col_name]]$Derivative <- bids_get_derivative(col_name, dd)
  }
  list_bids
}

#' @noRd
#' @return string, the unit of the variable or NA
bids_get_units <- function(col_name, dd) {
  dd |>
    filter(name == col_name) |>
    first() |>
    pull(unit)
}

#' @noRd
#' @return boolean, whether the variable is a derivative or not
bids_get_derivative <- function(col_name, dd) {
  type_var <- dd |>
    filter(name == col_name) |>
    first() |>
    pull(type_var)

  switch(
    type_var,
    "summary score" = TRUE,
    "derived item" = TRUE,
    "item" = FALSE,
    "administrative" = FALSE,
    cli::cli_abort(paste0(
      "Unknown type_var: '",
      type_var,
      "' for ",
      col_name
    ))
  )
}


# Convert column names ---------------------------------------------------------

#' Convert column names in a data frame
#'
#' @description
#' This function renames columns in a data frame to another type of column name
#' specified in the data dictionary.
#'
#' For example, this can be used to convert the ABCD column names introduced in
#' the 6.0 release to the previously used column names. If you instead want to
#' convert the column names in a file, use [convert_names_file()].
#'
#' Note: Please use this function with caution and make sure that the data in
#' the converted column is equivalent to the data in the original column. Also,
#' please make sure that the names can be mapped one-to-one. Some variables in
#' the ABCD data dictionary have been collapsed from previous releases and thus
#' might have multiple names in the `name_to` column that map to a single name
#' (see `skip_sep_check` argument below).
#'
#' @param data tibble. The input data frame with columns to be renamed.
#' @param dd tibble. The data dictionary table. One can use [get_dd()] family of
#'   functions to get the data dictionary for a given study and release or
#'   provide a custom data dictionary.
#' @param name_from character. The column name type in the data dictionary that
#'   the columns in `data` currently use (Default: `"name"`). This column
#'   must exist in the data dictionary.
#' @param name_to character. The column name type in the data dictionary
#'   that the columns in `data` should be renamed to. This column
#'   must exist in the data dictionary.
#' @param ignore_cols character vector. The columns to ignore (Default:
#'   identifier columns used in ABCD and HBCD).
#' @param skip_sep_check logical. Whether to skip the check for
#'   `name_to` column's separators validation.
#'   In our official data dictionaries, some columns have
#'   multiple names separated by a `"|"` in the same cell.
#'
#'   For columns with multiple names, it the recommended to use functions like
#'   [tidyr::separate_rows()] to split the names into multiple
#'   rows and decide which name to use for the renaming by filtering the rows,
#'   so that the `name_from` and `name_to` columns are one-to-one
#'   mapping.
#'
#'   If `skip_sep_check = FALSE` (default), the function will check if the
#'   `name_from` or `name_to` columns have the `"|"` separator and will throw
#'   an error if the separator is found. If `skip_sep_check = TRUE`, it means
#'   you understand that character strings with `"|"` inside will be used for
#'   rename mapping, and the function will not check for the separator.
#'
#' @return tibble. The data with renamed column names.
#' @export
#' @examples
#' \dontrun{
#' # rename columns to previous ABCD names used by NDA
#' convert_names_data(
#'   data,
#'   dd = get_dd("abcd"),
#'   name_from = "name",
#'   name_to = "name_nda"
#' )
#'
#' # rename columns to Stata names
#' convert_names_data(
#'   data,
#'   dd = get_dd("abcd"),
#'   name_from = "name",
#'   name_to = "name_stata"
#' )
#' }
convert_names_data <- function(
  data,
  dd,
  name_from = "name",
  name_to,
  ignore_cols = union(get_id_cols_abcd(), get_id_cols_hbcd()),
  skip_sep_check = FALSE
) {
  chk::chk_data(data)
  chk::chk_data(dd)
  chk::chk_string(name_from)
  chk::chk_string(name_to)
  chk::chk_character(ignore_cols)
  chk::chk_subset(name_from, colnames(dd))
  chk::chk_subset(name_to, colnames(dd))
  chk::chk_logical(skip_sep_check)

  if (name_from == name_to) {
    cli::cli_warn(
      "`name_from` and `name_to` are the same, no renaming will be done"
    )
    return(data)
  }

  # Check if all columns in data (except ignore_cols) are in data dictionary
  cols_to_check <- setdiff(names(data), ignore_cols)
  if (!all(cols_to_check %in% dd[[name_from]])) {
    missing_cols <- setdiff(cols_to_check, dd[[name_from]])
    cli::cli_abort(c(
      "Some columns in data do not exist in data dictionary: ",
      paste(missing_cols, collapse = ", "),
      "Please check your data or data dictionary.",
      "Or use `ignore_cols` to ignore these columns."
    ))
  }
  dd <- dd |>
    select(all_of(c(name_from, name_to))) |>
    filter(
      !is.na(!!sym(name_from)),
      !is.na(!!sym(name_to)),
      !!sym(name_from) %in% cols_to_check
    ) |>
    distinct()

  if (!skip_sep_check) {
    n_sep_to <- sum(stringr::str_detect(dd[[name_to]], "\\|"), na.rm = TRUE)
    if (n_sep_to > 0) {
      cli::cli_abort(c(
        "There are {.val {n_sep_to}} rows in {.val {name_to}} column of
        the data dictionary that contain the separator {.code |}. It usually
        means multiple names are stored in the same cell of these rows.",
        "It is recommended to keep one-to-one mapping of the two columns.
        Please clean the data dictionary or set {.code skip_sep_check = TRUE}."
      ))
    }
  }
  # Get the column names from data that exist in data dictionary
  data_cols <- intersect(names(data), dd[[name_from]])
  # Create a named vector for renaming
  mapping <- setNames(
    data_cols,
    dd[[name_to]][match(data_cols, dd[[name_from]])]
  )
  # Rename columns
  data |>
    rename(!!!mapping)
}

#' Convert column names in a file
#'
#' @description This function replaces all matched column names in a file
#' with another type of column name specified in the data dictionary.
#'
#' For example, this function can be used to convert script files that specified
#' previously used column names to the the ABCD column names introduced in the
#' 6.0 release. If you instead want to convert the column names in a data frame,
#' use [convert_names_data()].
#'
#' Note: Please use this function with caution and make sure that the data in
#' the converted column is equivalent to the data in the original column. Also,
#' please make sure that the names can be mapped one-to-one. Some variables in
#' the ABCD data dictionary have been collapsed from previous releases and thus
#' might have multiple names in the `name_from` column that map to a single name
#' (see `skip_sep_check` argument below).
#'
#' @inheritParams convert_names_data
#' @param file_in character. The input file path.
#' @param file_out character. The output file path. If not provided,
#'   defaults to the input file path with a "_converted" suffix.
#' @param skip_sep_check logical. Whether to skip the check for
#'   `name_from` and `name_to` columns' separators validation.
#'   In our official data dictionaries, some columns have
#'   multiple names separated by a `"|"` in the same cell.
#'
#'   For columns with multiple names, it the recommended to use functions like
#'   [tidyr::separate_rows()] to split the names into multiple
#'   rows and decide which name to use for the renaming by filtering the rows,
#'   so that the `name_from` and `name_to` columns are one-to-one
#'   mapping.
#'
#'   If `skip_sep_check = FALSE` (default), the function will check if the
#'   `name_from` or `name_to` columns have the `"|"` separator and will throw
#'   an error if the separator is found. If `skip_sep_check = TRUE`, it means
#'   you understand that character strings with `"|"` inside will be used for
#'   rename mapping, and the function will not check for the separator.
#' @details
#' ### Word matching
#'
#' The function uses word boundaries to match the names in the file.
#' It Uses regex word boundaries (`\\b`) to
#' ensure exact word matches. This prevents partial matches within larger
#' words. For example, matching "age" will not match "cage" or "page".
#'
#' ### Speed
#'
#' The data dictionary is big from [get_dd()], so the function
#' would loop through all the names in the data dictionary.
#' If there are only a few names to replace,
#' it is the best to trim the data dictionary to only those names
#' before using this function.
#'
#' @return character. The path to the output file with converted names,
#' invisible.
#' @export
#' @examples
#' \dontrun{
#' convert_names_file(
#'   file_in = "analysis_script.R",
#'   dd = get_dd("abcd"),
#'   name_from = "name_nda",
#'   name_to = "name"
#' )
#'
#' # Specify custom output file
#' convert_names_file(
#'   file_in = "analysis_script.py",
#'   file_out = "analysis_script_new.py",
#'   dd = get_dd("abcd"),
#'   name_from = "name_nda",
#'   name_to = "name"
#' )
#' }
convert_names_file <- function(
  file_in,
  file_out = NULL,
  dd,
  name_from,
  name_to,
  skip_sep_check = FALSE
) {
  chk::chk_string(file_in)
  if (!is.null(file_out)) chk::chk_string(file_out)
  chk::chk_data(dd)
  chk::chk_subset(c(name_from, name_to), colnames(dd))

  # Check if input file exists
  if (!file.exists(file_in)) {
    cli::cli_abort("Input file {.file {file_in}} does not exist.")
  }

  # Generate output file path if not provided
  if (is.null(file_out)) {
    file_dir <- dirname(file_in)
    file_name <- tools::file_path_sans_ext(basename(file_in))
    file_ext <- tools::file_ext(file_in)
    file_out <- file.path(
      file_dir,
      paste0(file_name, "_converted.", file_ext)
    )
  }

  # Read the input file
  file_content <- readLines(file_in, warn = FALSE)

  # Create mapping from data dictionary
  mapping <- dd |>
    select(all_of(c(name_from, name_to))) |>
    filter(!is.na(!!sym(name_from)), !is.na(!!sym(name_to))) |>
    distinct()
  if (nrow(mapping) == 0) {
    cli::cli_abort(c(
      "No valid mappings found in the data dictionary for
      {.val {name_from}} and {.val {name_to}}."
    ))
  }

  if (!skip_sep_check) {
    # we also want to know how many rows have the separator
    n_sep_from <- sum(stringr::str_detect(
      mapping[[name_from]], "\\|"),
      na.rm = TRUE
    )
    n_sep_to <- sum(
      stringr::str_detect(mapping[[name_to]], "\\|"),
      na.rm = TRUE
    )
    if (n_sep_from > 0 || n_sep_to > 0) {
      cli::cli_abort(c(
        "There are {.val {n_sep_from}} rows in {.val {name_from}} and
        {.val {n_sep_to}} rows in {.val {name_to}} columns of the data
        dictionary that contain the separator {.code |}. It usually
        means multiple names are stored in the same cell of these rows.",
        "It is recommended to keep one-to-one mapping of the two columns.
        Please clean the data dictionary or set {.code skip_sep_check = TRUE}."
      ))
    }
  }

  # Initialize progress bar
  cli::cli_progress_bar(
    "Replacing names",
    total = nrow(mapping)
  )

  for (i in seq_len(nrow(mapping))) {
    old_name <- mapping[[name_from]][i]
    new_name <- mapping[[name_to]][i]
    pattern <- paste0("\\b", stringr::str_escape(old_name), "\\b")
    file_content <- stringr::str_replace_all(file_content, pattern, new_name)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  writeLines(file_content, file_out)

  cli::cli_inform("File converted and saved to: {.file {file_out}}")

  invisible(file_out)
}


# DEAP ontology URLs -----------------------------------------------------------

#' Get DEAP ontology URL
#'
#' @description
#' Retrieves the URL for a position in the DEAP ontology tree. Allows to specify
#' a hierarchy order (including removing hierarchy levels) and a single or
#' multiple end node(s) that will show up as selected (and filter the data
#' dictionary table) when the URL is opened.
#'
#' @param nodes list of character vectors (length 2). A list of parent node &
#'   end node pairs for each of the nodes that should be selected.
#' @param order character vector. The order of hierarchy levels to display in
#'   the ontology tree from the following list:
#'
#'   - `"study"`
#'   - `"domain"`
#'   - `"sub_domain"` (only for ABCD)
#'   - `"source"`
#'   - `"metric"` (only for ABCD)
#'   - `"atlas"` (only for ABCD)
#' @param study character. The name of the NBDC study (one of `"abcd"` or
#'   `"hbcd"`).
#' @param encode logical. Whether or not to encode the URL (Default: `TRUE`). If
#'   `FALSE`, argument `as_link` must also be `FALSE`.
#' @param as_link logical. Whether or not to return the URL formatted as a
#'   hyperlink (Default: `TRUE`).
#' @param link_type character. The type of hyperlink to return (one of `"md"`,
#'   the default, or `"html"`).
#' @param link_name character. The name to use for the hyperlink (Default:
#'   `NULL`, i.e., the URL is used as name)
#' @noRd
#' @return character. The URL or hyperlink for the selected node(s) and study.
#' @keywords internal
#' @examples
#' NBDCtools:::get_url_deap(
#'   nodes = list(c("Standard Variables", "General")),
#'   order = c("study", "domain", "sub_domain", "source", "metric", "atlas"),
#'   study = "abcd",
#'   link_name = "ABCD 'Standard Variables' (standard order; Markdown link)"
#' )
#' NBDCtools:::get_url_deap(
#'   nodes = list(
#'     c("Imaging", "Desikan"),
#'     c("Imaging", "Destrieux")
#'   ),
#'   order = c("study", "domain", "atlas", "sub_domain", "metric"),
#'   study = "abcd",
#'   link_type = "html",
#'   link_name = "Desikan & Destrieux atlases (custom order; HTML link)"
#' )
#' NBDCtools:::get_url_deap(
#'   nodes = list(c("BioSpecimens", "Biological Mother")),
#'   order = c("study", "domain", "source"),
#'   study = "hbcd",
#'   as_link = FALSE # just return the URL, not a link
#' )
get_url_deap <- function(
  nodes,
  order,
  study,
  encode = TRUE,
  as_link = TRUE,
  link_type = "md",
  link_name = NULL
) {
  chk::chk_subset(study, names(get_data_pkg("dds")))
  chk::chk_subset(
    order,
    intersect(
      c(
        "study",
        "domain",
        "sub_domain",
        "source",
        "metric",
        "atlas"
      ),
      names(NBDCtools::get_dd(study))
    )
  )
  chk::chk_list(nodes)
  for (node in nodes) {
    chk::chk_character(node)
    chk::chk_length(node, 2)
  }
  chk::chk_logical(encode)
  if (!encode & as_link) {
    cli::cli_abort("`as_link = TRUE` requires `encode = TRUE`!")
  }
  chk::chk_logical(as_link)
  chk::chk_subset(link_type, c("md", "html"))
  if (!is.null(link_name)) chk::chk_string(link_name)

  url_base <- glue::glue(
    "https://{study}.deapscience.com/?hierarchyOrder=",
    "{{url_order}}&",
    "hierarchy={{url_hierarchy}}",
    "#/my-datasets/create-dataset"
  )
  url_order <- order |>
    stringr::str_replace("sub_domain", "subDomain") |>
    paste(collapse = '","') %>%
    paste0("[\"", ., "\"]")
  url_hierarchy <- nodes |>
    purrr::map(
      ~ glue::glue(
        '["{.x[[1]]}","{.x[[2]]}"]'
      )
    ) |>
    paste(collapse = ",") %>%
    paste0("[", ., "]")

  if (encode) {
    url_order <- utils::URLencode(url_order, reserved = TRUE)
    url_hierarchy <- utils::URLencode(url_hierarchy, reserved = TRUE)
  }
  url <- glue::glue(
    url_base,
    url_order = url_order,
    url_hierarchy = url_hierarchy
  )

  if (is.null(link_name)) {
    link_name <- url
  }
  if (as_link & link_type == "md") {
    url <- glue::glue("[{link_name}]({url})")
  }
  if (as_link & link_type == "html") {
    url <- glue::glue(
      '<a href="{url}" target="_blank">{link_name}</a>'
    )
  }

  url
}

#' Get DEAP ontology URL for a specific table
#'
#' @description
#' Wrapper around `get_url_deap()` to get the URL to open a specific table in
#' the DEAP ontology tree.
#'
#' In addition to the main `get_url_deap_table()` function, there are two
#' study-specific variations:
#'
#' - `get_url_deap_table_abcd()`: for the ABCD study.
#' - `get_url_deap_table_hbcd()`: for the HBCD study.
#'
#' They have the same arguments as the `get_url_deap_table()` function, except
#' that the `study` argument is set to the respective study by default, and
#' should not be set by the user.
#'
#' @param table character. The table name (`table_name` in the data dictionary).
#' @param link_name character. The name to use for the hyperlink (one of
#'   `"name"`, i.e., use the table name, or `"label"`, i.e., use the table
#'   label).
#' @param ... Additional arguments passed to the underlying
#' `get_url_deap_table()` function.
#' @inheritParams get_url_deap
#'
#' @return character. The URL or hyperlink for the specified table.
#' @keywords internal
#' @noRd
#' @examples
#' NBDCtools:::get_url_deap_table("ab_g_dyn", "abcd")
#'
#' NBDCtools:::get_url_deap_table("par_visit_data", "hbcd", link_name = "label")
#'
#' NBDCtools:::get_url_deap_table_abcd("ab_g_dyn")
#'
#' NBDCtools:::get_url_deap_table_hbcd("par_visit_data", link_name = "label")
get_url_deap_table <- function(
  table,
  study,
  as_link = TRUE,
  link_type = "md",
  link_name = "name"
) {
  chk::chk_string(table)
  chk::chk_logical(as_link)
  chk::chk_subset(link_type, c("md", "html"))
  chk::chk_subset(link_name, c("name", "label"))

  dd_table <- get_dd(study) |>
    filter(table_name == table) |>
    slice(1)
  if (nrow(dd_table) == 0) {
    cli::cli_abort(c(
      "Table '{table}' does not exist in the {toupper(study)} data dictionary."
    ))
  }

  order <- intersect(
    c(
      "study",
      "domain",
      "sub_domain",
      "source",
      "metric",
      "atlas"
    ),
    names(NBDCtools::get_dd(study))
  )
  node_parent <- dd_table |>
    select(all_of(order)) |>
    unlist(use.names = FALSE) |>
    na.omit() |>
    last()
  node <- dd_table$table_label[1]

  if (link_name == "name")  {
    link_name <- table
  } else if (link_name == "label")  {
    link_name <- node
  }

  get_url_deap(
    nodes = list(c(node_parent, node)),
    order = order,
    study = study,
    encode = TRUE,
    as_link = as_link,
    link_type = link_type,
    link_name = link_name
  )
}


#' @noRd
#' @keywords internal
get_url_deap_table_abcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_url_deap_table_abcd}. It is set to {.val abcd} by default.")
  }
  get_url_deap_table(study = "abcd", ...)
}

#' @noRd
#' @keywords internal
get_url_deap_table_hbcd <- function(...) {
  args <- list(...)
  if ("study" %in% names(args)) {
    cli::cli_abort("The {.arg study} argument should not be set for
    {.fn get_url_deap_table_hbcd}. It is set to {.val hbcd} by default.")
  }
  get_url_deap_table(study = "hbcd", ...)
}


# type and label internal functions --------------------------------------------

#' Check if the data is type transformed and/or labelled
#'
#' @return If `return_flag` = TRUE, returns a list of flags indicating
#' whether the data is type transformed and/or labelled. If `return_flag` =
#' FALSE, returns an error if the data is not type transformed and/or labelled.
#' @noRd
check_type_label <- function(
  data,
  check_type = TRUE,
  check_label = TRUE,
  return_flag = FALSE
) {
  transform_type <- FALSE
  if (check_type) {
    type_attr <- attr(data, "transform_type")
    transform_type <- !is.null(type_attr) && isTRUE(type_attr)
  }
  transform_label <- FALSE
  if (check_label) {
    label_attr <- attr(data, "transform_label")
    transform_label <- !is.null(label_attr) && isTRUE(label_attr)
  }
  if (return_flag) {
    return(
      list(
        transform_type = transform_type,
        transform_label = transform_label
      )
    )
  }
  if (is.null(transform_type) || !transform_type) {
    cli::cli_abort(
      "The data must be type transformed. Please run `transf_factor()` first."
    )
  }
  if (is.null(transform_label) || !transform_label) {
    cli::cli_abort(
      "The data must be labelled. Please run `transf_label()` first."
    )
  }
}

#' @param dd tibble. The data dictionary table.
#' @param col_names character vector. The column names of the data
#' @param silent logical. Whether to print warning/stop messages.
#' @return A vector of strings, the column names of the data that
#' are in the data dictionary.
#' @noRd
check_dd <- function(dd, col_names, silent = FALSE) {
  in_dd <- col_names %in%
    c(dd$name, union(get_id_cols_abcd(), get_id_cols_hbcd()))

  if (!all(in_dd) && !silent) {
    cli::cli_warn(
      "Skipping the following columns that are not in the data dictionary:"
    )
    col_names[!in_dd] |>
      paste(collapse = ", ") |>
      cli::cli_warn()
    if (length(col_names[in_dd]) == 0) {
      cli::cli_abort(
        "No column is found the data dictionary, please check data"
      )
    }
  }
  col_names[in_dd]
}


# Package checks ---------------------------------------------------------------

#' Check if packages are installed with the correct version
#'
#' @description
#' This function checks if the specified packages are installed with the
#' correct version. If the package is not installed, it raises an error
#' and provides a message with the installation method.
#'
#' @param pkgs A named nested list of packages to check. The names of the list
#'  should be the package names. Each item should also be a list with 0 or
#'  more items. Possible items in the list are:
#'
#'  - `version`: The version of the package to check. If this is provided,
#'    the function will check if the package is installed with equal or greater
#'    version. The format of the version should be a character numeric string,
#'    `"major.minor.patch"` or `"major.minor`, e.g., `"1.0.0"`, `2.0`.
#'  - `remote`: If the package is installed from a remote repository,
#'    list the owner and repo name in `"owner/repo"` format.
#' @param abort_msg A character string specifying the error message to
#' raise if one or more packages are not installed.
#' @param install_method A character string specifying the installation method
#' to format the installation message to the user if the package is not
#' installed. However, if the installation requires to install from a remote
#' repository, like GitHub, the function prefer "renv" if it is available,
#' otherwise "pak" if it is available, otherwise "remotes".
#' @param quiet A logical value indicating whether print messages and throws
#' errors if one more packages are not installed or not having the correct
#' version.
#' @returns If all packages are installed or `quiet = TRUE`,
#' a list of logical values indicating whether each package is installed
#' is returned. The names of the list are the package names.
#' If `quiet = FALSE` and one or more packages are not installed,
#' an error is raised.
#' @noRd
#' @keywords internal
#' @examples
#' pkgs <- list(
#'   dplyr = list(version = "1.0.0"),
#'   R6 = list(),
#'   mypkg1 = list(),
#'   mypkg2 = list(version = "0.1.0", remote = "owner/mypkg2")
#' )
#' try(check_pkgs(pkgs))
#' try(check_pkgs(pkgs, quiet = TRUE))
#' try(check_pkgs(
#'   pkgs,
#'   abort_msg = "This function requires following packages"
#' ))
check_pkgs <- function(
  pkgs,
  abort_msg = "One or more packages are not installed",
  install_method = c("builtin", "pak", "renv", "remotes"),
  quiet = FALSE
) {
  chk::chk_list(pkgs)
  chk::chk_string(abort_msg)
  chk::chk_logical(quiet)
  install_method <- match.arg(install_method)
  if (length(pkgs) == 0) {
    cli::cli_abort(
      "No packages to check. Please provide a list of packages to check."
    )
  }
  if (any(is.null(names(pkgs))) || any(names(pkgs) == "")) {
    cli::cli_abort(
      "Package names must be provided as named list names."
    )
  }
  if (any(duplicated(names(pkgs)))) {
    cli::cli_abort(
      "Package names must be unique.
      Please provide a list of packages with unique names."
    )
  }
  check_res <- lapply(names(pkgs), function(pkg_name) {
    pkg <- pkgs[[pkg_name]]
    if (!is.list(pkg)) {
      cli::cli_abort(
        "Package {pkg} must be a list."
      )
    }
    if (
      !is.null(pkg$remote) &&
        !stringr::str_detect(pkg$remote, "^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")
    ) {
      cli::cli_abort(
        "Package {pkg_name} remote {pkg$remote} is not a valid format."
      )
    }

    installed <- find.package(pkg_name, quiet = TRUE) %>%
      length(.) >
      0
    if (!installed) {
      return(FALSE)
    }

    if (!is.null(pkg$version)) {
      pkg$version <- package_version(pkg$version)
      if (is.na(pkg$version)) {
        cli::cli_abort(
          "Package {pkg_name} version {pkg$version} is not a valid version."
        )
      }
      if (utils::packageVersion(pkg_name) < pkg$version) {
        return(FALSE)
      }
    }

    TRUE
  }) |>
    setNames(names(pkgs))
  uninstalled <- names(check_res)[!unlist(check_res)]
  if (length(uninstalled) > 0 && !quiet) {
    missing_info <- lapply(uninstalled, function(pkg_name) {
      glue::glue(
        "`{pkg_name}` ",
        ifelse(
          is.null(pkgs[[pkg_name]]$version),
          "",
          glue::glue(">= ", pkgs[[pkg_name]]$version)
        )
      )
    }) |>
      unlist() |>
      glue::glue_collapse(sep = ", ", last = " and ")
    remote_info <- lapply(uninstalled, function(pkg_name) {
      if (is.null(pkgs[[pkg_name]]$remote)) {
        return(pkg_name)
      }
      pkgs[[pkg_name]]$remote
    }) |>
      unlist()
    # can't use builtin install.packages if remote is required
    if (
      install_method == "builtin" &&
        any(stringr::str_detect(remote_info, "/"))
    ) {
      install_method <- if (Sys.getenv("RENV_DEFAULT_R_ENVIRON") != "") {
        "renv"
      } else if (length(find.package("pak", quiet = TRUE)) > 0) {
        "pak"
      } else {
        "remotes"
      }
    }
    install_text <- switch(
      install_method,
      "renv" = "renv::install",
      "pak" = "pak::pak",
      "builtin" = "install.packages",
      "remotes" = "remotes::install_github"
    )
    install_info <- glue::glue(
      "Please install the missing packages with: \n\n",
      glue::glue(
        install_text,
        '(c("',
        glue::glue_collapse(
          remote_info,
          sep = '", "'
        ),
        '"))'
      )
    )
    cli::cli_abort(c(
      abort_msg,
      "x" = missing_info,
      "i" = install_info
    ))
  }

  check_res
}

# data pkg related --------------------------------------------------------

#' Check if the NBDCtoolsData package is installed
#'
#' @param quiet logical. If `TRUE`, suppresses messages and errors if the
#' NBDCtoolsData package is not installed.
#' @keywords internal
#' @noRd
check_data_pkg_installed <- function(quiet = FALSE) {
  check_pkgs(
     pkgs = list(
       NBDCtoolsData = list(
         remote = "nbdc-datahub/NBDCtoolsData"
       )
    ),
    abort_msg = "NBDCtoolsData package is not installed",
    quiet = quiet
  )
}

#' Get data from NBDCtoolsData package
#' @description
#' A workaround to use dev packge without getting the warning
#' @param x character. The name of the data to get.
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' get_data_pkg("dds")
#' get_data_pkg("levels")
#' get_data_pkg("sessions")
#' }
get_data_pkg <- function(x = c("dds", "levels", "sessions")) {
  x <- rlang::arg_match(x)
  data_name <- paste0("lst_", x)
  return(get(data_name, envir = asNamespace("NBDCtools")))
}


# CRAN checks ------------------------------------------------------------------
#' @noRd
#' @return logical. Whether the package is being checked on CRAN.
is_on_cran <- function() {
  (!is.na(Sys.getenv("_R_CHECK_CRAN_INCOMING_", unset = NA))) && !interactive()
}

# Memory and loading time estimation -------------------------------------------

# credit: https://stackoverflow.com/a/76664004/13002643
#' @noRd
#' @keywords internal
get_available_memory <- function(){
  # Get operating system
  OS <- tolower(Sys.info()["sysname"])

  # Branch based on OS
  if(OS == "windows"){ # Windows

    # System information
    system_info <- system("systeminfo", intern = TRUE)

    # Get available memory
    value <- system_info[
      grep("Available Physical Memory", system_info)
    ]

    # Remove extraneous information
    value <- gsub("Available Physical Memory: ", "", value)
    value <- gsub("\\,", "", value)

    # Convert to bytes
    value_split <- unlist(strsplit(value, split = " "))

    # Check for second value
    bytes <- as.numeric(value_split[1]) * switch(
      value_split[2],
      "KB" = 1e03,
      "MB" = 1e06,
      "GB" = 1e09
    )

  } else if(OS == "linux") { # Linux

    # Split system information
    info_split <- strsplit(system("free -b", intern = TRUE), split = " ")

    # Remove "Mem:" and "Swap:"
    info_split <- lapply(info_split, function(x){gsub("Mem:", "", x)})
    info_split <- lapply(info_split, function(x){gsub("Swap:", "", x)})

    # Get actual values
    info_split <- lapply(info_split, function(x){x[x != ""]})

    # Bind values
    info_split <- do.call(rbind, info_split[1:2])

    # Get free values
    bytes <- as.numeric(info_split[2, info_split[1,] == "free"])

  } else { # Mac

    # System information
    system_info <- system("top -l 1 -s 0 | grep PhysMem", intern = TRUE)
    # extract the "unused" value
    unused <- gsub(" .*,", "", system_info)

    # Get values only
    value <- gsub("PhysMem: ", "", unused)
    value <- gsub(" unused.", "", value)

    # Check for bytes
    if(grepl("M", value)){
      bytes <- as.numeric(gsub("M", "", value)) * 1e06
    }else if(grepl("G", value)){
      bytes <- as.numeric(gsub("G", "", value)) * 1e09
    }else if(grepl("K", value)){
      bytes <- as.numeric(gsub("K", "", value)) * 1e03
    }
  }

  # Return GB
  return(bytes/1024^3)

}

#' Estimate loading time and RAM usage for a given number of variables
#'
#' @description This function estimates the time and RAM usage for loading
#' a NBDC dataset with a given number of variables based on pre-trained
#' benchmark models.
#'
#' @param n_vars integer. must be a positive integer between 1 and 90000.
#'
#' @returns An list containing the estimated time in
#' seconds and RAM usage in GB, as well as human-readable formats for both.
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' estimate_loading(1000)
#' estimate_loading(50000)
#' }
estimate_loading <- function(
  n_vars
) {
  n_vars <- as.integer(n_vars)
  chk::chk_integer(n_vars)
  chk::chk_range(n_vars, c(1, 90000))

  est_time <- ifelse(
    n_vars <= 1000,
    stats::predict(
      NBDCtools::benchmark_models$time_small,
      newdata = data.frame(n_var = n_vars),
      type = "response"
    ),
    stats::predict(
      NBDCtools::benchmark_models$time_large,
      newdata = data.frame(n_var = n_vars),
      type = "response"
    )
  )

  est_ram <- stats::predict(
    NBDCtools::benchmark_models$ram,
    newdata = data.frame(n_var = n_vars),
    type = "response"
  )
  est_ram <- ifelse(
    n_vars < 100,
    max(0.000984168 * n_vars, est_ram), # inaccurate for very low, < 100 fix here
    est_ram
  )

  time_readable <- if (est_time < 60) {
    c(est_time, "seconds")
  } else if (est_time < 3600) {
    c(est_time / 60, "minutes")
  } else {
    c(est_time / 3600, "hours")
  }
  ram_readable <- if (est_ram  < 1) {
    c(est_ram * 1024, "MB")
  } else {
    c(est_ram, "GB")
  }

  list(
    time = est_time,
    ram = est_ram,
    time_readable = time_readable,
    ram_readable = ram_readable
  )
}
