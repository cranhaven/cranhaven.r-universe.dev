build_file_path <- function(data_source, chamber = "all", congress = NULL,
                            sheet_type = NULL, local = TRUE, local_dir = ".") {
  chamber_code <- match_chamber(chamber)

  congress_code <- match_congress(congress)

  file_path <- switch(
    tolower(data_source),
    voteview = build_voteview_file_path(sheet_type = sheet_type,
                                        chamber_code = chamber_code,
                                        congress_code = congress_code,
                                        local = local, local_dir = local_dir),
    hvw = build_hvw_file_path(chamber_code = chamber_code,
                              local = local, local_dir = local_dir),
    lhy = build_hvw_file_path(chamber_code = chamber_code,
                              local = local, local_dir = local_dir),
    les = build_les_file_path(les_2 = sheet_type,
                              chamber_code = chamber_code,
                              local = local, local_dir = local_dir),
    "source not implemented"
  )

  if (file_path == "source not implemented") {
    stop("Invalid data source name: ", data_source)
  }

  # if local file doesn't exist, fall back to online
  if (local && !file.exists(file_path)) {
    file_path <- build_file_path(data_source = data_source,
                                 chamber = chamber, congress = congress,
                                 sheet_type = sheet_type, local = FALSE)
  }

  file_path
}

build_voteview_file_path <- function(sheet_type, chamber_code = "HS", congress_code = "all",
                                     local = TRUE, local_dir = ".") {
  voteview_source <- paste0("https://voteview.com/static/data/out/", sheet_type)
  source <- ifelse(local,
                   local_dir,
                   voteview_source)

  paste0(source, "/", chamber_code, congress_code, "_", sheet_type, ".csv")
}

build_hvw_file_path <- function(chamber_code, local = TRUE, local_dir = ".") {
  # no "all" option for HVW
  if (!(chamber_code %in% c("H", "S"))) {
    stop("Invalid `chamber` argument (\"", chamber_code, "\") provided for `get_hvw_data()`.\n",
         "`chamber` must be either House or Senate, not both.")
  }

  if (local) {
    # local files
    source <- local_dir
    file <- ifelse(chamber_code == "H",
                   "HarbridgeYong_Volden_Wiseman_House_Replication.tab",
                   "HarbridgeYong_Volden_Wiseman_Senate_Replication.tab")
  } else {
    # online files
    source <- "https://dataverse.harvard.edu/api/access/datafile"
    file <- ifelse(chamber_code == "H",
                   "6299608",
                   "6299605")
  }

  paste0(source, "/", file)
}

build_les_file_path <- function(chamber_code, les_2 = FALSE, local = TRUE, local_dir = ".") {
  # no "all" option for LES
  if (!(chamber_code %in% c("H", "S"))) {
    stop("Invalid `chamber` argument (\"", chamber_code, "\") provided for `get_les()`.\n",
         "`chamber` must be either House or Senate, not both.")
  }

  les_source <- "https://thelawmakers.org/wp-content/uploads/2023/04"
  source <- ifelse(local,
                   local_dir,
                   les_source)
  chamber_name <- ifelse(chamber_code == "H",
                         "House",
                         "Senate")
  sheet_type <- ifelse(les_2,
                       "117ReducedLES2",
                       "93to117ReducedClassic")

  paste0(source, "/CEL", chamber_name, sheet_type, ".dta")
}

match_chamber <- function(chamber) {
  chamber_code <- dplyr::case_match(tolower(chamber),
                                    c("all", "congress") ~ "HS",
                                    c("house", "h", "hr") ~ "H",
                                    c("senate", "s", "sen") ~ "S",
                                    .default = "HS_default")

  # Warn for invalid chamber argument
  if (chamber_code == "HS_default") {
    warning("Invalid `chamber` argument (\"", chamber, "\") provided. Using `chamber = \"all\"`.")
    chamber_code <- "HS"
  }

  chamber_code
}

#' Get Voteview string for a specified Congress
#'
#' Get a Congress number as a three-digit string.
#' This is the format of Congress numbers in Voteview data file names.
#'
#' If an invalid Congress number (or none) is given, this will return `"all"`.
#'
#' @param congress A Congress number.
#'
#' Valid Congress numbers are integers between 1 and `r current_congress()`
#' (the current Congress).
#'
#'
#' @returns A three-character string.
#'
#' Either three digits between `"001"` and ``r paste0('"', current_congress(), '"')``,
#' or `"all"` in case of an invalid Congress number.
#'
#' @examples
#' match_congress(118)
#' match_congress(1)
#'
#' match_congress(NULL)
#' match_congress(300)
#' match_congress("not a valid number")
#'
#' @noRd
match_congress <- function(congress) {
  ifelse(test = is.numeric(congress) &&
           congress >= 1 &&
           congress <= current_congress(),
         yes = stringr::str_pad(string = as.integer(congress),
                                width = 3, side = "left", pad = 0),
         no = "all")
}
