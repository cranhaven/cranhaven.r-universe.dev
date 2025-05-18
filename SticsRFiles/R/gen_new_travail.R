#' Generates a `new_travail.usm` file from usm XML files content and some
#' switch parameters for lai forcing, parameters forcing and usm chaining.
#'
#' @param usm_data a named list of informations attached to an usm definition
#' (coming from an usms.xml file)
#' @param usm Usm name
#' @param lai_forcing 1, if `lai` is to be read from a daily lai, 0 otherwise
#' input file.
#' @param codesuite 1, if the usm is to be chained with the previous
#' simulated (for getting system state variables), 0 otherwise
#' input file.
#' @param codoptim 1, if parameters are to be read from a `param.sti` file
#' (containing parameters values to be forced), 0 otherwise
#' @param out_dir Directory path where to store the `new_travail.usm` file.
#'
#' @return None
#'
#' @keywords internal
#'
#' @noRd
#'

gen_new_travail <- function(usm_data,
                            usm,
                            workspace,
                            lai_forcing = NULL,
                            codesuite = NULL,
                            codoptim = NULL,
                            out_dir = NULL) {


  data_plt2 <- c()
  if (usm_data$nbplantes > 1)
    data_plt2 <- c("fplt2", "ftec2", "flai2")

  data_order <- c("codesimul", "codoptim", "codesuite", "nbplantes", "nom",
                  "datedebut", "datefin", "finit", "numsol", "nomsol",
                  "fstation",
                  "fclim1", "fclim2", "nbans", "culturean", "fplt1",
                  "ftec1", "flai1", data_plt2)

  if (is.null(out_dir)) out_dir <- workspace

  out_file <- file.path(out_dir, "new_travail.usm")

  p_table <- vector(mode = "character", length = 2 * length(data_order))

  for (p in seq_along(data_order)) {
    idx <- p * 2
    p_table[idx - 1] <- paste0(":", as.character(data_order[p]))
    p_table[idx] <- usm_data[[data_order[p]]]
  }

  ret <- try(writeLines(text = p_table, con = out_file))

  if (methods::is(ret, "try-error")) {
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}


#' Get information attached to a usm from usms.xml, and possibly change
#' some forcing options
#'
#' @param usms_doc xml document object loaded from an usms xml file
#' @param usm Usm name
#' @param workspace Path of a JavaSTICS workspace
#' @param lai_forcing 1, if `lai` is to be read from a daily lai, 0 otherwise
#' input file.
#' @param codesuite 1, if the usm is to be chained with the previous
#' simulated (for getting system state variables), 0 otherwise
#' input file.
#' @param codoptim 1, if parameters are to be read from a `param.sti` file
#' (containing parameters values to be forced), 0 otherwise
#'
#' @return a named list
#'
#' @keywords internal
#'
#' @noRd
#'
#'
get_usm_data <- function(usms_doc,
                         usm,
                         workspace,
                         lai_forcing = NULL,
                         codesuite = NULL,
                         codoptim = NULL) {

  data <- XML::getNodeSet(usms_doc@content,
                          path = paste0("//usm[@nom='", usm, "']"),
                          fun = XML::xmlToList)[[1]]
  n <- names(data)
  n[11] <- "plante1"
  n[12] <- "plante2"
  names(data) <- n

  # forcing codesimul
  # 0: culture, 1: feuille, lai forcing
  data$codesimul <- get_codesimul(as.numeric(data$codesimul))

  if (!is.null(lai_forcing) && lai_forcing %in% c(0, 1))
    data$codesimul <- get_codesimul(lai_forcing)

  # forcing codoptim
  data$codoptim <- 0

  if (!is.null(codoptim) && codoptim %in% c(0, 1))
    data$codoptim <- codoptim

  data$codesuite <- 0
  # forcing codesuite
  if (!is.null(codesuite) && codesuite %in% c(0, 1))
    data$codesuite <- codesuite

  # nbplantes
  data$nbplantes <- as.numeric(data$nbplantes)

  # nom
  data$nom <- usm

  # debut
  data$datedebut <- as.numeric(data$datedebut)

  # fin
  data$datefin <- as.numeric(data$datefin)

  # init
  # already defined

  # soil number
  # not used by STICS !!!!
  data$numsol <- 1

  # nomsol
  # already defined

  # station
  # already defined

  # fclim1
  # already defined

  # fclim2
  # already defined

  # add constraint on culturean
  data$culturean <- as.numeric(data$culturean)
  if (data$culturean != 1)
    data$culturean <- 2

  # add constraint on culturean
  data$culturean <- as.numeric(data$culturean)
  if (data$culturean != 1)
    data$culturean <- 0

  # nbans
  data$nbans <- get_years_number(
    file.path(workspace, c(data$fclim1, data$fclim2))
  )

  data$fplt1 <- data$plante1$fplt

  data$ftec1 <- data$plante1$ftec

  data$flai1 <- data$plante1$flai

  data$fobs1 <- data$plante1$fobs

  if (data$flai1 == "null" || data$flai1 == "defaut.lai")
    data$codesimul <- get_codesimul(0)

  if (data$nbplantes > 1) {
    data$fplt2 <- data$plante2$fplt

    data$ftec2 <- data$plante2$ftec

    data$flai2 <- data$plante2$flai

    data$fobs2 <- data$plante2$fobs
  }

  data[["ftec"]] <- NULL
  data[["fplt"]] <- NULL
  data[["flai"]] <- NULL
  data[["fobs"]] <- NULL
  data[["plante1"]] <- NULL
  data[["plante2"]] <- NULL
  data[[".attrs"]] <- NULL

  return(data)
}


#' Getting the string indicating a forcing lai mode or not
#'
#' @param lai_forcing 0 for forcing mode, 0 otherwise
#'
#' @return a string, either "culture" or "feuille"

#' @keywords internal
#'
#' @noRd
#'
get_codesimul <- function(lai_forcing = 0) {

  if (lai_forcing == 0) return("culture")

  if (lai_forcing == 1) return("feuille")

  stop("Error on lai forcing value: ",
       lai_forcing,
       "\nmIt must be 0 or 1 !")
}


#' Calculating simulation years number
#'
#' @param clim_path character vector of 2 weather data files
#' for the first and the last year
#'
#' @return years number

#' @keywords internal
#'
#' @noRd
#'

get_years_number <- function(clim_path) {

  year1 <- get_year(clim_path = clim_path[1])

  if (clim_path[1] == clim_path[2]) {
    year2 <- year1
  } else {
    year2 <- get_year(clim_path = clim_path[2])
  }

  if (any(is.na(c(year1, year2))))
    stop(
      "Impossible to calculate the number of years from weather data files !"
    )

  return(year2 - year1 + 1)

}

#' Get weather data file year
#'
#' @param clim_path path of a weather data file
#'
#' @return a year (numeric)

#' @keywords internal
#'
#' @noRd
#'

get_year <- function(clim_path) {

  if (!file.exists(clim_path)) stop()

  line_str <- gsub(pattern = "\\t",
                   x = trimws(readLines(con = clim_path, n = 1)),
                   replacement = " ")

  words <- strsplit(line_str, split = " ")[[1]]

  # filtering empty strings
  words <- words[words != ""]
  year_str <- words[2]

  ret <- try(year <- as.numeric(year_str))

  if (methods::is(ret, "try-error")) {
    return(invisible(NA))
  }

  return(year)

}
