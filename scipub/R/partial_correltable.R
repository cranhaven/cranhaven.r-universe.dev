#' Create partial correlation table (with stars for significance)
#'  for scientific publication
#'
#' The `partial_correltable` function can be used to create
#' partial correlation
#' table (with stars for significance) for scientific publication
#' This is intended to summarize partial correlations
#'  between (`vars`) from an input dataset (`data`),
#'  residualizing all `vars` by `partialvars`.
#' This function allows for numeric, binary, and factor
#'  variables as `partialvars`. but only numeric `vars`
#'  are used and any non-numeric `vars` will be dropped.
#'  All other flags follow from `scipub::correltable`.
#' Correlations are based on `stats::cor`, `use` and `method`
#'  follow from that function.
#' Stars indicate significance: `*p<.05, **p<.01, ***p<.001`
#' For formatting, variables can be renamed, numbers can be rounded,
#'  upper or lower triangle only can be selected (or whole matrix),
#'   and empty columns/rows can be dropped if using triangles.
#' For more compact columns, variable names can be numbered in the
#'  rows and column names will be corresponding numbers.
#' Requires `tidyverse` and `stats` libraries.
#' @param data The input dataset.
#' @param vars A list of the names of 2+ variables to correlate,
#'  e.g. c("Age","height","WASI"). All variables must be numeric.
#' @param var_names An optional list to rename the `vars` colnames
#'  in the output table, e.g. c("Age (years)","Height (inches)","IQ").
#'   Must match `vars` in length. If not supplied, `vars` will be printed as is.
#' @param partialvars A list of the names of 1+ variables to partial out,
#'  e.g. c("iq","Sex","Income"). Can include numeric, binary, factor variables.
#' @param partialvar_names An optional list to rename the `partialvars`
#' colnames in the output table, e.g. c("IQ (WASI)","Sex","Income").
#'   Must match `partialvar_names` in length.
#'   If not supplied, `partialvar_names` will be printed as is.
#' @param method Type of correlation to calculate c("pearson", "spearman"),
#'  based on `stats::cor`, default = "pearson".
#' @param use  Use pairwise.complete.obs or restrict to complete cases
#'  c("pairwise", "complete"), based on `stats::cor`, default = "pairwise".
#' @param round_n The number of decimal places to
#'  round all output to (default=2).
#' @param tri Select output formatting c("upper", "lower","all");
#'  KEEP the upper triangle, lower triangle, or all values, default ="upper.
#' @param cutempty If keeping only upper/lower triangle with `tri`,
#'  cut empty row/column, default=FALSE.
#' @param colnum For more concise column names, number row names and
#'  just use corresponding numbers as column names,
#'   default=FALSE, if TRUE overrides cutempty.
#' @param html Format as html in viewer or not (default=F, print in console),
#'  needs library(htmlTable) installed.
#' @return Output Table 1
#' @import 	dplyr
#' @importFrom 	purrr negate
#' @importFrom 	stats lm resid setNames as.formula
#' @import 	stringr
#' @importFrom 	tidyselect all_of
#' @export
#' @examples
#' partial_correltable(
#'   data = psydat, vars = c("Age", "Height", "iq"),
#'   partialvars = c("Sex", "Income"),
#'   tri = "lower", html = TRUE
#' )
#'
#' partial_correltable(
#'   data = psydat, vars = c("Age", "Height", "iq"),
#'   var_names = c("Age (months)", "Height (inches)", "IQ"),
#'   partialvars = c("Sex", "Income"),
#'   tri = "upper", colnum = TRUE, html = TRUE
#' )
#'
#' partial_correltable(
#'   data = psydat, vars = c("Age", "Height", "iq"),
#'   var_names = c("Age (months)", "Height (inches)", "IQ"),
#'   partialvars = c("anxT"),
#'   partialvar_names = "Anxiety",
#'   tri = "all", html = TRUE
#' )
partial_correltable <- function(data, vars = NULL, var_names = vars,
                                partialvars = NULL,
                                partialvar_names = partialvars,
                                method = c("pearson", "spearman"),
                                use = c("pairwise", "complete"),
                                round_n = 2,
                                tri = c("upper", "lower", "all"),
                                cutempty = c(FALSE, TRUE),
                                colnum = c(FALSE, TRUE),
                                html = c(FALSE, TRUE)) {




  # remove duplicates
  var_names <- var_names[!duplicated(vars)]
  vars <- vars[!duplicated(vars)]

  partialvar_names <- partialvar_names[!duplicated(partialvars)]
  partialvars <- partialvars[!duplicated(partialvars)]


  # REMOVE NON-NUMERIC FROM VARS
  if (ncol(data %>%
    dplyr::select(vars) %>%
    dplyr::select_if(purrr::negate(is.numeric))) > 0) {
    warning(paste0(
      "Dropping non-numeric vars: ",
      stringr::str_c(colnames(data %>%
        dplyr::select(vars) %>%
        dplyr::select_if(purrr::negate(is.numeric))),
      sep = " ", collapse = ","
      )
    ), call. = FALSE)

    var_names <- var_names[-which(vars %in%
      names(data %>%
        dplyr::select(vars) %>%
        dplyr::select_if(purrr::negate(is.numeric))))]
    data <- dplyr::select(
      data,
      -names(data %>%
        dplyr::select(vars) %>%
        dplyr::select_if(purrr::negate(is.numeric)))
    )
  }


  if (sum(partialvars %in% vars) > 0) {
    warning(paste0(
      "variable in both vars and partialvars dropped from partialvars: ",
      partialvars[partialvars %in% vars]
    ), call. = FALSE)
    partialvar_names <- partialvar_names[!(partialvars %in% vars)]
    partialvars <- partialvars[!(partialvars %in% vars)]
  }


  # check if vars is missing
  if (length(vars) < 2) {
    stop("not enough vars have been declared", call. = FALSE)
  }
  if (length(partialvars) < 1) {
    stop("no partialvars have been declared", call. = FALSE)
  }


  # Check var_names
  if (length(var_names) != length(vars)) {
    stop("length of var_names does not match length of vars", call. = FALSE)
  }
  if (length(partialvar_names) != length(partialvars)) {
    stop("length of partialvar_names does not match
       length of partialvars", call. = FALSE)
  }





  # select data
  varsall <- c(vars, partialvars)
  x <- data %>%
    dplyr::select(tidyselect::all_of(varsall))

  # if incomplete cases - set all data  na
  if (stringr::str_detect(use[1], "complete")) {
    x[!complete.cases(x), ] <- NA
  } else {

    # Make vars NA when any partialvars missing
    x <- x[complete.cases(x[, partialvars]), ]

    # count missingness in partial variables
    missing_n <- sum(!complete.cases(data %>%
      dplyr::select(tidyselect::all_of(partialvars))))
    missingness <- ifelse(missing_n > 0,
      paste0(
        "N=", missing_n,
        " excluded for missing covariates to be partialled out. * p<.05",
        collapse = ""
      ),
      ""
    )
  }







  # create blank residual data frame
  resdat <- stats::setNames(data.frame(matrix(
    ncol = length(vars),
    nrow = NROW(x)
  )), vars)

  # residualize all vars variables by partialvars
  resdat[, vars] <- lapply(vars, function(a) {
    stats::resid(stats::lm(
      data = x,
      stats::as.formula(paste("scale(", a, ") ~",
        paste(partialvars, collapse = "+"),
        sep = ""
      )),
      na.action = "na.exclude"
    ))
  })


  # use scipub::correltable on residualized data
  tabout <- scipub::correltable(
    data = resdat, var_names = var_names,
    use = use[1],
    method = method[1],
    round_n = round_n,
    tri = tri[1],
    cutempty = cutempty[1],
    colnum = colnum[1],
    html = FALSE
  )





  caption <- stringr::str_replace(
    string = tabout$caption,
    pattern = "correlation coefficients",
    replacement = paste0(
      "partial correlation coefficients controlling for ",
      paste0(partialvar_names, collapse = ", ")
    )
  )
  if (!stringr::str_detect(use[1], "complete")) {
    caption <- stringr::str_replace(
      string = caption,
      pattern = "\\* p<.05",
      replacement = missingness
    )
  }



  rmat <- tabout$table


  # check if htmlTable installed
  if (html[1] == TRUE & !requireNamespace("htmlTable", quietly = TRUE)) {
    warning("library(htmlTable) is needed for HTML format output,
            please install and try again")
    html <- FALSE
  }

  if (html[1] == TRUE) {
    return(print(htmlTable::htmlTable(rmat,
      useViewer = TRUE, caption = caption, pos.caption = "bottom"
    )))
  } else {
    return(list(table = noquote(rmat), caption = caption))
  }
}
