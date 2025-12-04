#' Create Table1 of group summary with stats for scientific publication
#'
#' The `FullTable1` function can be used to create a Table1 for
#' scientific publication. This is intended to summarize demographic
#' and other variables (`vars`) split by a grouping variable (`strata`)
#' from an input dataset (`data`).
#' Continuous variables will be summarized as mean (SD)
#' and tested across groups using t-test or ANOVA (for 3+ level `strata`).
#' Categorical variables will be summarized as N (%)
#' and tested across groups as chi-squared.
#' Effect sizes for group differences will be calculated as Cohen's d,
#'  partial eta-squared, Odds Ratio, Cramer's V depending on the test.
#' Requires `tidyverse` and `stats` libraries.
#' @param data The input dataset (will be converted to tibble).
#' @param strata The grouping variable of interest (converted to factor),
#'  if NULL will make one column table.
#' @param vars A list of variables to summarize, e.g. c("Age","sex","WASI").
#' @param var_names An optional list to rename the variable colnames in the
#' output table, e.g. c("Age (years)","Sex","IQ"). Must match `vars` in length.
#' If not supplied, `vars` will be printed as is.
#' @param factor_vars An optional list of variables from `vars` to use
#' as class factor, e.g. c("sex"). Note that any character, factor, or
#' logical class variables will be summarized as categorical by default.
#' @param round_n The number of decimal places to round output to (default=2).
#' @param es_col Include a column for effect size
#' of group difference? (default=T).
#' @param p_col Include a column for p-value of
#'  group difference? (default=TRUE).
#' @param stars Where to include stars indicating
#' significance of group differences.
#' Options: "col"=separate column (default), "name"= append to variable name,
#' "stat"= append to group difference statistic, "none" for no stars.
#' @param html Format as html in viewer or not
#'  (default=FALSE, print in console),
#'  needs library(htmlTable) installed.
#' @return Output Table 1
#' @import 	dplyr
#' @importFrom 	purrr map_dfc set_names
#' @importFrom 	stats anova aov chisq.test
#'  complete.cases fisher.test sd setNames t.test
#' @importFrom 	stringr str_c
#' @importFrom 	tibble add_row
#' @import 	tidyr
#' @importFrom 	tidyselect all_of
#' @export
#' @examples
#' FullTable1(
#'   data = psydat,
#'   vars = c("Age", "Height", "depressT"), strata = "Sex"
#' )
#' FullTable1(
#'   data = psydat,
#'   vars = c("Age", "Height", "depressT"), strata = "Sex"
#' )
#' FullTable1(
#'   data = psydat, vars = c("Age", "Sex", "Height", "depressT"),
#'   var_names = c("Age (months)", "Sex", "Height (inches)", "Depression T"),
#'   strata = "Income", stars = "name", p_col = FALSE
#' )
#' tmp <- FullTable1(data = psydat,
#'   vars = c("Age", "Height", "depressT"), strata = "Sex")
#'   tmp$caption <- "Write your own caption"
#'   #print(htmlTable(x$table, useViewer=T, rnames=F,caption=x$caption, pos.caption="bottom"))

FullTable1 <- function(data, strata = NULL, vars = NULL,
                       var_names = vars, factor_vars = NULL,
                       round_n = 2, es_col = c(TRUE, FALSE),
                       p_col = c(TRUE, FALSE),
                       stars = c("col", "name", "stat", "none"),
                       html = c(FALSE, TRUE)) {
  es <- p <- Stat <- Variable <- sig <- NULL

  # set df to tibble
  data <- dplyr::as_tibble(data)

  # if null strata
  if (is.null(strata)) {
    es_col <- FALSE
    p_col <- FALSE
    stars <- "none"
    strata <- "onecol"
    data$onecol <- "Sample"
    data$onecol <- as.factor(data$onecol)
  }




  # if vars is missing, use all except strata
  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% strata]
    var_names <- vars
  }

  # Check var_names
  if (length(var_names) != length(vars)) {
    stop("length of var_names does not match length of vars", call. = FALSE)
  }

  # subset columns to relevant data
  data_edit <- data %>%
    dplyr::select(tidyselect::all_of(c(vars, strata)))
  # convert any listed factors to factor just in case
  data_edit <- data_edit %>%
    dplyr::mutate_at(.vars = c(factor_vars, strata), .funs = list(factor))

  # convert any character to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(is.character)) > 0) {
    warning(paste0(
      "Character class variables converted to factor: ",
      stringr::str_c(names(data_edit %>%
        dplyr::select_if(is.character)),
      sep = " ", collapse = ","
      )
    ), call. = FALSE)
    data_edit <- data_edit %>%
      dplyr::mutate_if(is.character, .funs = list(factor))
  }
  # convert any logical to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(is.logical)) > 0) {
    warning(paste0(
      "Logical class variables converted to factor: ",
      stringr::str_c(names(data_edit %>%
        dplyr::select_if(is.logical)),
      sep = " ", collapse = ","
      )
    ), call. = FALSE)
    data_edit <- data_edit %>%
      dplyr::mutate_if(is.logical, .funs = list(factor))
  }
  # convert any ordered to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(is.ordered)) > 0) {
    warning(paste0(
      "Ordered class variables converted to factor: ",
      stringr::str_c(names(data_edit %>%
        dplyr::select_if(is.ordered)),
      sep = " ", collapse = ","
      )
    ), call. = FALSE)
    data_edit <- data_edit %>%
      dplyr::mutate_if(is.ordered, factor, ordered = FALSE)
  }
  # factorize any remaining variables with only 2 distinct options & warn
  if (ncol(data_edit %>%
    dplyr::select_if(function(col) {
      is.factor(col) == FALSE &
        dplyr::n_distinct(col, na.rm = TRUE) == 2
    })) > 0) {
    warning(paste0(
      "Variables with only two distinct values converted to factor: ",
      stringr::str_c(names(data_edit %>%
        dplyr::select_if(function(col) {
          is.factor(col) == FALSE &
            dplyr::n_distinct(col, na.rm = TRUE) == 2
        })),
      sep = " ", collapse = ","
      )
    ), call. = FALSE)
    data_edit <- data_edit %>%
      dplyr::mutate_if(function(col) {
        is.factor(col) == FALSE &
          dplyr::n_distinct(col, na.rm = TRUE) == 2
      }, factor)
  }

  # get list of factors
  factor_vars <- names(data_edit %>%
    dplyr::select_if(is.factor) %>%
    dplyr::select(-c(tidyselect::all_of(strata))))

  # drop any missing strata
  if (strata != "onecol") {
    if (sum(is.na(data[[strata]])) > 0) {
      warning(paste0(
        "N=", sum(is.na(data[[strata]])),
        " missing/NA in grouping variable: ", strata
      ), call. = FALSE)
      data_edit <- data_edit %>% tidyr::drop_na(tidyselect::all_of(strata))
    }
  }
  # check if all one type of variable
  type <- dplyr::case_when(
    length(factor_vars) == 0 ~ "numeric",
    length(factor_vars) == length(vars) ~ "factor",
    TRUE ~ "mixed"
  )





  ### create sub-function ###
  grouptests <- function(datafile, groupvar, outcome, ...) {
    perc <- NULL
    y <- datafile[[outcome]]
    x <- datafile[[groupvar]]
    # set group names with N
    grplvl <- stringr::str_c(levels(datafile[[groupvar]]), " (N=", (datafile %>%
      dplyr::group_by_at(groupvar) %>%
      dplyr::select(tidyselect::all_of(groupvar)) %>%
      dplyr::tally())$n, ")", sep = "")


    tableout <- c("Variable", grplvl, "Stat", "p", "sig", "es") %>%
      purrr::map_dfc(stats::setNames, object = list(character())) %>%
      tibble::add_row()


    # IF NUMERIC
    if (is.numeric(y)) {
      tableout$Variable <- var_names[which(vars == outcome)]

      # get mean (SD) per group
      tableout[, grplvl] <- as.data.frame(t((datafile %>%
        dplyr::group_by_at(groupvar) %>%
        dplyr::select_at(outcome) %>%
        dplyr::summarise(sd = sd(.data[[outcome]], na.rm = T),
                         mean = mean(.data[[outcome]], na.rm = T),
                         .groups = 'drop') %>%
        dplyr::mutate_at(c("mean", "sd"), round, round_n) %>%
        tidyr::unite("col", mean, sd, sep = " (") %>%
        dplyr::mutate(col = stringr::str_c(col, ")")))[2]))

      # IF 2 LEVEL
      if (groupvar != "onecol" & sum(table(x, !is.na(y))[, "TRUE"] > 0) == 2) {
        testtype <- ifelse(type == "mixed", "t=", "")
        # calcualte t-test & p-value
        tableout$Stat <- paste0(testtype,
          format(round(
            -1 * stats::t.test(y ~ x)$statistic,
            round_n
          ), nsmall = round_n),
          sep = ""
        )
        # -1* to flip so t direction is g2>g1
        p <- stats::t.test(y ~ x)$p.value

        # calculate effect size - cohens d
        estype <- ifelse(type == "mixed", "d=", "")
        tableout$es <- paste0(estype, format(round(
          (datafile %>%
            dplyr::group_by_at(groupvar) %>%
            dplyr::select_at(outcome) %>%
             dplyr::summarise(sd = sd(.data[[outcome]], na.rm = T),
                              mean = mean(.data[[outcome]], na.rm = T),
                              .groups = 'drop') %>%
             tidyr::drop_na() %>%
            dplyr::mutate(d = (mean[2] - mean[1]) /
              (sqrt((sd[2]^2 + sd[1]^2) / 2))))[[1, "d"]],
          round_n
        ), nsmall = round_n))
        # IF MORE THAN 2 LEVELS
      } else if (groupvar != "onecol" & sum(table(x, !is.na(y))[, "TRUE"] > 0) > 2) {
        testtype <- ifelse(type == "mixed", "F=", "")
        # calcualte anova & p-value
        tableout$Stat <- paste0(testtype,
          format(round(
            summary(stats::aov(y ~ x))[[1]][[4]][1],
            round_n
          ), nsmall = round_n),
          sep = ""
        )
        p <- summary(stats::aov(y ~ x))[[1]][[5]][1]

        # calculate effect size - cohens d
        estype <- ifelse(type == "mixed", paste0("\u03B7", "2="), "")
        tableout$es <- paste0(
          estype,
          format(round(
            ((stats::anova(stats::aov(y ~ x))[1, 2] /
              sum(stats::anova(stats::aov(y ~ x))[, 2]))),
            round_n
          ),
          nsmall = round_n
          )
        )
      }




      # IF FACTOR
    } else {
      if (groupvar != "onecol") {
        p <- stats::chisq.test(y, x)$p.value
        testtype <- ifelse(type == "mixed", paste0("\u03C7", "2="), "")
        tableout$Stat <- paste0(testtype,
          format(round(stats::chisq.test(y, x)$statistic, round_n),
            nsmall = round_n
          ),
          sep = ""
        )
      }

      # IF 2 LEVEL
      if (length(levels(y)) == 2) {
        lvl2 <- levels(y)[2]
        tableout$Variable <- paste0(
          var_names[which(vars == outcome)],
          " (", lvl2, ")"
        )
        # get N (%)
        tableout[, grplvl] <- as.data.frame(t((datafile %>%
          tidyr::drop_na(tidyselect::all_of(outcome)) %>%
          dplyr::group_by_at(c(groupvar, outcome), .drop=FALSE) %>%
          dplyr::select_at(outcome) %>%
          tally() %>%
          dplyr::ungroup() %>%
          dplyr::group_by_at(c(groupvar)) %>%
          dplyr::mutate(perc = 100 * n / sum(n)) %>%
          dplyr::mutate_at(c("perc"), round, round_n) %>%
          tidyr::unite("col", n, perc, sep = " (") %>%
          dplyr::mutate(col = stringr::str_c(col, "%)")) %>%
          dplyr::filter(base::get(outcome) == lvl2))$col))



        # calculate effect size - odds ratio
        if (sum(table(x, !is.na(y))[, "TRUE"] > 0) == 2 & groupvar != "onecol") {
          estype <- ifelse(type == "mixed", "OR=", "")
          tableout$es <- paste0(
            estype,
            format(round(stats::fisher.test(x, y)$estimate, round_n),
              nsmall = round_n
            )
          )
        } else if (sum(table(x, !is.na(y))[, "TRUE"] > 0) > 2 & groupvar != "onecol") {
          # calculate effect size - cramer v
          estype <- ifelse(type == "mixed", "V=", "")
          tableout$es <- paste0(
            estype,
            format(round(sqrt((stats::chisq.test(y, x)$statistic) /
              (sum(stats::complete.cases(cbind(y, x))) *
                stats::chisq.test(y, x)$parameter)), round_n),
            nsmall = round_n
            )
          )
        }

        # IF MORE THAN 2 LEVELS
      } else {
        tableout$Variable <- outcome
        tableout[2:(1 + length(levels(y))), grplvl] <- datafile %>%
          tidyr::drop_na(tidyselect::all_of(outcome)) %>%
          dplyr::group_by_at(c(groupvar, outcome), .drop=FALSE) %>%
          dplyr::select_at(outcome) %>%
          tally() %>%
          dplyr::ungroup() %>%
          dplyr::group_by_at(c(groupvar)) %>%
          dplyr::mutate(perc = 100 * n / sum(n)) %>%
          dplyr::mutate_at(c("perc"), round, round_n) %>%
          tidyr::unite("col", n, perc, sep = " (") %>%
          dplyr::mutate(col = stringr::str_c(col, "%)")) %>%
          tidyr::spread(groupvar, "col") %>%
          dplyr::select(-c(tidyselect::all_of(outcome)))

        tableout$Variable[2:(1 + length(levels(y)))] <- levels(y)

        # calculate effect size - cramer v
        estype <- ifelse(type == "mixed", "V=", "")
        if (groupvar != "onecol") {
          tableout$es[1] <- paste0(
            estype,
            format(round(sqrt((stats::chisq.test(y, x)$statistic) /
              (sum(stats::complete.cases(cbind(y, x))) *
                stats::chisq.test(y, x)$parameter)), round_n),
            nsmall = round_n
            )
          )
        }
      }
    }

    # set p for all test types
    if (groupvar != "onecol" & !is.null(p)) {
      tableout$sig[1] <- ifelse(p < .001, "***",
        ifelse(p < .01, "**", ifelse(p < .05, "*", ""))
      )
      tableout$p[1] <- ifelse(p < .001, "<.001",
        ifelse(p < .01, sub(format(round(p, 3), nsmall = 3),
          pattern = "0.", replacement = "."
        ),
        sub(format(round(p, 2), nsmall = 2),
          pattern = "0.", replacement = "."
        )
        )
      )
    }

    return(tableout)
  }






  # iterate on all variables
  finaltable <- do.call("rbind", lapply(
    vars,
    function(x) {
      grouptests(
        datafile = data_edit,
        groupvar = strata,
        outcome = x
      )
    }
  ))




  # FINAL FORMATTING
  # remove es if not requested
  if (!es_col[1]) {
    finaltable <- finaltable %>% dplyr::select(-c(es))
  }
  # remove p if not requested
  if (!p_col[1]) {
    finaltable <- finaltable %>% dplyr::select(-c(p))
  }
  # remove p if not requested
  if (strata == "onecol") {
    finaltable <- finaltable %>% dplyr::select(-c(Stat))
  }

  if (stars[1] == "name") {
    finaltable <- finaltable %>%
      tidyr::unite(Variable, Variable, sig, sep = " ")
  } else if (stars[1] == "stat") {
    finaltable <- finaltable %>% tidyr::unite(Stat, Stat, sig, sep = " ")
  } else if (stars[1] == "none") {
    finaltable <- finaltable %>% dplyr::select(-c(sig))
  }


  # remove missing cells
  finaltable[is.na(finaltable)] <- "-"
  finaltable[finaltable == "NaN (NA)"] <- "-"

  # caption
  missing_n <- data_edit %>%
    dplyr::select(tidyselect::all_of(vars)) %>%
    purrr::set_names(var_names) %>%
    dplyr::mutate_all(is.na) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::select_if(function(sum) sum > 0)
  missingness <- ifelse(ncol(missing_n) > 0,
    paste0(stringr::str_c(
      "N=", missing_n,
      " missing ", colnames(missing_n), ". "
    ),
    collapse = ""
    ),
    ""
  )


  caption <- paste0(
    "Note. ",
    ifelse(sum(is.na(data[[strata]])) > 0,
      paste0(
        "N=", sum(is.na(data[[strata]])),
        " excluded for missing group variable. "
      ), ""
    ),
    missingness,
    ifelse(strata == "onecol", "", "* p<.05, ** p<.01, *** p<.001")
  )


  # check if htmlTable installed
  if (html[1] == TRUE & !requireNamespace("htmlTable", quietly = TRUE)) {
    warning("library(htmlTable) is needed for HTML format output,
            please install and try again")
    html <- FALSE
  }

  if (html[1] == TRUE) {
    return(print(htmlTable::htmlTable(finaltable,
      useViewer = TRUE, rnames = FALSE,
      caption = caption,
      pos.caption = "bottom"
    )))
  } else {
    return(list(
      table = noquote(as.data.frame(finaltable, row.names = NULL)),
      caption = caption
    ))
  }
}
