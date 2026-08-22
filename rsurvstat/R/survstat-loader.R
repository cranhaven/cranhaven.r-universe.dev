#' Retrieve time series data from the `SurvStat` web service.
#'
#' This function gets a weekly timeseries of disease count or incidence data
#' from the  Robert Koch Institute `SurvStat` web service. The timeseries can be
#' stratified by any combination of age, geography, disease, disease subtype.
#' Queries to `SurvStat` are cached and paged, but obviously multidimensional
#' extracts have the potential to need a lot of downloading.
#'
#' @param disease the disease of interest as a `SurvStat` key, see
#'   `rsurvstat::diseases` for a current list of these. This is technically
#'   optional, and if omitted the counts of all diseases will be returned. Keys
#'   are the same as the options in the `SurvStat` user interface found
#'   [here](https://survstat.rki.de/Content/Query/Main.aspx#CreateQuery). `IfSG`
#'   and `state` variants of diseases are counts that are reported directly to
#'   the Robert Koch Institute or indirectly via state departments.
#' @param measure one of `"Count"` (default) or `"Incidence"` per 100,000 per
#'   week or year depending on the context.
#' @param ... not used, must be empty.
#' @param age_group (optional) the age group of interest as a `SurvStat` key,
#'   see `rsurvstat::age_groups` for a list of valid options.
#' @param age_range (optional) a length 2 vector with the minimum and maximum
#'   ages to consider
#' @param disease_subtype if `TRUE` the returned count will be broken down by
#'   disease or pathogen subtype (assuming `disease` was provided).
#' @param years (optional) a vector of years to limit the response to. This may
#'   be useful to limit the size of returned pages in the event the `SurvStat`
#'   service hits a data transfer limit.
#' @param geography (optional) a geographical breakdown. This can be given as a
#'   character where it must be one of `state`, `nuts`, or `county` specifying
#'   the 16 region `FedStateKey71Map`, 38 region `NutsKey71Map`, or 411 region
#'   `CountyKey71Map` data respectively. Alternatively it can be given as a
#'   as a `sf` dataframe, subsetting one of these maps, in which case only that
#'   subset of regions will be returned.
#' @param trim_zeros get rid of zero counts. Either "both" (from start and end),
#'   "leading" (from start only - the default) or "none".
#' @param .progress by default a progress bar is shown, which may be important
#'   if many downloads are needed to fulfil the request. It can be disabled
#'   by setting this to `FALSE` here.
#'
#' @return a data frame with at least `date` (weekly), and one of `count` or
#'   `incidence` columns. Most likely it will also have `disease_name` and
#'   `disease_code` columns, and some of `age_name`, `age_code`, `age_low`,
#'   `age_high`, `geo_code`, `geo_name`, `disease_subtype_code`,
#'   `disease_subtype_name` depending on options. The dataframe will be grouped
#'   to make sure each group contains a single timeseries.
#' @export
#' @concept survstat
#'
#' @examples
#' \donttest{
#' # age stratified
#' get_timeseries(
#'   diseases$`COVID-19`,
#'   measure = "Count",
#'   age_group = age_groups$children_coarse
#' ) %>% dplyr::glimpse()
#'
#' # geographic
#' get_timeseries(
#'   diseases$`COVID-19`,
#'   measure = "Count",
#'   geography = "state"
#' ) %>% dplyr::glimpse()
#'
#' # disease stratified, subset of years:
#' get_timeseries(
#'   measure = "Count",
#'   years = 2024
#' ) %>% dplyr::glimpse()
#' }
get_timeseries = function(
  disease = NULL,
  measure = c("Count", "Incidence"),
  ...,
  age_group = NULL,
  age_range = c(0, Inf),
  disease_subtype = FALSE,
  years = NULL,
  geography = NULL,
  trim_zeros = c("leading", "both", "none"),
  .progress = TRUE
) {
  rlang::check_dots_empty()
  measure = match.arg(measure)
  trim_zeros = match.arg(trim_zeros)

  # The API can handle 2 dimensions per page. One dimension will always be time
  # in weeks. The second dimension is decided here. Usually it will be disease / disease_subtype
  # if > 1 of these, or geography or age category.
  # decide which dimension is going to be queried for as a column
  if (is.null(disease)) {
    coltype = "disease"
    colhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease
    )
  } else if (isTRUE(disease_subtype)) {
    coltype = "disease_subtype"
    colhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease$pathogenlevel_1
    )
  } else if (is.character(geography)) {
    coltype = "geo"
    if (geography %in% names(geography_resolution)) {
      geography = geography_resolution[[geography]]
    } else {
      stop("geography parameter must be one of `state`, `nuts` or `county`")
    }
    colhier = geography
  } else if (is.character(age_group) && identical(age_range, c(0, Inf))) {
    coltype = "age"
    colhier = if (age_group %in% names(age_groups)) {
      age_groups[[age_group]]
    } else {
      age_group
    }
  } else {
    coltype = NA
    colhier = NULL
  }

  # Anything that is not the row dimension (time) or the column dimension (as
  # decided above), Has to be retrieved as a set of pages. There will be one
  # query for each of the pages.
  page_filters = NULL
  if (!isTRUE(coltype == "disease")) {
    page_filters = .cross_join_filters(page_filters, .disease_filter(disease))
  }
  if (!isTRUE(coltype == "geo")) {
    page_filters = .cross_join_filters(page_filters, .place_filter(geography))
  }
  if (!isTRUE(coltype == "age")) {
    page_filters = .cross_join_filters(
      page_filters,
      .age_filter(age_group, age_range)
    )
  }
  if (!is.null(years)) {
    page_filters = .cross_join_filters(
      page_filters,
      .year_filter(years)
    )
  }

  # This is the output dataframe:
  collect = NULL

  if (.progress) {
    cli::cli_progress_bar(total = length(page_filters))
  }
  if (is.null(page_filters)) {
    page_filters = list(NULL)
  }
  for (page in page_filters) {
    # Each item in the page filters list is itself a list of filters
    # that are added to the request for one `page` of results.
    # The page filters depend on options but might be a selection of
    # geography, age categories, or similar.

    tmp2 = .get_request(
      commands$olap_data,
      cube = cubes$survstat,
      language = languages$german,
      column_hierarchy = as.character(colhier),
      measure = measure,
      filters = page,
      row_hierarchy = "[ReportingDate].[YearWeek]"
    )

    tmp = try(tmp2 %>% .do_survstat_command(quiet = TRUE), silent = TRUE)

    # Do query and halt on error
    if (inherits(tmp, "try-error")) {
      message(as.character(tmp2))
      stop(
        "Aborting as the SurvStat query returned an error.\n",
        "It may be because too much data was requested in one go.\n",
        "you can try chunking the data by year (using `years`)\n",
        "The error was:\n",
        tmp
      )
      break
    }

    if (.progress) {
      cli::cli_progress_update()
    }

    # Basically extrac the XML into a dataframe
    tmp = tmp %>% .process_olap_data_result()

    # Post process dates:
    # Firstly make sure weeks are unique:
    # because of the use of epidemic weeks the same week can be split across
    # year ends, and count as week 53, and week 1. We convert epiweeks to weeks
    # elapsed from "2021-01-01" and aggregate. We need to do this before being
    # combined with other pages
    tmp = tmp %>%
      dplyr::mutate(
        year = row_name %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
        week = row_name %>% stringr::str_extract("[0-9]+$") %>% as.numeric()
      ) %>%
      dplyr::mutate(
        elapsed_week = (year - 2001) * 52 + week + (year - 2001) %/% 7,
        value = ifelse(is.na(value), 0, value),
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c(
        "col_name",
        "col_code",
        "elapsed_week"
      )))) %>%
      dplyr::summarise(
        value = sum(value)
      ) %>%
      dplyr::mutate(
        date = as.Date("2001-01-01") + elapsed_week * 7 # This is a monday
      ) %>%
      dplyr::select(-elapsed_week)

    # Extract the values from the filters used to get this page of results
    # add add them into the dataframe before combining with other pages.

    if (!is.null(names(page))) {
      # Somehow this gets flattened when only one option. I cannot find out where
      # so I;ve put in an explicit check for it.
      page = list(page)
    }
    values = unlist(lapply(page, function(dim) dim$values), recursive = FALSE)
    tmp = tmp %>% dplyr::mutate(!!!values)

    collect = if (is.null(collect)) tmp else dplyr::bind_rows(tmp, collect)
  }

  # The column data will be different depending on the configuration maybe age,
  # maybe geography
  # Here we rename columns to whatever it was we set as the column dimension.
  if (is.na(coltype)) {
    collect = collect %>% dplyr::select(-dplyr::starts_with("col"))
  } else {
    colnames(collect) = gsub("col", coltype, colnames(collect), fixed = TRUE)
  }

  # Fix age codes and break into age_name, age_low and age_high
  if ("age_code" %in% colnames(collect)) {
    tmp = .fmt_range(collect$age_code)
    collect = collect %>% dplyr::mutate(!!!tmp)
  }

  # Fix age codes and break into age_name, age_low and age_high
  if ("disease_name" %in% colnames(collect)) {
    collect = collect %>%
      dplyr::mutate(
        disease_name = ifelse(
          disease_name %in% diseases,
          names(diseases)[match(disease_name, diseases)],
          disease_name
        )
      )
  }

  # Fix grouping:
  collect = collect %>%
    dplyr::group_by(dplyr::across(
      -dplyr::any_of(c("date", "value", "year"))
    ))

  # Get rid of leading zeros
  if (trim_zeros != "none") {
    collect = collect %>%
      dplyr::filter(
        as.numeric(date) >
          suppressWarnings(min(as.numeric(date)[value != 0], na.rm = TRUE))
      )
  }
  # Get rid of trailing zeros
  if (trim_zeros == "both") {
    collect = collect %>%
      dplyr::filter(
        as.numeric(date) <=
          suppressWarnings(max(as.numeric(date)[value != 0], na.rm = TRUE))
      )
  }

  # rename "value" column to "count" or "incidence"
  collect = collect %>% dplyr::rename(!!(tolower(measure)) := value)

  if (.progress) {
    cli::cli_progress_done()
  }

  return(collect)
}


#' Retrieve data from the `SurvStat` web service relating to a single time period.
#'
#' This function gets a snapshot of disease count or incidence data
#' from the  Robert Koch Institute `SurvStat` web service, based on either whole
#' epidemiological season or an individual week within a season. Seasons are
#' whole years starting either at the beginning of the calendar year, at week 27
#' or at week 40.
#'
#' The snapshot can be stratified by any combination of age, geography, disease,
#' disease subtype. Queries to `SurvStat` are cached and paged, but obviously
#' multidimensional extracts have the potential to need a lot of downloading.
#'
#' @inheritParams get_timeseries
#' @param season the start year of the season in which the snapshot is taken
#' @param season_week the start week within the season of the snapshot. If missing
#'   then the whole season is used
#' @param season_start the week of the calendar year in which the season starts
#'   this can be one of `1`, `27` or `40`.
#'
#' @return a data frame with at least `year` (the start of the epidemiological
#'   season) and `start_week` (the calendar week in which the epidemiological
#'   season starts), and one of `count` or `incidence` columns. Most likely it
#'   will also have `disease_name` and `disease_code` columns, and some of
#'   `age_name`, `age_code`, `age_low`, `age_high`, `geo_code`, `geo_name`,
#'   `disease_subtype_code`, `disease_subtype_name` depending on options.
#' @export
#' @concept survstat
#'
#' @examples
#' \donttest{
#' get_snapshot(
#'   diseases$`COVID-19`,
#'   measure = "Count",
#'   season = 2024,
#'   age_group = age_groups$children_coarse
#' )
#'
#' get_snapshot(
#'   diseases$`COVID-19`,
#'   measure = "Count",
#'   age_group = age_groups$children_coarse,
#'   season = 2024,
#'   geography = rsurvstat::FedStateKey71Map[1:10,]
#' )
#' }
get_snapshot = function(
  disease = NULL,
  measure = c("Count", "Incidence"),
  ...,
  season,
  season_week = NULL,
  season_start = 1,
  age_group = NULL,
  age_range = c(0, Inf),
  disease_subtype = FALSE,
  geography = NULL,
  .progress = TRUE
) {
  rlang::check_dots_empty()
  measure = match.arg(measure)

  # The API can handle 2 dimensions per page.
  # Any additional dimensions are page filters

  # The first dimension is decided here. Usually it will be disease / disease_subtype
  # if > 1 of these, or geography or age category.
  # decide which dimension is going to be queried for as a column
  if (is.null(disease)) {
    coltype = "disease"
    colhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease
    )
  } else if (isTRUE(disease_subtype)) {
    coltype = "disease_subtype"
    colhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease$pathogenlevel_1
    )
  } else if (is.character(geography)) {
    coltype = "geo"
    if (geography %in% names(geography_resolution)) {
      geography = geography_resolution[[geography]]
    }
    colhier = geography
  } else if (is.character(age_group) && identical(age_range, c(0, Inf))) {
    coltype = "age"
    colhier = if (age_group %in% names(age_groups)) {
      age_groups[[age_group]]
    } else {
      age_group
    }
  }

  # The second dimension is decided here. Usually it will be disease / disease_subtype
  # if > 1 of these, or geography or age category.
  # decide which dimension is going to be queried for as a row
  if (is.null(disease) && coltype != "disease") {
    rowtype = "disease"
    rowhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease
    )
  } else if (isTRUE(disease_subtype) && coltype != "disease_subtype") {
    rowtype = "disease_subtype"
    rowhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease$pathogenlevel_1
    )
  } else if (is.character(geography) && coltype != "geo") {
    rowtype = "geo"
    if (geography %in% names(geography_resolution)) {
      geography = geography_resolution[[geography]]
    }
    rowhier = geography
  } else if (
    is.character(age_group) &&
      identical(age_range, c(0, Inf)) &&
      coltype != "age"
  ) {
    rowtype = "age"
    rowhier = if (age_group %in% names(age_groups)) {
      age_groups[[age_group]]
    } else {
      age_group
    }
  } else {
    rowtype = NA
    rowhier = NULL
  }

  # Anything that is not the row dimension or the column dimension (as
  # decided above), Has to be retrieved as a set of pages. There will be one
  # query for each of the pages.

  # The base page filter is the year and possibly week of interest
  page_filters = c(
    list(.epiyear_filter(start_year = season, epiweek = season_start)),
    if (!is.null(season_week)) {
      list(.epiweek_filter(start_week = season_week, epiweek = season_start))
    } else {
      NULL
    }
  )

  if (!isTRUE(coltype == "disease" || rowtype == "disease")) {
    page_filters = .cross_join_filters(page_filters, .disease_filter(disease))
  }
  if (!isTRUE(coltype == "geo" || rowtype == "geo")) {
    page_filters = .cross_join_filters(page_filters, .place_filter(geography))
  }
  if (!isTRUE(coltype == "age" || rowtype == "age")) {
    page_filters = .cross_join_filters(
      page_filters,
      .age_filter(age_group, age_range)
    )
  }

  # This is the output dataframe:
  collect = NULL

  if (.progress) {
    cli::cli_progress_bar(total = length(page_filters))
  }
  if (is.null(page_filters)) {
    page_filters = list(NULL)
  }
  for (page in page_filters) {
    # Each item in the page filters list is itself a list of filters
    # that are added to the request for one `page` of results.
    # The page filters depend on options but might be a selection of
    # geography, age categories, or similar.

    tmp2 = .get_request(
      commands$olap_data,
      cube = cubes$survstat,
      language = languages$german,
      column_hierarchy = as.character(colhier),
      measure = measure,
      filters = page,
      row_hierarchy = as.character(rowhier)
    )

    tmp = try(tmp2 %>% .do_survstat_command(quiet = TRUE), silent = TRUE)

    # Do query and halt on error
    if (inherits(tmp, "try-error")) {
      message(as.character(tmp2))
      stop(
        "Aborting as the SurvStat query returned an error.\n",
        "It may be because too much data was requested in one go.\n",
        "you can try chunking the data by year (using `years`)\n",
        "The error was:\n",
        tmp
      )
      break
    }

    if (.progress) {
      cli::cli_progress_update()
    }

    # Basically extract the XML into a dataframe
    tmp = tmp %>% .process_olap_data_result()

    # Extract the values from the filters used to get this page of results
    # add add them into the dataframe before combining with other pages.

    if (!is.null(names(page))) {
      # Somehow this gets flattened when only one option. I cannot find out where
      # so I;ve put in an explicit check for it.
      page = list(page)
    }
    values = unlist(lapply(page, function(dim) dim$values), recursive = FALSE)
    tmp = tmp %>% dplyr::mutate(!!!values)

    collect = if (is.null(collect)) tmp else dplyr::bind_rows(tmp, collect)
  }

  # The column data will be different depending on the configuration maybe age,
  # maybe geography
  # Here we rename columns to whatever it was we set as the column dimension.
  colnames(collect) = gsub("col", coltype, colnames(collect), fixed = TRUE)
  if (is.na(rowtype)) {
    collect = collect %>% dplyr::select(-dplyr::starts_with("row"))
  } else {
    colnames(collect) = gsub("row", rowtype, colnames(collect), fixed = TRUE)
  }

  # Fix age codes and break into age_name, age_low and age_high
  if ("age_code" %in% colnames(collect)) {
    tmp = .fmt_range(collect$age_code)
    collect = collect %>% dplyr::mutate(!!!tmp)
  }

  # Fix age codes and break into age_name, age_low and age_high
  if ("disease_name" %in% colnames(collect)) {
    collect = collect %>%
      dplyr::mutate(
        disease_name = ifelse(
          disease_name %in% diseases,
          names(diseases)[match(disease_name, diseases)],
          disease_name
        )
      )
  }

  # rename "value" column to "count" or "incidence"
  collect = collect %>% dplyr::rename(!!(tolower(measure)) := value)

  if (.progress) {
    cli::cli_progress_done()
  }

  return(collect)
}


# Utility functions ----

# Take the output from the olap data quesies and extracts rows and columns
# into a long format dataframe, fixing escaping.
.process_olap_data_result = function(response) {
  tmp = response
  rows = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:QueryResultRow/b:Caption/text()"
  ))
  rowIds = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:QueryResultRow/b:RowName/text()"
  ))
  cols = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:Columns//b:Caption/text()"
  ))
  colIds = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:Columns//b:ColumnName/text()"
  ))

  values = xml2::xml_find_all(tmp, "//b:QueryResultRow/b:Values/*") %>%
    xml2::xml_text()
  values = values %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_replace(",", ".") %>%
    as.numeric()

  if (length(rows) == 0) {
    rows = FALSE
    rowIds = NA
  }
  if (
    length(cols) == 0 ||
      (length(cols) == 1 && startsWith(colIds, "[Measures]."))
  ) {
    cols = FALSE
    colsIds = NA
  }
  if (length(values) != length(rows) * length(cols)) {
    stop("SurvStat response is not an expected format")
  }

  df = tibble::tibble(
    value = values,
    col_name = rep(cols, times = length(rows)),
    col_code = rep(colIds, times = length(rows)),
    row_name = rep(rows, each = length(cols)),
    row_code = rep(rowIds, each = length(cols)),
  )

  df2 = df %>%
    # Exclude total columns
    dplyr::filter(col_name != "Gesamt" & row_name != "Gesamt") %>%
    dplyr::filter(!is.na(col_name) & !is.na(row_name))

  if (isFALSE(rows)) {
    df2 = df2 %>% dplyr::select(-row_name, -row_code)
  }
  if (isFALSE(cols)) {
    df2 = df2 %>% dplyr::select(-col_name, -col_code)
  }

  return(df2)
}


# Format the age code into a prettier format.
.fmt_range = function(v) {
  l = as.numeric(stringr::str_extract(v, "\\[A([0-9]+)", 1))
  h = as.numeric(stringr::str_extract(v, "([0-9]+)\\]$", 1)) + 1
  f = dplyr::case_when(
    is.na(l) & is.na(h) ~ NA,
    is.na(h) ~ sprintf("%d+", l),
    l == h - 1 ~ sprintf("%d", l),
    TRUE ~ sprintf("%d\u2013%d", l, h - 1)
  )
  return(list(age_low = l, age_high = h, age_name = f))
}

# Filters ----

# Construct a filter for selecting one disease
.disease_filter = function(disease) {
  if (is.null(disease)) {
    return(NULL)
  }
  disease_nm = disease
  if (disease %in% names(diseases)) {
    disease = diseases[[disease]]
  }

  disease_code = sprintf(
    "[KategorieNz].[Krankheit DE].&[%s]",
    disease
  )

  list(list(
    values = list(
      disease_name = disease_nm,
      disease_code = disease_code
    ),
    dimension_id = "[PathogenOut].[KategorieNz]",
    hierarchy_id = "[PathogenOut].[KategorieNz].[Krankheit DE]",
    hierarchy_value = disease_code
  ))
}


# .match_values(hierarchy_list$time$seasonweek_27_)
.year_filter = function(years) {
  if (is.null(years)) {
    return(NULL)
  }
  lapply(years, function(v) {
    list(
      values = list(year = v),
      dimension_id = "[ReportingDate]",
      hierarchy_id = "[ReportingDate].[WeekYear]",
      hierarchy_value = sprintf("[ReportingDate].[WeekYear].&[%d]", v)
    )
  })
}


# .match_values(hierarchy_list$time$seasonweek_27_)
.epiyear_filter = function(start_year, epiweek = 1) {
  stopifnot(epiweek %in% c(1, 27, 40))
  if (length(start_year) != 1) {
    stop("`start_year` must be of length one")
  }
  hier = if (epiweek == 1) "WeekYear" else sprintf("Season%d Year", epiweek)

  hier = list(
    values = list(year = start_year, start_week = epiweek),
    dimension_id = "[ReportingDate]",
    hierarchy_id = sprintf("[ReportingDate].[%s]", hier),
    hierarchy_value = sprintf("[ReportingDate].[%s].&[%d]", hier, start_year)
  )

  return(hier)
}


# .match_values(hierarchy_list$time$seasonweek_27_)
.epiweek_filter = function(start_week, epiweek = 1) {
  stopifnot(epiweek %in% c(1, 27, 40))
  stopifnot(start_week %in% 1:53)
  if (!is.null(start_week) && length(start_week) != 1) {
    stop("`start_week` must be of length one")
  }

  hier2 = if (epiweek == 1) "Week" else sprintf("Season%d Week", epiweek)
  hier2 = list(
    values = list(week = start_week),
    dimension_id = "[ReportingDate]",
    hierarchy_id = sprintf("[ReportingDate].[%s]", hier2),
    hierarchy_value = sprintf("[ReportingDate].[%s].&[%d]", hier2, start_week)
  )

  return(hier2)
}

# Create a filter for a set of ages where the standard grop is being
# reduced (or this is just being done as a multipage where each page has a
# single age group)
# .match_values(age_groups$single_year)
# give use the kind of output codes we would be expecting
.age_filter = function(
  age_group,
  age_range = c(0, Inf)
) {
  if (is.null(age_group)) {
    return(NULL)
  }
  if (age_group %in% names(age_groups)) {
    age_group = age_groups[[age_group]]
  }
  age_group_values = .match_values(age_group)
  age_data = .fmt_range(age_group_values)

  low = age_data$age_low
  high = age_data$age_high
  high[is.na(high)] = 120
  age_group_values = age_group_values[
    min(age_range) <= as.numeric(low) &
      max(age_range) > as.numeric(high)
  ]

  unname(lapply(age_group_values, function(v) {
    list(
      values = list(
        age_code = v
      ),
      dimension_id = age_groups,
      hierarchy_id = age_groups,
      hierarchy_value = v
    )
  }))
}


# The geographic filters (if not primary row variable)
# map is either a character "state", "nuts2" or "county", or a map with one of
# FedStateKey71Map, NutsKey71Map, CountyKey71Map
.place_filter = function(
  map_sf
) {
  if (is.null(map_sf)) {
    return(NULL)
  }
  if (is.character(map_sf)) {
    map_sf = switch(
      map_sf,
      "state" = rsurvstat::FedStateKey71Map,
      "nuts" = rsurvstat::NutsKey71Map,
      "county" = rsurvstat::CountyKey71Map
    )
  }
  values = list()

  tmp = purrr::pmap(
    map_sf,
    function(Id, HierarchyId, ComponentId, Name, ...) {
      list(
        values = list(
          geo_name = Name,
          # geo_code = stringr::str_extract(ComponentId, "\\[([^\\]]+)\\]$", 1),
          # geo_code_type = stringr::str_extract(
          #   HierarchyId,
          #   "\\[([^\\]]+)\\]$",
          #   1
          # ),
          geo_code = Id
        ),
        dimension_id = as.character(
          hierarchy_list$place$state_territorial_unit_county
        ),
        hierarchy_id = HierarchyId,
        hierarchy_value = Id
      )
    }
  )
}


# Filter to specific time point(s) more generally than the year filter
# .time_filter = function() {
# }

# Filter utils ----

# Create a list with unique conbinations of input and
# devtools::load_all()
# l1 = list(a="A",b="B")
# l2 = list(c="C",d="D")
# l3 = list(e="E",f="F")
# l4 = list(g="G",h="H")
# l5 = list(i="I",j="J")
# tmp = .cross_join_filters(list(l1,l2), list(l3))
# tmp = .cross_join_filters(tmp, list(l4, l5))
# .tree(tmp)
# .tree(.cross_join_filters(l1,NULL))
# .tree(.cross_join_filters(NULL,list(l1,l2)))
.cross_join_filters = function(list1, list2) {
  if (is.null(list2)) {
    if (!is.null(names(list1))) {
      list1 = list(list1)
    }
    return(list1)
  }
  if (is.null(list1)) {
    if (!is.null(names(list2))) {
      list2 = list(list2)
    }
    return(list2)
  }
  return(
    unlist(
      lapply(list1, function(item1) {
        lapply(list2, function(item2) {
          # So not sure whether this is really working
          if (!is.null(names(item1))) {
            item1 = list(item1)
          }
          if (!is.null(names(item2))) {
            item2 = list(item2)
          }
          c(item1, item2)
        })
      }),
      recursive = FALSE
    )
  )
}
