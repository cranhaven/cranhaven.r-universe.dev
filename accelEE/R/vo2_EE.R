met_expand <- function(
  d, met_name, tag, met_mlkgmin = 3.5,
  min_mets = 1, max_mets = 20, RER = 0.85,
  warn_high_low = TRUE
) {

  d %>%

  dplyr::rename_with(
    function(x, met_name, tag) {
      gsub(met_name, "", x) %>%
      paste0(tag, "_METs_", .)
    },
    dplyr::matches(met_name),
    met_name = met_name,
    tag = tag
  )  %>%

  stats::setNames(., gsub("[_.-]+", "_", names(.))) %>%

  dplyr::mutate(

    dplyr::across(
      dplyr::contains("METs"),
      ~check_values(
        .x, minimum = min_mets, maximum = max_mets,
        label = gsub("[\\._\\-]+", "", toupper(tag)),
        variable = "MET", units = "MET(s)",
        warn_high_low = warn_high_low
      )
    ),

    dplyr::across(
      dplyr::contains("METs"),
      ~.x * met_mlkgmin,
      .names = "{gsub(\"METs\", \"vo2_mlkgmin\", .col)}"
    ),

    dplyr::across(
      dplyr::contains("vo2_mlkgmin"),
      ~ .x / 1000 * PAutilities::get_kcal_vo2_conversion(RER, "Lusk"),
      .names = "{gsub(\"vo2_mlkgmin\", \"kcal_kgmin\", .col)}"
    )

  ) %>%

  stats::setNames(., gsub("_+$", "", names(.)))

}


vo2_expand <- function(
  vo2_mlkgmin, d, tag, time_var = "Timestamp",
  min_vo2_mlkgmin = 3, max_vo2_mlkgmin = 70,
  warn_high_low = TRUE, met_mlkgmin = 3.5, RER = 0.85
) {

  vo2_mlkgmin %<>% check_values(
    min_vo2_mlkgmin, max_vo2_mlkgmin,
    gsub("[\\._\\-]+", " ", toupper(tag)),
    "VO2", "ml/kg/min", warn_high_low
  )

  vo2_L_kgmin <- vo2_mlkgmin / 1000

  kcal_kgmin <-
    PAutilities::get_kcal_vo2_conversion(RER, "Lusk") %>%
    {vo2_L_kgmin * .}

  data.frame(
    METs = vo2_mlkgmin / met_mlkgmin,
    kcal_kgmin = kcal_kgmin,
    vo2_mlkgmin = vo2_mlkgmin
  ) %>%
  stats::setNames(., paste(names(.), tag, sep = "_")) %>%
  dplyr::tibble(
    !!as.name(time_var) := d[[time_var]],
    .
  )

}


collapse_EE <- function(
  d, time_var = "Timestamp",
  unit = "60 sec", verbose = FALSE
) {


  ## Set up and check epoch lengths

    e <- epoch_length(d, time_var)

    s <- unit_to_sec(unit)

    if (s < e) stop(
      "Cannot collapse to a shorter epoch length",
      " (requested conversion from ",
      e, " sec to ", s, " sec)"
    )

    if (verbose & s == e) cat("\n...Formatting")

    if (verbose & s > e) cat("\n...Collapsing to ", s, "-second epochs", sep = "")


  ## Determine expected number of data points per epoch

    if (e < 1) {
      expected <- s * get_samp_freq(d, time_var)
    } else{
      expected <- round(s / e)
    }


  ## Implementation

    d %>%
    dplyr::group_by(
      !!as.name(time_var) := lubridate::floor_date(!!as.name(time_var), unit)
    ) %>%
    dplyr::summarise(
      dplyr::across(where(function(x) !is.numeric(x)), dplyr::first),
      dplyr::across(where(is.numeric), mean),
      n = dplyr::n()
    ) %T>%
    {if (sum(.$n != expected)>1) {
      warning(
        "Removing ", sum(.$n != expected),
        " incomplete epoch(s) from a file starting ",
        as.Date(dplyr::first(.[[time_var]])), call. = FALSE
      )
    }} %>%
    dplyr::filter(n == expected) %>%
    dplyr::select(-n) %>%
    data.frame(stringsAsFactors = FALSE)

}


join_EE <- function(d, ee_values) {

  stopifnot(
    is.data.frame(d),
    is.list(ee_values)
  )

  rows <-
    sapply(ee_values, nrow) %>%
    unique(.) %>%
    c(nrow(d)) %T>%
    {stopifnot(
      diff(range(.)) <= 1
    )} %>%
    min(.) %>%
    seq(.)


  ee_values %>%
  lapply(dplyr::slice, rows) %>%
  c(.name_repair = "minimal") %>%
  do.call(dplyr::bind_cols, .) %>%
  df_unique(.) %>%
  dplyr::select(!dplyr::any_of(names(d))) %>%
  dplyr::bind_cols(
    dplyr::slice(d, rows),
    .
  )

}


check_continuous <- function(d, time_var, expected) {

  if (nrow(d) == 0) stop("data frame is empty")

  mapply(
    difftime,
    d[[time_var]][-1],
    d[[time_var]][-nrow(d)],
    MoreArgs = list(units = "sec"),
    USE.NAMES = FALSE
  ) %>%
  {if (any(. != expected)) stop(
    "Discontinuity detected in file",
    call. = FALSE
  )}

  d

}


return_vals <- function(
  results, time_var,
  output_epoch, verbose,
  ..., default_override = FALSE
) {

  if (is_default(output_epoch) | default_override) {
    check_continuous(
      results,
      time_var,
      epoch_length(results, time_var)
    )
  } else {
    collapse_EE(results, time_var, output_epoch, verbose) %>%
    check_continuous(time_var, unit_to_sec(output_epoch))
  }


}
