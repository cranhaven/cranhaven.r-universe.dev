#' Create Data Model
#'
#' @description Create a data model object from \pkg{mlms} package datasets.
#'   A data model holds a list of tables and their relationships.
#'   Requires that the \pkg{dm} and \pkg{inldata} packages are available.
#'
#' @return Invisibly returns the data model, an object of class [`dm`][dm::dm].
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal

make_dm <- function() {

  # check packages
  for (pkg in c("dm", "inldata")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      sprintf("Creating a data model requires the '%s' package.", pkg) |>
        stop(call. = FALSE)
    }
  }

  # combine tables into a single list
  inldata::combine_tables(package = "mlms") |>

    # convert list to data model
    dm::as_dm() |>

    # add primary keys
    dm::dm_add_pk(table = "ports", columns = "site_no", check = TRUE) |>
    dm::dm_add_pk(table = "heads", columns = c("site_no", "press_dt"), check = TRUE) |>
    dm::dm_add_pk(table = "samples", columns = c("site_no", "sample_dt", "pcode"), check = TRUE) |>
    dm::dm_add_pk(table = "sensors", columns = "sensor_id", check = TRUE) |>
    dm::dm_add_pk(table = "visits", columns = "stime_dt", check = TRUE) |>
    dm::dm_add_pk(table = "wells", columns = "site_nm", check = TRUE) |>
    dm::dm_add_pk(table = "zones", columns = c("site_nm", "zone_nu"), check = TRUE) |>

    # add secondary keys
    dm::dm_add_fk(table = "calibrations", columns = "sensor_id", ref_table = "sensors", check = TRUE) |>
    dm::dm_add_fk(table = "ports", columns = "site_nm", ref_table = "wells", check = TRUE) |>
    dm::dm_add_fk(table = "ports", columns = c("site_nm", "zone_nu"), ref_table = "zones", check = TRUE) |>
    dm::dm_add_fk(table = "zones", columns = "site_nm", ref_table = "wells", check = TRUE) |>

    dm::dm_add_fk(table = "visits", columns = "site_nm", ref_table = "wells", check = TRUE) |>
    dm::dm_add_fk(table = "visits", columns = "sensor_id", ref_table = "sensors", check = TRUE) |>

    dm::dm_add_fk(table = "heads", columns = "site_nm", ref_table = "wells", check = TRUE) |>
    dm::dm_add_fk(table = "heads", columns = "stime_dt", ref_table = "visits", check = TRUE) |>
    dm::dm_add_fk(table = "heads", columns = "site_no", ref_table = "ports", check = TRUE) |>

    dm::dm_add_fk(table = "samples", columns = "site_nm", ref_table = "wells", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "site_no", ref_table = "ports", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "stime_dt", ref_table = "visits", check = FALSE) |>

    # validate data model
    dm::dm_validate()
}
