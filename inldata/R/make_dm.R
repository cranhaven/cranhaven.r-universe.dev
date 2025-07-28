#' Create Data Model
#'
#' @description Create a data model object from \pkg{inldata} package datasets.
#'   A data model holds a list of tables and their relationships.
#'   Requires that the \pkg{dm} package is available.
#'
#' @return Invisibly returns the data model, an object of class [`dm`][dm::dm].
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @seealso [`make_erd`] function for creating an entity-relationship diagram.

make_dm <- function() {

  # check packages
  check_package(pkg = "dm", msg = "Creating a data model")

  # combine tables into a single list
  combine_tables(package = "inldata") |>

    # convert list to data model
    dm::as_dm() |>

    # add primary keys
    dm::dm_add_pk(table = "background", columns = "pcode", check = TRUE) |>
    dm::dm_add_pk(table = "benchmarks", columns = "pcode", check = TRUE) |>
    dm::dm_add_pk(table = "cities", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "counties", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "dl", columns = c("pcode", "min_dt"), check = TRUE) |>
    dm::dm_add_pk(table = "facilities", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "gwl", columns = c("site_no", "lev_dt", "lev_meth_cd"), check = TRUE) |>
    dm::dm_add_pk(table = "lakes", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "parameters", columns = "pcode", check = TRUE) |>
    dm::dm_add_pk(table = "roads", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "samples", columns = c("sample_id", "pcode", "medium_cd"), check = TRUE) |>
    dm::dm_add_pk(table = "sites", columns = "site_no", check = TRUE) |>
    dm::dm_add_pk(table = "streams", columns = "id", check = TRUE) |>
    dm::dm_add_pk(table = "swm", columns = c("site_no", "stage_dt"), check = TRUE) |>
    dm::dm_add_pk(table = "units", columns = "unit_cd", check = TRUE) |>

    # add secondary keys
    dm::dm_add_fk(table = "background", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "benchmarks", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "dl", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "gwl", columns = "site_no", ref_table = "sites", check = TRUE) |>
    dm::dm_add_fk(table = "parameters", columns = "unit_cd", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "percponds", columns = "facility_id", ref_table = "facilities", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "site_no", ref_table = "sites", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "pcode", ref_table = "parameters", check = TRUE) |>
    dm::dm_add_fk(table = "samples", columns = "unit_cd", ref_table = "units", check = TRUE) |>
    dm::dm_add_fk(table = "swm", columns = "site_no", ref_table = "sites", check = TRUE) |>

    # validate data model
    dm::dm_validate()
}
