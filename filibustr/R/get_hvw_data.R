#' Get replication data from Harbridge-Yong, Volden, and Wiseman (2023)
#'
#' @description
#' `get_hvw_data()` returns replication data from:
#'
#' Harbridge-Yong, L., Volden, C., & Wiseman, A. E. (2023).
#' The bipartisan path to effective lawmaking.
#' *The Journal of Politics*, *85*(3), 1048–1063.
#' \doi{doi:10.1086/723805}
#'
#' @details
#' The replication data is available at the
#' [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EARLA4&version=1.0).
#'
#' The House and Senate data come from the files
#' `HarbridgeYong_Volden_Wiseman_House_Replication.tab` and
#' `HarbridgeYong_Volden_Wiseman_Senate_Replication.tab`, respectively.
#'
#' The data spans the 93rd through 114th Congresses (1973-2016).
#'
#' These datasets have been dedicated to the public domain
#' under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/).
#'
#' @inheritParams get_voteview_members
#'
#' @param chamber Which chamber to get data for. Options are:
#'  * `"house"`, `"h"`, `"hr"`: House data only.
#'  * `"senate"`, `"s"`, `"sen"`: Senate data only.
#'
#'  These options are case-insensitive. Any other argument results in an error.
#'
#'  **Note:** Unlike the Voteview functions, there is no `"all"` option.
#'  The House and Senate data do not have the same number of variables,
#'  or the same variable names, so it is not trivial to join the two tables.
#'
#'  You *must* specify either House or Senate data, since there is no "default" option.
#'
#' @param local `r lifecycle::badge('experimental')` `r doc_arg_local("Harvard Dataverse")`
#'
#' @returns A [tibble()].
#' @export
#'
#' @examples
#' get_hvw_data("senate")
#' @examplesIf interactive()
#' get_hvw_data("house")
get_hvw_data <- function(chamber, local = TRUE, local_dir = ".") {
  file <- build_file_path(data_source = "hvw", chamber = chamber,
                          local = local, local_dir = local_dir)

  # request data from online
  if (R.utils::isUrl(file)) {
    file <- get_online_data(url = file, source_name = "Harvard Dataverse")
  }

  readr::read_tsv(file, show_col_types = FALSE)
}
