#' Shiny GUI for mtscr
#'
#' Shiny app used as graphical interface for mtscr. Simply invoke `mtscr_app()` to run.
#'
#' @return Runs the app. No explicit return value.
#'
#' @export
#'
#' @details
#' To use the GUI you need to have the following packages installed:
#' `DT`, `broom.mixed`, `datamods`, `writexl`.
#'
#' First thing you see after running the app is `datamods` window for importing your data.
#' You can use the data already loaded in your environment or any other option.
#' Then you'll see four dropdown lists used to choose arguments for `mtscr_model()`
#' and `mtscr_score()` functions. Consult these functions' documentation for
#' more details (execute `?mtscr_score` in the console). When the parameters are chosen,
#' click "Generate model" button. After a while (up to a dozen or so seconds) models'
#' parameters and are shown along with a scored dataframe.
#'
#' You can download your data as a .csv or an .xlsx file using buttons in the sidebar.
#' You can either download the scores only (i.e. the dataframe you see displayed) or
#' your whole data with `.all_max` and `.all_top2` columns added.
#'
#' For testing purposes, you may use `mtscr_creativity` dataframe. In the importing
#' window change "Global Environment" to "mtscr" and our dataframe should appear
#' in the upper dropdown list. Use `id` for the ID column, `item` for the item
#' column and `SemDis_MEAN` for the score column.
#'
#' @seealso
#'   [mtscr::mtscr_score()] for more information on the arguments.
#'
#'   [mtscr_creativity] for more information about the example dataset.
#'
#'   Forthmann, B., Karwowski, M., & Beaty, R. E. (2023).
#'     Don’t throw the “bad” ideas away!
#'     Multidimensional top scoring increases reliability of divergent thinking tasks.
#'     Psychology of Aesthetics, Creativity, and the Arts. \doi{10.1037/aca0000571}
#'
#' @examples
#'if(interactive()){
#' mtscr_app()
#' }
mtscr_app <- function() {
  needed_packages <- c("shiny", "DT", "datamods", "writexl", "shinyWidgets")

  needed_packages <- sapply(needed_packages, \(x) {
    if (system.file(package = x) != "") {
      c("v" = paste0("You have {.pkg ", x, "} installed."))
    } else {
      c("x" = paste0("You don't have {.pkg ", x, "} installed. Install it with {.run install.packages(\"", x, "\")}."))
    }
  },
  USE.NAMES = FALSE
  )

  if (any(names(needed_packages) == "x")) {
    cli::cli_abort(
      c(
        "The GUI requires some additional packages to be installed.",
        needed_packages
      )
    )
  }

  app_dir <- system.file("GUI", package = "mtscr")
  if (app_dir == "") {
    cli::cli_abort(
      c(
        "The app not found.",
        "i" = "Try reinstalling the {.pkg mtscr} package with {.run devtools::intall_github(\"jakub-jedrusiak/mtscr\")"
      )
    )
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
