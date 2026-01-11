#' @importFrom  shiny reactiveValues
server <- function(input, output, session) {

  r <- reactiveValues(
    new_data = NULL,
    h_control_data = NULL,
    new_control_data = NULL,
    excluded_data = NULL,
    min_points = NULL,
    summary_new_data = NULL,
    summary_control = NULL,
    mod_end_day = NULL,
    con_meas = NULL,
    model_type = NULL,
    cut_off = NULL,
    classify_final = NULL,
    classify_final_filtered = NULL,
    classification_tv_plot = NULL,
    classification_gr_plot = NULL,
    classification_waterfall_plot = NULL,
    stat_analysis_done = FALSE,
    plot_animal_number = NULL,
    df_animal_number = NULL,
    df_efficacy = NULL,
    df_categories = NULL,
    plot_categories = NULL
  )

  mod_load_server("load_data", r = r)

  mod_classification_server("classification", r = r)

  mod_stat_analysis_server("stat_analysis", r = r)

  mod_report_server("report", r = r)


}
