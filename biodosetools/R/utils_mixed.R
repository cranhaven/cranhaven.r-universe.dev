#' Load and show rds files
#'
#' @param input_data what you upload as rds file in the UI.
#' @param data_type gamma or neutron.
#' @param reactive_data access to the reactive data.
#' @param output output for the UI.
#' @param session let interaction with the shiny UI.
#'
#' @return uploaded rds file's curve information-tables.
#'
load_rds_data <- function(input_data, data_type, reactive_data, output, session) {
  req(input_data)
  data <- readRDS(input_data$datapath)

  if (data_type == "gamma") {

    reactive_data$gamma_data <- data
    reactive_data$fit_coeffs_gamma <- data$fit_coeff
    reactive_data$formula_gamma <- data$fit_formula_tex
    reactive_data$cov_mat_gamma <- data$fit_var_cov_mat

    # Formula
    output$gamma_formula <- renderUI({
      withMathJax(helpText(paste0("$$", reactive_data$formula_gamma, "$$")))
    })

    # Format coefficients and row names
    df_coef <- data.frame(reactive_data$fit_coeffs_gamma %>% formatC(format = "e", digits = 3))
    rownames(df_coef) <- if (nrow(df_coef) == 2) c("C", "\u03B1") else c("C", "\u03B1", "\u03B2")

    output$gamma_coeffs <- renderRHandsontable({
      rhandsontable(df_coef) %>%
        hot_cols(format = "0.00000") %>%
        hot_cols(colWidths = 90) %>%
        hot_cols(halign = "htCenter")
    })

  } else if (data_type == "neutrons") {
    reactive_data$neutrons_data <- data
    reactive_data$fit_coeffs_neutrons <- data$fit_coeff
    reactive_data$formula_neutrons <- data$fit_formula_tex
    reactive_data$cov_mat_neutrons <- data$fit_var_cov_mat

    output$neutrons_formula <- renderUI({
      withMathJax(helpText(paste0("$$", reactive_data$formula_neutrons, "$$")))
    })

    df_coef <- data.frame(reactive_data$fit_coeffs_neutrons %>% formatC(format = "e", digits = 3))
    rownames(df_coef) <- if (nrow(df_coef) == 2) c("C", "\u03B1") else c("C", "\u03B1", "\u03B2")

    output$neutrons_coeffs <- renderRHandsontable({
      rhandsontable(df_coef) %>%
        hot_cols(format = "0.00000") %>%
        hot_cols(colWidths = 90) %>%
        hot_cols(halign = "htCenter")
    })
  }
}



#' Load manual data
#'
#' @param data_type gamma or neutron
#' @param input what you upload in the UI
#' @param reactive_data access to the reactive data
#' @param output output for the UI
#' @param session let interaction with the shiny UI
#'
#' @return manual curve information tables.
#'
load_manual_data <- function(data_type, input, reactive_data, output, session) {

  table_reset <- reactiveValues(value = 0)
  table_var_reset <- reactiveValues(value = 0)

  observeEvent(input[[paste0("formula_select_mixed_", data_type)]], {
    table_reset$value <- 1
    table_var_reset$value <- 1
  })

  previous_coeffs <- reactive({
    formula_select <- input[[paste0("formula_select_mixed_", data_type)]]
    fit_coeffs_names <- if (formula_select == "lin-quad") c("C", "\u03B1", "\u03B2") else c("C", "\u03B1")

    data.frame(matrix(0.0, nrow = length(fit_coeffs_names), ncol = 2)) %>%
      `row.names<-`(fit_coeffs_names) %>%
      `colnames<-`(c("estimate", "std.error"))
  })

  previous_var <- reactive({
    model_formula <- input[[paste0("formula_select_mixed_", data_type)]]
    fit_coeffs_names <- names_from_model_formula(model_formula)

    matrix(0.0, nrow = length(fit_coeffs_names), ncol = length(fit_coeffs_names)) %>%
      `row.names<-`(fit_coeffs_names) %>%
      `colnames<-`(fit_coeffs_names) %>%
      as.data.frame()
  })

  changed_coeffs_data <- reactive({
    hot_input <- input[[paste0("fit_coeffs_hot_mixed_", data_type)]]
    if (is.null(hot_input) || isolate(table_reset$value == 1)) {
      table_reset$value <- 0
      return(previous_coeffs())
    } else if (!identical(previous_coeffs(), hot_input)) {
      fit_coeffs_names <- row.names(hot_to_r(hot_input))
      as.data.frame(hot_to_r(hot_input)) %>%
        dplyr::mutate_all(as.numeric) %>%
        `row.names<-`(fit_coeffs_names)
    }
  })

  changed_var_data <- reactive({
    hot_input <- input[[paste0("fit_var_cov_mat_hot_", data_type)]]
    if (is.null(hot_input) || isolate(table_var_reset$value == 1)) {
      table_var_reset$value <- 0
      return(previous_var())
    } else if (!identical(previous_var(), hot_input)) {
      fit_coeffs_names <- row.names(hot_to_r(hot_input))
      as.data.frame(hot_to_r(hot_input)) %>%
        dplyr::mutate_all(as.numeric) %>%
        `row.names<-`(fit_coeffs_names)
    }
  })
  # Render coeff table
  output[[paste0("fit_coeffs_hot_mixed_", data_type)]] <- renderRHandsontable({
    num_cols <- ncol(changed_coeffs_data())
    changed_coeffs_data() %>%
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000000", type = "numeric")
  })

  # Render var-cov table
  output[[paste0("fit_var_cov_mat_hot_", data_type)]] <- renderRHandsontable({
    num_cols <- ncol(changed_var_data())
    changed_var_data() %>%
      fix_coeff_names(type = "rows", output = "rhot") %>%
      fix_coeff_names(type = "cols", output = "rhot") %>%
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.00000000", type = "numeric")
  })

}


#' Prepare data to dose estimate using mixed fields manual
#'
#' @param input what you upload in the UI
#' @param reactive_data access to the reactive data
#' @param data_type gamma or neutron
#'
#' @return data for dose estimation using manual input.
#'
dose_estimation_mx <- function(input, reactive_data, data_type) {
  coeff_input <- input[[paste0("fit_coeffs_hot_mixed_", data_type)]]

  cov_input <- input[[paste0("fit_var_cov_mat_hot_", data_type)]]
  use_cov_matrix <- input[[paste0("use_var_cov_matrix_", substr(data_type, 1, 1))]]

  fit_coeffs <- hot_to_r(coeff_input)

  rownames(fit_coeffs) <- if (data_type == "gamma") {
    if (nrow(fit_coeffs) == 3) {
      c("coeff_C", "coeff_alpha", "coeff_beta")
    } else {
      c("coeff_C", "coeff_alpha")
    }
  } else { # For neutron
    if (nrow(fit_coeffs) == 3) {
      c("coeff_C", "coeff_alpha", "coeff_beta")
    } else {
      c("coeff_C", "coeff_alpha")
    }
  }

  reactive_data[[paste0("fit_coeffs_", data_type)]] <- fit_coeffs

  if (use_cov_matrix) {

    cov_matrix <- hot_to_r(cov_input) %>% as.matrix()
    colnames(cov_matrix) <- rownames(fit_coeffs)
    rownames(cov_matrix) <- rownames(fit_coeffs)
  } else {

    # Calculate covariance matrix if not provided
    cov_matrix <- matrix(0, nrow = nrow(fit_coeffs), ncol = nrow(fit_coeffs)) %>%
      `colnames<-`(rownames(fit_coeffs)) %>%
      `rownames<-`(rownames(fit_coeffs))
    for (x_var in rownames(cov_matrix)) {
      cov_matrix[[x_var, x_var]] <- fit_coeffs[x_var, "std.error"] ^ 2
    }
  }
  reactive_data[[paste0("cov_mat_", data_type)]] <- cov_matrix
}



#'  Update dose estimation box with yield values
#'
#' @param num_cases number of cases to estimate.
#' @param reactive_data to access reactive data.
#' @param output output for the UI.
#' @param yield_fun function for calculating yield, in utils_estimation.R
#' @param manual logical, indicates if using manual input or file data.
#' @param table the data input table.
#'
#' @return yield per dose. Numeric
#'
update_outputs <- function(num_cases, reactive_data, output, yield_fun, manual, table) {

  # Function to render tables
  render_output_table <- function(df, output_id) {

    output[[output_id]] <- renderRHandsontable({
      rhandsontable(df) %>%
        hot_cols(format = "0.00000") %>%
        hot_cols(colWidths = 80, halign = "htCenter")
    })
  }
  ID_mx <- list()

  # Gamma
  df_dose_gamma <- do.call(rbind, lapply(1:num_cases, function(i) {
    data.frame(
      lower = reactive_data$est[[i]]$gamma[['lwr']],
      estimate = reactive_data$est[[i]]$gamma[['est']],
      upper = reactive_data$est[[i]]$gamma[['upr']]
    )
  }))
  for (i in 1:num_cases){
    ID_mx[[i]] <- table$data[[i]][[1]]
  }
  ID_mx <- do.call(rbind, ID_mx)
  df_dose_gamma <- cbind(ID = ID_mx, df_dose_gamma)
  render_output_table(df_dose_gamma, "value_g_d")
  reactive_data$df_dose_g <- df_dose_gamma

  if (manual== FALSE){

    fit_coeffs <- reactive_data$gamma_data$fit_coeffs
    fit_var_cov_mat <- reactive_data$gamma_data$fit_var_cov_mat
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])

  }else{
    fit_coeffs <- reactive_data$fit_coeffs_gamma
    fit_coeffs_vec <- fit_coeffs$estimate
    if(length(fit_coeffs_vec)==3){
      names(fit_coeffs_vec) <- c("coeff_C", "coeff_alpha", "coeff_beta")
    }else if(length(fit_coeffs_vec)==2){
      names(fit_coeffs_vec) <- c("coeff_C", "coeff_alpha")
    }

    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs_vec)
  }

  df_yield_gamma <- do.call(rbind, lapply(1:num_cases, function(i) {
    data.frame(
    lower = c(yield_fun(reactive_data$est[[i]]$gamma[['lwr']], general_fit_coeffs)),
    estimate = c(yield_fun(reactive_data$est[[i]]$gamma[['est']], general_fit_coeffs)),
    upper = c(yield_fun(reactive_data$est[[i]]$gamma[['upr']], general_fit_coeffs))
  )
  }))
  df_yield_gamma <- cbind(ID = ID_mx, df_yield_gamma)
  render_output_table(df_yield_gamma, "value_g_y")
  reactive_data$df_yield_g <- df_yield_gamma

  # Neutron
  df_dose_neutrons <- do.call(rbind, lapply(1:num_cases, function(i) {
    data.frame(
    lower = c(reactive_data$est[[i]]$neutron[['lwr']]),
    estimate = c(reactive_data$est[[i]]$neutron[['est']]),
    upper = c(reactive_data$est[[i]]$neutron[['upr']])
  )
  }))
  df_dose_neutrons <- cbind(ID = ID_mx, df_dose_neutrons)
  render_output_table(df_dose_neutrons, "value_n_d")
  reactive_data$df_dose_n <- df_dose_neutrons

  if (manual == FALSE){
    fit_coeffs <- reactive_data$neutrons_data$fit_coeffs
    fit_var_cov_mat <- reactive_data$neutrons_data$fit_var_cov_mat
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
  }else{
    fit_coeffs <- reactive_data$fit_coeffs_neutrons
    fit_coeffs_vec <- fit_coeffs$estimate
    if(length(fit_coeffs_vec)==3){
      names(fit_coeffs_vec) <- c("coeff_C", "coeff_alpha", "coeff_beta")
    }else if(length(fit_coeffs_vec)==2){
      names(fit_coeffs_vec) <- c("coeff_C", "coeff_alpha")
    }
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs_vec)
  }

  df_yield_neutrons <- do.call(rbind, lapply(1:num_cases, function(i) {
    data.frame(
    lower = c(yield_fun(reactive_data$est[[i]]$neutron[['lwr']], general_fit_coeffs)),
    estimate = c(yield_fun(reactive_data$est[[i]]$neutron[['est']], general_fit_coeffs)),
    upper = c(yield_fun(reactive_data$est[[i]]$neutron[['upr']], general_fit_coeffs))
  )
  }))
  df_yield_neutrons <- cbind(ID = ID_mx, df_yield_neutrons)
  render_output_table(df_yield_neutrons, "value_n_y")
  reactive_data$df_yield_n <- df_yield_neutrons

  # Total
  df_dose_total <- do.call(rbind, lapply(1:num_cases, function(i) {
    data.frame(
    lower = c(reactive_data$est[[i]]$total[['lwr']]),
    estimate = c(reactive_data$est[[i]]$total[['est']]),
    upper = c(reactive_data$est[[i]]$total[['upr']])
  )
  }))
  df_dose_total <- cbind(ID = ID_mx, df_dose_total)
  render_output_table(df_dose_total, "value_t_d")
  reactive_data$df_dose_total <- df_dose_total


  df_yield_total <- data.frame(
    lower = c(df_yield_neutrons$lower + df_yield_gamma$lower),
    estimate = c(df_yield_neutrons$estimate + df_yield_gamma$estimate),
    upper = c(df_yield_neutrons$upper + df_yield_gamma$upper)
  )
  df_yield_total <- cbind(ID = ID_mx, df_yield_total)
  render_output_table(df_yield_total, "value_t_y")
  reactive_data$df_yield_total <- df_yield_total

  # Prepare output yield
  yield_neutron <- c(est = df_yield_neutrons$estimate, lwr = df_yield_neutrons$lower, upr = df_yield_neutrons$upper)
  yield_gamma <- c(est = df_yield_gamma$estimate, lwr = df_yield_gamma$lower, upr = df_yield_gamma$upper)
  yield_total <- c(est = df_yield_total$estimate, lwr = df_yield_total$lower, upr = df_yield_total$upper)

  return(list(gamma = yield_gamma, neutron = yield_neutron, total = yield_total))
}



#' Criticality Plot
#'
#' @param name the dose to plot.
#' @param est_doses List of dose estimations results.
#' @param fit_coeffs Fitting coefficients matrix.
#' @param fit_var_cov_mat Fitting variance-covariance matrix.
#' @param curve_type gamma or neutron.
#' @param protracted_g_value Protracted \eqn{G(x)} value.
#' @param conf_int_curve Confidence interval of the curve.
#' @param place UI, report or save. Where the plot will be displayed.
#'
#' @return ggcurve
#' @example man/examples/plot_estimated_dose_curve_mx_example.R
#' @export
#'
plot_estimated_dose_curve_mx <- function(name, est_doses, fit_coeffs, fit_var_cov_mat,
                                         curve_type,
                                         protracted_g_value = 1,
                                         conf_int_curve = 0.95, place) {

  if (is.null(est_doses) ||
      (curve_type == "gamma" && is.null(est_doses$gamma)) ||
      (curve_type == "neutron" && is.null(est_doses$neutron))) {
    return(NULL)
  }
  selected_dose <- if (curve_type == "gamma") est_doses$gamma else est_doses$neutron
  color <- if (curve_type == "gamma") "blue" else "red"
  i <- name


  max_dose <- max(selected_dose[["upr"]], na.rm = TRUE) * 1.05

  # Generalised fit coefficients and var-cov matrix
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs)
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Correct confidence intervals
  conf_int_curve <- conf_int_curve %>%
    correct_conf_int(general_fit_var_cov_mat, protracted_g_value, type = "curve")

  curves_data <- data.frame(
    dose = seq(0, max_dose, length.out = 100)
  ) %>%
    dplyr::mutate(
      yield = calculate_yield(.data$dose, type = "estimate", general_fit_coeffs, NULL, protracted_g_value, 0),
      yield_low = calculate_yield(.data$dose, type = "lower", general_fit_coeffs, general_fit_var_cov_mat, protracted_g_value, conf_int_curve),
      yield_upp = calculate_yield(.data$dose, type = "upper", general_fit_coeffs, general_fit_var_cov_mat, protracted_g_value, conf_int_curve)
    )

  est_doses_df <- data.frame(
    dose = c(selected_dose[["lwr"]], selected_dose[["est"]], selected_dose[["upr"]]),
    yield = c(
      yield_fun(selected_dose[["lwr"]], general_fit_coeffs, protracted_g_value),
      yield_fun(selected_dose[["est"]], general_fit_coeffs, protracted_g_value),
      yield_fun(selected_dose[["upr"]], general_fit_coeffs, protracted_g_value)
    ),
    type = factor(c("Lower", "Estimation", "Upper"))
  )
  plot_labels <- if (place == "UI") {
    ggplot2::labs(x = "Dose (Gy)", y = "dics/cells", title = paste0("Curve plot Case number: ", " ", i))
  } else {
    ggplot2::labs(x = "Dose (Gy)", y = "dics/cells")
  }

  gg_curve <- ggplot2::ggplot(curves_data) +
    # Fitted curve
    ggplot2::geom_line(
      ggplot2::aes(x = .data$dose, y = .data$yield),
      color = color,
      linetype = "dashed"
    ) +
    # Confidence bands
    ggplot2::geom_ribbon(
      ggplot2::aes(x = .data$dose, ymin = .data$yield_low, ymax = .data$yield_upp),
      alpha = 0.15,
      fill = color
    ) +
    # Estimated dose points
    ggplot2::geom_point(
      data = est_doses_df,
      ggplot2::aes(x = .data$dose, y = .data$yield, shape = .data$type),
      size = 4,
      color = "black"
    ) +
    ggplot2::scale_shape_manual(
      name = "Dose Type",
      values = c("Lower" = 15, "Estimation" = 16, "Upper" = 17),
      labels = c("Estimation","Lower", "Upper")
    ) +
    plot_labels +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right") +
    if (place == "save") {
      list(
        ggplot2::labs(caption = "Created with Biodosetools"),
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
      )
    } else {
      NULL
    }

  return(gg_curve)
}



#' Plot at the UI and save
#'
#' @param data_type gamma or neutron
#' @param reactive_data access to the reactive data
#' @param output connection with UI
#' @param input what you upload in the UI
#' @param manual logical, indicates if using manual input or file data.
#' @param dose_plot the number of the dose case to plot.
#' @param name_num associates a number to the name.
#'
#' @return plot at UI and saving plot server logic
#'
generate_plot_and_download <- function(data_type, reactive_data, output, input, manual, dose_plot, name_num) {
  if (data_type == "gamma") {
    fit_coeffs <- if (manual) {
      fit_coeffs_vector <- reactive_data$fit_coeffs_gamma[, 1]
      if(length(fit_coeffs_vector)==3){
        names(fit_coeffs_vector) <- c("coeff_C", "coeff_alpha", "coeff_beta")
        fit_coeffs_vector
      }else if(length(fit_coeffs_vector)==2){
        names(fit_coeffs_vector) <- c("coeff_C", "coeff_alpha")
        fit_coeffs_vector
      }
    } else {
      reactive_data$gamma_data$fit_coeffs[, 1]
    }

    cov_matrix <- if (manual) {
      cov_mat <- reactive_data$cov_mat_gamma
      if(ncol(cov_mat)==3){
        colnames(cov_mat) <- c("coeff_C", "coeff_alpha", "coeff_beta")
        rownames(cov_mat) <- c("coeff_C", "coeff_alpha", "coeff_beta")
        cov_mat
      }else if(ncol(cov_mat)==2){
        colnames(cov_mat) <- c("coeff_C", "coeff_alpha")
        rownames(cov_mat) <- c("coeff_C", "coeff_alpha")
        cov_mat
      }
    } else {
      reactive_data$gamma_data$fit_var_cov_mat
    }

    filename_prefix <- "estimation-curve-mx-gamma"
    plot_output_id <- "curve_plot_mx_g"
    download_output_id <- "save_plot_g"

  } else {  # data_type == "neutron"
    fit_coeffs <- if (manual) {
      fit_coeffs_vector <- reactive_data$fit_coeffs_neutrons[, 1]
      if(length(fit_coeffs_vector)==3){
        names(fit_coeffs_vector) <- c("coeff_C", "coeff_alpha", "coeff_beta")
        fit_coeffs_vector
      }else if(length(fit_coeffs_vector)==2){
        names(fit_coeffs_vector) <- c("coeff_C", "coeff_alpha")
        fit_coeffs_vector
      }
    } else {
      reactive_data$neutrons_data$fit_coeffs[, 1]
    }

    cov_matrix <- if (manual) {
      cov_mat <- reactive_data$cov_mat_neutrons
      if(ncol(cov_mat)==3){
        colnames(cov_mat) <- c("coeff_C", "coeff_alpha", "coeff_beta")
        rownames(cov_mat) <- c("coeff_C", "coeff_alpha", "coeff_beta")
        cov_mat
      }else if(ncol(cov_mat)==2){
        colnames(cov_mat) <- c("coeff_C", "coeff_alpha")
        rownames(cov_mat) <- c("coeff_C", "coeff_alpha")
        cov_mat
      }
    } else {
      reactive_data$neutrons_data$fit_var_cov_mat
    }

    filename_prefix <- "estimation-curve-mx-neutron"
    plot_output_id <- "curve_plot_mx_n"
    download_output_id <- "save_plot_n"
  }

  # Generate plot
    reactive_data[[paste0("plot_", data_type)]] <- plot_estimated_dose_curve_mx(
      name = dose_plot(),
      est_doses = reactive_data$est[[name_num()]],
      fit_coeffs = fit_coeffs,
      fit_var_cov_mat = cov_matrix,
      curve_type = data_type,
      place = "UI"
    )
    # Generate plot save
    reactive_data[[paste0("plot_save_", data_type)]] <- plot_estimated_dose_curve_mx(
      name = dose_plot(),
      est_doses = reactive_data$est[[name_num()]],
      fit_coeffs = fit_coeffs,
      fit_var_cov_mat = cov_matrix,
      curve_type = data_type,
      place = "save"
    )

    reactive_data$plot_list_gamma <- reactive({
      plot_list_gamma <- list()

      for (i in seq_len(input$num_case_mx)) {

        plot <- plot_estimated_dose_curve_mx(
          name = input$select_case,
          est_doses = reactive_data$est[[i]],
          fit_coeffs = fit_coeffs,
          fit_var_cov_mat = cov_matrix,
          curve_type = "gamma",
          place = "report"
        ) +
          ggplot2::ggtitle(paste0("Curve Plot - Case ", reactive_data$df_dose_g[,1][i]))

          if (!is.null(plot)) {
            plot_list_gamma[[i]] <- plot
          }
      }
      return(plot_list_gamma)
    })

    reactive_data$plot_list_neutron <- reactive({
      plot_list_neutron <- list()

      for (i in seq_len(input$num_case_mx)) {
        plot <- plot_estimated_dose_curve_mx(
          name = input$select_case,
          est_doses = reactive_data$est[[i]],
          fit_coeffs = fit_coeffs,
          fit_var_cov_mat = cov_matrix,
          curve_type = "neutron",
          place = "report"
        )+
          ggplot2::ggtitle(paste0("Curve Plot - Case ", reactive_data$df_dose_g[,1][i]))

        if (!is.null(plot)) {
          plot_list_neutron[[i]] <- plot
        }
      }
      return(plot_list_neutron)
    })

  output[[plot_output_id]] <- renderPlot({
    reactive_data[[paste0("plot_", data_type)]]
  })

  output[[download_output_id]] <- downloadHandler(
    filename = function() {
      paste(filename_prefix, ifelse(manual, "m", "f"), Sys.Date(), input[[paste0("save_plot_format_", data_type)]], sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(
        plot = reactive_data[[paste0("plot_save_", data_type)]],
        filename = file,
        width = 6, height = 4.5, dpi = 96,
        device = gsub("\\.", "", input[[paste0("save_plot_format_", data_type)]])
      )
    }
  )
}
