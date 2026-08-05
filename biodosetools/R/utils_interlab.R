#' Summary and curve tables
#'
#' @param num_labs number of laboratories participating.
#' @param list_lab_names list with the laboratory names.
#' @param all_rds list with the rds files.
#'
#' @export
#' @example man/examples/summary_curve_tables_example.R
#' @return list(sorted_table_ilc, sorted_table_curve)

summary_curve_tables <- function(num_labs, list_lab_names, all_rds) {
  req(num_labs)
  list.plot <- list()

  for (L in 1:num_labs) {
    req(all_rds[[L]])

    if(all_rds[[L]]$aberr_module == "translocations")
      req(all_rds[[L]]$case_data$Fg)
    else
      req(all_rds[[L]]$case_data$y)

    #Build the list necessary for the plots
    list.plot[[L]] <- data.frame(
      Lab = list_lab_names[[L]],
      Module = all_rds[[L]]$aberr_module,
      type = if (is.null(as.character(all_rds[[L]]$irr_conds$scoring_method[2]))) NA else as.character(all_rds[[L]]$irr_conds$scoring_method[2]),
      Sample = all_rds[[L]]$case_data$ID,
      N = all_rds[[L]]$case_data$N,
      dics = all_rds[[L]]$case_data$X,
      est = if(!is.null(all_rds[[L]]$est_doses_partial)) all_rds[[L]]$est_doses_partial$estimate else all_rds[[L]]$est_doses_whole$estimate,
      lwr = if(!is.null(all_rds[[L]]$est_doses_partial)) all_rds[[L]]$est_doses_partial$lower else all_rds[[L]]$est_doses_whole$lower,
      upr = if(!is.null(all_rds[[L]]$est_doses_partial)) all_rds[[L]]$est_doses_partial$upper else all_rds[[L]]$est_doses_whole$upper,
      y = if(is.null(all_rds[[L]]$case_data$Fg)) all_rds[[L]]$case_data$y else all_rds[[L]]$case_data$Fg,
      y.err = if(is.null(all_rds[[L]]$case_data$Fg_err)) all_rds[[L]]$case_data$y_err else all_rds[[L]]$case_data$Fg_err,
      rad_qual = if(is.null(as.character(all_rds[[L]]$irr_conds$radiation_quality[2]))) NA else as.character(all_rds[[L]]$irr_conds$radiation_quality[2]),
      cal_air_water = if(is.null(as.character(all_rds[[L]]$irr_conds$cal_air_water[2]))) NA else as.character(all_rds[[L]]$irr_conds$cal_air_water[2]),
      irrad_air_water = if(is.null(as.character(all_rds[[L]]$irr_conds$irrad_air_water[2]))) NA else as.character(all_rds[[L]]$irr_conds$irrad_air_water[2]),
      temperature = if(is.null(as.character(all_rds[[L]]$irr_conds$temperature[2]))) NA else as.character(all_rds[[L]]$irr_conds$temperature[2]),
      dose_rate = if(is.null(as.character(all_rds[[L]]$irr_conds$dose_rate[2]))) NA else as.character(all_rds[[L]]$irr_conds$dose_rate[2]),
      origin_curve = if(is.null(as.character(all_rds[[L]]$irr_conds$origin_curve[2]))) NA else as.character(all_rds[[L]]$irr_conds$origin_curve[2]),
      C = all_rds[[L]]$fit_coeffs["coeff_C", "estimate"],
      alpha = all_rds[[L]]$fit_coeffs["coeff_alpha", "estimate"],
      beta = if(nrow(all_rds[[L]]$fit_coeffs)< 3) NA else all_rds[[L]]$fit_coeffs["coeff_beta", "estimate"],
      SE_C = all_rds[[L]]$fit_coeffs["coeff_C", "std.error"],
      SE_alpha = all_rds[[L]]$fit_coeffs["coeff_alpha", "std.error"],
      SE_beta = if(nrow(all_rds[[L]]$fit_coeffs)< 3) NA else all_rds[[L]]$fit_coeffs["coeff_beta", "std.error"],
      DI = all_rds[[L]]$case_data$DI,
      u  = all_rds[[L]]$case_data$u,
      max_dose_curve = ifelse (length(as.numeric(all_rds[[L]]$max_dose_curve)) == 0, "", as.numeric(all_rds[[L]]$max_dose_curve))
    )
  }
  combined_table <- do.call(rbind, list.plot) # Combine rows
  sorted_table <- combined_table[order(combined_table$Sample, combined_table$Lab), ] #Sort by Sample and Lab

  if(any(sorted_table[, 2] == 'translocations')){
    col_names_summary <- c("Lab", "Module", "Type", "Sample", "N", "X",
                           "estimate", "lower", "upper", "Fg", "Fg.err",
                           "radiation quality", "calibration", "irradiation",
                           "temperature", "dose rate", "curve origin", "C",
                           "alpha", "beta", "C std.error", "alpha std.error",
                           "beta std.error", "DI","u","max curve dose")
    colnames(sorted_table) <- col_names_summary
    sorted_table_ilc <- sorted_table[,c("Lab", "Module", "Type", "Sample", "N", "X",
                                        "estimate", "lower", "upper", "Fg", "Fg.err","DI","u")]


  }else{
    col_names_summary <- c("Lab", "Module", "Type", "Sample", "N", "X",
                           "estimate", "lower", "upper", "y", "y.err",
                           "radiation quality", "calibration", "irradiation",
                           "temperature", "dose rate", "curve origin", "C",
                           "alpha", "beta", "C std.error", "alpha std.error",
                           "beta std.error", "DI","u","max curve dose")

    colnames(sorted_table) <- col_names_summary
    sorted_table_ilc <- sorted_table[,c("Lab", "Module", "Type", "Sample", "N", "X",
                                        "estimate", "lower", "upper", "y", "y.err","DI","u")]

  }

  sorted_table_curve <- unique(sorted_table[,c("Lab", "Module", "Type", "radiation quality", "calibration", "irradiation",
                                               "temperature", "dose rate", "curve origin", "C",
                                               "alpha", "beta", "C std.error", "alpha std.error",
                                               "beta std.error","max curve dose")])

  return(list(sorted_table_ilc, sorted_table_curve))

}



#' Plot zscore v2
#'
#' @param zscore data frame with Lab, Sample, Type, Reference, Dose,
#' Deviation and Zscore.
#' @param select_method Zscore algorithm.
#' @param sum_table summary table.
#' @param place UI or save.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @example man/examples/plot_interlab_v2_example.R
#' @return \code{ggplot2} object.

plot_interlab_v2 <- function(zscore, select_method, sum_table, place) {
  req(zscore)
  plot_zscore2 <- list()

  for (i in unique(sum_table[["Sample"]])){
    zscore_i <- zscore[zscore$Sample == i, ]

    background <- data.frame(
    ymin = c(-Inf, -3, -2, 2, 3),
    ymax = c(-3, -2, 2, 3, Inf),
    fill = c("unsatisfactory", "questionable", "satisfactory", "questionable","unsatisfactory")
    )

    plot_zscore2[[i]] <-  ggplot(zscore_i) +
      geom_rect(
        data = background,
        aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
        alpha = 0.2
      ) +
      #Points
      geom_point(aes(x = .data$Lab, y = .data$Zscore), size = 3) +

      scale_fill_manual(
        name = "",
        values = c("satisfactory" = "green", "questionable" = "orange", "unsatisfactory" = "red")
      ) +
      #x-axis
      scale_x_discrete(
        breaks = zscore_i$Lab,
        labels = as.character(zscore_i$Lab)
      ) +
      # Add labels and theme
      labs(
        x = "Laboratory",
        y = "Z-Score",
        title = paste0("Z-Score plot. Method:", " ", select_method, ". Sample:", " " , i)
      ) +
        theme_minimal() +
        theme(
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5)
        ) +
      if (place == "save") {
        list(
          ggplot2::labs(caption = "Created with Biodosetools"),
          ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
        )
      } else {
        NULL
      }


  }
  return(plot_zscore2)
}


#' Plot Z-scores all blind samples
#'
#' @param zscore zscore vector to plot for each lab.
#' @param select_method chosen algorithm for zscore.
#' @param place UI or save.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @example man/examples/plot_zscore_all_example.R
#' @return \code{ggplot2} object.

plot_zscore_all <- function(zscore, select_method, place) {
  req(zscore)

  background <- data.frame(
    ymin = c(-Inf, -3, -2, 2, 3),
    ymax = c(-3, -2, 2, 3, Inf),
    fill = c("unsatisfactory", "questionable", "satisfactory", "questionable","unsatisfactory")
  )

  plot_zscore_all <-
    ggplot(zscore) +
    geom_rect(
      data = background,
      aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
      alpha = 0.2
    ) +
    #Points
    geom_jitter(aes(x = .data$Lab, y = .data$Zscore, col = .data$Sample, shape=.data$Sample), size = 3, width = 0.1, height = 0) +
    scale_fill_manual(
      name = "",
      values = c("satisfactory" = "green", "questionable" = "orange", "unsatisfactory" = "red")
    ) +
    #x-axis
    scale_x_discrete(
      breaks = zscore$Lab,
      labels = as.character(zscore$Lab)
    ) +
    # Add labels and theme
    labs(
      x = "Laboratory",
      y = "Z-Score",
      title = paste0("Z-Score plot. Method: ", select_method)
    ) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5)
    )+
    if (place == "save") {
      list(
        ggplot2::labs(caption = "Created with Biodosetools"),
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
      )
    } else {
      NULL
    }

  return(plot_zscore_all)

}


#' Plot Deviaition from ref dose all blind samples
#'
#' @param zscore zscore vector to plot for each lab.
#' @param select_method chosen algorithm for zscore.
#' @param place UI or save.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @example man/examples/plot_deviation_all_example.R
#' @return \code{ggplot2} object.

plot_deviation_all <- function(zscore, select_method, place) {
  req(zscore)

  background <- data.frame(
    ymin = c(-Inf, -1, -0.5, 0.5, 1),
    ymax = c(-1, -0.5, 0.5, 1, Inf),
    fill = c("abs(dev) > 1 Gy",  "0.5 Gy < abs(dev) < 1 Gy",  "abs(dev) < 0.5 Gy",
             "0.5 Gy < abs(dev) < 1 Gy", "abs(dev) > 1 Gy"))


  plot_deviation_all <-
    ggplot(zscore) +
    geom_rect(
      data = background,
      aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
      alpha = 0.2
    ) +
    #Points
    geom_jitter(aes(x = .data$Lab, y = .data$Deviation, col = .data$Sample, shape = .data$Sample), size = 3, width = 0.1, height = 0) +
    scale_fill_manual(
      name = "",
      values = c("abs(dev) < 0.5 Gy" = "green", "0.5 Gy < abs(dev) < 1 Gy" = "orange", "abs(dev) > 1 Gy" = "red")
    ) +
    #x-axis
    scale_x_discrete(
      breaks = zscore$Lab,
      labels = as.character(zscore$Lab)
    ) +
    # Add labels and theme
    labs(
      x = "Laboratory",
      y = "Deviation from reference dose [Gy]",
      title = "Deviation from reference dose"
    ) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5)
    )+
    if (place == "save") {
      list(
        ggplot2::labs(caption = "Created with Biodosetools"),
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
      )
    } else {
      NULL
    }

  return(plot_deviation_all)

}




#' Plot deviation
#'
#' @param zscore data frame with Lab, Sample, Type, Reference, Dose,
#' Deviation and Zscore.
#' @param sum_table summary table.
#' @param place UI or save.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @example man/examples/plot_interlab_deviation_example.R
#' @return \code{ggplot2} object.

plot_interlab_deviation <- function(zscore, sum_table, place) {
  req(zscore)
  plot_deviation2 <- list()

  for (i in unique(sum_table[["Sample"]])){
    background <- data.frame(
      ymin = c(-Inf, -1, -0.5, 0.5, 1),
      ymax = c(-1, -0.5, 0.5, 1, Inf),
      fill = c("abs(dev) > 1 Gy",  "0.5 Gy < abs(dev) < 1 Gy",  "abs(dev) < 0.5 Gy",
               "0.5 Gy < abs(dev) < 1 Gy", "abs(dev) > 1 Gy"))

    zscore_i <- zscore[zscore$Sample == i, ]

    plot_deviation2[[i]] <- ggplot(zscore_i) +
      geom_rect(
        data = background,
        aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
        alpha = 0.2
      ) +
      #Points
      geom_point(aes(x = .data$Lab, y = .data$Deviation), size = 3) +
      scale_fill_manual(
        name = "",
        values = c("abs(dev) < 0.5 Gy" = "green", "0.5 Gy < abs(dev) < 1 Gy" = "orange", "abs(dev) > 1 Gy" = "red")
      ) +
      #x-axis
      scale_x_discrete(
      breaks = zscore_i$Lab,
      labels = as.character(zscore_i$Lab)
      ) +
      # Add labels and theme
      labs(
        x = "Laboratory",
        y = "Deviaition from reference [Gy]",
        title = paste0("Deviation from reference dose Sample:", " " , i)
      ) +
      theme_minimal() +
      theme(
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5)
      )+
      if (place == "save") {
        list(
          ggplot2::labs(caption = "Created with Biodosetools"),
          ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
        )
      } else {
        NULL
      }
  }
  return(plot_deviation2)
}

#' Plot triage interlab
#'
#' @param line_triage reference value for selected sample.
#' @param sum_table summary table.
#' @param place UI or save.
#'
#' @import ggplot2
#' @return \code{ggplot2} object.
#' @example man/examples/plot_triage_interlab_example.R
#' @export

plot_triage_interlab <- function(line_triage, sum_table, place) {

  plot_triage_interlab <- list()

  #if (any(is.na(sum_table[, c("Sample", "estimate", "lower", "upper")]))){
  #  NULL
  #}else{
  all_data <- data.frame(
    labs = sum_table[["Lab"]],
    sample = sum_table[["Sample"]],
    doses = sum_table[["estimate"]],
    lower = sum_table[["lower"]],
    upper = sum_table[["upper"]]
  )

 for (i in unique(sum_table$Sample)){
   data <- subset(all_data, sample == i)
   background <- data.frame(
     ymin = c(0, 1, 2),
     ymax = c(1, 2, Inf),
     fill = c("low exposure", "medium exposure", "high exposure")
   )

   hline_data <- data.frame(
     yintercept = line_triage[[i]],
     label = "Reference Dose"
   )

   data$labs <- factor(data$labs, levels = sort(unique(data$labs)))

   plot_triage_interlab[[i]] <- ggplot(data) +
     geom_rect(
       data = background,
       aes(xmin = -Inf, xmax = Inf, ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
       alpha = 0.2
     ) +
     #Points and error bars
     geom_point(aes(x = .data$labs, y = .data$doses), size = 3) +
     geom_errorbar(aes(x = .data$labs, ymin = .data$lower, ymax = .data$upper), width = 0.2) +
     #Horizontal line
     geom_hline(
       data = hline_data,
       aes(yintercept = .data$yintercept, linetype = .data$label),
       color = "red"
     ) +
     scale_fill_manual(
       name = "Dose Ranges",
       values = c("low exposure" = "green", "medium exposure" = "orange", "high exposure" = "red")
     ) +
     scale_linetype_manual(
       name = "Thresholds",
       values = c("Reference Dose" = "dashed")
     ) +
     # Ensure X-axis does not display decimals
     scale_x_discrete(
       labels = data$labs
     ) +
     #Labels and theme
     labs(
       x = "Laboratories",
       y = "Dose (Gy)",
       title = paste0("Dose estimation plot. Sample: ", " " , i)
     ) +
     theme_minimal() +
     theme(
       legend.text = element_text(size = 7),
       legend.title = element_text(size = 8),
       plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5)
     )+
     if (place == "save") {
       list(
         ggplot2::labs(caption = "Created with Biodosetools"),
         ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
       )
     } else {
       NULL
     }
 }

  return(plot_triage_interlab)
 # }
}


#' Plot curves
#'
#' @param dat data frame of data values.
#' @param curve manual or auto.
#' @param curve_type lin or lin_quad.
#' @param place UI or save.
#'
#' @import ggplot2 dplyr
#' @return \code{ggplot2} object.
#' @example man/examples/curves_plot_example.R
#' @export

curves_plot <- function(dat, curve, curve_type = "lin_quad", place) {
  if (any(is.na(dat[, c("Lab", "Type", "C", "alpha", "beta",
                        "C std.error", "alpha std.error", "beta std.error", "max curve dose")]))){
    NULL
  }else{

    curve.data <- dat[, c("Lab", "Type", "C", "alpha", "beta",
                          "C std.error", "alpha std.error", "beta std.error", "max curve dose")]
    if (curve == "manual") {
      curve.data <- curve.data %>%
        dplyr::filter(.data$Type == "manual") %>% #.data so they are not mistaken for global var
        group_by(.data$Lab) %>%
        slice(1)
      title <- "Manual curves plot"
    } else {
      curve.data <- curve.data %>%
        dplyr::filter(.data$Type == "auto") %>%
        group_by(.data$Lab) %>%
        slice(1)
      title <- "Automatic curves plot"
    }

    if (nrow(curve.data) == 0){
      NULL

    }else{
        xmax <- max(as.numeric(curve.data[[ncol(curve.data)]]))

        plot_data <- data.frame()

        for (i in 1:nrow(curve.data)) {
          cc <- curve.data[i, ]
          d.tmp <- seq(0, cc[[ncol(cc)]], 0.05)

          if (curve_type == "lin_quad") {
            y_values <- fun.curve(d.tmp, as.numeric(cc[c("C", "alpha", "beta")]), curve_type = "lin_quad")
          } else if (curve_type == "lin") {
            y_values <- fun.curve(d.tmp, as.numeric(cc[c("C", "alpha")]), curve_type = "lin")
          }

          temp_df <- data.frame(Dose = d.tmp, Response = y_values, Lab = cc$Lab)
          plot_data <- rbind(plot_data, temp_df)
        }

      ggplot(plot_data, aes(x = .data$Dose, y = .data$Response, color = .data$Lab, group = .data$Lab)) +
          geom_line() +
          labs(x = "Dose (Gy)", y = "aberr/cell", title = title) +
          theme( plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5))+
          if (place == "save") {
            list(
              ggplot2::labs(caption = "Created with Biodosetools"),
              ggplot2::theme(plot.caption = ggplot2::element_text(size = 8, colour = "black", hjust = 1))
            )
          } else {
            NULL
          }
    }
  }
}

#' Bar plots dosimetry
#'
#' @param dat data frame of data values.
#' @param curve manual or auto.
#' @param place UI or save.
#'
#' @import dplyr
#' @importFrom graphics barplot par
#' @return plot
#' @example man/examples/bar_plots_example.R
#' @export

bar_plots <- function(dat, curve, place) {
  req(dat)
  col_indices <- c(1:6, 8)
  col_indices <- col_indices[col_indices <= ncol(dat)]

  if (any(is.na(dat[, col_indices, drop = FALSE]))){
    NULL
  }else{
    tmp <- dat[, col_indices, drop = FALSE]

    if (curve == "manual"){
      tmp <- tmp %>%
        dplyr::filter(.data$Type == "manual")%>%
        group_by(.data$Lab) %>%
        slice(1)
      curve <- "Manual curves"
    }else{
      tmp <- tmp %>%
        dplyr::filter(.data$Type == "auto")%>%
        group_by(.data$Lab) %>%
        slice(1)
      curve <- "Automatic curves"
    }

    if (nrow(tmp) == 0){
      NULL

    }else{
      old <- par(las = 1, mfrow = c(2, 3))
      valid_cols <- c("curve origin", "radiation quality", "calibration", "irradiation", "temperature")
      valid_cols <- valid_cols[valid_cols %in% colnames(tmp)]

      xlab <- setNames(valid_cols, valid_cols)

      for (i in valid_cols) {
        tab.tmp <- table(tmp[, i], useNA = "ifany")

        if (sum(is.na(names(tab.tmp))) > 0)
          names(tab.tmp)[is.na(names(tab.tmp))] <- "NA"
          barplot(tab.tmp, ylab = "Laboratories", xlab = xlab[i], main = curve)

      }
      if (place == "save") {
        mtext(
          "Created with Biodosetools",
          side = 1,
          line = 7, #dist from x
          adj = 1,
          cex = 0.5,
          las = 0
        )
      }
      # Restore
      on.exit(par(old))
      }}
}

#' Boxplot yield
#'
#' @param dat data frame of data values.
#' @param place UI or save.
#' @importFrom grDevices rainbow
#' @importFrom graphics abline boxplot legend mtext stripchart
#' @importFrom stats setNames
#'
#' @return plot
#' @example man/examples/yield_boxplot_example.R
#' @export

yield_boxplot <- function(dat, place) {
  #if (any(is.na(dat[, 10]))){
  #  NULL
 # }else{
     if(any(dat[, "Module"] == "translocations")){
       yield <- dat[, c("Lab","Sample", "Fg")]
       ylab <- "translocations/cell"
       X <- yield$Fg
     }else{
      yield <- dat[, c("Lab","Sample", "y")]
      ylab <- paste0(unique(dat[, "Module"]), "/cell")
      X <- yield$y
    }

    lab_colors <- setNames(rainbow(length(unique(yield$Lab))), unique(yield$Lab))
    yield$Color <- lab_colors[yield$Lab]
    old <- par(las = 2)
    boxplot(X ~ Sample, data = yield, ylab = ylab, xlab = NA, boxwex=.4, main = "Frequencies plot")

    for (lab in unique(yield$Lab)) {
      lab_subset <- yield[yield$Lab == lab, ]  #Filter data for current lab
       if(any(dat[, "Module"] == "translocations")){
         Y <- lab_subset$Fg
       }else{
         Y <- lab_subset$y
       }
      stripchart(Y ~ lab_subset$Sample,
                 data = lab_subset,
                 method = "jitter",
                 pch = 19,
                 col = lab_colors[lab],  #different color for each lab
                 vertical = TRUE,
                 add = TRUE,
                 jitter = 0.15)  #avoid overlap
    }

    legend("topright", legend = names(lab_colors), col = lab_colors, pch = 19, title = "Lab")
    if (place == "save") {
      mtext(
        "Created with Biodosetools",
        side = 1,
        line = 2,
        adj = 1,
        cex = 0.5,
        las = 0
      )
    }
    # Restore
    on.exit(par(old))
 # }
  }

#' Boxplot dose estimates
#'
#' @param dat data frame of data values.
#' @param place UI or save.
#'
#' @return plot
#' @example man/examples/dose_boxplot_example.R
#' @export

dose_boxplot <- function(dat, place) {
  #if (any(is.na(dat[, "estimate"]))){
   # NULL
  #}else{
    dose.est <- dat[, c("Lab","Sample", "estimate")]

    lab_colors <- setNames(rainbow(length(unique(dose.est$Lab))), unique(dose.est$Lab))
    dose.est$Color <- lab_colors[dose.est$Lab]
    old <- par(las = 2)
    boxplot(dose.est$estimate ~ Sample, data = dose.est, ylab = "Dose estimates [Gy]", xlab = NA, boxwex=.4, main = "Doses plot")

    for (lab in unique(dose.est$Lab)) {
      lab_subset <- dose.est[dose.est$Lab == lab, ]  #Filter data for current lab
      stripchart(lab_subset$estimate ~ lab_subset$Sample,
                 data = lab_subset,
                 method = "jitter",
                 pch = 19,
                 col = lab_colors[lab],  #different color for each lab
                 vertical = TRUE,
                 add = TRUE,
                 jitter = 0.15)  #avoid overlap
    }

    legend("topright", legend = names(lab_colors), col = lab_colors, pch = 19, title = "Lab")
    if (place == "save") {
      mtext(
        "Created with Biodosetools",
        side = 1,
        line = 2,
        adj = 1,
        cex = 0.5,
        las = 0
      )
    }
    # Restore
    on.exit(par(old))
  }
  #}

#' U-test
#'
#' @param dat data frame of data values.
#' @param place UI or save.
#'
#' @return plot
#' @example man/examples/u_test_plot_example.R
#' @export

u_test_plot <- function(dat, place) {
 # if (any(is.na(dat[, "u"]))){
#    NULL
 # }else{
  U.data <- dat[, c("Lab","Sample", "u")]

  lab_colors <- setNames(rainbow(length(unique(U.data$Lab))), unique(U.data$Lab))
  U.data$Color <- lab_colors[U.data$Lab]
  old <- par(las = 2)
  boxplot(U.data$u ~ Sample, data = U.data, ylab = "U-test", xlab = NA, boxwex=.4, main = "U-test plot")

  for (lab in unique(U.data$Lab)) {
    lab_subset <- U.data[U.data$Lab == lab, ]  #Filter data for current lab
    stripchart(lab_subset$u ~ lab_subset$Sample,
               data = lab_subset,
               method = "jitter",
               pch = 19,
               col = lab_colors[lab],  #different color for each lab
               vertical = TRUE,
               add = TRUE,
               jitter = 0.15)  #avoid overlap
  }

  legend("topright", legend = names(lab_colors), col = lab_colors, pch = 19, title = "Lab")
  abline(h = c(-1.96, 1.96), col = "red")
  if (place == "save") {
    mtext(
      "Created with Biodosetools",
      side = 1,
      line = 2,
      adj = 1,
      cex = 0.5,
      las = 0
    )
  }
  # Restore
  on.exit(par(old))
#}
}

#' Dispersion index
#'
#' @param dat data frame of data values.
#' @param place UI or save.
#'
#' @return plot
#' @example man/examples/DI_plot_example.R
#' @export

DI_plot <- function(dat, place) {
#  if (any(is.na(dat[, "DI"]))){
#    NULL
#  }else{
    DI.data <- dat[, c("Lab","Sample", "DI")]

    lab_colors <- setNames(rainbow(length(unique(DI.data$Lab))), unique(DI.data$Lab))
    DI.data$Color <- lab_colors[DI.data$Lab]
    old <- par(las = 2)
    boxplot(DI.data$DI ~ Sample, data = DI.data, ylab = "Dispersion Index", xlab = NA, boxwex=.4, main = "Dispersion index plot")

    for (lab in unique(DI.data$Lab)) {
      lab_subset <- DI.data[DI.data$Lab == lab, ] #Filter data for current lab
      stripchart(lab_subset$DI ~ lab_subset$Sample,
                 data = lab_subset,
                 method = "jitter",
                 pch = 19,
                 col = lab_colors[lab],  #different color for each lab
                 vertical = TRUE,
                 add = TRUE,
                 jitter = 0.15)  #avoid overlap
    }
    legend("topright", legend = names(lab_colors), col = lab_colors, pch = 19, title = "Lab")
    abline(h = c(1), col = "red")
    if (place == "save") {
      mtext(
        "Created with Biodosetools",
        side = 1,
        line = 2,
        adj = 1,
        cex = 0.5,
        las = 0
      )
    }
    # Restore
    on.exit(par(old))
}
#}

# #' Deviation from reference
# #'
# #' @param dat data frame of data values.
# #' @param input UI inputs.
# #' @param place UI or save.
# #'
# #' @importFrom graphics abline boxplot legend stripchart
# #' @importFrom grDevices rainbow
# #' @importFrom stats setNames
#'
# #' @return plot
# #' @export
#'
#' Dev_plot <- function(dat, input, place) {
#'   req(dat)
#'   units_plot <- input$dose_or_freq
#'   if(units_plot == "Dose"){
#'     name_plot <- "Deviation from reference [Gy]"
#'   }else if(units_plot == "Frequency"){
#'     name_plot <- "Deviation from reference [aberr/cell]"
#'   }
#'
#'   Dev.data <- as.data.frame(dat)
#'
#'   #Generate unique colors for each lab
#'   lab_colors <- setNames(rainbow(length(unique(Dev.data$Lab))), unique(Dev.data$Lab))
#'   Dev.data$Color <- lab_colors[Dev.data$Lab]
#'
#'   par(las = 2)
#'   #Create boxplots
#'   boxplot(Dev.data$Deviation ~ Dev.data$Sample,
#'           data = Dev.data,
#'           ylab = name_plot,
#'           xlab = "Sample",
#'           boxwex = 0.4,
#'           main = name_plot)
#'
#'   for (lab in unique(Dev.data$Lab)) {
#'     lab_subset <- Dev.data[Dev.data$Lab == lab, ]  #Filter data for current lab
#'     stripchart(lab_subset$Deviation ~ lab_subset$Sample,
#'                data = lab_subset,
#'                method = "jitter",
#'                pch = 19,
#'                col = lab_colors[lab],  #different color for each lab
#'                vertical = TRUE,
#'                add = TRUE,
#'                jitter = 0.15)  #avoid overlap
#'   }
#'
#'
#'   # Add legend
#'   legend("topright", legend = names(lab_colors), col = lab_colors, pch = 19, title = "Lab")
#'
#'   abline(h = 0, col = "orange")
#'
#' }
