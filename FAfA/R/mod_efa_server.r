#' EFA Factor Retention Server Module
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @export
efa_server_fac_ret <- function(id, data, error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {
    factor_ret_result <- reactiveVal(NULL)
    factor_ret_cache <- new_session_cache()

    observeEvent(data(), {
      clear_session_cache(factor_ret_cache)
      factor_ret_result(NULL)
    }, ignoreNULL = TRUE)

    observeEvent(list(
      input$dimension_methods,
      input$lubbe_iterations,
      input$lubbe_quantile,
      input$lubbe_seed
    ), {
      factor_ret_result(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$run_factor_ret, {
      req(data())

      lubbe_settings <- if (identical(input$dimension_methods, "pa_lubbe")) {
        list(
          iterations = input$lubbe_iterations,
          quantile = input$lubbe_quantile,
          seed = input$lubbe_seed
        )
      } else {
        NULL
      }
      cache_key <- session_cache_key(input$dimension_methods, lubbe_settings)
      cached_result <- session_cache_get(factor_ret_cache, cache_key)
      if (!is.null(cached_result)) {
        factor_ret_result(cached_result)
        showNotification("Saved session result was used.", type = "message")
        return()
      }

      result <- tryCatch({
        if (identical(input$dimension_methods, "scree_plot")) {
          list(
            method = "scree_plot",
            table = calculate_scree_eigenvalues(data())
          )
        } else {
          method_table <- factor_ret(
            data(),
            method = input$dimension_methods,
            n.iter = input$lubbe_iterations %||% 100,
            quant = input$lubbe_quantile %||% 0.95,
            seed = input$lubbe_seed %||% 2026
          )
          method_labels <- rownames(method_table)
          rownames(method_table) <- NULL
          list(
            method = input$dimension_methods,
            table = data.frame(
              Method = method_labels,
              method_table,
              check.names = FALSE
            )
          )
        }
      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("EFA", "Factor retention error")
        showNotification(
          paste("Factor retention analysis failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        NULL
      })

      factor_ret_result(result)
      if (!is.null(result)) session_cache_set(factor_ret_cache, cache_key, result)
    })

    output$dim_ret_results <- renderTable({
      result <- factor_ret_result()
      req(result)
      result$table
    }, rownames = FALSE, digits = 3)

    output$scree_plot <- renderPlot({
      result <- factor_ret_result()
      validate(need(
        !is.null(result) && identical(result$method, "scree_plot"),
        "Select Scree Plot and run the analysis."
      ))
      draw_scree_plot(result$table)
    })

    output$download_scree_png <- downloadHandler(
      filename = function() paste0("scree_plot_", Sys.Date(), ".png"),
      content = function(file) {
        result <- factor_ret_result()
        req(result, identical(result$method, "scree_plot"))
        grDevices::png(file, width = 8, height = 6, units = "in", res = 300)
        on.exit(grDevices::dev.off(), add = TRUE)
        draw_scree_plot(result$table)
      }
    )

    output$download_scree_svg <- downloadHandler(
      filename = function() paste0("scree_plot_", Sys.Date(), ".svg"),
      content = function(file) {
        result <- factor_ret_result()
        req(result, identical(result$method, "scree_plot"))
        grDevices::svg(file, width = 8, height = 6, pointsize = 12, family = "sans")
        on.exit(grDevices::dev.off(), add = TRUE)
        draw_scree_plot(result$table)
      }
    )
  })
}

#' EFA Analysis Server Module
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param error_recorder Optional function used for anonymous diagnostics.
#' @export
efa_server_analysis <- function(id, data, error_recorder = NULL) {
  moduleServer(id, function(input, output, session) {
    efa_res <- reactiveVal(NULL)
    efa_cache <- new_session_cache()

    observeEvent(data(), {
      clear_session_cache(efa_cache)
      efa_res(NULL)
    }, ignoreNULL = TRUE)

    observeEvent(input$run_efa, {
      req(data())
      cache_key <- session_cache_key(
        input$cor_kind,
        input$number_factor,
        input$rotating_method,
        input$fact_method
      )
      cached_result <- session_cache_get(efa_cache, cache_key)
      if (!is.null(cached_result)) {
        efa_res(cached_result)
        showNotification("Saved session result was used.", type = "message")
        return()
      }
      showNotification("Running EFA...", type="message")

      tryCatch({
        cor_type_arg <- if(input$cor_kind == "pea") "cor" else "poly"

        res <- psych::fa(
          r = data(),
          nfactors = as.numeric(input$number_factor),
          rotate = input$rotating_method,
          fm = input$fact_method,
          cor = cor_type_arg
        )

        efa_res(res)
        session_cache_set(efa_cache, cache_key, res)
        showNotification("EFA Completed!", type="message")

      }, error = function(e) {
        if (is.function(error_recorder)) error_recorder("EFA", "Analysis error")
        showNotification(paste("EFA Failed:", e$message), type="error", duration=10)
        efa_res(NULL)
      })
    })
    return(efa_res)
  })
}

format_efa_variance_table <- function(vaccounted, language = "en") {
  variance_table <- as.data.frame(unclass(vaccounted), check.names = FALSE)
  percentage_rows <- intersect(
    c("Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion"),
    rownames(variance_table)
  )
  if (length(percentage_rows)) {
    percentage_table <- variance_table[percentage_rows, , drop = FALSE] * 100
    rownames(percentage_table) <- paste0(percentage_rows, " (%)")
    variance_table <- rbind(variance_table, percentage_table)
  }

  if (identical(fafa_language(language), "tr")) {
    row_labels <- c(
      "SS loadings" = "Kareler Toplam\u0131 Y\u00fckleri",
      "Proportion Var" = "A\u00e7\u0131klanan Varyans Oran\u0131",
      "Cumulative Var" = "Birikimli Varyans Oran\u0131",
      "Proportion Explained" = "A\u00e7\u0131klanan Oran",
      "Cumulative Proportion" = "Birikimli A\u00e7\u0131klanan Oran",
      "Proportion Var (%)" = "A\u00e7\u0131klanan Varyans (%)",
      "Cumulative Var (%)" = "Birikimli Varyans (%)",
      "Proportion Explained (%)" = "A\u00e7\u0131klanan Oran (%)",
      "Cumulative Proportion (%)" = "Birikimli A\u00e7\u0131klanan Oran (%)"
    )
    matched <- match(rownames(variance_table), names(row_labels))
    rownames(variance_table)[!is.na(matched)] <- unname(row_labels[matched[!is.na(matched)]])
  }

  variance_table
}

#' EFA Reporting Server Module
#'
#' @param id Module namespace ID.
#' @param data Input data (reactive).
#' @param efa_output_reactive Reactive containing the EFA results.
#' @param efa_settings_reactive Reactive containing the EFA settings.
#' @param language Optional reactive interface language.
#' @export
efa_server_report <- function(id, data, efa_output_reactive,
                              efa_settings_reactive, language = NULL) {
  moduleServer(id, function(input, output, session) {
    local_text <- function(english, turkish) {
      fafa_text(language, english, turkish)
    }
    heatmap_palettes <- list(
      blue_red = c("#2166AC", "#FFFFFF", "#B2182B"),
      grayscale = c("#525252", "#FFFFFF", "#BDBDBD"),
      purple_green = c("#762A83", "#FFFFFF", "#1B7837"),
      orange_blue = c("#E66101", "#FFFFFF", "#0571B0"),
      teal_rose = c("#0F766E", "#FFFFFF", "#BE185D")
    )

    heatmap_correlation <- reactive({
      req(data(), efa_settings_reactive())
      current_data <- data()
      validate(need(
        ncol(current_data) >= 2,
        local_text(
          "Heatmap requires at least two numeric variables.",
          "Is\u0131 haritas\u0131 i\u00e7in en az iki say\u0131sal de\u011fi\u015fken gerekir."
        )
      ))

      correlation_result <- tryCatch({
        if (identical(efa_settings_reactive()$cor_kind, "pea")) {
          stats::cor(current_data, use = "pairwise.complete.obs")
        } else {
          suppressWarnings(psych::polychoric(current_data)$rho)
        }
      }, error = function(e) e)

      validate(need(
        !inherits(correlation_result, "error"),
        if (inherits(correlation_result, "error")) {
          paste(
            local_text(
              "Correlation matrix could not be calculated:",
              "Korelasyon matrisi hesaplanamad\u0131:"
            ),
            conditionMessage(correlation_result)
          )
        } else {
          local_text(
            "Correlation matrix could not be calculated.",
            "Korelasyon matrisi hesaplanamad\u0131."
          )
        }
      ))

      correlation_result <- as.matrix(correlation_result)
      validate(need(
        all(is.finite(correlation_result)),
        local_text(
          "The correlation matrix contains missing or infinite values.",
          "Korelasyon matrisi kay\u0131p veya sonsuz de\u011ferler i\u00e7eriyor."
        )
      ))
      correlation_result
    })

    heatmap_plot <- reactive({
      correlation_matrix <- heatmap_correlation()
      selected_palette <- input$heatmap_palette %||% "blue_red"
      palette_colors <- heatmap_palettes[[selected_palette]] %||%
        heatmap_palettes$blue_red

      variable_names <- colnames(correlation_matrix)
      if (is.null(variable_names)) {
        variable_names <- paste0("V", seq_len(ncol(correlation_matrix)))
      }
      row_names <- rownames(correlation_matrix) %||% variable_names
      lower_indices <- which(lower.tri(correlation_matrix, diag = TRUE), arr.ind = TRUE)
      plot_data <- data.frame(
        heatmap_x = factor(variable_names[lower_indices[, "col"]], levels = variable_names),
        heatmap_y = factor(row_names[lower_indices[, "row"]], levels = rev(row_names)),
        heatmap_correlation = correlation_matrix[lower_indices],
        stringsAsFactors = FALSE
      )

      plot_result <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = heatmap_x, y = heatmap_y, fill = heatmap_correlation)
      ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
        ggplot2::scale_fill_gradient2(
          low = palette_colors[1],
          mid = palette_colors[2],
          high = palette_colors[3],
          midpoint = 0,
          limits = c(-1, 1),
          name = local_text("Correlation", "Korelasyon")
        ) +
        ggplot2::coord_fixed() +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(family = "sans"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )

      show_values <- !identical(input$heatmap_show_values, FALSE)
      if (show_values) {
        label_digits <- if (ncol(correlation_matrix) <= 20) 2 else 1
        plot_data$heatmap_label <- formatC(
          plot_data$heatmap_correlation,
          format = "f",
          digits = label_digits
        )
        label_size <- max(1.3, min(3, 42 / ncol(correlation_matrix)))
        plot_result <- plot_result + ggplot2::geom_text(
          data = plot_data,
          ggplot2::aes(label = heatmap_label),
          size = label_size
        )
      }
      plot_result
    })

    # KMO & Bartlett logic
    output$kmo_result <- renderUI({
      req(data())
      tryCatch({
        kmo_res <- psych::KMO(data())
        val <- round(kmo_res$MSA, 3)
        color <- if(val >= 0.8) "green" else if(val >= 0.6) "orange" else "red"
        label <- local_text(
          "KMO Measure of Sampling Adequacy:",
          "KMO \u00d6rneklem Yeterli\u011fi \u00d6l\u00e7\u00fcs\u00fc:"
        )
        HTML(paste0("<b>", label, "</b> <span style='color:", color, "'>", val, "</span>"))
      }, error = function(e) paste(local_text("Error:", "Hata:"), e$message))
    })

    output$bartlett <- renderTable({
      req(data())
      tryCatch({
        cor_mat <- if(efa_settings_reactive()$cor_kind == "pea") {
          stats::cor(data(), use = "pairwise.complete.obs")
        } else psych::polychoric(data())$rho
        test <- psych::cortest.bartlett(cor_mat, n = nrow(data()))
        result <- data.frame(
          Statistic = round(test$chisq, 2),
          df = test$df,
          p_value = if(test$p.value < 0.001) "< .001" else round(test$p.value, 3)
        )
        if (identical(fafa_language(language), "tr")) {
          names(result) <- c("\u0130statistik", "sd", "p_de\u011feri")
        }
        result
      }, error = function(e) {
        result <- data.frame(e$message, check.names = FALSE)
        names(result) <- local_text("Error", "Hata")
        result
      })
    })

    # Loadings Table
    output$efa_result_str <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      loadings_df <- as.data.frame(unclass(res$loadings))
      if(!is.null(res$communality)) {
        loadings_df$h2 <- res$communality
      }
      return(loadings_df)
    }, rownames = TRUE, digits = 3)

    # Variance Table
    output$efa_result_expl_var <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      if(is.null(res$Vaccounted)) {
        message_text <- local_text(
          "Variance table not available.",
          "A\u00e7\u0131klanan varyans tablosu kullan\u0131lam\u0131yor."
        )
        result <- data.frame(message_text, check.names = FALSE)
        names(result) <- local_text("Message", "\u0130leti")
        return(result)
      }
      format_efa_variance_table(res$Vaccounted, fafa_language(language))
    }, rownames = TRUE, digits = 3)

    # Phi Matrix
    output$efa_result_interf_cor <- renderTable({
      req(efa_output_reactive())
      res <- efa_output_reactive()
      if(is.null(res$Phi)) {
        info_text <- local_text(
          "Correlations not available. Reasons: 1) Orthogonal rotation used, or 2) Only 1 factor extracted.",
          "Korelasyonlar kullan\u0131lam\u0131yor. Nedenler: 1) Dik d\u00f6nd\u00fcrme kullan\u0131ld\u0131 veya 2) Yaln\u0131zca bir fakt\u00f6r \u00e7\u0131kar\u0131ld\u0131."
        )
        result <- data.frame(info_text, check.names = FALSE)
        names(result) <- local_text("Info", "Bilgi")
        return(result)
      }
      as.data.frame(unclass(res$Phi))
    }, rownames = TRUE, digits = 3)

    output$heat_map <- renderPlot({
      heatmap_plot()
    })

    output$cor_range_text <- renderText({
      tryCatch({
        cor_mat <- heatmap_correlation()
        tri <- cor_mat[upper.tri(cor_mat)]
        paste0(
          local_text("Off-diagonal correlations - ", "K\u00f6\u015fegen d\u0131\u015f\u0131 korelasyonlar - "),
          local_text("Min: ", "En d\u00fc\u015f\u00fck: "), round(min(tri),  3), "  |  ",
          local_text("Max: ", "En y\u00fcksek: "), round(max(tri),  3), "  |  ",
          local_text("Mean: ", "Ortalama: "), round(mean(tri), 3), "  |  ",
          local_text("Median: ", "Ortanca: "), round(stats::median(tri), 3)
        )
      }, error = function(e) local_text(
        "Correlation range could not be computed.",
        "Korelasyon aral\u0131\u011f\u0131 hesaplanamad\u0131."
      ))
    })

    output$download_heatmap_png <- downloadHandler(
      filename = function() {
        paste0("efa_heatmap_", Sys.Date(), ".png")
      },
      content = function(file) {
        plot_object <- heatmap_plot()
        grDevices::png(
          file,
          width = 9,
          height = 8,
          units = "in",
          res = 300
        )
        on.exit(grDevices::dev.off(), add = TRUE)
        print(plot_object)
      }
    )

    output$download_heatmap_svg <- downloadHandler(
      filename = function() {
        paste0("efa_heatmap_", Sys.Date(), ".svg")
      },
      content = function(file) {
        plot_object <- heatmap_plot()
        grDevices::svg(
          file,
          width = 9,
          height = 8,
          pointsize = 12,
          family = "sans"
        )
        on.exit(grDevices::dev.off(), add = TRUE)
        print(plot_object)
      }
    )

    # Download Loadings
    output$download_efa_loadings <- downloadHandler(
      filename = "efa_loadings.csv",
      content = function(file) {
        req(efa_output_reactive())
        res <- efa_output_reactive()
        write.csv(unclass(res$loadings), file)
      }
    )
    output$download_efa_apa7 <- downloadHandler(
      filename = function() paste0("efa_APA7_report_", Sys.Date(), ".docx"),
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        req(efa_output_reactive(), data(), efa_settings_reactive())
        report_language <- fafa_language(language)
        result <- efa_output_reactive()
        settings <- efa_settings_reactive()

        kmo_value <- tryCatch(psych::KMO(data())$MSA, error = function(e) NA_real_)
        bartlett_result <- tryCatch({
          correlation <- if (identical(settings$cor_kind, "pea")) {
            stats::cor(data(), use = "pairwise.complete.obs")
          } else {
            psych::polychoric(data())$rho
          }
          psych::cortest.bartlett(correlation, n = nrow(data()))
        }, error = function(e) NULL)
        diagnostics <- data.frame(
          Test = c(
            "KMO",
            fafa_text(report_language, "Bartlett chi-square", "Bartlett ki-kare"),
            fafa_text(report_language, "Bartlett df", "Bartlett sd"),
            fafa_text(report_language, "Bartlett p", "Bartlett p")
          ),
          Value = c(
            round(kmo_value, 3),
            if (is.null(bartlett_result)) NA else round(bartlett_result$chisq, 3),
            if (is.null(bartlett_result)) NA else bartlett_result$df,
            if (is.null(bartlett_result)) NA else format.pval(bartlett_result$p.value, digits = 3, eps = 0.001)
          ),
          check.names = FALSE
        )

        loadings <- as.data.frame(unclass(result$loadings), check.names = FALSE)
        loadings <- cbind(
          stats::setNames(data.frame(rownames(loadings)), fafa_text(report_language, "Item", "Madde")),
          loadings
        )
        rownames(loadings) <- NULL
        variance <- if (is.null(result$Vaccounted)) NULL else {
          format_efa_variance_table(result$Vaccounted, report_language)
        }
        sections <- list(
          list(
            title = fafa_text(report_language, "Analysis Settings", "Analiz Ayarlar\u0131"),
            text = paste0(
              fafa_text(report_language, "Factors: ", "Fakt\u00f6r say\u0131s\u0131: "),
              settings$number_factor,
              "; ",
              fafa_text(report_language, "extraction: ", "\u00e7\u0131kar\u0131m: "),
              settings$fact_method,
              "; ",
              fafa_text(report_language, "rotation: ", "d\u00f6nd\u00fcrme: "),
              settings$rotating_method,
              "."
            )
          ),
          list(
            title = fafa_text(report_language, "Factorability Diagnostics", "Fakt\u00f6rlenebilirlik Tan\u0131lamalar\u0131"),
            table = diagnostics
          ),
          list(
            title = fafa_text(report_language, "Factor Loadings", "Fakt\u00f6r Y\u00fckleri"),
            table = loadings,
            note = fafa_text(
              report_language,
              "Loadings are reported from the selected extraction and rotation solution.",
              "Y\u00fckler se\u00e7ilen \u00e7\u0131kar\u0131m ve d\u00f6nd\u00fcrme \u00e7\u00f6z\u00fcm\u00fcnden raporlanm\u0131\u015ft\u0131r."
            )
          )
        )
        if (!is.null(variance)) {
          sections <- c(sections, list(list(
            title = fafa_text(report_language, "Explained Variance", "A\u00e7\u0131klanan Varyans"),
            table = variance
          )))
        }
        if (!is.null(result$Phi)) {
          sections <- c(sections, list(list(
            title = fafa_text(report_language, "Factor Correlations", "Fakt\u00f6r Korelasyonlar\u0131"),
            table = as.data.frame(unclass(result$Phi), check.names = FALSE)
          )))
        }
        write_apa7_report(
          file,
          title = fafa_text(report_language, "Exploratory Factor Analysis Report", "A\u00e7\u0131mlay\u0131c\u0131 Fakt\u00f6r Analizi Raporu"),
          subtitle = paste0("FAfA ", fafa_package_version(), " - APA 7"),
          sections = sections,
          language = report_language
        )
      }
    )
  })
}
