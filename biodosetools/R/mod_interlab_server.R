#' Interlab server Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable dplyr readr pdftools tidyr
#' @importFrom grDevices dev.off pdf jpeg png
#' @importFrom stats reshape
#' @noRd

mod_interlab_server <- function(id, label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  #reactive Values
    reactive_data_interlab <- reactiveValues(
      sum_up = NULL,
      curve_up = NULL,
      ilc_plots_dose = NULL,
      ilc_plots_freq = NULL,
      curve_plots=NULL,
      summary_plots=NULL,
      dev_table = NULL,
      zscore_table = NULL
    )

   #If button Dose estimation summary is pressed:
    observeEvent(input$button_table_interlab, {
      list_lab_names <- list()
      all_rds <- list()

      for (i in 1:input$num_labs){
          list_lab_names[[i]] <- input[[paste0("lab_name_", i)]]
          all_rds[[i]] <- readRDS(input[[paste0("load_data_interlab_", i)]]$datapath)
      }
      reactive_data_interlab$list_lab_names <- list_lab_names

      #tables:
      tables_list <- summary_curve_tables(input$num_labs, list_lab_names, all_rds)

      reactive_data_interlab$sum_up <- tables_list[[1]]
      reactive_data_interlab$curve_up<- tables_list[[2]]

      output$summary_table_ilc <- renderRHandsontable({
        rhandsontable(tables_list[[1]], rowHeaders = FALSE) %>%
          hot_cols(colWidths = 120) %>%
          hot_cols(halign = "htRight") %>%
          hot_col(col = c(7:13), format = "0.00000")
      })
      output$summary_table_curve <- renderRHandsontable({
        rhandsontable(tables_list[[2]], rowHeaders = FALSE) %>%
                  hot_cols(colWidths = 120) %>%
                  hot_cols(halign = "htRight")  %>%
                  hot_col(col = c(2:16), format = "0.00000")
      })
    })


  #Observe changes in the summary table and update
    observe({
      req(input$summary_table_ilc)
      updated_table_ilc <- hot_to_r(input$summary_table_ilc)

      updated_sorted_table_ilc <- updated_table_ilc[order(updated_table_ilc$Sample, updated_table_ilc$Lab), ]
      reactive_data_interlab$sum_up <- updated_sorted_table_ilc

      #Force re-rendering of the table
      output$summary_table_ilc <- renderRHandsontable({
        rhandsontable(updated_sorted_table_ilc, rowHeaders = FALSE) %>%
          hot_cols(colWidths = 80) %>%
          hot_cols(halign = "htRight") %>%
          hot_col(col = c(7:13), format = "0.00000")
      })
    })
  #Observe changes in the curve table and update
    observe({
      req(input$summary_table_curve)
      updated_table_curve <- hot_to_r(input$summary_table_curve)

      updated_sorted_table_curve <- updated_table_curve[order(updated_table_curve$Lab), ]
      reactive_data_interlab$curve_up <- updated_sorted_table_curve

      #Force re-rendering of the table
      output$summary_table_curve <- renderRHandsontable({
        rhandsontable(updated_sorted_table_curve, rowHeaders = FALSE) %>%
          hot_cols(colWidths = 80) %>%
          hot_cols(halign = "htRight") %>%
          hot_col(col = c(2:16), format = "0.00000")
      })
    })

  #Update the drop down choices dynamically based on the Sample names
    observe({
      req(reactive_data_interlab$sum_up)
      #Store sample_names in a reactive variable
      reactive_sample_names <- reactive(unique(reactive_data_interlab$sum_up$Sample))

    #Select sample to plot bar-zscore
      output$doses_tab <- renderUI({
        req(reactive_sample_names())

        sample_names <- reactive_sample_names()
          selectInput(
            ns("select_dose_interlab"),
            width = 165,
            label = "Select sample to plot",
            choices = sample_names,
            selected = sample_names[1]
          )
      })
    #Numeric input for reference data for each sample-zscore
      output$ref_vals <- renderUI({
        if(input$dose_or_freq == "Frequency"){
          NULL
        }else{
          req(reactive_sample_names())
          sample_names <- reactive_sample_names()
          lapply(sample_names, function(sample) {
            numericInput(
              ns(paste("ref_", sample, sep = "")),
              label = paste("Reference dose for", sample),
              value = 0,
              step = 0.1,
              min = 0
            )
          })
      }})

    })

  #Returns mini data frame for dose estimation tab
    data_table_mini <- reactive({
      req(reactive_data_interlab$sum_up)

      #Create a minimal data frame
      data_frame <- data.frame(
        Sample = reactive_data_interlab$sum_up[["Sample"]],
        Lab = reactive_data_interlab$sum_up[["Lab"]],
        Dose = reactive_data_interlab$sum_up[["estimate"]],
        Freq = if(any(reactive_data_interlab$sum_up[, "Module"] == "translocations"))
          reactive_data_interlab$sum_up[["Fg"]]
          else reactive_data_interlab$sum_up[["y"]]
        )

      mini_data <- reshape(
        data_frame,
        timevar = "Sample",
        idvar = "Lab",
        direction = "wide"
      )

      #Remove "Dose." prefix in column names
      colnames(mini_data) <- gsub("Dose\\.", "", colnames(mini_data))
      colnames(mini_data) <- gsub("Freq\\.", "", colnames(mini_data))

      return(mini_data)
    })

  #Dose estimation table output
    output$dose_table <- renderRHandsontable({
      req(data_table_mini())
      rhandsontable(data_table_mini()[, c(1, seq(2, ncol(data_table_mini()), by = 2))], rowHeaders = FALSE) %>%
        hot_cols(colWidths = 120) %>%
        hot_cols(halign = "htRight")
    })
  #Frequency estimation table output
    output$freq_table <- renderRHandsontable({
      req(data_table_mini())
      rhandsontable(data_table_mini()[, c(seq(1, ncol(data_table_mini()), by = 2))], rowHeaders = FALSE) %>%
        hot_cols(colWidths = 120) %>%
        hot_cols(halign = "htRight")
    })

  #If summary table is modified modify the mini tables
    observeEvent(input$summary_table_ilc, {
      output$dose_table <- renderRHandsontable({
        req(data_table_mini())
        rhandsontable(data_table_mini()[, c(1, seq(2, ncol(data_table_mini()), by = 2))], rowHeaders = FALSE) %>%
          hot_cols(colWidths = 120) %>%
          hot_cols(halign = "htRight")
      })
      output$freq_table <- renderRHandsontable({
        req(data_table_mini())
        rhandsontable(data_table_mini()[, c(seq(1, ncol(data_table_mini()), by = 2))], rowHeaders = FALSE) %>%
          hot_cols(colWidths = 120) %>%
          hot_cols(halign = "htRight")
      })
    })
#-------------------------------------------------------------------------------
    observeEvent(input$zscore_info, {
      showModal(modalDialog(
        title = "Help",
        HTML('When a new algorithm is selected, please click again
       <span style="padding:5px 10px; background-color:#3399FF; color:white; border-radius:4px; font-weight:bold;">
         Calculate and plot
       </span> to refresh the graphic. Information about the algorithms
       is available in the Data input box help icon.'),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    #If Calculate and plot button from zscore tab is pressed:
    observeEvent(input$button_zscore, {
      req(reactive_data_interlab$sum_up)
    #Error messages if laboratory names are missing
      if (any(reactive_data_interlab$sum_up$Lab == "")) {
      showModal(modalDialog(
        title = "Error",
        "Laboratory names are missing. Please provide them in the designated section in Data Input.",
        footer = modalButton("Close")
      ))
      return()
      }
    #Error messages if data is missing
      if (any(reactive_data_interlab$sum_up == "")|| any(is.na(reactive_data_interlab$sum_up))) {
        showModal(modalDialog(
          title = "Error",
          "Some data is missing. Certain plots may not be generated or may be incomplete.
          Please consider adding the missing information in the Data Summary tab.",
          footer = modalButton("Close")
        ))
      }


    #Get the correct data
      if(input$dose_or_freq == "Dose"){
        data_frame_zscore <- data.frame(
          Lab = reactive_data_interlab$sum_up[["Lab"]],
          Sample = reactive_data_interlab$sum_up[["Sample"]],
          Type = reactive_data_interlab$sum_up[["Type"]],
          Reference = paste0("ref_", reactive_data_interlab$sum_up[["Sample"]]),
          Dose = reactive_data_interlab$sum_up[["estimate"]],
          stringsAsFactors = FALSE
        )

        #Modify reference column
        data_frame_zscore$Reference <-  sapply(data_frame_zscore$Reference, function(ref){
          input[[ref]]
        })
        #Compute z-score by grouping doses by Sample
        for(tt in unique(data_frame_zscore$Sample)){
          sub <- data_frame_zscore[data_frame_zscore$Sample == tt, ]
          sample_ref <- paste0("ref_", tt)
          reference_value <- as.numeric(input[[sample_ref]])
          if (all(is.na(sub$Dose))) {
            zscores <- rep(NA, nrow(sub))
            showModal(modalDialog(
              title = "Error",
              "This algorithm cannot be applied in one or more samples due to insufficient data.",
              footer = modalButton("Close")
            ))
          }else{
            zscores <- rep(NA, nrow(sub))
            # Use the correct algorithm depending on the chosen method
            if (input$select_method_zscore == "algA") {
              zscores <- calc.zValue.new(sub$Dose, type = "dose", alg = "algA", reference_value)
            } else if (input$select_method_zscore == "algB") {
              zscores[!is.na(sub$Dose)] <- calc.zValue.new(sub$Dose[!is.na(sub$Dose)], type = "dose", alg = "algB", reference_value)
            } else if (input$select_method_zscore == "QHampel") {
              zscores[!is.na(sub$Dose)] <- calc.zValue.new(sub$Dose[!is.na(sub$Dose)], type = "dose", alg = "QHampel", reference_value)
            }
          }

          data_frame_zscore$Zscore[data_frame_zscore$Sample == tt] <- zscores
        }
        #Ensure zscore column is numeric
        data_frame_zscore$Deviation <- as.numeric(data_frame_zscore$Dose) - data_frame_zscore$Reference
        data_frame_zscore$Zscore <- as.numeric(data_frame_zscore$Zscore)

        #Inputs for plots
        select_method <- input$select_method_zscore
        select_sample <- input$select_dose_interlab
        sum_table <- reactive_data_interlab$sum_up
        line_triage <- list()
        for (i in unique(sum_table$Sample)){
          line_triage[[i]] <- input[[paste0("ref_", i)]]
        }

        output$plot_interlab_dose <- renderPlot(
          plot_interlab_v2(data_frame_zscore, select_method, sum_table, "UI")[select_sample]
        )

        output$plot_triage_interlab_dose  <- renderPlot(
          plot_triage_interlab(line_triage, sum_table, "UI")[select_sample]
        )
        output$plot_zscore_all_dose  <- renderPlot(
          plot_zscore_all(data_frame_zscore, select_method, "UI")
        )
        output$plot_interlab_deviation <- renderPlot(
          plot_interlab_deviation(data_frame_zscore, sum_table, "UI")[select_sample]
        )
        output$plot_deviation_all <- renderPlot(
          plot_deviation_all(data_frame_zscore, select_method, "UI")
        )

        #save to a list
        ilc_plots_dose <- list()
        ilc_plots_dose[[1]] <- function() plot_triage_interlab(line_triage, sum_table, "save")
        ilc_plots_dose[[2]] <- function() plot_interlab_v2(data_frame_zscore, select_method, sum_table, "save")
        ilc_plots_dose[[3]] <- function() plot_interlab_deviation(data_frame_zscore, sum_table, "save")
        ilc_plots_dose[[4]] <- function() plot_zscore_all(data_frame_zscore, select_method, "save")
        ilc_plots_dose[[5]] <- function() plot_deviation_all(data_frame_zscore, select_method, "save")

        reactive_data_interlab$ilc_plots_dose <- ilc_plots_dose

      }else{

        data_frame_zscore <- data.frame(
          Lab = reactive_data_interlab$sum_up[["Lab"]],
          Sample = reactive_data_interlab$sum_up[["Sample"]],
          Type = reactive_data_interlab$sum_up[["Type"]],
          Frequency = if(any(reactive_data_interlab$sum_up[, "Module"] == "translocations"))
            reactive_data_interlab$sum_up[["Fg"]]
            else reactive_data_interlab$sum_up[["y"]],
          stringsAsFactors = FALSE
        )
        #Compute z-score by grouping doses by Sample
          for(tt in unique(data_frame_zscore$Sample)){
            sub <- data_frame_zscore[data_frame_zscore$Sample == tt, ]
            if (all(is.na(sub$Frequency))) {
              zscores <- rep(NA, nrow(sub))
              showModal(modalDialog(
                title = "Error",
                "This algorithm cannot be applied in one or more samples due to insufficient data.",
                footer = modalButton("Close")
              ))
            }else{
              zscores <- rep(NA, nrow(sub))
              # Use the correct algorithm depending on the chosen method
              if (input$select_method_zscore == "algA") {
                zscores <- calc.zValue.new(sub$Frequency, type = "frequency", alg = "algA", NA)
              } else if (input$select_method_zscore == "algB") {
                zscores[!is.na(sub$Frequency)] <- calc.zValue.new(sub$Frequency[!is.na(sub$Frequency)], type = "frequency", alg = "algB", NA)
              } else if (input$select_method_zscore == "QHampel") {
                zscores[!is.na(sub$Frequency)] <- calc.zValue.new(sub$Frequency[!is.na(sub$Frequency)], type = "frequency", alg = "QHampel", NA)
              }
            }

          data_frame_zscore$Zscore[data_frame_zscore$Sample == tt] <- zscores
        }

        #Ensure zscore column is numeric
        data_frame_zscore$Deviation <- NULL
        data_frame_zscore$Zscore <- as.numeric(data_frame_zscore$Zscore)

        #Inputs for plots
        select_method <- input$select_method_zscore
        select_sample <- input$select_dose_interlab
        sum_table <- reactive_data_interlab$sum_up
        line_triage <- input[[paste0("ref_", select_sample)]]

        output$plot_interlab_freq <- renderPlot(
          plot_interlab_v2(data_frame_zscore, select_method, sum_table, "UI")[select_sample]
        )

        output$plot_zscore_all_freq <- renderPlot(
          plot_zscore_all(data_frame_zscore, select_method, "UI")
        )

        #save to a list
        ilc_plots_freq <- list()
        ilc_plots_freq[[1]] <- function() plot_interlab_v2(data_frame_zscore, select_method, sum_table, "save")
        ilc_plots_freq[[2]] <- function() plot_zscore_all(data_frame_zscore, select_method, "save")
        reactive_data_interlab$ilc_plots_freq <- ilc_plots_freq
      }


      #Assign status based on Z-score
      data_frame_zscore$Status <- case_when(
        is.na(data_frame_zscore$Zscore) | is.infinite(data_frame_zscore$Zscore) ~ NA_character_,
        abs(data_frame_zscore$Zscore) <= 2 ~ "Satisfactory",
        abs(data_frame_zscore$Zscore) > 3 ~ "Unsatisfactory",
        TRUE ~ "Questionable"
      )

      reactive_data_interlab$zscore_table <- data_frame_zscore

    #Z-score table output
      output$zscore_summary <- renderRHandsontable({
        rhandsontable(data_frame_zscore, rowHeaders = FALSE) %>%
          hot_cols(colWidths = 80) %>%
          hot_col(col = "Status", width = 120) %>%
          hot_cols(halign = "htRight")%>%
          hot_col(col = "Status", renderer = "
          function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (value == 'Satisfactory' ) {
              td.style.background = 'rgba(0, 255, 0, 0.2)';
            }else{
              if(value == 'Unsatisfactory'){
                td.style.background = 'rgba(255, 0, 0, 0.2)';
              }else{
                if(value == 'Questionable'){
                  td.style.background = 'rgba(255, 255, 0, 0.2)';
                }
              }
            }
          }")
      })

    })

    #-------------------------------------------------------------------------------
    #If Plot curves button from curves tab is pressed:
    observeEvent(input$button_plot_curves, {
      req(reactive_data_interlab$curve_up)
      #Error messages if laboratory names are missing
      if (any(reactive_data_interlab$curve_up$Lab == "")) {
        showModal(modalDialog(
          title = "Error",
          "Laboratory names are missing. Please provide them in the designated
          section in Data Input.",
          footer = modalButton("Close")
        ))
        return()
      }
      #Error messages if data is missing
      if (any(reactive_data_interlab$curve_up == "")|| any(is.na(reactive_data_interlab$curve_up))) {
        showModal(modalDialog(
          title = "Error",
          "Some data is missing. Certain plots may not be generated or may be
          incomplete. Please consider adding the missing information in the Data Summary tab.",
          footer = modalButton("Close")
        ))
      }


  #PLOTS - Curves -----------------------------------------------------------------
      sum_curves <- reactive_data_interlab$curve_up
      output$curves_manual <- renderPlot(
        curves_plot(sum_curves, curve = "manual",curve_type = "lin_quad", "UI")
      )
      output$curves_auto <- renderPlot(
        curves_plot(sum_curves, curve = "auto",curve_type = "lin_quad", "UI")
      )
      output$barplot_manual <- renderPlot(
        bar_plots(sum_curves, curve="manual", "UI")
      )
      output$barplot_auto <- renderPlot(
        bar_plots(sum_curves, curve="auto", "UI")
      )

      #save to a list
      curve_plots <- list()

      curve_plots[[1]] <- function() curves_plot(sum_curves, curve = "manual", curve_type = "lin_quad", "save")
      curve_plots[[2]] <- function() curves_plot(sum_curves, curve = "auto", curve_type = "lin_quad", "save")
      curve_plots[[3]] <- function() bar_plots(sum_curves, curve="manual", "save")
      curve_plots[[4]] <- function() bar_plots(sum_curves, curve="auto", "save")
      reactive_data_interlab$curve_plots <- curve_plots

    })
    #-------------------------------------------------------------------------------
    #If Calculate and plot summary from Data summary tab is pressed:
    observeEvent(input$button_plot_summary, {
      req(reactive_data_interlab$sum_up)
      #Error messages if laboratory names are missing
      if (any(reactive_data_interlab$sum_up$Lab == "")) {
        showModal(modalDialog(
          title = "Error",
          "Laboratory names are missing. Please provide them in the designated
          section in Data Input.",
          footer = modalButton("Close")
        ))
        return()
      }
      #Error messages if data is missing
      if (any(reactive_data_interlab$sum_up == "")|| any(is.na(reactive_data_interlab$sum_up))) {
        showModal(modalDialog(
          title = "Error",
          "Some data is missing. Certain plots may not be generated or may be
          incomplete. Please consider adding the missing information in the Data Summary tab.",
          footer = modalButton("Close")
        ))
      }


      #PLOTS - Summary -----------------------------------------------------------------

      output$u_test_plot <- renderPlot(
        u_test_plot(reactive_data_interlab$sum_up, "UI")
      )
      output$dose_boxplot <- renderPlot(
        dose_boxplot(reactive_data_interlab$sum_up, "UI")
      )

      output$yield_boxplot <- renderPlot(
        yield_boxplot(reactive_data_interlab$sum_up, "UI")
      )

      output$DI_plot <- renderPlot(
        DI_plot(reactive_data_interlab$sum_up, "UI")
      )
      #save to a list
      summary_plots <- list()
      summary_plots[[1]] <- function() u_test_plot(reactive_data_interlab$sum_up, "save")
      summary_plots[[2]] <- function() DI_plot(reactive_data_interlab$sum_up, "save")
      summary_plots[[3]] <- function() dose_boxplot(reactive_data_interlab$sum_up, "save")
      summary_plots[[4]] <- function() yield_boxplot(reactive_data_interlab$sum_up, "save")
      reactive_data_interlab$summary_plots <- summary_plots

    })

  #Inputs for uploading rds files and name for each laboratory
    output$file_inputs_interlab <- renderUI({
      lapply(1:input$num_labs, function(i) {
        tagList(

          textInput(
            inputId = ns(paste0("lab_name_", i)),
            label = paste("Lab", i, "name"),
            value = ""
          ),

          fileInput(
            inputId = ns(paste0("load_data_interlab_", i)),
            label = paste("Upload dose estimation file for Lab", i, "(.rds)"),
            accept = c(".rds")
          )
        )
      })
    })

#SAVE TABLES -------------------------------------------------------------------

    #Save to csv or xlsx the summary table for ilc data
    output$save_summary_inter_ilc <- downloadHandler(
      filename = function() {
        paste("Interlab_summary_", Sys.Date(), input$save_summary_format_ilc, sep = "")
      },
      content = function(file, verbose = TRUE) {
        data_to_save <- reactive_data_interlab$sum_up

        if (input$save_summary_format_ilc == ".csv") {
          write.csv(data_to_save, file)
        } else if (input$save_summary_format_ilc == ".tex") {
          if(verbose){
            xtable::print.xtable(xtable::xtable(data_to_save), type = "latex", file = file)
          }
        } else if(input$save_summary_format_ilc == ".xlsx"){
          write.xlsx(data_to_save, file, rowNames = FALSE)
        }else{
          stop("Unsupported file format selected.")
        } })

    #Save to csv or xlsx the summary table for curve data
    output$save_summary_inter_curve <- downloadHandler(
      filename = function() {
        paste("Interlab_curves_", Sys.Date(), input$save_summary_format_curve, sep = "")
      },
      content = function(file, verbose=TRUE) {
        data_to_save <- reactive_data_interlab$curve_up

        if (input$save_summary_format_curve == ".csv") {
          write.csv(data_to_save, file)
        } else if (input$save_summary_format_curve == ".tex") {
          if(verbose){
            xtable::print.xtable(xtable::xtable(data_to_save), type = "latex", file = file)
          }
        } else if(input$save_summary_format_curve == ".xlsx"){
          write.xlsx(data_to_save, file, rowNames = FALSE)
        }else{
          stop("Unsupported file format selected.")
        } })

    #Save to csv or xlsx the zscore table
    output$save_summary_zscore <- downloadHandler(
      filename = function() {
        paste("Interlab_zscores_", Sys.Date(), input$save_summary_format_zscore, sep = "")
      },
      content = function(file, verbose=TRUE) {
        data_to_save <- reactive_data_interlab$zscore_table

        if (input$save_summary_format_zscore == ".csv") {
          write.csv(data_to_save, file)
        } else if (input$save_summary_format_zscore == ".tex") {
          if(verbose){
           xtable::print.xtable(xtable::xtable(data_to_save), type = "latex", file = file)
          }
        } else if(input$save_summary_format_zscore == ".xlsx"){
          write.xlsx(data_to_save, file, rowNames = FALSE)
        }else{
          stop("Unsupported file format selected.")
        }})



#SAVE PLOTS --------------------------------------------------------------------


    #Plot dose estimation
    output$save_plot_triage_interlab_dose <- downloadHandler(
      filename = function() {
          paste("plot_dose_est_triage_", input$select_dose_interlab, "_", Sys.Date(), input$save_plot_format_triage_interlab_dose, sep = "")
        },

      content = function(file) {
        ggplot2::ggsave(
          plot = reactive_data_interlab$ilc_plots_dose[[1]]()[[input$select_dose_interlab]],
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_triage_interlab_dose)
        )
      }
    )


    #Plot curve manual
    output$save_plot_interlab_cm <- downloadHandler(
      filename = function() {
        paste("plot_curves_manual_", Sys.Date(), input$save_plot_format_interlab_cm, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$curve_plots[[1]](),
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_interlab_cm)
        )
      }
    )
    #Plot curve automatic
    output$save_plot_interlab_ca <- downloadHandler(
      filename = function() {
        paste("plot_curves_auto_", Sys.Date(), input$save_plot_format_interlab_ca, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$curve_plots[[2]](),
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_interlab_ca)
        )
      }
    )
    #Plot barplot manual curves
    output$save_plot_interlab_bm <- downloadHandler(
      filename = function() {
        paste("barplot_manual_curves_", Sys.Date(), input$save_plot_format_interlab_bm, sep = "")
      },
      content = function(file) {
        # Determine the device based on file extension
        ext <- gsub("\\.", "", input$save_plot_format_interlab_bm)

        if (ext == "jpg") {
          jpeg(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "png") {
          png(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "pdf") {
          pdf(file, width = 6, height = 4.5)
        } else {
          stop("Unsupported file format")
        }
        reactive_data_interlab$curve_plots[[3]]()
        dev.off()
      }
    )
    #Plot barplot automatic curves
    output$save_plot_interlab_ba <- downloadHandler(
      filename = function() {
        paste("barplot_auto_curves_", Sys.Date(), input$save_plot_format_interlab_ba, sep = "")
      },
      content = function(file) {
        # Determine the device based on file extension
        ext <- gsub("\\.", "", input$save_plot_format_interlab_ba)

        if (ext == "jpg") {
          jpeg(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "png") {
          png(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "pdf") {
          pdf(file, width = 6, height = 4.5)
        } else {
          stop("Unsupported file format")
        }

       reactive_data_interlab$curve_plots[[4]]()
       dev.off()
      }
    )
    #Plot u-test
    output$save_plot_interlab_u <- downloadHandler(
      filename = function() {
        paste("boxplot_summary_U_", Sys.Date(), input$save_plot_format_interlab_u, sep = "")
      },
      content = function(file) {
        # Determine the device based on file extension
        ext <- gsub("\\.", "", input$save_plot_format_interlab_u)

        if (ext == "jpg") {
          jpeg(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "png") {
          png(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "pdf") {
          pdf(file, width = 6, height = 4.5)
        } else {
          stop("Unsupported file format")
        }

        reactive_data_interlab$summary_plots[[1]]()
        dev.off()
      }
    )

    # Boxplot dose estimates
    output$save_boxplot_interlab_dose <- downloadHandler(
      filename = function() {
        paste("boxplot_summary_dose_", Sys.Date(), input$save_boxplot_format_interlab_dose, sep = "")
      },
      content = function(file) {
        # Determine the device based on file extension
        ext <- gsub("\\.", "", input$save_boxplot_format_interlab_dose)

        if (ext == "jpg") {
          jpeg(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "png") {
          png(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "pdf") {
          pdf(file, width = 6, height = 4.5)
        } else {
          stop("Unsupported file format")
        }

        reactive_data_interlab$summary_plots[[3]]()
        dev.off()
      }
    )

    # Boplot yield
    output$save_boxplot_interlab_yield <- downloadHandler(
      filename = function() {
        paste("boxplot_summary_yield_", Sys.Date(), input$save_boxplot_format_interlab_yield, sep = "")
      },
      content = function(file) {
        # Determine the device based on file extension
        ext <- gsub("\\.", "", input$save_boxplot_format_interlab_yield)

        if (ext == "jpg") {
          jpeg(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "png") {
          png(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "pdf") {
          pdf(file, width = 6, height = 4.5)
        } else {
          stop("Unsupported file format")
        }

        reactive_data_interlab$summary_plots[[4]]()
        dev.off()
      }
    )


    #Plot Dispersion index
    output$save_plot_interlab_DI <- downloadHandler(
      filename = function() {
        paste("boxplot_summary_dispersion_", Sys.Date(), input$save_plot_format_interlab_DI, sep = "")
      },
      content = function(file) {
        # Determine the device based on file extension
        ext <- gsub("\\.", "", input$save_plot_format_interlab_DI)

        if (ext == "jpg") {
          jpeg(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "png") {
          png(file, width = 6, height = 4.5, units = "in", res = 96)
        } else if (ext == "pdf") {
          pdf(file, width = 6, height = 4.5)
        } else {
          stop("Unsupported file format")
        }

        reactive_data_interlab$summary_plots[[2]]()
        dev.off()
      }
    )

    #Plot zscore dose
    output$save_plot_interlab_dose <- downloadHandler(
      filename = function() {
        paste("plot_zscore_dose_", input$select_dose_interlab, "_",  Sys.Date(), input$save_plot_format_interlab_dose, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$ilc_plots_dose[[2]]()[[input$select_dose_interlab]],
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_interlab_dose)
        )
      }
    )

    #Plot zscore frequency
    output$save_plot_interlab_freq <- downloadHandler(
      filename = function() {
        paste("plot_zscore_yield_", input$select_dose_interlab, "_", Sys.Date(), input$save_plot_format_interlab_freq, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$ilc_plots_freq[[1]]()[[input$select_dose_interlab]],
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_interlab_freq)
        )
      }
    )


    #Plot deviation from reference
    output$save_plot_interlab_dev <- downloadHandler(
      filename = function() {
        paste("plot_deviation_", input$select_dose_interlab, "_", Sys.Date(), input$save_plot_format_interlab_dev, sep = "")
      },

      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$ilc_plots_dose[[3]]()[[input$select_dose_interlab]],
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_interlab_dev)
        )
      }
    )

    #Plot z scores all samples dose
    output$save_plot_zscore_all_dose <- downloadHandler(
      filename = function() {
        paste("plot_zscore_all_samples_dose_", Sys.Date(), input$save_plot_format_zscore_all_dose, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$ilc_plots_dose[[4]](),
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_zscore_all_dose)
        )
      }
    )


    #Plot z scores all samples frequency
    output$save_plot_zscore_all_freq <- downloadHandler(
      filename = function() {
        paste("plot_zscore_all_samples_yield_", Sys.Date(), input$save_plot_format_zscore_all_freq, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          plot =  reactive_data_interlab$ilc_plots_freq[[2]](),
          filename = file,
          width = 6, height = 4.5, dpi = 96,
          device = gsub("\\.", "", input$save_plot_format_zscore_all_freq)
        )
      }
    )

    #Plot deviation from ref dose all samples
    output$save_plot_deviation_all <- downloadHandler(
        filename = function() {
          paste("plot_deviation_all_samples_", Sys.Date(), input$save_plot_format_deviation_all, sep = "")
        },

        content = function(file) {
          ggplot2::ggsave(
            plot =  reactive_data_interlab$ilc_plots_dose[[5]](),
            filename = file,
            width = 6, height = 4.5, dpi = 96,
            device = gsub("\\.", "", input$save_plot_format_deviation_all)
          )
        }
    )

#SAVE REPORT -------------------------------------------------------------------
 #Data for the report
   report_data <- reactive({
        report_list <- list(
         curve_table = reactive_data_interlab$curve_up,
         summary_table = reactive_data_interlab$sum_up,
         zscore_table = reactive_data_interlab$zscore_table,
         comments = input$results_comments_i
       )
   return(report_list)
   })

   # Export report ----
    output$save_report_i <- downloadHandler(
      filename = function() {
        paste0("interlab-report-", Sys.Date(), ".pdf")  # Ensure PDF output
      },
      content = function(file, verbose =TRUE) {
        temp_dir <- tempdir()  # Temporary directory

        # Render RMarkdown to PDF
        temp_report <- file.path(temp_dir, "report.Rmd")
        local_report <- load_rmd_report(
          paste0("interlab-report-", gsub("^\\.", "", input$save_report_format_i), ".Rmd")
        )
        file.copy(local_report, temp_report, overwrite = TRUE)

        report_pdf <- file.path(temp_dir, "report.pdf")  #Define final report path

        rmarkdown::render(
          input = temp_report,
          output_format = "pdf_document",
          output_file = report_pdf,
          params = list(report_list = report_data()),
          envir = new.env(parent = globalenv())
        )

        pdf_files <- c(report_pdf)  #Start list with the report PDF

        if(!is.null(reactive_data_interlab$curve_plots)){
          # Generate PDF plots
          for (i in 1:length(reactive_data_interlab$curve_plots)) {
            pdf_file <- file.path(temp_dir, paste0("curve_plot_", i, ".pdf"))
            pdf(pdf_file)  #Open a new PDF file

            plot_func <- reactive_data_interlab$curve_plots[[i]]

            if (is.function(plot_func)) {
              plot_obj <- plot_func()
              if (inherits(plot_obj, "ggplot")) {
                if(verbose){
                 print(plot_obj)  #Print ggplot objects
                }
              }
            }
            dev.off()  #Close PDF file
            pdf_files <- c(pdf_files, pdf_file)  #Add to the list

          }
        }
        if(!is.null(reactive_data_interlab$summary_plots)){
          # Generate PDF plots
          for (i in c(4, 3, 2, 1)) {
            pdf_file <- file.path(temp_dir, paste0("summary_plot_", i, ".pdf"))
            pdf(pdf_file)  #Open a new PDF file

            plot_func <- reactive_data_interlab$summary_plots[[i]]

            if (is.function(plot_func)) {
              plot_obj <- plot_func()
              if (inherits(plot_obj, "ggplot")) {
                if(verbose){
                 print(plot_obj)  #Print ggplot objects
                }
              }
            }
            dev.off()  #Close PDF file
            pdf_files <- c(pdf_files, pdf_file)  #Add to the list
          }
        }
        if (input$dose_or_freq == "Dose") {
          for (i in seq_along(reactive_data_interlab$ilc_plots_dose)) {
            pdf_file <- file.path(temp_dir, paste0("ilc_plot_dose_", i, ".pdf"))
            pdf(pdf_file)  #Open a new PDF file

            plot_func <- reactive_data_interlab$ilc_plots_dose[[i]]

            if (is.function(plot_func)) {
              plot_obj <- plot_func()

              if (inherits(plot_obj, "ggplot")) {
                if(verbose){
                  print(plot_obj)
                }
              } else if (is.list(plot_obj)) {
                for (p in plot_obj) {
                  if (inherits(p, "ggplot")) {
                    if(verbose){
                      print(p)  #Each plot in the list
                    }

                  }
                }
              }
            }

            dev.off()  # Close PDF
            pdf_files <- c(pdf_files, pdf_file)
          }
        }


        if(input$dose_or_freq == "Frequency"){
          # Generate PDF plots
          for (i in 1:length(reactive_data_interlab$ilc_plots_freq)) {
            pdf_file <- file.path(temp_dir, paste0("ilc_plot_freq_", i, ".pdf"))
            pdf(pdf_file)  #Open a new PDF file

            plot_func <- reactive_data_interlab$ilc_plots_freq[[i]]

            if (is.function(plot_func)) {
              plot_obj <- plot_func()

              if (inherits(plot_obj, "ggplot")) {
                print(plot_obj)
              } else if (is.list(plot_obj)) {
                for (p in plot_obj) {
                  if (inherits(p, "ggplot")) {
                    print(p)  #Each plot in the list
                  }
                }
              }
            }
            dev.off()  #Close PDF file
            pdf_files <- c(pdf_files, pdf_file)  #Add to the list
          }
        }

        pdf_combine(pdf_files, output = file)
        file.remove(pdf_files)#Clean
      })
  })
}

