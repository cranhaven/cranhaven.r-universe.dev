library(shiny)
library(ggplot2)
library(CancerGram)
library(CancerGramModel)
library(ranger)
library(DT)
library(shinythemes)
library(rmarkdown)
library(pander)

source("shiny-server-utils.R")

data(CancerGram_model)

options(shiny.maxRequestSize=10*1024^2)

shinyServer(function(input, output) {
  
  prediction <- reactive({
    
    if (!is.null(input[["seq_file"]]))
      input_sequences <- read_txt(input[["seq_file"]][["datapath"]])
    input[["use_area"]]
    isolate({
      if (!is.null(input[["text_area"]]))
        if(input[["text_area"]] != "")
          input_sequences <- read_txt(textConnection(input[["text_area"]]))
    })
    
    if(exists("input_sequences")) {
      if(length(input_sequences) > 50) {
        #dummy error, just to stop further processing
        stop("Too many sequences. Please use CancerGram locally.")
      } else {
        if(any(lengths(input_sequences) < 5)) {
          #dummy error, just to stop further processing
          stop("The minimum length of the sequence is 5 amino acids.")
        } else {
          predict_in_shiny(CancerGram_model, input_sequences)
        }
      }
    } else {
      NULL
    }
  })
  
  decision_table <- reactive({
    if(!is.null(prediction())) {
      pred2df(prediction())
    }
  })
  
  output[["decision_table"]] <- renderDataTable({
    df <- decision_table()
    colnames(df) <- c("Protein name", "ACP prediction", "AMP prediction", "Negative prediction", "Decision")
    my_DT(df) %>% 
      formatRound(columns = c("ACP prediction", "AMP prediction", "Negative prediction"), digits = 4) 
    
  })
  
  #selected_proteins <- reactive({
    #validate(
     # need(input[["decision_table_rows_selected"]], 
    #       "Select at least one row in the Results table")
   # )
  #  
 #   prediction()[input[["decision_table_rows_selected"]]]
#  })
  
#  detailed_preds <- reactive(({
    
#    selected_pred_data <- selected_proteins()
    
#    detailed_pred_list <- lapply(1L:length(selected_pred_data), function(ith_pred_id) {
 #     ith_pred <- selected_pred_data[[ith_pred_id]]
      
  #    data.frame(seq_name = names(selected_pred_data)[ith_pred_id],
   #              start = 1L:length(ith_pred[["all_mers_pred"]]), 
    #             end = 1L:length(ith_pred[["all_mers_pred"]]) + 9, 
     #            pred = ith_pred[["all_mers_pred"]],
      #           decision = factor(ith_pred[["all_mers_pred"]] > 0.5, 
       #                            levels = c("FALSE", "TRUE"),
        #                           labels = c("No", "Yes")))
  #  })
    
#    names(detailed_pred_list) <- names(selected_pred_data)
#    detailed_pred_list
  #  
 # }))
  
  
  
 # output[["detailed_preds"]] <- renderUI({
#    detailed_preds_list <- lapply(1L:length(detailed_preds()), function(i) {
 #     list(plotOutput(paste0("detailed_plot", i)),
  #         dataTableOutput(paste0("detailed_table", i)))
  #  })
  #  c(list(downloadButton("download_long_graph", "Download long output (with graphics)")),
  #    do.call(tagList, unlist(detailed_preds_list, recursive = FALSE)))
#  })
  
  
 # for (i in 1L:50) {
#    local({
 #     my_i <- i
      
#      output[[paste0("detailed_plot", my_i)]] <- renderPlot({
        #browser()
 #       plot_single_protein(detailed_preds()[[my_i]])
  #      })
   #   output[[paste0("detailed_table", my_i)]] <- renderDataTable(AMP_DT(AmpGram:::get_AMPs(selected_proteins()[[my_i]]))) # tutaj zmiany
  #  })
  #}
  
#  output[["detailed_tab"]] <- renderUI({
 #   uiOutput("detailed_preds")
#  })
  
  output[["dynamic_ui"]] <- renderUI({
    if (!is.null(input[["seq_file"]]))
      input_sequences <- read_txt(input[["seq_file"]][["datapath"]])
    input[["use_area"]]
    isolate({
      if (!is.null(input[["text_area"]]))
        if(input[["text_area"]] != "")
          input_sequences <- read_txt(textConnection(input[["text_area"]]))
    })
    
    if(exists("input_sequences")) {
      fluidRow(
        tags$p(HTML("<h3><A HREF=\"javascript:history.go(0)\">Start a new query</A></h3>"))
      )
    } else {
      fluidRow(
        h4("Exemplary sequences"),
        pre(includeText("prots.txt"))
      )
    }
  })
  
  output[["dynamic_tabset"]] <- renderUI({
    if(is.null(prediction())) {
      
      tabPanel(title = "Sequence input",
               tags$textarea(id = "text_area", style = "width:90%",
                             placeholder="Paste sequences (FASTA format required) here...", 
                             rows = 22, cols = 60, ""),
               tags$p(""),
               actionButton("use_area", "Submit data from field above"),
               tags$p(""),
               fileInput('seq_file', 'Submit .fasta or .txt file:'))
      
      
    } else {
      tabsetPanel(
        tabPanel("Results",
                 dataTableOutput("decision_table")
        )
        #tabPanel("Detailed results",
         #        uiOutput("detailed_tab")
        #)
      )
    }
  })
  
  file_name <- reactive({
    if(is.null(input[["seq_file"]][["name"]])) {
      part_name <- "CancerGram_results"
    } else {
      part_name <- strsplit(input[["seq_file"]][["name"]], ".", fixed = TRUE)[[1]][1]
    }
    part_name
  })
  
  output[["download_long"]] <- downloadHandler(
    filename  = function() { 
      paste0(file_name(), "_pred.txt") 
    },
    content <- function(file) {
      sink(file, type = "output")
      cat("Input file name: ", ifelse(is.null(input[["seq_file"]][["name"]]), "none",
                                      input[["seq_file"]][["name"]]), "\n\n")
      cat(paste0("Date: ", Sys.time()), "\n\n")
      for (i in 1L:length(prediction())) {
        cat("\n\n")
        summary(prediction()[[i]])
        cat("\n\n")
      }
      sink()
    }
  )
  
  output[["download_long_graph"]] <- downloadHandler(
    filename  = function() { 
      paste0(file_name(), "_pred.html") 
    },
    content <- function(file) {
      src <- normalizePath("CancerGram-report.Rmd")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "CancerGram-report.Rmd")
      out <- render("CancerGram-report.Rmd", output_format = "html_document", file, quiet = TRUE)
      file.rename(out, file)
    }
  )
  
})
