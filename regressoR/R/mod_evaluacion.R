#' evaluacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_evaluacion_ui <- function(id) {
  ns <- NS(id)
  
  opcVC <- fluidRow(
    col_7(tags$b("Experimento: ")),
    col_5(selectInput(ns("vci"), NULL, c(1), selected = 1, width = "100%"))
  )
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxEval"), opciones = NULL, title = opcVC,
      
      tabPanel(
        title = labelInput("tablaComp"), value = "tabEvalTable",
        withLoader(DT::dataTableOutput(ns("TABE")), 
                   type = "html", loader = "loader4")
      ),
      
      tabPanel(
        title = labelInput("plotComp"), value = "tabEvalPlot",
        withLoader(echarts4rOutput(ns("PLOTE"), height = "75vh", width = "100%"),
                   type = "html", loader = "loader4")
      )
    )
  )
}

#' evaluacion Server Function
#'
#' @noRd 
mod_evaluacion_server <- function(input, output, session, updateData, modelos, codedioma) {
  ns <- session$ns
  modo <- rv(tt = T)
  modelosrv <- rv(i = 1)
  
  observeEvent(updateData$variable.predecir, {
    datos       <- updateData$datos
    variable    <- updateData$variable.predecir
    vci <- 1:length(updateData$grupos)
    vc <- updateData$numValC
    
    if(!is.null(datos)) {
      vci <- 1:length(updateData$grupos)
      if(is.null(vc)) {
        modo$tt <- T
        updateSelectInput(session, "vci", choices = vci)
      } else {
        modo$tt <- F
        vci <- c(vci, "AVG")
        updateSelectInput(session, "vci", choices = vci)
      }
    }
  })
  
  observeEvent(input$vci, {
    if(input$vci != "AVG") { 
      modelosrv$i <- as.numeric(input$vci)
    } else {
      modelosrv$i <- input$vci
    }
  })
  
  # Tabla de la evaluacion
  output$TABE <- DT::renderDataTable({
    lng  <- codedioma$idioma
    vci  <- modelosrv$i
    tt   <- modo$tt
    decs <- updateData$decimals
    
    df <- data.frame()
    
    if(tt) {
      for (modelo in names(modelos)) {
        for (k in names(modelos[[modelo]])) {
          res <- modelos[[modelo]][[k]][[vci]]$indices
          nuevo <- data.frame(Modelo = k, RMSE = res$RMSE, MAE = res$MAE,
                              RE = res$RE, COR = res$COR)
          df <- rbind(df, nuevo)
        }
      }
    } else {
      for (modelo in names(modelos)) {
        for (k in names(modelos[[modelo]])) {
          auxmodelo <- modelos[[modelo]][[k]]
          if(is.numeric(vci)) {
            res <- calc_cross_groups(auxmodelo[[paste0("VC", vci)]])
          } else {
            res <- calc_cross_index(auxmodelo)
          }
          
          nuevo <- data.frame(Modelo = k, RMSE = res$RMSE, MAE = res$MAE,
                              RE = res$RE, COR = res$COR)
          df <- rbind(df, nuevo)
        }
      }
    }
    
    for (i in 1:ncol(df)) {
      if(colnames(df)[i] == "Modelo") {
        colnames(df)[i] <- tr("modelo", lng)
      }
    }
    
    df[, -1] <- round(df[, -1], decs)
    
    DT::datatable(
      df, rownames = FALSE, selection = "none",
      editable = FALSE, escape = FALSE,
      options = list(dom = "frtip", pageLength = 10)
    )
  }, server = FALSE)
  
  # Gráfico de la evaluacion
  output$PLOTE <- renderEcharts4r({
    lng  <- codedioma$idioma
    vci  <- modelosrv$i
    tt   <- modo$tt
    decs <- updateData$decimals
    
    df <- data.frame()
    
    if(tt) {
      for (modelo in names(modelos)) {
        for (k in names(modelos[[modelo]])) {
          res <- modelos[[modelo]][[k]][[vci]]$indices
          nuevo <- data.frame(Modelo = k, RMSE = res$RMSE, MAE = res$MAE,
                              RE = res$RE, COR = res$COR)
          df <- rbind(df, nuevo)
        }
      }
    } else {
      for (modelo in names(modelos)) {
        for (k in names(modelos[[modelo]])) {
          auxmodelo <- modelos[[modelo]][[k]]
          if(is.numeric(vci)) {
            res <- calc_cross_groups(auxmodelo[[paste0("VC", vci)]])
          } else {
            res <- calc_cross_index(auxmodelo)
          }
          
          nuevo <- data.frame(Modelo = k, RMSE = res$RMSE, MAE = res$MAE,
                              RE = res$RE, COR = res$COR)
          df <- rbind(df, nuevo)
        }
      }
    }
    
    tiulos <- c(tr("modelo", lng), "Error")
    
    e_eval_bar(df, tiulos)
  })
}

## To be copied in the UI
# mod_evaluacion_ui("evaluacion_ui_1")

## To be copied in the server
# callModule(mod_evaluacion_server, "evaluacion_ui_1")

