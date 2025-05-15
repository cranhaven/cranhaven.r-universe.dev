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
      ),
      
      tabPanel(
        title = labelInput("rocCurva"), value = "tabEvalROC",
        wellPanel(
          style = "background-color: #666;color: white;",
          selectInput(ns("sel_cat"), labelInput("selectCat"), choices = c())
        ),
        withLoader(echarts4rOutput(ns("ROC"), height = "75vh", width = "100%"),
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
      choices <- as.character(unique(datos[, variable]))
      
      vci <- 1:length(updateData$grupos)
      if(is.null(vc)) {
        modo$tt <- T
        updateSelectInput(session, "vci", choices = vci)
      } else {
        modo$tt <- F
        vci <- c(vci, "AVG")
        updateSelectInput(session, "vci", choices = vci)
      }
      
      if(length(choices) == 2) {
        updateSelectInput(session, "sel_cat", choices = choices)
        showTab(inputId = "BoxEval", target = "tabEvalROC")
      } else {
        hideTab(inputId = "BoxEval", target = "tabEvalROC")
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
    lng <- codedioma$idioma
    vci <- modelosrv$i
    tt  <- modo$tt
    
    df <- data.frame()
    for (modelo in names(modelos)) {
      if(!is.null(modelos[[modelo]])) {
        df <- rbind(df, calc_cross_indices(modelos[[modelo]], vci, tt))
      }
    }
    
    for (i in 1:ncol(df)) {
      if(colnames(df)[i] == "Modelo") {
        colnames(df)[i] <- tr("modelo", lng)
      } else if(colnames(df)[i] == "Corte") {
        colnames(df)[i] <- tr("probC", lng)
      } else if(colnames(df)[i] == "PG") {
        colnames(df)[i] <- tr("precG", lng)
      } else {
        colnames(df)[i] <- paste0(tr("prec", lng), " ", colnames(df)[i])
      }
    }
    
    DT::datatable(
      df, rownames = FALSE, selection = "none",
      editable = FALSE, escape = FALSE,
      options = list(dom = "frtip", pageLength = 10)
    )
  }, server = FALSE)
  
  # Gráfico de la evaluacion
  output$PLOTE <- renderEcharts4r({
    lng <- codedioma$idioma
    vci <- modelosrv$i
    tt  <- modo$tt
    
    df <- data.frame()
    for (modelo in names(modelos)) {
      if(!is.null(modelos[[modelo]])) {
        df <- rbind(df, calc_cross_indices(modelos[[modelo]], vci, tt))
      }
    }
    
    tiulos <- c(tr("modelo", lng), tr("prec", lng)) 
    
    e_eval_bar(df, tiulos)
  })
  
  # Evaluación ROC
  output$ROC <- renderEcharts4r({
    lng <- codedioma$idioma
    vci <- modelosrv$i
    tt  <- modo$tt
    
    datos    <- updateData$datos
    grupos   <- updateData$grupos
    variable <- updateData$variable.predecir
    cat      <- input$sel_cat
    
    series <- list()
    i <- 1
    for (modelo in names(modelos)) {
      x <- modelos[[modelo]]
      if(!is.null(x)) {
        for (k in names(x)) {
          if(tt) {
            prob <- x[[k]][[vci]]$prob$prediction[, cat]
            test <- datos[grupos[[vci]][["test"]], variable] 
            roc.data <- roc.values(prob, test)
            roc.area <- ROC.area(prob, test)
          } else {
            if(is.numeric(vci)) {
              vc <- paste0("VC", vci)
              auxcalc  <- calc_g_prob(x[[k]][[vc]], grupos[[vci]])
              roc.data <- roc.values(auxcalc[[1]][, cat], datos[, variable])
              roc.area <- ROC.area(auxcalc[[1]][, cat], datos[, variable])
            } else {
              auxcalc  <- calc_cross_prob(x[[k]], updateData$grupos)
              roc.data <- roc.values(auxcalc[[1]][, cat], datos[, variable])
              roc.area <- ROC.area(auxcalc[[1]][, cat], datos[, variable])
            }
          }
          nuevo       <- list(
            type = "line", data = roc.data, name = k,
            tooltip = list(formatter = e_JS(paste0(
              "function(params) {",
              "  return('<b>", k, "</b><br>' + 'AREA ROC: ' + ", round(roc.area, 3), ")",
              "}"
            )))
          )
          series[[i]] <- nuevo
          i <- i + 1
        }
      }
    }
    
    res <- e_charts() |> e_list(list(
      xAxis = list(show = TRUE, inverse = TRUE),
      yAxis = list(show = TRUE),
      series = series
    )) |> e_show_loading() |> e_legend() |> 
      e_x_axis(scale = T) |> e_y_axis(scale = T) |>
      e_datazoom(show = F) |> e_tooltip()
    
    return(res)
  })
}

## To be copied in the UI
# mod_evaluacion_ui("evaluacion_ui_1")
    
## To be copied in the server
# callModule(mod_evaluacion_server, "evaluacion_ui_1")
 
