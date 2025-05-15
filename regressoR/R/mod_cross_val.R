#' Train-Test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cross_val_ui <- function(id, nombrePred) {
  ns <- NS(id)
  
  opcVC <- tags$div(
    id = ns("titleVC"),
    fluidRow(
      col_7(tags$b("Experimento: ")),
      col_5(selectInput(ns("vci"), NULL, c(1), selected = 1, width = "100%"))
    )
  )
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxModel"), opciones = NULL, title = opcVC,
      
      tabPanel(
        title = labelInput("generatem"), value = "tabModelo",
        tags$div(
          style = "height: 70vh;",
          col_5(
            wellPanel(
              style = "background-color: #666;color: white;",
              options.run(ns("runModelo")), tags$hr(style = "margin-top: 0px;"),
              generar.opciones(nombrePred, ns, T)
            )
          ),
          col_7(
            withLoader(verbatimTextOutput(ns("txtmodel")), 
                       type = "html", loader = "loader4"))
        )
      ),
      
      tabPanel(
        title = labelInput("indices"), value = "tabIndices",
        wellPanel(
          style = "background-color: #666;color: white;",
          fluidRow(
            col_12(selectInput(ns("sel_k"), labelInput("modelo"), choices = c()))
          ),
          hr(),
          fluidRow(
            col_12(actionButton(ns("aplicar"), labelInput("aplicar"), NULL, "100%"))
          )
        ),
        tags$div(
          col_6(
            tags$div(style = "margin-top: 77px;"),
            valueBoxOutput(ns("RMSEBox"), width = 6),
            valueBoxOutput(ns("MAEBox"), width = 6),
            valueBoxOutput(ns("REBox"), width = 6),
            valueBoxOutput(ns("CORBox"), width = 6)
          ),
          col_6(
            withLoader(DT::dataTableOutput(ns("modeloPredTable")), 
                       type = "html", loader = "loader4")
          )
        ),
        tags$hr(style = "margin-top: 50vh;"),
        tags$div(
          column(
            width = 12, align = "center", 
            tags$h3(labelInput("resumenVarPre"))
          )
        ),
        tags$div(
          style = "text-align: -webkit-center;",
          withLoader(tableOutput(ns('VARP')), type = "html", loader = "loader4")
        )
      ),
      
      tabPanel(
        title = labelInput("varError"), value = "tabVariacion",
        wellPanel(
          style = "background-color: #666;color: white;",
          fluidRow(
            col_12(selectInput(ns("sel_ind_err"), labelInput("selindice"), 
                              choices = c("RMSE", "MAE", "RE", "COR"))),
          ),
          hr(),
          fluidRow(
            col_12(actionButton(ns("aplicar2"), labelInput("aplicar"), NULL, "100%"))
          )
        ),
        withLoader(echarts4rOutput(ns("VARE"), height = "65vh", width = "100%"), 
                   type = "html", loader = "loader4")
      )
    )
  )
}

#' Train-Test Server Function
#'
#' @noRd 
mod_cross_val_server <- function(input, output, session, updateData, modelos, nombrePred, codedioma){
  ns <- session$ns
  modelosrv <- rv(modelos = NULL, modelo = NULL, i = 1)
  
  observe({
    datos    <- isolate(updateData$datos)
    variable <- isolate(updateData$variable.predecir)
    choices  <- as.character(unique(datos[, variable]))
    
    if(!is.null(modelos[[nombrePred]])) {
      showTab(inputId = "BoxModel", target = "tabIndices")
      showTab(inputId = "BoxModel", target = "tabVariacion")
    } else {
      updateTabsetPanel(session, "BoxModel", "tabModelo")
      hideTab(inputId = "BoxModel", target = "tabIndices")
      hideTab(inputId = "BoxModel", target = "tabVariacion")
    }
    
    if(!is.null(input$BoxModel)) {
      if(input$BoxModel == "tabVariacion") {
        hide("titleVC")
      } else {
        show("titleVC")
      }
    }
  })
  
  observeEvent(modelosrv$modelos, {
    auxmodelos <- modelosrv$modelos
    if(!is.null(auxmodelos)) {
      datos <- updateData$datos
      ks    <- names(auxmodelos[[1]][[1]])
      
      updateSelectInput(session, "sel_k", choices = ks)
      updateTabsetPanel(session, "BoxModel", selected = "tabModelo")
    }
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    modelosrv$modelos <- NULL
    datos <- updateData$datos
    
    updateNumericInput(session, "kmax", value = round(sqrt(nrow(datos))))
    updateNumericInput(session, "mtry", value = round(sqrt(ncol(datos))))
    updateNumericInput(session, "ncomp", value = round((ncol(datos))))
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    output$txtmodel <- renderPrint({cat("No se ha generado el modelo.")})
  })
  
  observeEvent(updateData$grupos, {
    choices <- c(1:length(updateData$grupos), "AVG")
    updateSelectInput(session, "vci", choices = choices)
  })
  
  observeEvent(input$vci, {
    if(input$vci != "AVG") { 
      modelosrv$i <- as.numeric(input$vci)
    } else {
      modelosrv$i <- input$vci
    }
  })
  
  # Generar modelo
  observeEvent(input$runModelo, {
    output$txtmodel <- renderPrint({
      datos     <- isolate(updateData$datos)
      variable  <- isolate(updateData$variable.predecir)
      cant.vc   <- isolate(updateData$numValC)
      numGrupos <- isolate(updateData$numGrupos)
      grupos    <- isolate(updateData$grupos)
      
      tryCatch({
        auxmodelos <- lapply(1:cant.vc, function(vc) {
          lapply(1:numGrupos, function(g) {
            train <- datos[-grupos[[vc]][[g]], ]
            formula <- as.formula(paste0(variable, "~ ."))
            generar.modelos(nombrePred, formula, train, input)
          })
        })
        
        modelosrv$modelos <- auxmodelos
        print(auxmodelos)
      }, error = function(e) {
        modelosrv$modelos <- NULL
        stop(e)
      })
    })
  })
  
  observeEvent(modelosrv$modelos, {
    cv_modelos <- modelosrv$modelos
    
    datos    <- isolate(updateData$datos)
    grupos   <- isolate(updateData$grupos)
    variable <- isolate(updateData$variable.predecir)
    
    tryCatch({
      if(!is.null(cv_modelos)) {
        for (vc in 1:length(cv_modelos)) {
          for (g in 1:length(cv_modelos[[vc]])) {
            for (nombre in names(cv_modelos[[vc]][[g]])) {
              modelo <- cv_modelos[[vc]][[g]][[nombre]]
              test   <- datos[grupos[[vc]][[g]], ]
              
              if(nombrePred == "rdim") {
                ncomp <- modelo$optimal.ncomp
                auxp  <- predict(modelo, test, ncomp = ncomp)
                pred  <- list(prediction = auxp[, , 1], var.pred = variable)
              } else {
                pred <- predict(modelo, test)
              }
              
              auxvc <- paste0("VC", vc)
              auxg  <- paste0("G", g)
              modelos[[nombrePred]][[nombre]][[auxvc]][[auxg]] <- list(
                nombre = nombre, modelo = modelo,
                pred = pred, test = test
              )
            }
          }
        }
        
        modelosrv$modelo <- modelos[[nombrePred]][[nombre]]
        modelosrv$i <- isolate(as.numeric(input$vci))
      }
    }, error = function(e) {
      print(e)
      modelos[[nombrePred]] <- NULL
    })
  })
  
  observeEvent(input$aplicar, {
    k          <- input$sel_k
    cv_modelos <- modelosrv$modelos
    
    tryCatch({
      if(!is.null(cv_modelos)) {
        modelosrv$modelo <- modelos[[nombrePred]][[k]]
      }
    }, error = function(e) {
      print(e)
      modelos[[nombrePred]] <- NULL
    })
  })
  
  # Resumen variable a predecir
  output$VARP <- renderTable({
    datos <- updateData$datos
    var   <- updateData$variable.predecir
    lg    <- codedioma$idioma
    decs  <- updateData$decimals
    x     <- datos[, var]
    
    tryCatch({
      df <- data.frame(
        MIN = min(x), Q1 = quantile(x, prob = 0.25),
        Q3 = quantile(x, prob = 0.75), MIN = max(x)
      )
      df <- round(df, decs)
      
      colnames(df) <- c(tr("minimo",lg), tr("q1",lg), tr("q3", lg), tr("maximo", lg))
      return(df)
    }, error = function(e) {
      showNotification(paste0("Error (RL-06) : ", e), 
                       duration = 10, type = "error")
      return(NULL)
    })
  }, striped = TRUE, bordered = TRUE, spacing = 'l', width = '100%', align = 'c')
  
  output$RMSEBox <- renderValueBox({
    lng    <- codedioma$idioma
    modelo <- modelosrv$modelo
    decs   <- updateData$decimals
    i      <- modelosrv$i
    if(is.numeric(i)) {
      res <- calc_cross_groups(modelo[[paste0("VC", i)]])
    } else {
      res <- calc_cross_index(modelo)
    }
    
    RMSE <- round(res$RMSE, decs)
    valueBox(RMSE, tr("RMSE", lng), icon = NULL, color = "yellow")
  })
  
  output$MAEBox <- renderValueBox({
    lng    <- codedioma$idioma
    modelo <- modelosrv$modelo
    decs   <- updateData$decimals
    i      <- modelosrv$i
    if(is.numeric(i)) {
      res <- calc_cross_groups(modelo[[paste0("VC", i)]])
    } else {
      res <- calc_cross_index(modelo)
    }
    
    MAE <- round(res$MAE, decs)
    valueBox(MAE, tr("MAE", lng), icon = NULL, color = "yellow")
  })
  
  output$REBox <- renderValueBox({
    lng    <- codedioma$idioma
    modelo <- modelosrv$modelo
    decs   <- updateData$decimals
    i      <- modelosrv$i
    if(is.numeric(i)) {
      res <- calc_cross_groups(modelo[[paste0("VC", i)]])
    } else {
      res <- calc_cross_index(modelo)
    }
    
    RE <- round(res$RE, decs)
    valueBox(RE, tr("ER", lng), icon = NULL, color = "yellow")
  })
  
  output$CORBox <- renderValueBox({
    lng    <- codedioma$idioma
    modelo <- modelosrv$modelo
    decs   <- updateData$decimals
    i      <- modelosrv$i
    if(is.numeric(i)) {
      res <- calc_cross_groups(modelo[[paste0("VC", i)]])
    } else {
      res <- calc_cross_index(modelo)
    }
    
    COR <- round(res$COR, decs)
    valueBox(COR, tr("correlacion", lng), icon = NULL, color = "yellow")
  })
  
  # Tabla de la prediccion
  output$modeloPredTable <- DT::renderDataTable({
    lng    <- codedioma$idioma
    modelo <- modelosrv$modelo
    decs   <- updateData$decimals
    i      <- modelosrv$i
    
    datos  <- isolate(updateData$datos)
    grupos <- isolate(updateData$grupos)
    var    <- isolate(updateData$variable.predecir)
    
    if(is.numeric(i)) {
      modelo <- modelo[[paste0("VC", i)]]
      grupos <- grupos[[i]]
      pred   <- table_group(modelo, grupos)
    } else {
      pred <- cross_group(modelo, grupos)
    }
    
    etqs <- c(tr("reald", lng), tr("pred", lng))
    tabla.pred(datos[, var], pred, etqs, decs)
  }, server = FALSE)
  
  # Variación del Error
  output$VARE <- renderEcharts4r({
    input$aplicar2
    lng    <- codedioma$idioma
    decs   <- updateData$decimals
    modelo <- modelos[[nombrePred]]
    ind    <- isolate(input$sel_ind_err)
    df     <- calc_cross_error(modelo, ind)
    df     <- round(df, decs)
    
    tiulos <- c(tr("numExp", lng), tr(ind, lng))
    
    e_var_error(df, T, tiulos)
  })
}

## To be copied in the UI
# mod_cross_val_ui("mod_cross_val_ui_1")

## To be copied in the server
# callModule(mod_cross_val_server, "mod_cross_val_ui_1")

