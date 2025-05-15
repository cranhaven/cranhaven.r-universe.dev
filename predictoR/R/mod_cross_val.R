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
          fluidRow(
            id = ns("panelCorte"),
            col_5(selectInput(ns("sel_cat"), labelInput("selectCat"),
                              choices = c())
            ),
            col_2(tags$span(
              style = "display: block;text-align: center;font-size: xxx-large;",
              HTML("<span>&#8805;</span>"))
            ),
            col_5(numericInput(ns("sel_corte"), labelInput("probC"),
                               0.5, min = 0, max = 1, step = 0.1)
            )
          ),
          hr(),
          fluidRow(
            col_12(actionButton(ns("aplicar"), labelInput("aplicar"), NULL, "100%"))
          )
        ),
        tags$div(
          style = "text-align: -webkit-center;",
          withLoader(uiOutput(ns("MC")), 
                     type = "html", loader = "loader4")
        ),
        tags$hr(),
        tags$div(
          col_6(
            withLoader(echarts4rOutput(ns("PREC"), width = "100%"), 
                       type = "html", loader = "loader4")
          ),
          col_6(
            withLoader(DT::dataTableOutput(ns("modeloPredTable")), 
                       type = "html", loader = "loader4")
          )
        )
      ),
      
      tabPanel(
        title = labelInput("varError"), value = "tabVariacion",
        wellPanel(
          style = "background-color: #666;color: white;",
          fluidRow(
            col_6(selectInput(ns("sel_ind_err"), labelInput("selindice"), 
                              choices = c("Error", "Precision"))),
            col_6(selectInput(ns("sel_cat_err"), "", choices = c()))
          ),
          hr(),
          fluidRow(
            col_12(actionButton(ns("aplicar2"), labelInput("aplicar"), NULL, "100%"))
          )
        ),
        withLoader(echarts4rOutput(ns("VARE"), height = "65vh", width = "100%"), 
                   type = "html", loader = "loader4")
      ),
      
      tabPanel(
        title = labelInput("probCstep"), value = "tabCortes",
        tags$div(
          style = "height: 70vh;",
          col_5(
            wellPanel(
              style = "background-color: #666;color: white;",
              options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
              selectInput(ns("sel_k_prob"), labelInput("modelo"), choices = c()),
              selectInput(ns("sel_cat_prob"), labelInput("selectCat"), 
                          choices = c()),
              numericInput(ns("sel_corte_prob"), labelInput("probC"),
                           0.1, min = 0, max = 1, step = 0.1)
            )
          ),
          col_7(
            withLoader(verbatimTextOutput(ns("txtcortes")), 
                       type = "html", loader = "loader4"))
        )
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
      if(length(choices) > 2) {
        hideTab(inputId = "BoxModel", target = "tabCortes")
      } else {
        showTab(inputId = "BoxModel", target = "tabCortes")
      }
    } else {
      updateTabsetPanel(session, "BoxModel", "tabModelo")
      hideTab(inputId = "BoxModel", target = "tabIndices")
      hideTab(inputId = "BoxModel", target = "tabVariacion")
      hideTab(inputId = "BoxModel", target = "tabCortes")
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
      datos       <- updateData$datos
      variable    <- updateData$variable.predecir
      choices     <- as.character(unique(datos[, variable]))
      predictoras <- colnames.empty(var.numericas(updateData$datos))
      vcs         <- 1:length(auxmodelos)
      gs          <- 1:length(auxmodelos[[1]])
      ks          <- names(auxmodelos[[1]][[1]])
      
      updateSelectInput(session, "sel_k", choices = ks)
      updateSelectInput(session, "sel_k_prob", choices = ks)
      updateSelectInput(session, "sel_cat_err", choices = c("Global", choices))
      
      if(length(choices) == 2) {
        updateSelectInput(session, "sel_cat", choices = choices)
        updateNumericInput(session, "sel_corte", value = 0.5)
        updateSelectInput(session, "sel_cat_prob", choices = choices)
        updateNumericInput(session, "sel_corte_prob", value = 0.1)
        show("panelCorte")
      } else {
        hide("panelCorte")
        hideTab(inputId = "BoxModel", target = "tabCortes")
      }
      
      updateSelectizeInput(session, "var_svm_plot", choices = predictoras)
      updateSelectInput(session, "sel_cat_lambda", choices = choices)
      
      updateTabsetPanel(session, "BoxModel", selected = "tabModelo")
    }
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    modelosrv$modelos <- NULL
    datos    <- updateData$datos
    variable <- updateData$variable.predecir
    choices  <- as.character(unique(datos[, variable]))
    predictoras <- colnames.empty(var.numericas(updateData$datos))
    
    updateNumericInput(session, "kmax", value = round(sqrt(nrow(datos))))
    updateNumericInput(session, "mtry", value = round(sqrt(ncol(datos))))
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
      datos      <- isolate(updateData$datos)
      variable   <- isolate(updateData$variable.predecir)
      cant.vc    <- isolate(updateData$numValC)
      numGrupos  <- isolate(updateData$numGrupos)
      grupos     <- isolate(updateData$grupos)
      categorias <- levels(datos[, variable])
      
      tryCatch({
        if(nombrePred == "reg" & length(categorias) != 2) {
          modelosrv$modelos <- NULL
          modelosrv$modelo  <- NULL
          modelosrv$i       <- 1
          stop("Solo se permiten 2 categorias para este modelo.")
        } else {
          auxmodelos <- lapply(1:cant.vc, function(vc) {
            lapply(1:numGrupos, function(g) {
              train <- datos[-grupos[[vc]][[g]], ]
              formula <- as.formula(paste0(variable, "~ ."))
              generar.modelos(nombrePred, formula, train, input, variable)
            })
          })
          modelosrv$modelos <- auxmodelos
          print(auxmodelos)  
        }
      }, error = function(e) {
        modelosrv$modelos <- NULL
        stop(e)
      })
    })
  })
  
  observeEvent(c(modelosrv$modelos), {
    cv_modelos <- modelosrv$modelos
    
    datos <- isolate(updateData$datos)
    grupos   <- isolate(updateData$grupos)
    variable <- isolate(updateData$variable.predecir)
    choices  <- as.character(unique(datos[, variable]))
    #& !is.null(input$sel_cat) & !is.null(input$sel_corte)
    
    tryCatch({
      if(!is.null(cv_modelos)) {
        for (vc in 1:length(cv_modelos)) {
          for (g in 1:length(cv_modelos[[vc]])) {
            for (nombre in names(cv_modelos[[vc]][[g]])) {
              modelo <- cv_modelos[[vc]][[g]][[nombre]]
              
              test       <- datos[grupos[[vc]][[g]], ]
              prob       <- predict(modelo, test, type = 'prob')
              categorias <- levels(test[, variable])
              
              if(length(categorias) == 2) {
                categoria <- choices[1]
                corte     <- 0.5
                if(nombrePred == "regp") {
                  score <- prob$prediction[, categoria, ]
                  prob$prediction <- prob$prediction[, , 1]
                } else {
                  score <- prob$prediction[, categoria]
                }
                prueba    <- test[, variable]
                results   <- indices.corte(corte, score, categorias, categoria, 
                                           prueba, print = FALSE)
                mc     <- results$MC
                pred   <- results$pred
              } else {
                pred      <- predict(modelo, test, type = 'class')
                mc        <- confusion.matrix(test, pred)
                pred      <- pred$prediction
                categoria <- NULL
                corte     <- NULL
                if(nombrePred == "regp") {
                  prob$prediction <- prob$prediction[, , 1]
                }
              }
              
              vcaux <- paste0("VC", vc)
              gaux  <- paste0("G", g)
              modelos[[nombrePred]][[nombre]][[vcaux]][[gaux]] <- list(
                nombre = nombre, modelo = modelo,
                pred = pred, prob = prob, mc = mc
              )
              modelos[[nombrePred]][[nombre]][["categoria"]] <- categoria
              modelos[[nombrePred]][[nombre]][["corte"]]     <- corte
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
  
  observeEvent(input$sel_k, {
    nombre <- input$sel_k
    modelo <- modelos[[nombrePred]][[nombre]]
    
    updateNumericInput(session, "sel_cat", value = modelo[["categoria"]])
    updateNumericInput(session, "sel_corte", value = modelo[["corte"]])
  })
  
  observeEvent(input$aplicar, {
    k          <- input$sel_k
    cv_modelos <- modelosrv$modelos
    datos      <- isolate(updateData$datos)
    variable   <- isolate(updateData$variable.predecir)
    categorias <- levels(datos[, variable])
    grupos     <- isolate(updateData$grupos)
    #& !is.null(input$sel_cat) & !is.null(input$sel_corte)
    
    tryCatch({
      if(!is.null(cv_modelos)) {
        if(length(categorias) == 2) {
          categoria <- input$sel_cat
          corte     <- input$sel_corte
          modeloaux <- modelos[[nombrePred]][[k]]
          
          for (vc in names(modeloaux)) {
            if(vc %in% c("categoria", "corte")) {
              next
            } else {
              vci <- as.numeric(gsub("VC", "", vc))
            }
            for (g in 1:length(modeloaux[[vc]])) {
              test   <- datos[grupos[[vci]][[g]], ]
              modelo <- modeloaux[[vc]][[g]]
              prob   <- modelo[["prob"]]
              
              if(nombrePred == "regp") {
                score <- prob$prediction[, categoria, ]
                prob$prediction <- prob$prediction[, , 1]
              } else {
                score <- prob$prediction[, categoria]
              }
              prueba    <- test[, variable]
              results   <- indices.corte(corte, score, categorias, categoria, 
                                         prueba, print = FALSE)
              mc     <- results$MC
              pred   <- results$pred
              
              vcaux <- paste0("VC", vci)
              gaux  <- paste0("G", g)
              modelos[[nombrePred]][[k]][[vcaux]][[gaux]] <- list(
                nombre = k, modelo = modelo$modelo, 
                pred = pred, prob = prob, mc = mc
              )
              modelos[[nombrePred]][[k]][["categoria"]] <- categoria
              modelos[[nombrePred]][[k]][["corte"]]     <- corte
            }
          }
        }
        
        modelosrv$modelo <- modelos[[nombrePred]][[k]]
      }
    }, error = function(e) {
      print(e)
      modelos[[nombrePred]] <- NULL
    })
  })
  
  # Matriz de Confusion 
  output$MC <- renderUI({
    idioma <- codedioma$idioma
    modelo <- modelosrv$modelo
    i <- modelosrv$i
    if(is.numeric(i)) {
      mc <- calc_g_mc(modelo[[paste0("VC", i)]])
      mc <- as.data.frame.matrix(mc)
    } else {
      mc <- calc_cross_mc(modelo)
      mc <- as.data.frame.matrix(mc)
    }
    
    ECH <- "text-align: center;padding: 8px;font-weight: 400;"
    ECR <- "vertical-align: middle;padding: 8px;writing-mode: vertical-lr;text-align: center;transform: rotate(180deg);"
    ECP <- "text-align: center;padding: 20px;border: 2px solid gray;font-size: large;background-color: #91db57;min-width: 150px;"
    ECE <- "text-align: center;padding: 20px;border: 2px solid gray;font-size: large;background-color: #db5f57;min-width: 150px;"
    
    res <- tags$table(
      style = "width:auto;font-size: x-large;",
      tags$thead(
        tags$tr(
          tags$th(rowspan = "2", colspan = "2", style = ECH),
          tags$th(colspan = ncol(mc), style = ECH, tags$b(tr("pred", idioma)))
        ),
        tags$tr(
          unname(lapply(names(mc), function(x) tags$th(style = ECH, x)))
        )
      ),
      tags$tbody(
        lapply(1:ncol(mc), function(i) {
          tags$tr(
            if(i == 1) {
              tags$th(rowspan = nrow(mc), style = ECR, tags$b("Real"))
            },
            tags$td(style = ECH, names(mc)[i]),
            unname(lapply(1:length(mc[names(mc)[i], ]), function(j) {
              val <- paste0(mc[i, j], " (", round((mc[i, j] / sum(mc[i, ])) * 100), "%)")
              if(i == j) {
                tags$td(style = ECP, val)
              } else {
                tags$td(style = ECE, val)
              }
            }))
          )
        })
      )
    )
    
    return(res)
  })
  
  # Indices Generales
  output$PREC <- renderEcharts4r({
    idioma <- codedioma$idioma
    modelo <- modelosrv$modelo
    i <- modelosrv$i
    if(is.numeric(i)) {
      mc <- calc_g_mc(modelo[[paste0("VC", i)]])
      mc <- data.matrix(mc)
    } else {
      mc <- calc_cross_mc(modelo)
      mc <- data.matrix(mc)
    }
    
    indices <- indices.generales(mc)
    df <- data.frame(
      "tipo"  = "Global",
      "prec"  = indices$precision.global,
      "error" = indices$error.global)
    
    for (x in names(indices$precision.clase)) {
      df <- rbind(df, c(x, indices$precision.clase[x], indices$error.clase[x]))
    }
    df$prec  <- round(as.numeric(df$prec), 2)
    df$error <- round(as.numeric(df$error), 2)
    df <- rbind(df, df[1, ])
    df <- df[-1, ]
    
    formatoLabel <- e_JS(
      "function(params) {
        if(params.value[0] > 0) {
          return(params.value[0] + '%')
        } else{
          return('')
        }
      }"
    )
    
    df |> 
      e_charts(tipo) |> 
      e_bar(prec, name = tr("prec",idioma), stack = "g1", 
            label = list(show = TRUE, formatter = formatoLabel, position = "inside")) |> 
      e_bar(error, name = "Error", stack = "g1", 
            label = list(show = TRUE, formatter = formatoLabel, position = "inside")) |>
      e_color(c("#91db57", "#db5f57")) |>
      e_flip_coords() |> 
      e_x_axis(max = 100) |>
      e_text_style(axisLabel = list(fontSize = 16))
  })
  
  # Tabla de la prediccion
  output$modeloPredTable <- DT::renderDataTable({
    lng    <- codedioma$idioma
    modelo <- modelosrv$modelo
    i <- modelosrv$i
    
    categoria <- isolate(input$sel_cat)
    datos  <- isolate(updateData$datos)
    grupos <- isolate(updateData$grupos)
    var    <- isolate(updateData$variable.predecir)
    
    if(is.numeric(i)) {
      modelo <- modelo[[paste0("VC", i)]]
      grupos <- grupos[[i]]
      aux  <- calc_g_prob(modelo, grupos)
    } else {
      aux  <- calc_cross_prob(modelo, grupos)
    }
    
    pred <- aux[[2]]
    names(pred) <- row.names(datos)
    prob <- aux[[1]]
    
    if(ncol(prob) == 2) {
      etqs <- c(tr("reald", lng), tr("pred", lng), "Probabilidad")
      res <- tabla.pred(datos[, var], pred, prob[, categoria], etqs)
    } else {
      etqs <- c(tr("reald", lng), tr("pred", lng))
      res <- tabla.pred(datos[, var], pred, NULL, etqs)
    }
    
    return(res)
  }, server = FALSE)
  
  # Variación del Error
  output$VARE <- renderEcharts4r({
    input$aplicar2
    ind    <- isolate(input$sel_ind_err)
    cat    <- isolate(input$sel_cat_err)
    modelo <- modelos[[nombrePred]]
    df     <- calc_cross_pg(modelo, cat)
    lng    <- codedioma$idioma
    
    tiulos <- c(tr("numExp", lng), paste0(tr("prec", lng), " ", cat))
    
    if(ind == "Error") {
      df <- round(100 - df, 2)
      e_var_error(df, T, tiulos)
    } else {
      df <- round(df, 2)
      e_var_error(df, F, tiulos)
    }
  })
  
  # Genera la probabilidad de corte multiple
  output$txtcortes <- renderPrint({
    input$runProb
    i          <- modelosrv$i
    datos      <- isolate(updateData$datos)
    var        <- isolate(updateData$variable.predecir)
    prueba     <- datos[, var] 
    grupos     <- isolate(updateData$grupos)
    k          <- isolate(input$sel_k_prob)
    categorias <- levels(datos[, var])
    positiva   <- isolate(input$sel_cat_prob)
    paso       <- isolate(input$sel_corte_prob)
    
    tryCatch({
      if(is.numeric(i)) {
        i <- as.numeric(i)
        modelo <- modelos[[nombrePred]][[k]][[paste0("VC", i)]]
        grupos <- grupos[[i]]
        calc_g_cortes(modelo, grupos, prueba, categorias, positiva, paso)
      } else {
        modelo <- modelos[[nombrePred]][[k]]
        calc_cross_cortes(modelo, grupos, prueba, categorias, positiva, paso)
      }
    }, error = function(e) {
      if(length(categorias) != 2) {
        stop(paste0("ERROR: ", tr("errorprobC", codedioma$idioma)))
      } else {
        stop(paste0("ERROR: ", e))
      }
    })
  })
}
    
## To be copied in the UI
# mod_cross_val_ui("mod_cross_val_ui_1")
    
## To be copied in the server
# callModule(mod_cross_val_server, "mod_cross_val_ui_1")
 
