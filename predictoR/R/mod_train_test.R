#' Train-Test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_train_test_ui <- function(id, nombrePred) {
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
              generar.opciones(nombrePred, ns)
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
          id = ns("panelCorte"),
          style = "background-color: #666;color: white;",
          fluidRow(
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
      
      if(nombrePred == "svm") {
        tabPanel(
          title = labelInput("gclasificacion"), value = "tabSvmPlot",
          wellPanel(
            style = "background-color: #666;color: white;",
            selectizeInput(
              ns("var_svm_plot"), label = "Variables Predictoras:", 
              multiple = T, choices = c(), 
              options = list(maxItems = 2, placeholder = ""), width = "100%")
          ),
          withLoader(plotOutput(ns('plot_svm'), height = "70vh"), 
                     type = "html", loader = "loader4")
        )
      },
      
      if(nombrePred == "tree") {
        tabPanel(title = labelInput("garbol"), value = "tabPlotTree",
                 withLoader(plotOutput(ns('plot_tree'), height = "70vh"), 
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "tree") {
        tabPanel(title = labelInput("reglas"), value = "tabRulesTree",
                 withLoader(verbatimTextOutput(ns("rules_tree")), 
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "rndf") {
        tabPanel(title = labelInput("evolerror"), value = "tabErrorForest",
                 withLoader(echarts4rOutput(ns('plot_error_rndf'), height = "70vh"),
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "rndf") {
        tabPanel(title = labelInput("varImp"), value = "tabVarForest",
                 withLoader(echarts4rOutput(ns('plot_var_rndf'), height = "70vh"),
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "rndf") {
        tabPanel(
          title = labelInput("reglas"), value = "tabRulesForest",
          tags$div(
            style = "height: 70vh;",
            col_5(
              wellPanel(
                style = "background-color: #666;color: white;",
                numericInput(ns("n_rule_rndf"), labelInput("ruleNumTree"),
                             1, min = 1, step = 1)
              )
            ),
            col_7(
              withLoader(verbatimTextOutput(ns("rules_rndf")),
                         type = "html", loader = "loader4")
            )
          )
        )
      },
      
      if(nombrePred == "boost") {
        tabPanel(title = labelInput("evolerror"), value = "tabErrorBoost",
                 withLoader(echarts4rOutput(ns('plot_error_boost'), height = "70vh"),
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "boost") {
        tabPanel(title = labelInput("varImp"), value = "tabVarBoost",
                 withLoader(echarts4rOutput(ns('plot_var_boost'), height = "70vh"),
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "boost") {
        tabPanel(
          title = labelInput("reglas"), value = "tabRulesBoost",
          tags$div(
            style = "height: 70vh;",
            col_5(
              wellPanel(
                style = "background-color: #666;color: white;",
                numericInput(ns("n_rule_boost"), labelInput("ruleNumTree"),
                             1, min = 1, step = 1)
              )
            ),
            col_7(
              withLoader(verbatimTextOutput(ns("rules_boost")),
                         type = "html", loader = "loader4")
            )
          )
        )
      },
      
      if(nombrePred == "xgb") {
        tabPanel(title = labelInput("varImp"), value = "tabVarXGB",
                 withLoader(echarts4rOutput(ns('plot_var_xgb'), height = "70vh"),
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "nnet") {
        tabPanel(title = labelInput("redPlot"), value = "tabPlotRedes",
                 withLoader(plotOutput(ns('plot_nnet'), height = "70vh"), 
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "regp") {
        tabPanel(title = labelInput("posibLanda"), value = "tabLambda",
                 withLoader(echarts4rOutput(ns('plot_regp_lambda'), height = "70vh"), 
                            type = "html", loader = "loader4"))
      },
      
      if(nombrePred == "regp") {
        tabPanel(
          title = labelInput("gcoeff"),value = "tabCoeff",
          tags$div(
            style = "height: 70vh;",
            col_5(
              wellPanel(
                style = "background-color: #666;color: white;",
                selectInput(ns("sel_cat_lambda"), label = labelInput("selectCat"),
                            choices =  "", width = "100%"),
                fluidRow(
                  col_8(
                    numericInput(ns("lambda"), labelInput("lambda"),
                                 value = 0, width = "100%")
                  ),
                  col_4(
                    tags$div(style = "margin-top: 24px;"),
                    actionButton(ns("calcularLambda"), "Calcular lambda",
                                 width = "100%")
                  )
                ),
                tags$hr(),
                fluidRow(
                  col_12(actionButton(ns("aplicarLambda"), tr("aplicar"),
                                      width = "100%"))
                )
              ),
              withLoader(verbatimTextOutput(ns('txtBetas')), 
                         type = "html", loader = "loader4")
            ),
            col_7(
              withLoader(echarts4rOutput(ns('plot_regp_coeff'), height = "70vh"), 
                         type = "html", loader = "loader4"))
          )
        )
      },
      
      if(nombrePred == "lda") {
        tabPanel(title = labelInput("gclasificacion"), value = "tabLdaPlot",
                 withLoader(plotOutput(ns('plot_lda'), height = "70vh"),
                            type = "html", loader = "loader4"))
      },
      
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
mod_train_test_server <- function(input, output, session, updateData, modelos, nombrePred, codedioma){
  ns <- session$ns
  modelosrv <- rv(modelos = NULL, modelo = NULL, i = 1)
  
  # Actualiza la cantidad de capas ocultas
  observeEvent(c(input$n_hidden, updateData$datos), {
    if(!is.null(updateData$datos) && !is.null(input$n_hidden)) {
      for (i in 1:10) {
        if(i <= input$n_hidden) {
          shinyjs::show(paste0("hidden", i))
        } else {
          shinyjs::hide(paste0("hidden", i))
        }
      }
    }
  })
  
  observe({
    datos    <- isolate(updateData$datos)
    variable <- isolate(updateData$variable.predecir)
    choices  <- as.character(unique(datos[, variable]))
    
    if(!is.null(modelos[[nombrePred]])) {
      showTab(inputId = "BoxModel", target = "tabIndices")
      showTab(inputId = "BoxModel", target = "tabEvolucion")
      showTab(inputId = "BoxModel", target = "tabSvmPlot")
      showTab(inputId = "BoxModel", target = "tabPlotTree")
      showTab(inputId = "BoxModel", target = "tabRulesTree")
      showTab(inputId = "BoxModel", target = "tabErrorForest")
      showTab(inputId = "BoxModel", target = "tabVarForest")
      showTab(inputId = "BoxModel", target = "tabRulesForest")
      showTab(inputId = "BoxModel", target = "tabErrorBoost")
      showTab(inputId = "BoxModel", target = "tabVarBoost")
      showTab(inputId = "BoxModel", target = "tabRulesBoost")
      showTab(inputId = "BoxModel", target = "tabVarXGB")
      showTab(inputId = "BoxModel", target = "tabPlotRedes")
      showTab(inputId = "BoxModel", target = "tabLambda")
      showTab(inputId = "BoxModel", target = "tabCoeff")
      showTab(inputId = "BoxModel", target = "tabLdaPlot")
      showTab(inputId = "BoxModel", target = "tabVariacion")
      if(length(choices) > 2) {
        hideTab(inputId = "BoxModel", target = "tabCortes")
      } else {
        showTab(inputId = "BoxModel", target = "tabCortes")
      }
    } else {
      updateTabsetPanel(session, "BoxModel", "tabModelo")
      hideTab(inputId = "BoxModel", target = "tabIndices")
      hideTab(inputId = "BoxModel", target = "tabEvolucion")
      hideTab(inputId = "BoxModel", target = "tabCortes")
      hideTab(inputId = "BoxModel", target = "tabVariacion")
      hideTab(inputId = "BoxModel", target = "tabSvmPlot")
      hideTab(inputId = "BoxModel", target = "tabPlotTree")
      hideTab(inputId = "BoxModel", target = "tabRulesTree")
      hideTab(inputId = "BoxModel", target = "tabErrorForest")
      hideTab(inputId = "BoxModel", target = "tabVarForest")
      hideTab(inputId = "BoxModel", target = "tabRulesForest")
      hideTab(inputId = "BoxModel", target = "tabErrorBoost")
      hideTab(inputId = "BoxModel", target = "tabVarBoost")
      hideTab(inputId = "BoxModel", target = "tabRulesBoost")
      hideTab(inputId = "BoxModel", target = "tabVarXGB")
      hideTab(inputId = "BoxModel", target = "tabPlotRedes")
      hideTab(inputId = "BoxModel", target = "tabLambda")
      hideTab(inputId = "BoxModel", target = "tabCoeff")
      hideTab(inputId = "BoxModel", target = "tabLdaPlot")
    }
    
    if(!is.null(input$BoxModel)) {
      if(input$BoxModel == "tabVariacion") {
        hide("titleVC")
      } else {
        show("titleVC")
      }
    }
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    modelosrv$modelos <- NULL
    modelosrv$modelo  <- NULL
    modelosrv$i       <- 1
    datos    <- updateData$datos
    variable <- updateData$variable.predecir
    choices  <- as.character(unique(datos[, variable]))
    predictoras <- colnames.empty(var.numericas(updateData$datos))
    
    updateNumericInput(session, "kmax", value = round(sqrt(nrow(datos))))
    updateNumericInput(session, "mtry", value = round(sqrt(ncol(datos))))
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
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    output$txtmodel <- renderPrint({cat("No se ha generado el modelo.")})
  })
  
  observeEvent(updateData$grupos, {
    choices <- 1:length(updateData$grupos)
    updateSelectInput(session, "vci", choices = choices)
  })
  
  observeEvent(input$vci, {
    modelosrv$i <- as.numeric(input$vci)
  })
  
  # Generar modelo
  observeEvent(input$runModelo, {
    output$txtmodel <- renderPrint({
      datos      <- isolate(updateData$datos)
      variable   <- isolate(updateData$variable.predecir)
      cant.tt    <- isolate(updateData$numTT)
      grupos     <- isolate(updateData$grupos)
      categorias <- levels(datos[, variable])
      
      tryCatch({
        if(nombrePred == "reg" & length(categorias) != 2) {
          modelosrv$modelos <- NULL
          modelosrv$modelo  <- NULL
          modelosrv$i       <- 1
          stop("Solo se permiten 2 categorias para este modelo.")
        } else {
          auxmodelos <- lapply(1:cant.tt, function(vc) {
            train <- datos[grupos[[vc]][["train"]], ]
            formula <- as.formula(paste0(variable, "~ ."))
            generar.modelo(nombrePred, formula, train, input, variable)
          })
          modelosrv$modelos <- auxmodelos
          if(nombrePred == "regp") {
            updateNumericInput(session, "lambda", value = log(auxmodelos[[1]]$modelo$lambda.1se))
          }
          print(auxmodelos)  
        }
      }, error = function(e) {
        modelosrv$modelos <- NULL
        modelosrv$modelo  <- NULL
        modelosrv$i       <- 1
        print(e)
      }, warning = function(w) {
        modelosrv$modelos <- NULL
        modelosrv$modelo  <- NULL
        modelosrv$i       <- 1
        print(w)
      })
    })
  })
  
  observeEvent(c(modelosrv$modelos, input$aplicar, input$aplicarLambda), {
    cv_modelos <- modelosrv$modelos
    
    datos    <- isolate(updateData$datos)
    grupos   <- isolate(updateData$grupos)
    variable <- isolate(updateData$variable.predecir)
    lambda <- input$lambda
    if(is.null(lambda) | !is.numeric(lambda)) {
      lambda <- 0
    }
    #& !is.null(input$sel_cat) & !is.null(input$sel_corte)
    
    tryCatch({
      if(!is.null(cv_modelos)) {
        for (vc in 1:length(cv_modelos)) {
          modelo <- cv_modelos[[vc]]$modelo
          nombre <- cv_modelos[[vc]]$algoritmo
          
          test       <- datos[grupos[[vc]][["test"]], ]
          categorias <- levels(test[, variable])
          
          if(nombrePred == "regp") {
            prueba <- test[, variable]
            test[[variable]] <- NULL
            test <- model.matrix(~., test)[, -1]
            prob <- list(prediction = predict(modelo, test, s = exp(lambda), type = 'response'),
                         var.pred = variable)
            if(length(categorias) == 2) {
              categoria <- input$sel_cat
              corte     <- input$sel_corte
              score <- prob$prediction[, categoria, ]
              prob$prediction <- prob$prediction[, , 1]
              results   <- indices.corte(corte, score, categorias, categoria,
                                         prueba, print = FALSE)
              mc     <- results$MC
              pred   <- results$pred
            } else {
              categoria <- NULL
              corte     <- NULL
              pred <- predict(modelo, test, s = exp(lambda), type = 'class')
              pred <- factor(pred[, 1], levels = categorias)
              mc   <- table(prueba, pred)
              pred <- list(prediction = pred, var.pred = variable)
              prob$prediction <- prob$prediction[, , 1]
              pred   <- pred$prediction
            }
          } else {
            prob <- predict(modelo, test, type = 'prob')
            
            if(length(categorias) == 2) {
              categoria <- input$sel_cat
              corte     <- input$sel_corte
              score     <- prob$prediction[, categoria]
              prueba    <- test[, variable]
              results   <- indices.corte(corte, score, categorias, categoria,
                                         prueba, print = FALSE)
              mc     <- results$MC
              pred   <- results$pred
            } else {
              categoria <- NULL
              corte     <- NULL
              pred   <- predict(modelo, test, type = 'class')
              mc     <- confusion.matrix(test, pred)
              pred   <- pred$prediction
            }
          }
          
          modelos[[nombrePred]][[nombre]][[vc]] <- list(
            nombre = nombre, modelo = modelo,
            pred = pred, prob = prob, mc = mc
          )
        }
        
        modelos[[nombrePred]][[nombre]][["categoria"]] <- categoria
        modelos[[nombrePred]][[nombre]][["corte"]]     <- corte
        modelosrv$modelo <- modelos[[nombrePred]][[nombre]]
        modelosrv$i <- isolate(as.numeric(input$vci))
      }
    }, error = function(e) {
      print(e)
      modelos[[nombrePred]] <- NULL
    })
  })
  #91db57
  #db5f57
  #00FF00
  #FF0000
  # Matriz de Confusion 
  output$MC <- renderUI({
    idioma <- codedioma$idioma
    modelo <- modelosrv$modelo[[modelosrv$i]]
    mc <- as.data.frame.matrix(modelo$mc)
    
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
            })))
        })
      )
    )
    
    return(res)
  })
  
  # Indices Generales
  output$PREC <- renderEcharts4r({
    idioma <- codedioma$idioma
    modelo <- modelosrv$modelo[[modelosrv$i]]
    indices <- indices.generales(modelo$mc)
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
      e_bar(prec, name = tr("prec", idioma), stack = "g1", 
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
    lng  <- codedioma$idioma
    modelo <- modelosrv$modelo[[modelosrv$i]]
    
    categoria <- isolate(input$sel_cat)
    datos  <- isolate(updateData$datos)
    grupos <- isolate(updateData$grupos)
    test <- datos[grupos[[modelosrv$i]][["test"]], ]
    var  <- isolate(updateData$variable.predecir)
    
    real <- test[, var]
    pred <- modelo$pred
    names(pred) <- row.names(test)
    prob <- modelo$prob$prediction
    
    if(ncol(prob) == 2) {
      etqs <- c(tr("reald", lng), tr("pred", lng), "Probabilidad")
      res <- tabla.pred(real, pred, prob[, categoria], etqs)
    } else {
      etqs <- c(tr("reald", lng), tr("pred", lng))
      res <- tabla.pred(real, pred, NULL, etqs)
    }
    
    return(res)
  }, server = FALSE)
  
  # Variación del Error
  output$VARE <- renderEcharts4r({
    input$aplicar2
    ind    <- isolate(input$sel_ind_err)
    cat    <- isolate(input$sel_cat_err)
    modelo <- modelosrv$modelo
    df  <- calc_cross_pg_tt(modelo, cat)
    lng <- codedioma$idioma
    
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
    datos      <- updateData$datos
    grupos     <- updateData$grupos
    test       <- datos[grupos[[modelosrv$i]][["test"]], ]
    variable   <- updateData$variable.predecir
    categorias <- levels(test[, variable])
    
    tryCatch({
      positiva   <- isolate(input$sel_cat_prob)
      paso       <- isolate(input$sel_corte_prob)
      prediccion <- modelosrv$modelo[[modelosrv$i]]$prob
      prob <- prediccion$prediction[, positiva]
      # Valor real
      prueba <- test[, variable] 
      # Se calculan las MC para la categoria y paso seleccionado
      indices.corte.paso(paso, prob, categorias, positiva, prueba)
    }, error = function(e) {
      if(length(categorias) != 2) {
        stop(paste0("ERROR: ", tr("errorprobC", codedioma$idioma)))
      } else {
        stop(paste0("ERROR: ", e))
      }
    })
  })
  
  # Grafico de Voronoi (SVM)
  output$plot_svm <- renderPlot({
    tryCatch({
      datos    <- updateData$datos
      grupos   <- updateData$grupos
      train    <- datos[grupos[[modelosrv$i]][["train"]], ]
      varpred  <- isolate(updateData$variable.predecir)
      vars     <- input$var_svm_plot
      kernel   <- isolate(input$kernel)
      
      voronoi_svm_plot(train, varpred, vars, kernel)
    }, error = function(e) {
      showNotification(e, duration = 10, type = "error")
      return(NULL)
    })
  })
  
  # Plotear el arbol
  output$plot_tree <- renderPlot({
    tryCatch({
      tipo   <- isolate(input$split)
      datos  <- updateData$datos
      var    <- updateData$variable.predecir
      num    <- length(levels(datos[,var]))
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      
      tree_plot(modelo, num)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Mostrar Reglas Arboles
  output$rules_tree <- renderPrint({
    modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
    varpred <- modelo$prmdt$var.pred
    
    tree_rules(modelo, varpred)
  })
  
  # Grafico de evolucion del error Bosques
  output$plot_error_rndf <- renderEcharts4r({
    tryCatch({
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      cod  <- paste0("### evolerror\n", plot.rf.error())
      isolate(codedioma$code <- append(codedioma$code, cod))
      e_rf_error(modelo, strsplit(tr("numTree", codedioma$idioma), ':')[[1]])
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Grafico de importancia de variables Bosques
  output$plot_var_rndf <- renderEcharts4r({
    tryCatch({
      cod  <- paste0("### docImpV\n", rf.importance.plot())
      isolate(codedioma$code <- append(codedioma$code, cod))
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      e_rndf_importance(modelo)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Mostrar Reglas Bosques
  output$rules_rndf <- renderPrint({
    idioma <- codedioma$idioma
    n      <- input$n_rule_rndf
    modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
    datos    <- updateData$datos
    grupos   <- updateData$grupos
    train    <- datos[grupos[[modelosrv$i]][["train"]], ]
    modelo$call$data <- train
    
    tryCatch({
      isolate(codedioma$code <- append(codedioma$code, paste0("### reglas\n", "rulesRandomForest(modelo.rf, ",n,")\n")))
      rulesRandomForest(modelo, n)
    }, error = function(e) {
      stop(tr("NoDRule", idioma))
    })
  })
  
  # Grafico de evolucion del error Potenciacion
  output$plot_error_boost <- renderEcharts4r({
    datos    <- updateData$datos
    grupos   <- updateData$grupos
    train    <- datos[grupos[[modelosrv$i]][["train"]], ]
    modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
    label  <- strsplit(tr("numTree", codedioma$idioma), ':')[[1]]
    
    tryCatch({
      e_ada_evol_error(modelo, train, label)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Grafico de importancia de variables Potenciacion
  output$plot_var_boost <- renderEcharts4r({
    tryCatch({
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      e_boost_importance(modelo)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Mostrar Reglas Potenciacion
  output$rules_boost <- renderPrint({
    modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
    n <- input$n_rule_boost
    datos    <- isolate(updateData$datos)
    grupos   <- isolate(updateData$grupos)
    train    <- isolate(datos[grupos[[modelosrv$i]][["train"]], ])
    isolate(var.pred <- updateData$variable.predecir)
    tryCatch({
      isolate(codedioma$code <- append(codedioma$code, rules.boosting(n)))
      rules(modelo$trees[[n]], train, var.pred)
    }, error = function(e) {
      stop(tr("NoDRule", codedioma$idioma))
    })
  })
  
  # Grafico de importancia de variables XGB
  output$plot_var_xgb <- renderEcharts4r({
    tryCatch({
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      e_xgb_importance(modelo)
    }, error = function(e) {
      showNotification(paste0("Error :",e), duration = 15, type = "error")
      return(NULL)
    })
  })
  
  # Genera el grafico de la red neuronal
  output$plot_nnet <- renderPlot({
    idioma <- codedioma$idioma
    tryCatch({
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      hidden <- as.numeric(as.character(modelo$call$hidden)[-1])
      if((length(hidden) * sum(hidden)) <= 1500 & ncol(modelo$covariate) <= 20) {
        #cod <- ifelse(input$fieldCodeNnPlot == "", nn.plot(), input$fieldCodeNnPlot)
        nnet_plot(modelo)
      } else {
        showNotification(tr("bigPlot", idioma), duration = 10, type = "message")
        return(NULL)
      }
    }, error = function(e){
      return(NULL)
    })
  })
  
  # Grafico de los Lambdas
  output$plot_regp_lambda <- renderEcharts4r({
    idioma <- codedioma$idioma
    scales <- isolate(as.logical(input$scales))
    alpha  <- isolate(input$alpha)
    
    tryCatch({
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      e_posib_lambda(modelo, labels = c(tr("superior", idioma), tr("inferior", idioma), tr("lambda", idioma)))
    }, error = function(e) { 
      showNotification(paste0("Error (R/L) : ", e), duration = 15, type = "error")
      return(NULL)
    })
  })
  
  observeEvent(input$calcularLambda, {
    modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
    updateNumericInput(session, "lambda", value = log(modelo$lambda.1se))
  })
  
  # Texto de los Betas
  output$txtBetas <- renderPrint({
    tryCatch({
      lambda <- isolate(input$lambda)
      if(is.null(lambda)) {
        lambda <- 0
      }
      cat    <- isolate(input$sel_cat_lambda)
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      
      res <- coef(modelo, s = lambda)
      print(res[[cat]][, 1])
    },
    error = function(e){ 
      return(NULL)
    })
  })
  
  # Grafico de los coeficientes Lambdas
  output$plot_regp_coeff <- renderEcharts4r({
    lambda <- isolate(input$lambda)
    if(is.null(lambda)) {
      lambda <- 0
    }
    cat    <- input$sel_cat_lambda
    modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
    
    tryCatch({
      e_coeff_lambda(modelo, cat, lambda, tr("lambda", codedioma$idioma))
    },
    error = function(e){ 
      showNotification(paste0("Error (R/L) : ", e), duration = 15, type = "error")
    })
  })
  
  # Grafico de Voronoi (LDA)
  output$plot_lda <- renderPlot({
    tryCatch({
      idioma    <- codedioma$idioma
      datos    <- updateData$datos
      grupos   <- updateData$grupos
      train    <- datos[grupos[[modelosrv$i]][["train"]], ]
      variable  <- isolate(updateData$variable.predecir)
      modelo <- modelosrv$modelo[[modelosrv$i]]$modelo
      plot.lda(modelo, train, train[, variable], col = as.numeric(train[, variable]))
    }, error = function(e) {
      showNotification(paste0(e), duration = 10, type = "error")
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_train_test_ui("train_test_ui_1")
    
## To be copied in the server
# callModule(mod_train_test_server, "train_test_ui_1")
 
