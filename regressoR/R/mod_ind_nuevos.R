#' ind_nuevos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_ind_nuevos_ui <- function(id){
  
  ns <- NS(id)
  
  btn_style <- "width: 100%;background-color: #3d8dbc;color: white;"

  muestra.datos.pred  <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred2 <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred2'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred3 <- tabPanel(title = labelInput("data"),
                                  div(style = "height: 65vh; overflow: auto;",
                                      withLoader(DT::dataTableOutput(ns('contentsPred3')), 
                                                 type = "html", loader = "loader4")))
  
  
  tagList(
    div(id = ns("primera"),
        div(
          col_11(
            loadeR::mod_carga_datos_ui("carga_datos_ui_2", p(labelInput("cargarComp"),class = "wrapper-tag"), "discoveR")
          ),
          col_1(actionButton(inputId = ns("cargarnext"),width = "100%",
                             label = NULL, icon = icon("forward")) ))
    ),
    div(id = ns("tercera"),
        style = "display:none",
        div(col_1(actionButton(ns("modelback"), label = NULL, width = "100%",
                               icon = icon("backward"))),
            col_10(
              tabBoxPrmdt(
                id = "BoxModeloa",
                tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag") ,solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                         div(
                           col_6(selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%")),
                           col_6(selectInput(inputId = ns("selectModelsPred"), label = labelInput("selectMod"),
                                             choices = list("knn", "dt", "rf", "boost", "svm", "nn", "rl", "rlr", "rd"), width = "100%"))
                         ), hr(style = "border-top: 2px solid #cccccc;" ),
                         uiOutput(ns('opcModelsPredN')),
                         
                         actionButton(ns("PredNuevosBttnModelo"), labelInput("generarM"), width  = "100%" ),br(),br(),
                         withLoader(verbatimTextOutput(ns("txtPredNuevos")),
                                    type = "html", loader = "loader4"))
                
              )),
            col_1(actionButton(ns("modelnext"), label = NULL, width = "100%",
                               icon = icon("forward")))
        )
    ),
    div(id = ns("cuarta"),
        style = "display:none",
        div(col_1(actionButton (ns("nuevosback"), label = NULL, width = "100%",
                                icon = icon("backward"))),
            col_10(
              box(
                title = p(labelInput("cargarNuev"),class = "wrapper-tag2"), width = 12, solidHeader = FALSE,
                collapsible = FALSE, collapsed = FALSE,value = "CargarNuevos",
                footer = muestra.datos.pred3,
                div(col_12(
                  fileInput(ns('archivoNPred2'), labelInput("cargarchivo"), 
                            width = "100%",placeholder = "", buttonLabel = labelInput("subir"),
                            accept = c('text/csv', '.csv', '.txt'))),
                  col_12(
                    fileInput(ns('archivoNPred3'), labelInput("cargarchivo"), 
                              width = "100%",placeholder = "", buttonLabel = labelInput("subir"),
                              accept = c('.xlsx', '.xls'))))))
        ),
        col_1(actionButton(ns("nuevosnext"), label = NULL, width = "100%",
                           icon = icon("forward")))
    ),
    div(id = ns("quinta"),
        style = "display:none",
        div(col_1(actionButton (ns("predicback"), label = NULL, width = "100%",
                                icon = icon("backward"))),
            col_11(
              tabBoxPrmdt(
                id = "BoxModelo",
                tabPanel(title = p(labelInput("predicnuevos"),class = "wrapper-tag"), value = "predicModelo",
                         DT::dataTableOutput(ns("PrediTablePN")),
                         actionButton(ns("predecirPromidat"), "preditc"),  br())
              ))
        )
    )
  )
  
}

#' ind_nuevos Server Function
#' @keywords internal
#' 
mod_ind_nuevos_server <- function(input, output, session, newCases, updateData2, codedioma){
  ns <- session$ns
  shinyjs::runjs('get_file()')
  
  observeEvent(codedioma$idioma, {
    
    # "knn"= kvecinos , "svm"= maquinas de soporte vectorial, "dt"= árboles de decisión, "rf"= Bosques aleatorios, "boost"= potenciacion,"nn"= redes neuronales, "rl"= regresion lineal, "rlr"= regresion lineal penalizada, "rd" = reducción de la dimension
    nombres <- list( "knn", "svm", "dt", "rf", "boost","nn", "rl", "rlr", "rd") 
    names(nombres) <- tr(c("knn", "svm", "dt", "rf", "boost",  "nn", "rl", "rlr", "rd"),codedioma$idioma)
    
    updateSelectInput(session, "selectModelsPred", choices = nombres, selected = input$selectModelsPred)
  })
  
  observeEvent(updateData2$datos, {
    if(!is.null(updateData2$datos)){
      file_type <- input$jsfile_type
      if(file_type == "Excel"){
        shinyjs::hide("archivoNPred2", anim = TRUE, animType = "slide")
        shinyjs::show("archivoNPred3", anim = TRUE, animType = "slide")
      }else{
        shinyjs::hide("archivoNPred3", anim = TRUE, animType = "slide")
        shinyjs::show("archivoNPred2", anim = TRUE, animType = "slide")
        
      }
      shinyjs::runjs('get_inputs()')
      shinyjs::runjs('get_inputs_xlsx()')
      
      cod <-  "datos.aprendizaje.completos <<- datos\n"
      isolate(codedioma$code <- append(codedioma$code, cod))
      
      newCases$originales <- updateData2$originales
      newCases$datos.aprendizaje <- updateData2$datos
      
      shinyjs::show("cargarnext", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide("cargarnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
  #Actualiza el texto del modelo
  output$txtPredNuevos <- renderPrint({
    input$PredNuevosBttnModelo
    train                      <- newCases$datos.aprendizaje
    variable                   <- isolate(input$sel.predic.var.nuevos)
    m.seleccionado             <- isolate(input$selectModelsPred)
    newCases$variable.predecir <- NULL
    newCases$modelo            <- NULL
    newCases$m.seleccionado    <- NULL
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    codigo                     <- ""
    cont                       <- 1
    sub <- "modelo.nuevos <<- "
    tryCatch({
      var    <- paste0(variable, "~.")
      codigo <- switch (m.seleccionado ,
                        knn   = {
                          k.value<- isolate(input$kmax.knn.pred)
                          scales <- isolate(input$switch.scale.knn.pred)
                          kernel <- isolate(input$kernel.knn.pred)
                          distance <- isolate(input$distance_knn)
                          isolate(modelo <- traineR::train.knn(as.formula(var), data = train, scale = as.logical(scales), 
                                                               kernel = kernel, kmax = k.value, distance = distance ))
                          gen.code <- codeKnn(variable, scales, k.value, kernel,distance)
                          
                          isolate(codedioma$code <- append(codedioma$code, paste0(sub, gen.code)))
                          isolate(modelo)
                        },
                        dt    = {
                          minsplit<-isolate(input$minsplit.dt.pred)
                          maxdepth<-isolate(input$maxdepth.dt.pred)
                          isolate(modelo  <- traineR::train.rpart(as.formula(var), data = train,
                                                                  control = rpart.control(minsplit = minsplit, maxdepth = maxdepth), model = TRUE))
                          
                          isolate(codedioma$code <- append(codedioma$code, paste0(sub, codeDt(variable,
                                                                                        minsplit = minsplit,
                                                                                        maxdepth = maxdepth))))
                          isolate(modelo)
                        },
                        rf    = {
                          mtry   <- isolate(input$mtry.rf.pred)
                          ntree  <- isolate(input$ntree.rf.pred)
                          isolate(modelo <- traineR::train.randomForest(as.formula(var), data = train, mtry = mtry, ntree = ntree, importance = TRUE))
                          gen.code <- codeRf(variable,
                                             ntree = ntree,
                                             mtry  = mtry)
                          isolate(codedioma$code <- append(codedioma$code,  paste0(sub, gen.code)))
                          isolate(modelo)
                        },
                        svm   = {
                          scales <- isolate(input$switch.scale.svm.pred)
                          k      <- isolate(input$kernel.svm.pred)
                          isolate(modelo <- traineR::train.svm(as.formula(var), data = train, scale = as.logical(scales), kernel = k))
                          gen.code <- codeSvm(variable, scales, k)
                          isolate(codedioma$code <- append(codedioma$code,  paste0(sub, gen.code)))
                          isolate(modelo)
                        },
                        
                        boost = {
                          n.trees      <- isolate(input$iter.boosting.pred)
                          distribution <- isolate(input$tipo_boosting)
                          shrinkage    <- isolate(input$shrinkage_boosting)
                          gen.code     <- codeBoost(variable, n.trees, distribution, shrinkage)
                          isolate(codedioma$code <- append(codedioma$code,  paste0(sub, gen.code)))
                          isolate(modelo <- traineR::train.gbm(as.formula(var), data = train, distribution = distribution, n.trees = n.trees, shrinkage = shrinkage))
                          isolate(modelo)
                        },
                        nn = {
                          threshold  <- isolate(input$threshold.nn.pred)
                          stepmax    <- isolate(input$stepmax.nn.pred)
                          capas.np   <- c(isolate(input$nn_cap_1),isolate(input$nn_cap_2),
                                          isolate(input$nn_cap_3),isolate(input$nn_cap_4),
                                          isolate(input$nn_cap_5),isolate(input$nn_cap_6),
                                          isolate(input$nn_cap_7),isolate(input$nn_cap_8),
                                          isolate(input$nn_cap_9),isolate(input$nn_cap_10))
                          cant.capas <- isolate(input$cant.capas.nn.pred)
                          capas.np   <<- as.vector(as.numeric(capas.np[1:cant.capas]))
                          gen.code <- codeNn(variable, capas.np, threshold, stepmax)
                          isolate(codedioma$code <- append(codedioma$code,  paste0(sub, gen.code)))
                          
                          # --- NUEVO BLOQUE: guardar estructura del model.matrix ---
                          # Expandir los factores del entrenamiento
                          mm_train <- model.matrix(as.formula(var), data = train)
                          mm_train <- as.data.frame(mm_train)
                          newCases$model.matrix.template <- colnames(mm_train)  # guardar nombres de columnas
                          
                          isolate(modelo <- traineR::train.neuralnet(
                            formula   = as.formula(var),
                            data      = train,
                            threshold = threshold,
                            stepmax   = stepmax,
                            hidden    = capas.np
                          ))
                          isolate(modelo)
                        },
                        
                        rl   = {
                          gen.code       <- codeRl(variable)
                          isolate(modelo <- lm(as.formula(var), 
                                               data   = train))
                          isolate(codedioma$code <- append(codedioma$code,  paste0(sub, gen.code)))
                          isolate(modelo)
                        },
                        
                        rlr = {
                          scales <- isolate(input$switch.scale.rlr.pred)
                          alpha  <- isolate(input$alpha.rlr.pred)
                          isolate(modelo <- traineR::train.glmnet( as.formula(var), data = train, standardize = as.logical(scales), alpha = alpha, family = "gaussian"))
                        
                          gen.code <- codeRlr(variable, alpha, as.logical(scales))
                          isolate(codedioma$code <- append(codedioma$code, paste0(sub, gen.code)))
                          
                          isolate(modelo)
                        },
                        
                        rd = {
                          modo.rd  <- isolate(input$mode_rd)
                          scale    <- as.logical(isolate(input$switch.scale.rd.pred))
                          gen.code <- codeRd(variable, modo.rd, scale)
                          ncomp    <- isolate(input$ncomp_rd)
                          ncompdef <- as.logical(isolate(input$permitir_ncomp))
                          form     <- as.formula(var)
                          datos    <- train
                          
                          if (ncompdef) {
                            if (modo.rd == "ACP") {
                              modelo <- pcr(
                                form, ncomp = ncomp, data = datos,
                                scale = scale, validation = "CV"
                              )
                            } else {
                              modelo <- plsr(
                                form, ncomp = ncomp, data = datos,
                                scale = scale, validation = "CV"
                              )
                            }
                          } else {
                            if (modo.rd == "ACP") {
                              modelo <- pcr(
                                form, data = datos,
                                scale = scale, validation = "CV"
                              )
                            } else {
                              modelo <- plsr(
                                form, data = datos,
                                scale = scale, validation = "CV"
                              )
                            }
                          }
                          
                          optimal.ncomp <- which.min(RMSEP(modelo)$val[, 1, ]) - 1
                          isolate(modelo$optimal.ncomp <- optimal.ncomp)
                          isolate(codedioma$code <- append(codedioma$code, paste0(sub, gen.code)))
                          isolate(modelo)
                        }
      )
      newCases$variable.predecir <- variable
      newCases$m.seleccionado    <- m.seleccionado
      newCases$modelo      <- codigo
      tabla.prueba()
      print(codigo)
      
    }, error = function(e) {
      if(cont !=1)
        showNotification(paste0("ERROR al generar el modelo: ", e), type = "error")
      cont <- cont + 1
      return(invisible(""))
    },
    warning = function(w){
      if(m.seleccionado == "nn"){
        showNotification(paste0(tr("nnWar", codedioma$idioma)," (NN-01) : ",w), duration = 20, type = "warning")
        return(invisible(""))
        
      }     
    })
  })
  # Load Button Function (New Cases)
  observeEvent(input$archivoNPred2, {
    
    rowname    <- isolate(input$jsrowname)
    ruta       <- isolate(input$archivoNPred2)
    sep        <- isolate(input$jssep)
    dec        <- isolate(input$jsdec)
    encabezado <- isolate(input$jsheader)
    deleteNA   <- isolate(input$jsnas)
    variable   <- newCases$variable.predecir
    originales  <- newCases$originales
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    
    if(!is.null(variable)){
      tryCatch({
        codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
        codigo <- paste0(codigo, "datos.prueba.completos <<- datos\n")
        
        isolate(codedioma$code <- append(codedioma$code, codigo))
        
        test                  <- carga.datos.np(rowname, 
                                                ruta$datapath, 
                                                sep, 
                                                dec, 
                                                encabezado)
        
        #Verifica que los datos contengan las mismas columnas
        if(any(!(c(colnames(test),variable) %in% colnames(originales))))
          stop(tr("NoTamColum", codedioma$idioma))
        
        test[,variable]       <- NULL
        test                  <- accion.NAs(test, deleteNA)
        test[,variable]       <- NA
        newCases$datos.prueba <- test
        newCases$datos.prueba[,variable] <- NA
        
        validar()
        # unificar.factores()
        # 
        if(ncol(test) <= 1) {
          showNotification(
            "ERROR: Check Separators", duration = 10, type = "error")
          newCases$datos.prueba      <- NULL
          newCases$prediccion        <- NULL
          
        } 
        tabla.prueba()
      }, error = function(e) {
        newCases$datos.prueba      <- NULL
        newCases$prediccion        <- NULL
        showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
      })
    }
    else {
      newCases$datos.prueba      <- NULL
      newCases$prediccion        <- NULL
      
    }    
  })
  
  # Load Button Function (New Cases)
  observeEvent(input$archivoNPred3, {
    
    ruta        <-  isolate(input$archivoNPred3)
    variable    <-  newCases$variable.predecir
    encabezado  <- isolate(input$jsheader_xlsx)
    rowname     <- isolate(input$jsrowname_xlsx)
    num_hoja    <- as.numeric(isolate(input$jsnum_hoja))
    fila_inicio <- as.numeric(isolate(input$jsfila_inicio))
    col_inicio  <- as.numeric(isolate(input$jscol_inicio))
    fila_final  <- as.numeric(isolate(input$jsfila_final))
    col_final   <- as.numeric(isolate(input$jscol_final))
    deleteNA    <- as.logical(isolate(input$jsdeleteNA_xlsx))
    originales  <- newCases$originales
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    
    if(!is.null(variable)){
      tryCatch({
        #codigo <- loadeR:::code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
        #codigo <- paste0(codigo, "datos.prueba.completos <<- datos\n")
        
        #isolate(codedioma$code <- append(codedioma$code, codigo))
        
        test                  <- carga.datos.excel(
          ruta$datapath, num_hoja, encabezado, fila_inicio, col_inicio, 
          fila_final, col_final, rowname, deleteNA)
        #Verifica que los datos contengan las mismas columnas
        if(any(!(c(colnames(test),variable) %in% colnames(originales))))
          stop(tr("NoTamColum", codedioma$idioma))
        
        test[,variable]       <- NULL
        test                  <- accion.NAs(test, deleteNA)
        test[,variable]       <- NA
        newCases$datos.prueba <- test
        newCases$datos.prueba[,variable] <- NA
        
        validar()
        # unificar.factores()
        # 
        if(ncol(test) <= 1) {
          showNotification(
            "ERROR: Check Separators", duration = 10, type = "error")
          newCases$datos.prueba      <- NULL
          newCases$prediccion        <- NULL
          
        } 
        tabla.prueba()
      }, error = function(e) {
        newCases$datos.prueba      <- NULL
        newCases$prediccion        <- NULL
        showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
      })
    }
    else {
      newCases$datos.prueba      <- NULL
      newCases$prediccion        <- NULL
      
    }    
  })
  
  #Tabla de datos de prueba
  tabla.prueba <- function(){
    output$contentsPred3 <- DT::renderDataTable({
    datos  <- newCases$datos.prueba
    tipos  <- c(
      tr("numerico",   isolate(codedioma$idioma)),
      tr("categorico", isolate(codedioma$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas   <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'><i class='fa fa-sort-numeric-up wrapper-tag'></i><br>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'><i class='fa fa-font wrapper-tag'></i><br>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        extensions = 'Buttons',
        options = list(dom = 'Bfrtip', buttons = list(list(extend = "csv",   text = '<i class="fa fa-file-csv"></i>', filename = "dataTest",
                                                           exportOptions = list(modifier = list(page = "all"))), 
                                                      list(extend = "excel", text = '<i class="fa fa-file-excel"></i>', filename = "dataTest",
                                                           exportOptions = list(modifier = list(page = "all")))))
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = F)
  }
  
  # Genera la tabla con los valores predichos por el modelo seleccionado.
  # Nota: Esta funcion fue modificada para resolver incompatibilidades en modelos especificos
  # ("boost", "rlr", "rd"), que retornaban sus predicciones en formatos distintos o no compatibles
  # con el flujo general de la aplicacion. Ahora, la salida de estos modelos se procesa y transforma
  # adecuadamente para garantizar una visualizacion uniforme y sin errores.
  prediccion <- function(){
    output$PrediTablePN <- DT::renderDataTable({
      input$predecirPromidat
      test <- newCases$datos.prueba
      train<- newCases$datos.aprendizaje
      model<- newCases$modelo
      sel  <- newCases$m.seleccionado
      vari <- newCases$variable.predecir
      newCases$prediccion <- NULL
      tipos  <- c(
        tr("numerico",   isolate(codedioma$idioma)),
        tr("categorico", isolate(codedioma$idioma))
      )
      tryCatch({
        if(sel == "svm") {
          pred <- predict(model, test[, -which(colnames(test) == vari)])
        } else if(sel == "rlr") {
          nd <- as.data.frame(test)
          
          s_val <- NULL
          if(!is.null(model$lambda.min) && is.numeric(model$lambda.min)) {
            s_val <- model$lambda.min
          } else if(!is.null(model$lambda.1se) && is.numeric(model$lambda.1se)) {
            s_val <- model$lambda.1se
          } else if(!is.null(model$bestTune) && !is.null(model$bestTune$lambda)) {
            s_val <- as.numeric(model$bestTune$lambda)
          } else if(!is.null(model$lambda) && is.numeric(model$lambda)) {
            if(!is.null(model$cvm) && length(model$cvm)==length(model$lambda)) {
              s_val <- model$lambda[ which.min(model$cvm) ]
            } else {
              s_val <- model$lambda[1]
            }
          }
          
          pred_try <- try({
            if(!is.null(s_val) && is.finite(s_val)) {
              predict(model, newdata = nd, s = s_val)
            } else {
              predict(model, newdata = nd)
            }
          }, silent = TRUE)
          
          if(inherits(pred_try, "try-error")) {
            pred_try <- predict(model, newdata = nd)
          }
          
          # Convertir a vector numérico
          if(is.list(pred_try) && !is.null(pred_try$prediction)) {
            pred <- as.numeric(pred_try$prediction)
          } else if(is.matrix(pred_try)) {
            pred <- as.numeric(pred_try[, 1])
          } else {
            pred <- as.numeric(pred_try)
          }
        } else if(sel == "rd") {
          ncomp <- model$optimal.ncomp
          auxp  <- predict(model, test, ncomp = ncomp)
          pred  <- list(prediction = auxp[, , 1], var.pred = vari)
        } else if(sel == "boost") {
          ntrees <- isolate(input$iter.boosting.pred)
          pred <- predict(model, test, n.trees = ntrees)
        } else {
          pred <- predict(model, test)
        }
        
        datos <- test
        
        if(sel %in% c("rlr", "rl", "rd", "boost")) {
          if(is.list(pred)) {
            datos[, vari] <- as.numeric(pred$prediction)
          } else if(is.matrix(pred)) {
            datos[, vari] <- as.numeric(pred[, 1])
          } else {
            datos[, vari] <- as.numeric(pred)
          }
        } else {
          datos[, vari] <- pred$prediction
        }
        
        newCases$prediccion <- pred
        
        nombre.columnas <- c("ID", colnames(datos))
        isolate(codedioma$code <- append(codedioma$code, "predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos)"))
        
        tipo.columnas <- sapply(colnames(datos), function(i)
          ifelse(class(datos[,i]) %in% c("numeric", "integer"),
                 paste0("<span data-id='numerico'><i class='fa fa-sort-numeric-up wrapper-tag'></i><br>", tipos[1], "</span>"),
                 paste0("<span data-id='categorico'><i class='fa fa-font wrapper-tag'></i><br>", tipos[2], "</span>")))
        
        sketch = htmltools::withTags(table(
          tableHeader(nombre.columnas),
          tags$tfoot(
            tags$tr(tags$th(), lapply(tipo.columnas, function(i)
              tags$th(shiny::HTML(i))))
          )
        ))
        DT::datatable(
          datos, selection = 'none', editable = TRUE,
          container = sketch, extensions = 'Buttons',
          options = list(dom = 'Bfrtip',
                         buttons = list(list(extend   = "csv",
                                             text     = '<i class="fa fa-file-csv"></i>',
                                             filename = "dataPred",
                                             exportOptions = list(modifier = list(page = "all"))),
                                        list(extend   = "excel",
                                             text     = '<i class="fa fa-file-excel"></i>',
                                             filename = "dataPred",
                                             exportOptions = list(modifier = list(page = "all")))
                         )))
      }, error = function(e) {
        showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
        return(NULL)
      })
    }, server = F)
  }
  
  
  #Agrega la prediccion a los datos
  crear.datos.np <- function(){
    datos.aux.prueba <- newCases$datos.prueba
    datos.aux.prueba[,newCases$variable.predecir]   <- newCases$prediccion$prediction
    
    return(datos.aux.prueba)
  }
  
  observeEvent(newCases$modelo, {
    if(!is.null(newCases$modelo)){
      shinyjs::show("modelnext", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide("modelnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
  observeEvent(input$cargarnext, {
    shinyjs::hide("primera", anim = TRUE )
    shinyjs::show("tercera", anim = TRUE)
    shinyjs::show("modelback", anim = TRUE)
  })
  
  observeEvent(input$modelback, {
    shinyjs::show("primera", anim = TRUE)
    shinyjs::hide("tercera", anim = TRUE)
  })
  observeEvent(input$modelnext, {
    shinyjs::show("cuarta", anim = TRUE)
    shinyjs::hide("tercera", anim = TRUE)
  })
  
  
  observeEvent(input$nuevosback, {
    shinyjs::hide("cuarta",  anim = TRUE)
    shinyjs::show("tercera", anim = TRUE)
  })
  
  observeEvent(input$nuevosnext, {
    prediccion()
    shinyjs::hide("cuarta", anim = TRUE)
    shinyjs::show("quinta", anim = TRUE)
  })  
  
  observeEvent(input$predicback, {
    shinyjs::show("cuarta", anim = TRUE)
    shinyjs::hide("quinta", anim = TRUE)
  })
  
  #Actualiza la cantidad de capas ocultas (neuralnet)
  observeEvent(input$cant.capas.nn.pred, {
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn_cap_", i))
        } else {
          shinyjs::hide(paste0("nn_cap_", i))
        }
      }
    }
  })
  
  
  # Habilitada o deshabilitada el numero de componenetes 
  observeEvent(input$permitir_ncomp, {
    if (as.logical(input$permitir_ncomp)) {
      shinyjs::enable("ncomp_rd")
    } else {
      shinyjs::disable("ncomp_rd")
    }
  })
  
  # Update Models Options
  output$opcModelsPredN = renderUI({
    datos   <- newCases$datos.aprendizaje
    idioma  <- codedioma$idioma
    modelo  <- input$selectModelsPred 
    
    opc_knn <- list(fluidRow(col_3(numericInput(ns("kmax.knn.pred"), tr("kmax", idioma), min = 1,step = 1, value = 7)),
                             col_3(selectInput(inputId = ns("kernel.knn.pred"), label = tr("selkernel", idioma),selected = 1,
                                               choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                           "triweight", "cos","inv","gaussian"))),
                             col_3(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no"),idioma = idioma )),
                             col_3(numericInput(ns("distance_knn"), tr("distknn", idioma), min = 1,step = 1, value = 2 ))))
    
    opc_svm <- list(fluidRow(col_6(
      radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"),idioma = idioma )),
      col_6(selectInput(inputId = ns("kernel.svm.pred"), label = tr("selkernel", idioma),selected = "radial",
                        choices = c("linear", "polynomial", "radial", "sigmoid")))))
    
    opc_rf  <- list(fluidRow(col_6(numericInput(ns("ntree.rf.pred"), tr("numTree", idioma), 20, width = "100%", min = 0)),
                             col_6(numericInput(ns("mtry.rf.pred"),  tr("numVars", idioma),1, width = "100%", min = 1))))
    
    opc_dt  <- list(fluidRow(col_6(numericInput(ns("minsplit.dt.pred"), tr("minsplit", idioma), 20, width = "100%",min = 1)),
                             col_6(numericInput(ns("maxdepth.dt.pred"), tr("maxdepth", idioma), 15, width = "100%",min = 0, max = 30, step = 1))))

    opc_potenciacion <- list(fluidRow(col_4(numericInput(ns("iter.boosting.pred"), tr("numTree", idioma), 20, width = "100%",min = 1)),
                                      col_4(numericInput(ns("shrinkage_boosting"),tr("shrinkage", idioma), 0.1, width = "75%",min = 0.0001)),
                                      col_4(selectInput(inputId = ns("tipo_boosting"), label = tr("selectAlg", idioma),selected = 1, width = "75%",
                                                         choices =  c("gaussian", "laplace", "tdist")))))
    opc_rl  <- list(tags$span())
    
    opc_rlr <- list(fluidRow(col_6(selectInput(inputId = ns("alpha.rlr.pred"), label = tr("selectAlg", idioma),selected = 1,
                                               choices = list("Ridge" = 0, "Lasso" = 1))),
                             col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"),idioma = idioma )))
    )
    
    opc_nn <- list(fluidRow(col_4(numericInput(ns("threshold.nn.pred"),tr("threshold", idioma),
                                               min = 0,   step = 0.01, value = 0.05)),
                            col_4(numericInput(ns("stepmax.nn.pred"),tr("stepmax", idioma),
                                               min = 100, step = 100,  value = 5000)),
                            col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                              label = tr("selectCapas", idioma), value = 10))),
                   fluidRow(lapply(1:10, function(i) tags$span(numericInput(ns(paste0("nn_cap_",i)), NULL,
                                                                            min = 1, step = 1, value = 2),
                                                               class = "mini-numeric-select"))))
    opc_rd <-  list(fluidRow(col_3(selectInput(inputId = ns("mode_rd"), label = tr("selectAlg", idioma), selected = 0,
                                               choices = list("ACP" = 0, "MCP" = 1))),
                             col_3(radioSwitchNP(ns("switch.scale.rd.pred"), "escal", c("si", "no"),idioma = idioma )),
                             col_3(id = ns("colManualCom"), numericInput(ns("ncomp_rd"), tr("ncomp", idioma), value = 2, min = 0, "NULL", width = "100%")),
                             col_3(radioSwitchNP(id = ns("permitir_ncomp"), label = "",
                                               names = c("manual", "automatico"), val.def = FALSE))))
    
    
    res <-  switch(modelo,
                   knn   =  opc_knn,
                   svm   =  opc_svm,
                   rf    =  opc_rf,
                   nn    =  opc_nn,
                   boost =  opc_potenciacion,
                   rl    =  opc_rl,
                   rd    =  opc_rd,
                   rlr   =  opc_rlr,
                   dt    =  opc_dt)
    
    if(!is.null(newCases$datos.aprendizaje)){
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.numericas(newCases$datos.aprendizaje))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(newCases$datos.aprendizaje))))
      updateNumericInput(session, "mtry.rf.pred",  value = round(sqrt(ncol(newCases$datos.aprendizaje) -1)))
      
    }
    
    res <-  do.call(tagList, res)
    
    return(res)
  })
  
  
  #Valida que los datos contengan la misma cantidad de columnas 
  #Valida que los datos contengan la misma cantidad de columnas 
  validar <- function() {
    originales <- newCases$originales
    datos.aprendizaje <- newCases$datos.aprendizaje
    datos.prueba <- newCases$datos.prueba
    
    tryCatch({
      # --- Paso 1: Sincronizar columnas ---
      faltantes <- setdiff(colnames(datos.aprendizaje), colnames(datos.prueba))
      for (col in faltantes) {
        if (is.numeric(datos.aprendizaje[, col])) {
          datos.prueba[, col] <- 0
        } else {
          datos.prueba[, col] <- NA
        }
      }
      
      extra <- setdiff(colnames(datos.prueba), colnames(datos.aprendizaje))
      if (length(extra) > 0) {
        datos.prueba <- datos.prueba[, !(colnames(datos.prueba) %in% extra)]
      }
      
      datos.prueba <- datos.prueba[, colnames(datos.aprendizaje), drop = FALSE]
      
      # --- Paso 2: Igualar tipos y niveles de factores ---
      for (col in colnames(datos.aprendizaje)) {
        tipo_train <- class(datos.aprendizaje[, col])
        tipo_test  <- class(datos.prueba[, col])
        
        if (tipo_train != tipo_test) {
          if (tipo_train %in% c("numeric", "integer")) {
            datos.prueba[, col] <- as.numeric(datos.prueba[, col])
          } else if (tipo_train == "factor") {
            datos.prueba[, col] <- as.factor(datos.prueba[, col])
          }
        }
        
        if (is.factor(datos.aprendizaje[, col])) {
          niveles_train <- levels(datos.aprendizaje[, col])
          datos.prueba[, col] <- factor(datos.prueba[, col], levels = niveles_train)
        }
      }
      
      # --- Paso 3: Aplicar estructura del model.matrix (solo para redes neuronales) ---
      if (identical(newCases$m.seleccionado, "nn") && !is.null(newCases$model.matrix.template)) {
        var <- newCases$variable.predecir
        form <- as.formula(paste0(var, "~."))
        
        # Excluir variable respuesta antes del model.matrix
        if (var %in% colnames(datos.prueba)) {
          datos.prueba.sin.y <- datos.prueba[, setdiff(colnames(datos.prueba), var), drop = FALSE]
        } else {
          datos.prueba.sin.y <- datos.prueba
        }
        
        # Evitar eliminación de filas con NA
        mm_test <- model.matrix(~ ., data = datos.prueba.sin.y, na.action = na.pass)
        mm_test <- as.data.frame(mm_test)
        
        
        # Agregar columnas faltantes
        faltantes <- setdiff(newCases$model.matrix.template, colnames(mm_test))
        for (col in faltantes) mm_test[, col] <- 0
        
        # Eliminar columnas extra y reordenar
        extra <- setdiff(colnames(mm_test), newCases$model.matrix.template)
        if (length(extra) > 0) mm_test <- mm_test[, !(colnames(mm_test) %in% extra)]
        
        mm_test <- mm_test[, newCases$model.matrix.template, drop = FALSE]
        
        datos.prueba <- mm_test
      }
      
      # --- Paso 4: Guardar resultado final ---
      newCases$datos.prueba <- datos.prueba
    },
    error = function(e) {
      message("Error al validar datos: ", e$message)
    })
  }
  
  
}

## To be copied in the UI
# mod_ind_nuevos_ui("ind_nuevos_ui_1")

## To be copied in the server
# callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1")

