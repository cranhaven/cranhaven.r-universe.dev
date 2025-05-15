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
          col_1(
            actionButton(ns("cargarnext"), label = NULL, width = "100%",
                         icon = icon("forward"))
          )
        )
      ),
    div(id = ns("segundo"),
        style = "display:none",
        div(
          col_1(actionButton (ns("transback"), label = NULL, width = "100%",
                              icon = icon("backward"))),
          col_10(
            tabBoxPrmdt(
            id = "BoxModeloe",
            tabPanel(
              title = p(labelInput("trans"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
              collapsible = FALSE, collapsed = FALSE, value = "Trasformar",
              div(
                col_5(
                      uiOutput(ns('transDataPredN')), hr(),
                      actionButton(ns('transButton'), labelInput("aplicar"), width = "100%"), hr()
                ),
                col_7(br(),muestra.datos.pred2)), hr())
            )),
          col_1(actionButton(ns("transnext"), label = NULL, width = "100%",
                             icon = icon("forward")))
        )
    ),
    div(id = ns("tercera"),
        style = "display:none",
        div(col_1(actionButton(ns("modelback"), label = NULL, width = "100%",
                                          icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModeloa",
                     tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag") ,solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                                fluidRow(
                                  col_4(selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%")),
                                  col_4(selectInput(inputId = ns("selectModelsPred"), label = labelInput("selectMod"),
                                                    choices =  list("knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr"), width = "100%")),
                                  col_2(numericInput(ns("sel.model.corte"), label = labelInput("probC"), 0.5, 0, 1, 0.1, width = "100%")),
                                  col_2(selectInput(ns("sel.corte.cat"), choices = "", label = labelInput("selectCat"), width = "100%"))
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
                       title = p(labelInput("cargarNuev"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
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
#'
#' @noRd 
mod_ind_nuevos_server <- function(input, output, session, newCases, updateData2, codedioma){
  ns <- session$ns
  
  shinyjs::runjs('get_file()')
  observeEvent(codedioma$idioma, {
    
    nombres <- list( "knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr")
    names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb", "nn", "rl", "rlr"),codedioma$idioma)
    
    updateSelectInput(session, "selectModelsPred", choices = nombres, selected = input$selectModelsPred)
  })

  
  #Valida que los datos contengan la misma cantidad de columnas 
  validar <- function() {
    cod        <- ""
    originales        <-  newCases$originales
    datos.aprendizaje <-  newCases$datos.aprendizaje
    datos.prueba      <-  newCases$datos.prueba

    tryCatch(
      for (var in colnames(originales)) {
        if(var %in% colnames(datos.aprendizaje)) {
          if(class(datos.prueba[, var]) %not_in% c("numeric", "integer") & 
             class(datos.aprendizaje[, var]) %in% c("numeric", "integer")) {
            datos.prueba[, var]       <- as.numeric(datos.prueba[, var])
          }
          if(class(datos.prueba[, var]) %in% c("numeric", "integer") & 
             class(datos.aprendizaje[, var]) %not_in% c("numeric", "integer")) {
            datos.prueba[, var]       <- as.factor(datos.prueba[, var])
          }
        }
        else{
          if(paste0(var, ".", unique(originales[, var])[1]) %in% colnames(datos.aprendizaje)){
            datos.prueba <- loadeR::datos.disyuntivos(datos.prueba, var)
            datos.prueba[, var]       <- NULL
          }else{
            datos.prueba[, var]       <- NULL
          }
        }
      }
    )
    
    newCases$datos.prueba <- datos.prueba
  }

  
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
        codigo <- loadeR:::code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
        codigo <- paste0(codigo, "datos.prueba.completos <- datos\n")
        
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
        unificar.factores()
        # 
        if(ncol(test) <= 1) {
          showNotification(
            "ERROR: Check Separators", duration = 10, type = "error")
          newCases$datos.prueba      <- NULL
          newCases$prediccion        <- NULL
          
        } 
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
        #codigo <- paste0(codigo, "datos.prueba.completos <- datos\n")
        
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
        unificar.factores()
        # 
        if(ncol(test) <= 1) {
          showNotification(
            "ERROR: Check Separators", duration = 10, type = "error")
          newCases$datos.prueba      <- NULL
          newCases$prediccion        <- NULL
          
        } 
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
  
  
  #Actualiza la cantidad de capas ocultas (neuralnet)
  observeEvent(input$cant.capas.nn.pred, {
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  })

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
    if(m.seleccionado == "rl")
      if(length(levels(train[,variable])) != 2)
        stop(tr("limitModel", codedioma$idioma), call. = FALSE)
    
    tryCatch({
      dat.a  <- "datos.aprendizaje.completos"
      var    <- paste0(variable, "~.")
      codigo <- switch (m.seleccionado ,
                        knn   = {
                          k.value<- isolate(input$kmax.knn.pred)
                          scales <- isolate(input$switch.scale.knn.pred)
                          kernel <- isolate(input$kernel.knn.pred)
                          isolate(modelo <- traineR::train.knn(as.formula(var), data = train, scale = as.logical(scales), kernel = kernel, kmax = k.value ))
                          cod <- code.kkn.modelo(variable.pr = variable,
                                               scale = scales,
                                               kmax = k.value,
                                               kernel = kernel,
                                               datos = dat.a)
                          isolate(codedioma$code <- append(codedioma$code, cod))
                          isolate(modelo)
                        },
                        dt    = {
                          tipo    <-isolate(input$split.dt.pred)
                          minsplit<-isolate(input$minsplit.dt.pred)
                          maxdepth<-isolate(input$maxdepth.dt.pred)
                          isolate(modelo  <- traineR::train.rpart(as.formula(var), data = train,
                                                         control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),parms = list(split = tipo)))
                          
                          isolate(codedioma$code <- append(codedioma$code, dt.modelo (variable.pr = variable,
                                                                                        minsplit = minsplit,
                                                                                        maxdepth = maxdepth,
                                                                                        split = tipo,
                                                                                        datos = dat.a)))
                          isolate(modelo)
                        },
                        rf    = {
                          mtry   <- isolate(input$mtry.rf.pred)
                          ntree  <- isolate(input$ntree.rf.pred)
                          isolate(modelo <- traineR::train.randomForest(as.formula(var), data = train, mtry = mtry, ntree = ntree, importance = TRUE))
                          
                          isolate(codedioma$code <- append(codedioma$code, rf.modelo (variable.pr = variable,
                                                                                        ntree = ntree,
                                                                                        mtry  = mtry,
                                                                                        datos = dat.a)))
                          isolate(modelo)
                        },
                        svm   = {
                          scales <- isolate(input$switch.scale.svm.pred)
                          k      <- isolate(input$kernel.svm.pred)
                          isolate(modelo <- traineR::train.svm(as.formula(var), data = train, scale = as.logical(scales), kernel = k))
                          isolate(codedioma$code <- append(codedioma$code, svm.modelo (variable.pr =variable,
                                                                                         scale  = scales,
                                                                                         kernel = k,
                                                                                         datos = dat.a)))
                          isolate(modelo)
                        },
                        bayes = {
                          isolate(modelo <- traineR::train.bayes(as.formula(var), data = train))
                          isolate(codedioma$code <- append(codedioma$code, codigo.modelo (variable.pr=variable, model.name = "bayes",
                                                                                          datos = dat.a )))
                          isolate(modelo)
                          
                        },
                        xgb   = {
                          tipo     <- isolate(input$boosterXgb.pred)
                          max.depth<- isolate(input$maxdepthXgb.pred)
                          n.rounds <- isolate(input$nroundsXgb.pred)
                          isolate(modelo   <- traineR::train.xgboost(as.formula(var), data = train, booster = tipo, 
                                                           max_depth = max.depth, nrounds = n.rounds))
                          isolate(codedioma$code <- append(codedioma$code, xgb.modelo (variable.pr=variable,
                                                                                         booster   = tipo,
                                                                                         max.depth = max.depth,
                                                                                         n.rounds  = n.rounds,
                                                                                         datos = dat.a)))
                          isolate(modelo)
                        },
                        rl    = {
                          isolate(modelo <- traineR::train.glm(as.formula(var), data = train))
                          isolate(codedioma$code <- append(codedioma$code, codigo.modelo (variable.pr=variable, model.name = "glm",
                                                                                          datos = dat.a )))
                          isolate(modelo)
                          
                          },
                        nn    = {
                          threshold  <- isolate(input$threshold.nn.pred)
                          stepmax    <- isolate(input$stepmax.nn.pred)
                          capas.np   <- c(isolate(input$nn.cap.pred.1),isolate(input$nn.cap.pred.2),
                                          isolate(input$nn.cap.pred.3),isolate(input$nn.cap.pred.4),
                                          isolate(input$nn.cap.pred.5),isolate(input$nn.cap.pred.6),
                                          isolate(input$nn.cap.pred.7),isolate(input$nn.cap.pred.8),
                                          isolate(input$nn.cap.pred.9),isolate(input$nn.cap.pred.10))
                          cant.capas <- isolate(input$cant.capas.nn.pred)
                          capas.np   <- as.vector(as.numeric(capas.np[1:cant.capas]))
                          
                          isolate(modelo     <- traineR::train.neuralnet(
                            formula   = as.formula(var),
                            data      = train,
                            threshold = threshold,
                            stepmax   = stepmax,
                            hidden    = capas.np))
                          isolate(codedioma$code <- append(codedioma$code, nn.modelo (variable.pr=variable,
                                                                                        threshold,
                                                                                        stepmax,
                                                                                        cant.capas,
                                                                                        isolate(input$nn.cap.pred.1),isolate(input$nn.cap.pred.2),
                                                                                        isolate(input$nn.cap.pred.3),isolate(input$nn.cap.pred.4),
                                                                                        isolate(input$nn.cap.pred.5),isolate(input$nn.cap.pred.6),
                                                                                        isolate(input$nn.cap.pred.7),isolate(input$nn.cap.pred.8),
                                                                                        isolate(input$nn.cap.pred.9),isolate(input$nn.cap.pred.10),
                                                                                        datos = dat.a)))
                          isolate(modelo)
                        },
                        rlr    = {
                          scales <- isolate(input$switch.scale.rlr.pred)
                          alpha  <- isolate(input$alpha.rlr.pred)
                          isolate(modelo <- traineR::train.glmnet(as.formula(var), data = train, standardize = as.logical(scales), alpha = alpha, family = 'multinomial' ))
                          isolate(codedioma$code <- append(codedioma$code, rlr.modelo (variable.pr = variable,
                                                                                         alpha,
                                                                                         scales,
                                                                                         datos = dat.a)))
                          isolate(modelo)
                        },
                        ada    = {
                          iter     <- isolate(input$iter.boosting.pred)
                          maxdepth <- isolate(input$maxdepth.boosting.pred)
                          minsplit <- isolate(input$minsplit.boosting.pred)
                          coeff    <- isolate(input$coeff.boosting.pred)
                          isolate(modelo <- traineR::train.adabag(as.formula(var), data = train, mfinal = iter, coeflearn = coeff,
                                                          control = rpart.control(minsplit =minsplit, maxdepth = maxdepth)))
                          isolate(codedioma$code <- append(codedioma$code, boosting.modelo(
                            variable.pr = variable, iter = iter, maxdepth = maxdepth,
                            minsplit = minsplit, coeflearn = coeff, datos = dat.a)))
                          isolate(modelo)
                        }
      )
      newCases$variable.predecir <- variable
      newCases$m.seleccionado    <- m.seleccionado
      newCases$modelo      <- codigo
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
      if(m.seleccionado == "rl"){
        isolate(modelo <- traineR::train.glm(as.formula(var), data = train))
        isolate(codedioma$code <- append(codedioma$code, codigo.modelo (variable.pr=variable, model.name = "glm",
                                                                        datos = dat.a )))
        newCases$modelo      <- modelo
        print(modelo)
      }
    })
  })
  
  
  #Genera la tabla de predicciones
  prediccion <- function(){
  output$PrediTablePN <- DT::renderDataTable({
    input$predecirPromidat
    test <- newCases$datos.prueba
    train<- newCases$datos.aprendizaje
    model<- newCases$modelo
    sel  <- newCases$m.seleccionado
    vari <- newCases$variable.predecir
    newCases$prediccion        <- NULL
    choices <- as.character(unique(train[, vari]))
    cat_sel <- input$sel.corte.cat
    Corte <- input$sel.model.corte
    tipos  <- c(
        tr("numerico",   isolate(codedioma$idioma)),
        tr("categorico", isolate(codedioma$idioma))
    )
      tryCatch({
        if(length(choices) == 2) {
          if(sel == "svm") {
            pred <- predict(model, test[,-which(colnames(test) == vari)], type = 'prob')    
          } else {
            pred <- predict(model, test, type = 'prob')
          }
          positive <- cat_sel
          negative <- choices[choices != cat_sel][1]
          Score <- pred$prediction[, positive]
          pred  <- ifelse(Score > Corte, positive, negative)
          
        } else{
          if(sel == "svm") {
            pred <- predict(model, test[,-which(colnames(test) == vari)], type = 'class')
            pred <- pred$prediction
          } else {
            pred <- predict(model, test, type = 'class')
            pred <- pred$prediction
          }
        }
        
        datos               <- test
        datos[,vari]        <- pred
        newCases$prediccion <- pred
        nombre.columnas <- c("ID", colnames(datos))
        isolate(codedioma$code <- append(codedioma$code, "predic.nuevos <- predict(modelo.nuevos, datos.prueba.completos, type = 'class')"))
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
    }, server = F)}



  #Agrega la predicción a los datos
  crear.datos.np <- function(){
    datos.aux.prueba <- newCases$datos.prueba
    datos.aux.prueba[,newCases$variable.predecir]   <- newCases$prediccion
    
    return(datos.aux.prueba)
  }
  
  #Unifica las variables de tipo factor en training-testing
  unificar.factores <- function(){
    prueba      <- newCases$datos.prueba
    aprendizaje <- newCases$datos.aprendizaje
    for(nombre in colnames(prueba)){
      if(class(prueba[,nombre])  %in% c("factor")){
        levels(prueba[,nombre]) <- unique(c(levels(prueba[,nombre]),
                                                             levels(aprendizaje[,nombre])))
      }
    }
    newCases$datos.prueba <- prueba
  }
  
  # Habilita o deshabilita la semilla RLR
  observeEvent(input$permitir.lambda.pred, {
    if (input$permitir.lambda.pred) {
      shinyjs::enable("lambda.pred")
    } else {
      shinyjs::disable("lambda.pred")
    }
  })
  
  # Wizard Opts Ind.Nuevos--------------------------------------------------------------------------------------------------
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
      cod <-  "datos.aprendizaje.completos <- datos\n"
      isolate(codedioma$code <- append(codedioma$code, cod))

      newCases$originales <- updateData2$originales
      newCases$datos.aprendizaje <- updateData2$datos
      
      shinyjs::show("cargarnext", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide("cargarnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
  
  observeEvent(newCases$datos.prueba, {
    if(!is.null(newCases$datos.prueba)){
      shinyjs::show("nuevosnext", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide("nuevosnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
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
  })
  
  observeEvent(input$transback, {
    shinyjs::show("primera", anim = TRUE)
    shinyjs::hide("segundo", anim = TRUE)
  })
  
  observeEvent(input$transnext, {
    shinyjs::show("tercera", anim = TRUE)
    shinyjs::hide("segundo", anim = TRUE)
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
  
  
  
  # Update Models Options
  output$opcModelsPredN = renderUI({
    datos   <- newCases$datos.aprendizaje
    idioma  <- codedioma$idioma
    modelo  <- input$selectModelsPred 
    
    opc_knn <- list(fluidRow(col_4(numericInput(ns("kmax.knn.pred"), tr("kmax", idioma), min = 1,step = 1, value = 7)),
                             col_4(selectInput(inputId = ns("kernel.knn.pred"), label = tr("selkernel", idioma),selected = 1,
                                           choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                       "triweight", "cos","inv","gaussian"))),
                             col_4(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no"),idioma = idioma ))))
    
    opc_svm <- list(fluidRow(col_6(
                                  radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"),idioma = idioma )),
                             col_6(selectInput(inputId = ns("kernel.svm.pred"), label = tr("selkernel", idioma),selected = "radial",
                                          choices = c("linear", "polynomial", "radial", "sigmoid")))))
    
    opc_rf  <- list(fluidRow(col_6(numericInput(ns("ntree.rf.pred"), tr("numTree", idioma), 20, width = "100%", min = 0)),
                             col_6(numericInput(ns("mtry.rf.pred"),  tr("numVars", idioma),1, width = "100%", min = 1))))
    
    opc_dt  <- list(fluidRow(col_4(numericInput(ns("minsplit.dt.pred"), tr("minsplit", idioma), 20, width = "100%",min = 1)),
                             col_4(numericInput(ns("maxdepth.dt.pred"), tr("maxdepth", idioma), 15, width = "100%",min = 0, max = 30, step = 1)),
                             col_4(selectInput(inputId = ns("split.dt.pred"), label = tr("splitIndex", idioma),selected = 1,
                                         choices =  list("gini" = "gini", "Entropia" = "information")))))
    opc_bayes <- list(tags$span())
    
    opc_potenciacion <- list(fluidRow(col_6(numericInput(ns("iter.boosting.pred"), tr("numTree", idioma), 20, width = "100%",min = 1)),
                                      col_6(numericInput(ns("maxdepth.boosting.pred"),tr("maxdepth", idioma), 15, width = "100%",min = 1)),
                                      col_6(numericInput(ns("minsplit.boosting.pred"),tr("minsplit", idioma), 20, width = "100%",min = 1)),
                                      col_6(selectInput(inputId = ns("coeff.boosting.pred"), label = tr("selkernel", idioma), selected = 1,
                                                        choices = c("Breiman", "Freund", "Zhu")))))
    opc_rl  <- list(tags$span())
   
    opc_rlr <- list(fluidRow(col_6(selectInput(inputId = ns("alpha.rlr.pred"), label = tr("selectAlg", idioma),selected = 1,
                                  choices = list("Ridge" = 0, "Lasso" = 1))),
                             col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"),idioma = idioma )))
                   )
  
    opc_xgb <- list(fluidRow(col_4(selectInput(inputId = ns("boosterXgb.pred"), label = tr("selbooster", idioma), selected = 1,
                                               choices = c("gbtree", "gblinear", "dart"))),
                             col_4(numericInput(ns("maxdepthXgb.pred"), tr("maxdepth", idioma),  min = 1,  step = 1, value = 6)),
                             col_4(numericInput(ns("nroundsXgb.pred"),  tr("selnrounds", idioma), min = 0, step = 1, value = 50))))
  
    opc_nn <- list(fluidRow(col_4(numericInput(ns("threshold.nn.pred"),tr("threshold", idioma),
                                               min = 0,   step = 0.01, value = 0.05)),
                            col_4(numericInput(ns("stepmax.nn.pred"),tr("stepmax", idioma),
                                               min = 100, step = 100,  value = 5000)),
                            col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                              label = tr("selectCapas", idioma), value = 10))),
                   fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(
                            col_2(numericInput(ns(paste0("nn.cap.pred.",i)), NULL, min = 1, step = 1, value = 2),
                                               class = "mini-numeric-select")))))

    res <-  switch(modelo,
                   knn   =  opc_knn,
                   svm   =  opc_svm,
                   rf    =  opc_rf,
                   bayes =  opc_bayes,
                   nn    =  opc_nn,
                   ada   =  opc_potenciacion,
                   xgb   =  opc_xgb,
                   rl    =  opc_rl,
                   rlr   =  opc_rlr,
                   dt    =  opc_dt)
    
    if(!is.null(newCases$datos.aprendizaje)){
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.categoricas(newCases$datos.aprendizaje))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(newCases$datos.aprendizaje))))
      updateNumericInput(session, "mtry.rf.pred",  value = round(sqrt(ncol(newCases$datos.aprendizaje) -1)))
    }
  
    res <-  do.call(tagList, res)

    return(res)
  })
  
  observeEvent(input$sel.predic.var.nuevos, {
    datos <- newCases$datos.aprendizaje
    n <- levels(datos[[input$sel.predic.var.nuevos]])
    
    if(length(n) == 2) {
      shinyjs::show("sel.model.corte", anim = TRUE, animType = "fade")
      shinyjs::show("sel.corte.cat", anim = TRUE, animType = "fade")
      updateSelectInput(session, "sel.corte.cat", choices = n)
    }else{
      shinyjs::hide("sel.model.corte", anim = TRUE, animType = "fade")
      shinyjs::hide("sel.corte.cat", anim = TRUE, animType = "fade")
      updateSelectInput(session, "sel.corte.cat", choices = "")
    }
  })
}
    
## To be copied in the UI
# mod_ind_nuevos_ui("ind_nuevos_ui_1")
    
## To be copied in the server
# callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1", updateData)
 
