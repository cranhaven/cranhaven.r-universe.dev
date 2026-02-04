#' The application server-side
#'
#' @description A shiny Module.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'
#' @noRd 
#'
#' @import shiny
#' @keywords internal
app_server <- function(input, output, session) {
  
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'), emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  exe(paste0("library(traineR)"))
  ##################################  Variables  ##############################
  updateData <- rv(datos              = NULL, 
                   originales         = NULL, 
                   datos.tabla        = NULL,
                   variable.predecir  = NULL,
                   indices            = NULL, 
                   numGrupos          = NULL, 
                   numValC            = NULL,
                   numTT              = NULL, 
                   grupos             = NULL)
  
  codedioma <- rv(idioma = NULL, code = list())
  
  updateData2 <- rv(datos              = NULL, 
                    originales         = NULL, 
                    datos.tabla        = NULL, 
                    datos.prueba       = NULL, 
                    datos.aprendizaje  = NULL,
                    variable.predecir  = NULL,
                    indices            = NULL, 
                    numGrupos          = NULL, 
                    numValC            = NULL, 
                    grupos             = NULL)
  
  newCases   <-     rv(originales        = NULL, 
                       datos.prueba      = NULL, 
                       datos.aprendizaje = NULL,
                       m.seleccionado    = NULL,
                       modelo            = NULL,
                       prediccion        = NULL,
                       variable.predecir = NULL)
  
  modelos <- rv(knn   = NULL,
                svm   = NULL,
                trees = NULL,
                rndf  = NULL,
                boost = NULL,
                xgb   = NULL,
                bayes = NULL,
                nnet  = NULL,
                reg   = NULL,
                regp  = NULL,
                lda   = NULL,
                qda   = NULL)
  
  ###################################  Update  ################################

  #' Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- names(translation)
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  observeEvent(updateData$datos, {
    modelos <- rv(knn   = NULL,
                  svm   = NULL,
                  trees = NULL,
                  rndf  = NULL,
                  boost = NULL,
                  xgb   = NULL,
                  bayes = NULL,
                  nnet  = NULL,
                  reg   = NULL,
                  regp  = NULL,
                  lda   = NULL,
                  qda   = NULL)
  })
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg <- input$idioma
    
    keys <- names(translation)

    for (k in keys) {
      codigo <- gsub(paste0(" ", k, "\n"), paste0(" ", tr(k, idioma = lg), "\n"), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(traineR)\n", "library(xgboost)\n",
      "library(rpart)\n", "library(rpart.plot)\n",
      "library(glmnet)\n", "library(predictoR)\n",
      "library(echarts4r)\n", "library(loadeR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  output$btn_code <- downloadHandler(
    filename = "codigo.R",
    content = function(con) {
      write(input$fieldCode, con)
    }
  )
  
  observe({
    shinyjs::disable(selector = 'a[href^="#shiny-tab-exploratorio"]')
    shinyjs::disable(selector = 'a[href^="#shiny-tab-TrainTest"]')
    shinyjs::disable(selector = 'a[href^="#shiny-tab-cv"]')
    shinyjs::disable(selector = 'a[href^="#shiny-tab-evaluar"]')
    
    if(!is.null(updateData$datos)) {
      shinyjs::enable(selector = 'a[href^="#shiny-tab-exploratorio"]')
    }
    
    if(!is.null(updateData$numTT) & is.null(updateData$numValC)) {
      shinyjs::enable(selector = 'a[href^="#shiny-tab-TrainTest"]')
    }
    
    if(!is.null(updateData$numValC)) {
      shinyjs::enable(selector = 'a[href^="#shiny-tab-cv"]')
    }
    
    for (modelo in names(modelos)) {
      if(!is.null(modelos[[modelo]])) {
        shinyjs::enable(selector = 'a[href^="#shiny-tab-evaluar"]')
      }
    }
  })
  
  ###################################  Modules  ###############################
  
  # Carga de Datos
  loadeR::mod_carga_datos_server("carga_datos_ui_1", updateData, modelos, codedioma, "predictoR")
  loadeR::mod_carga_datos_server("carga_datos_ui_2", updateData2, NULL, codedioma, "discoveR")
  
  # Estadísticas Básicas
  loadeR::mod_r_numerico_server("r_numerico_ui_1",         updateData, codedioma)
  loadeR::mod_normal_server("normal_ui_1",                 updateData, codedioma)
  loadeR::mod_dispersion_server("dispersion_ui_1",         updateData, codedioma)
  loadeR::mod_distribuciones_server("distribuciones_ui_1", updateData, codedioma)
  loadeR::mod_correlacion_server("correlacion_ui_1",       updateData, codedioma)
  mod_poder_pred_server("poder_pred_ui_1",                 updateData, codedioma)
  
  ########################## Entrenamiento-Prueba #############################
  callModule(mod_train_test_server, "tt_knn_ui",   updateData, modelos, "knn", codedioma)
  callModule(mod_train_test_server, "tt_svm_ui",   updateData, modelos, "svm", codedioma)
  callModule(mod_train_test_server, "tt_tree_ui",  updateData, modelos, "tree", codedioma)
  callModule(mod_train_test_server, "tt_rndf_ui",  updateData, modelos, "rndf", codedioma)
  callModule(mod_train_test_server, "tt_boost_ui", updateData, modelos, "boost", codedioma)
  callModule(mod_train_test_server, "tt_xgb_ui",   updateData, modelos, "xgb", codedioma)
  callModule(mod_train_test_server, "tt_bayes_ui", updateData, modelos, "bayes", codedioma)
  callModule(mod_train_test_server, "tt_nnet_ui",  updateData, modelos, "nnet", codedioma)
  callModule(mod_train_test_server, "tt_reg_ui",   updateData, modelos, "reg", codedioma)
  callModule(mod_train_test_server, "tt_regp_ui",  updateData, modelos, "regp", codedioma)
  callModule(mod_train_test_server, "tt_lda_ui",   updateData, modelos, "lda", codedioma)
  callModule(mod_train_test_server, "tt_qda_ui",   updateData, modelos, "qda", codedioma)
  
  ############################# Validación Cruzada ############################
  callModule(mod_cross_val_server, "cv_knn_ui",   updateData, modelos, "knn", codedioma)
  callModule(mod_cross_val_server, "cv_svm_ui",   updateData, modelos, "svm", codedioma)
  callModule(mod_cross_val_server, "cv_tree_ui",  updateData, modelos, "tree", codedioma)
  callModule(mod_cross_val_server, "cv_rndf_ui",  updateData, modelos, "rndf", codedioma)
  callModule(mod_cross_val_server, "cv_boost_ui", updateData, modelos, "boost", codedioma)
  callModule(mod_cross_val_server, "cv_xgb_ui",   updateData, modelos, "xgb", codedioma)
  callModule(mod_cross_val_server, "cv_bayes_ui", updateData, modelos, "bayes", codedioma)
  callModule(mod_cross_val_server, "cv_nnet_ui",  updateData, modelos, "nnet", codedioma)
  callModule(mod_cross_val_server, "cv_reg_ui",   updateData, modelos, "reg", codedioma)
  callModule(mod_cross_val_server, "cv_regp_ui",  updateData, modelos, "regp", codedioma)
  callModule(mod_cross_val_server, "cv_lda_ui",   updateData, modelos, "lda", codedioma)
  callModule(mod_cross_val_server, "cv_qda_ui",   updateData, modelos, "qda", codedioma)
  
  # Evaluacion
  callModule(mod_evaluacion_server, "evaluacion_ui_1", updateData, modelos, codedioma)

  # Predicción de Individuos Nuevos
  callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1",  newCases, updateData2, codedioma)
  
}
