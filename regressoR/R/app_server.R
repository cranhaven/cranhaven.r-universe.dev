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
app_server <- function( input, output, session ) {

  options(
    shiny.maxRequestSize = 200*1024^2, width = 200,
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10, 
      language = list(search = HTML('<i class="fa fa-search"></i>'),
                      emptyTable = "", zeroRecords = "",
                      paginate = list(
                        "previous" = HTML('<i class="fa fa-backward"></i>'),
                        "next" = HTML('<i class="fa fa-forward"></i>'),
                        "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                        "last" = HTML('<i class="fa fa-fast-forward"></i>')))
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
                   grupos             = NULL,
                   summary.var.pred   = NULL,
                   decimals           = 2)
  
  codedioma <- reactiveValues(idioma = "es", code = list())
  
  modelos <-  reactiveValues(
    reg  = NULL, regp  = NULL, tree = NULL, 
    rndf = NULL, boost = NULL, knn  = NULL, 
    svm  = NULL, rdim  = NULL, nnet = NULL)
  
  newCases <- rv(
    originales = NULL, datos.prueba = NULL, datos.aprendizaje = NULL,
    m.seleccionado = NULL, modelo = NULL, prediccion = NULL, 
    variable.predecir = NULL)
  
  updateData2 <- reactiveValues(
    originales = NULL, datos = NULL, datos.prueba = NULL,
    datos.aprendizaje = NULL, variable.predecir = NULL, decimals = 2)
  
  # Enable/disable on load data
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
  
  # CHANGE LANGUAGE -------------------------------------------------------------------------------------------------------
  
  # Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- names(translation)
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  observeEvent(input$decimals_confg,{
    n <- input$decimals_confg
    if(is.numeric(n)){
      if(n >= 0 & n <= 20){
        updateData$decimals <- n
        updateData2$decimals <- n
      }
      else{
        updateNumericInput(session,inputId = "decimals_confg",value = 2)
        updateData$decimals <- 2
        updateData2$decimals <- 2
      }
    }
    else{
      updateData$decimals <- 2
      updateData2$decimals <- 2
    }
  })
  
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg     <- input$idioma
    
    keys <- names(translation)
    
    for (k in keys) {
      codigo <- gsub(paste0(" ", k, "\n"), paste0(" ", tr(k, idioma = lg), "\n"), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(traineR)\n", "library(glmnet)\n",
      "library(rpart.plot)\n", "library(htmltools)\n",
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
  
  ###################################  Modules  ###############################
  #Carga de Datos
  loadeR::mod_carga_datos_server("carga_datos_ui_1", updateData,  modelos, codedioma, "regressoR")
  loadeR::mod_carga_datos_server("carga_datos_ui_2", updateData2, NULL,    codedioma, "discoveR")
  
  #Estadísticas Básicas
  loadeR::mod_r_numerico_server("r_numerico_ui_1",                 updateData, codedioma)
  loadeR::mod_normal_server("normal_ui_1",                         updateData, codedioma)
  loadeR::mod_dispersion_server("dispersion_ui_1",                 updateData, codedioma)
  loadeR::mod_distribuciones_server("distribuciones_ui_1",         updateData, codedioma)
  loadeR::mod_correlacion_server("correlacion_ui_1",               updateData, codedioma)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData, codedioma)
  
  ########################## Entrenamiento-Prueba #############################
  callModule(mod_train_test_server, "tt_reg_ui",   updateData, modelos, "reg", codedioma)
  callModule(mod_train_test_server, "tt_regp_ui",  updateData, modelos, "regp", codedioma)
  callModule(mod_train_test_server, "tt_tree_ui",  updateData, modelos, "tree", codedioma)
  callModule(mod_train_test_server, "tt_rndf_ui",  updateData, modelos, "rndf", codedioma)
  callModule(mod_train_test_server, "tt_boost_ui", updateData, modelos, "boost", codedioma)
  callModule(mod_train_test_server, "tt_knn_ui",   updateData, modelos, "knn", codedioma)
  callModule(mod_train_test_server, "tt_svm_ui",   updateData, modelos, "svm", codedioma)
  callModule(mod_train_test_server, "tt_rdim_ui",  updateData, modelos, "rdim", codedioma)
  callModule(mod_train_test_server, "tt_nnet_ui",  updateData, modelos, "nnet", codedioma)
  
  ############################# Validación Cruzada ############################
  callModule(mod_cross_val_server, "cv_reg_ui",   updateData, modelos, "reg", codedioma)
  callModule(mod_cross_val_server, "cv_regp_ui",  updateData, modelos, "regp", codedioma)
  callModule(mod_cross_val_server, "cv_tree_ui",  updateData, modelos, "tree", codedioma)
  callModule(mod_cross_val_server, "cv_rndf_ui",  updateData, modelos, "rndf", codedioma)
  callModule(mod_cross_val_server, "cv_boost_ui", updateData, modelos, "boost", codedioma)
  callModule(mod_cross_val_server, "cv_knn_ui",   updateData, modelos, "knn", codedioma)
  callModule(mod_cross_val_server, "cv_svm_ui",   updateData, modelos, "svm", codedioma)
  callModule(mod_cross_val_server, "cv_rdim_ui",  updateData, modelos, "rdim", codedioma)
  
  # Evaluacion
  callModule(mod_evaluacion_server, "evaluacion_ui_1", updateData, modelos, codedioma)
  
  # Predicción Ind. Nuevos
  callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1", newCases, updateData2, codedioma)

  # About
  callModule(mod_information_page_server,     "information_page_ui_1", codedioma)
}
