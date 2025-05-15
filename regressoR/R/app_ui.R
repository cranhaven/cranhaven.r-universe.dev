#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#' DO NOT REMOVE.
#' @import shiny
#' @import loadeR
#' @import traineR
#' @import rlang
#' @import glmnet
#' @import shinyAce
#' @import rpart.plot
#' @import stats
#' @import echarts4r
#' @import shinycustomloader
#' @import htmltools
#' @import gbm
#' @importFrom psych pairs.panels
#' @importFrom utils head read.table write.csv
#' @importFrom grDevices adjustcolor hcl
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom graphics hist abline lines pairs par points polygon rect smoothScatter strwidth text identify
#' @importFrom DT tableHeader formatStyle
#' @importFrom pls pcr plsr MSEP RMSEP explvar R2 mvrValstats
#' @rawNamespace import(shinydashboard, except = c(dashboardHeader,dashboardPage,dashboardSidebar))
#' @rawNamespace import(shinyjs, except = c(runExample,colourInput))
#' @keywords internal
#' @noRd
app_ui <- function(request) {
  
  # MENU --------------------------------------------------------------------------------------------------------------------
  load.menu <- menuItem(labelInput("data"), tabName = "cargar", icon = icon("database"))
  
  
  statistics.menu <- menuItem(
    labelInput("basico"), tabName = "exploratorio", icon = icon("square-poll-vertical"),
    menuSubItem(labelInput("resumen"), tabName = "resumen", icon = icon("sort-numeric-down")),
    menuSubItem(labelInput("normalidad"), tabName = "normalidad", icon = icon("chart-bar")),
    menuSubItem(labelInput("dispersion"), tabName = "dispersion", icon = icon("chart-line")),
    menuSubItem(labelInput("distribucion"), tabName = "distribucion", icon = icon("chart-area")),
    menuSubItem(labelInput("correlacion"), tabName = "correlacion", icon = icon("table")),
    menuItem(labelInput("poderpred"), tabName = "poderPred", icon = icon("rocket"))
  )
  
  supervised.learning.menu    <- menuItem(
    labelInput("tt"), tabName = "TrainTest", icon = icon("th-list"),
    menuSubItem(labelInput("rl"),    tabName = "reg",   icon = icon("chart-line")),
    menuSubItem(labelInput("rlr"),   tabName = "regp",  icon = icon("wave-square")),
    menuSubItem(labelInput("dt"),    tabName = "tree",  icon = icon("tree")),
    menuSubItem(labelInput("rf"),    tabName = "rndf",  icon = icon("sitemap")),
    menuSubItem(labelInput("boost"), tabName = "boost", icon = icon("superscript")),
    menuSubItem(labelInput("knn"),   tabName = "knn",   icon = icon("dot-circle")),
    menuSubItem(labelInput("svm"),   tabName = "svm",   icon = icon("vector-square")),
    menuSubItem(labelInput("rd"),    tabName = "rdim",  icon = icon("chart-pie")),
    menuSubItem(labelInput("nn"),    tabName = "nnet",  icon = icon("brain")))
  
  calibracion.menu <- menuItem(
    labelInput("calibracion"), tabName = "cv", icon = icon("gears"),
    menuSubItem(labelInput("rl"),   tabName = "cv_reg",   icon = icon("chart-line")),
    menuSubItem(labelInput("rlr"),  tabName = "cv_regp",  icon = icon("wave-square")),
    menuSubItem(labelInput("dtl"),  tabName = "cv_tree",  icon = icon("tree")),
    menuSubItem(labelInput("rfl"),  tabName = "cv_rndf",  icon = icon("sitemap")),
    menuSubItem(labelInput("bl"),   tabName = "cv_boost", icon = icon("superscript")),
    menuSubItem(labelInput("knnl"), tabName = "cv_knn",   icon = icon("dot-circle")),
    menuSubItem(labelInput("svml"), tabName = "cv_svm",   icon = icon("vector-square")),
    menuSubItem(labelInput("rd"),   tabName = "cv_rdim",  icon = icon("chart-pie")))
  
  
  
  cross.val.menu <- menuItem(labelInput("comparacion"), tabName = "evaluar", icon = icon("balance-scale"))
  
  new.prediction.menu <- menuItem(labelInput("predicnuevos"), tabName = "predNuevos", icon = icon("table"))
  
  info.menu <- menuItem(labelInput("acercade"), tabName = "acercaDe", icon = icon("circle-info"))
  
  menu.language <- tags$li(class = "nodisabled treeview",
                           tags$a(href = "#shiny-tab-tabdioma",
                                  tags$i(class="fa fa-cog"),
                                  labelInput("confg"),
                                  tags$i(class="fa fa-angle-left pull-right")),
                           tags$ul(class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
                                   radioButtons('idioma', labelInput("selidioma"), 
                                                choiceNames = c(tr('espanol', 'es'), 'English'),
                                                choiceValues = c("es", "en")),
                                   tags$br(),
                                   numericInput("decimals_confg", label = labelInput("ndec"),
                                                min = 0, max = 20, value = 2,step = 1,width = "50%"),
                                   tags$br()))
  
  #Los sliderInput y colourpicker por un motivo imprevisto se tienen que inicializar
  #De lo contrario no se van a mostrar en algunas partes de la interfaz
  init.inputs <- tags$div(
    style = "display:none;", 
    sliderInput("auxs", min = 2, value = 2, label = "auxs", max = 10),
    colourInput("auxc", NULL, value =  "red")
  )
  
  # The side menu
  mi.menu <- sidebarMenu(id = "principal",
                         tags$div(id = "espacioMenu"),
                         load.menu,
                         statistics.menu,
                         supervised.learning.menu,
                         calibracion.menu,
                         cross.val.menu,
                         new.prediction.menu,
                         info.menu,
                         hr(),
                         menu.language,
                         hr(), 
                         img(src = "img/regressoR.png", style = "margin-left: auto;margin-right: auto;display: block;width: 80%;"),
                         init.inputs)
  
  
  # HEAD HTML ---------------------------------------------------------------------------------------------------------------
  
  #Imports .css and .js, also decide the icon
  mi.head <- tags$head(
    tags$link(rel="icon", href="https://www.promidat.education/theme/image.php/formal_white/theme/1438713216/favicon"),
    useShinyjs())
  
  #The loading page generating model
  load.page <- conditionalPanel(condition="($('html').hasClass('shiny-busy')) && generating_model == true",
                                div(id = "loaderWrapper", div(id="loaderModel")))
  
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      title="PROMiDAT - regressoR",
      shinydashboardPlus::dashboardHeader(
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href = "https://promidat.com" target = "_blank">
              <img src = "img/Logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        )), controlbarIcon = icon("cogs")
      ),
      dashboardSidebar(mi.menu),
      dashboardBody(mi.head,
                    load.page,
                    tabItems(
                      tabItem(tabName = "cargar",  
                              loadeR::mod_carga_datos_ui("carga_datos_ui_1", labelInput("data"))),
                      tabItem(tabName = "resumen",
                              loadeR::mod_r_numerico_ui("r_numerico_ui_1")),
                      tabItem(tabName = "normalidad",
                              loadeR:: mod_normal_ui("normal_ui_1")),
                      tabItem(tabName = "dispersion",
                              loadeR::mod_dispersion_ui("dispersion_ui_1")),
                      tabItem(tabName = "distribucion",
                              loadeR::mod_distribuciones_ui("distribuciones_ui_1")),
                      tabItem(tabName = "correlacion",
                              loadeR::mod_correlacion_ui("correlacion_ui_1")),
                      tabItem(tabName = "poderPred",
                              mod_Predictive_Power_ui("Predictive_Power_ui_1")),
                      
                      tabItem(tabName = "reg",   mod_train_test_ui("tt_reg_ui", "reg")),
                      tabItem(tabName = "regp",  mod_train_test_ui("tt_regp_ui", "regp")),
                      tabItem(tabName = "tree",  mod_train_test_ui("tt_tree_ui", "tree")),
                      tabItem(tabName = "rndf",  mod_train_test_ui("tt_rndf_ui", "rndf")),
                      tabItem(tabName = "boost", mod_train_test_ui("tt_boost_ui", "boost")),
                      tabItem(tabName = "knn",   mod_train_test_ui("tt_knn_ui", "knn")),   
                      tabItem(tabName = "svm",   mod_train_test_ui("tt_svm_ui", "svm")),
                      tabItem(tabName = "rdim",  mod_train_test_ui("tt_rdim_ui", "rdim")),
                      tabItem(tabName = "nnet",  mod_train_test_ui("tt_nnet_ui", "nnet")),

                      ############### Validación Cruzada ############### 
                      tabItem(tabName = "cv_reg",   mod_cross_val_ui("cv_reg_ui", "reg")),
                      tabItem(tabName = "cv_regp",  mod_cross_val_ui("cv_regp_ui", "regp")),
                      tabItem(tabName = "cv_tree",  mod_cross_val_ui("cv_tree_ui", "tree")),
                      tabItem(tabName = "cv_rndf",  mod_cross_val_ui("cv_rndf_ui", "rndf")),
                      tabItem(tabName = "cv_boost", mod_cross_val_ui("cv_boost_ui", "boost")),
                      tabItem(tabName = "cv_knn",   mod_cross_val_ui("cv_knn_ui", "knn")),   
                      tabItem(tabName = "cv_svm",   mod_cross_val_ui("cv_svm_ui", "svm")),
                      tabItem(tabName = "cv_rdim",  mod_cross_val_ui("cv_rdim_ui", "rdim")),
                      
                      # Evaluación de Modelos
                      tabItem(tabName = "evaluar", mod_evaluacion_ui("evaluacion_ui_1")),
                      
                      # Predicción Individuos Nuevos
                      tabItem(tabName = "predNuevos", mod_ind_nuevos_ui("ind_nuevos_ui_1")),
                      
                      # Acerca De
                      tabItem(tabName = "acercaDe", mod_information_page_ui("information_page_ui_1"))
                    )),
      shinydashboardPlus::dashboardControlbar(
        width = 500,
        div(
          style = "margin-right: 15px; margin-left: 15px;",
          h3(labelInput('code')), hr(),
          codigo.monokai("fieldCode", height = "70vh"),
          downloadButton("btn_code", NULL, style = "width: 100%;")
        )
      )
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  jsCode <- '
  get_inputs = function() {
  var rowname = $("#carga_datos_ui_2-rowname")[0].checked;
  var header = $("#carga_datos_ui_2-header")[0].checked;
  var sep = $("input[name=\'carga_datos_ui_2-sep\']:checked").val();
  var dec = $("input[name=\'carga_datos_ui_2-dec\']:checked").val();
  var nas = $("input[name=\'carga_datos_ui_2-deleteNA\']:checked").val();

  Shiny.setInputValue("ind_nuevos_ui_1-jsrowname", rowname);
  Shiny.setInputValue("ind_nuevos_ui_1-jsheader", header);
  Shiny.setInputValue("ind_nuevos_ui_1-jssep", sep);
  Shiny.setInputValue("ind_nuevos_ui_1-jsdec", dec);
  Shiny.setInputValue("ind_nuevos_ui_1-jsnas", nas);
  }
  
  get_inputs_xlsx = function() {
  var rowname = $("#carga_datos_ui_2-rowname_xlsx")[0].checked;
  var header = $("#carga_datos_ui_2-header_xlsx")[0].checked;
  var num_hoja = $("#carga_datos_ui_2-num_hoja").val();
  var fila_inicio = $("#carga_datos_ui_2-fila_inicio").val();
  var col_inicio = $("#carga_datos_ui_2-col_inicio").val();
  var fila_final = $("#carga_datos_ui_2-fila_final").val();
  var col_final = $("#carga_datos_ui_2-col_final").val();
  var deleteNA_xlsx = $("input[name=\'carga_datos_ui_2-deleteNA_xlsx\']:checked").val();

  Shiny.setInputValue("ind_nuevos_ui_1-jsrowname_xlsx", rowname);
  Shiny.setInputValue("ind_nuevos_ui_1-jsheader_xlsx", header);
  Shiny.setInputValue("ind_nuevos_ui_1-jsnum_hoja", num_hoja);
  Shiny.setInputValue("ind_nuevos_ui_1-jsfila_inicio", fila_inicio);
  Shiny.setInputValue("ind_nuevos_ui_1-jscol_inicio", col_inicio);
  Shiny.setInputValue("ind_nuevos_ui_1-jsfila_final", fila_final);
  Shiny.setInputValue("ind_nuevos_ui_1-jscol_final", col_final);
  Shiny.setInputValue("ind_nuevos_ui_1-jsdeleteNA_xlsx", deleteNA_xlsx);
}
  
  get_file = function() {
  $("#carga_datos_ui_2-run_data").on("click", function(){
        var file_type = $("#carga_datos_ui_2-file_type").find(".active")[0].firstElementChild.getAttribute("data-value")
        Shiny.setInputValue("ind_nuevos_ui_1-jsfile_type", file_type);
        });
}
  '
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
  
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'regressoR'
    ),
    shinyjs::useShinyjs(),
    tags$script(HTML(jsCode))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
}

