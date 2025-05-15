#' poder_pred UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_poder_pred_ui <- function(id){
  ns <- NS(id)
  
  title_comp <- div(conditionalPanel("input.BoxPodPred == 'tabDistpredcat'",
                                     div(shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selvar"),class = "wrapper-tag"),
                                         tags$div(class="multiple-select-var",
                                                  selectInput(inputId = ns("sel_pred_cat"),label = NULL,
                                                              choices =  "", width = "100%")))), 
                    conditionalPanel("input.BoxPodPred == 'tabDenspred'",
                                     div(shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selvar"),class = "wrapper-tag"),
                                         tags$div(class="multiple-select-var",
                                                  selectInput(inputId = ns("sel_dens_pred"),label = NULL,
                                                              choices =  "", width = "100%")))), )
  opc_podpred <- div(
      tabsOptions(heights = c(70), tabs.content = list(
        list(options.run(ns("run_pp")), tags$hr(style = "margin-top: 0px;"),
             col_12(color.input(ns("ppColor"))))
      )))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxPodPred",opciones = opc_podpred ,title = title_comp,
      tabPanel(
        title = labelInput("distpred"), value = "tabDistpred",
        withLoader(echarts4rOutput(ns('hc_distpred'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("pares"), value = "tabPares",
        withLoader(plotOutput(ns('plot_pairs_poder'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("distpredcat"), value = "tabDistpredcat",
        withLoader(echarts4rOutput(ns('plot_dist_poder'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("denspred"), value = "tabDenspred",
        withLoader(echarts4rOutput(ns('plot_density_poder'), height = "75vh"), 
                   type = "html", loader = "loader4"))
    )
  )
}

#' poder_pred Server Functions
#'
#' @noRd 
mod_poder_pred_server <- function(id,       updateData, codedioma){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # Update on load testing data
    observeEvent(updateData$datos.prueba, {
      variable     <- updateData$variable.predecir
      datos        <- updateData$datos
      nombres      <- colnames.empty(var.numericas(datos))
      cat.sin.pred <- colnames.empty(var.categoricas(datos))
      cat.sin.pred <- cat.sin.pred[cat.sin.pred != variable]
      mostrar.colores("ppColor", length(levels(datos[, variable])))
      
      updateSelectInput(session, "sel_pred_cat", choices = cat.sin.pred)
      updateSelectInput(session, "sel_dens_pred", choices = nombres)
    })
    
    # Gráfico de Distribución Variable a Predecir 
    output$hc_distpred = renderEcharts4r({
      input$run_pp
      var  <- updateData$variable.predecir
      validate(need(var != "", tr("errorcat", isolate(codedioma$idioma))))
      
      tryCatch({
        data <- updateData$datos[, var]
        cod  <- paste0("### distpred\n",code.dist.varpred(var))
        n <- length(levels(data))
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        label   <- levels(data) 
        color   <- unname(sapply(1:n, function(i) 
                   isolate(input[[paste0("ppColor", i)]])))
        value   <- summary(data, maxsum = length(levels(data)))
        prop    <- value/length(data)
        grafico <- data.frame (
          name  = var,
          label = label, 
          value = value,
          color = color,
          prop  = prop
        )
        
        grafico |> 
          e_charts(label) |> 
          e_bar(value, prop, name = var) |> 
          e_tooltip(formatter = e_JS(paste0("function(params){
                                      return('<strong>' +  params.value[0] +
                                             '</strong><br />", tr("porcentaje", codedioma$idioma) ,": ' + parseFloat(params.name * 100).toFixed(2)+
                                             '%<br /> ' + '", tr("cant", codedioma$idioma),": ' + params.value[1])}"))) |> 
          e_legend(FALSE)|> e_show_loading()|>        
          e_datazoom(show = F) |> e_add_nested("itemStyle", color)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
    

    
    #Pairs Plot Output
    output$plot_pairs_poder <- renderPlot({
      input$run_pp
      tryCatch({
        variable  <- updateData$variable.predecir
        datos     <- updateData$datos
        cod.pairs <- code.pairs_poder(variable)
        idioma    <- codedioma$idioma
        res       <-    NULL
        cod  <- paste0("### pares\n",cod.pairs)
        n   <- length(levels(datos[, variable]))
        
        colores <- unname(sapply(1:n, function(i) 
          isolate(input[[paste0("ppColor", i)]])))
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        if (ncol(var.numericas(datos)) >= 2) {
          if(ncol(var.numericas(datos)) <= 25){
            pairs_poder(datos,variable, colores)
          }else{
            showNotification(tr("bigPlot",idioma), duration = 10, type = "message")
            
          }
        }else{
          showNotification(paste0(tr("errornum",idioma)),
                           duration = 10,
                           type = "message")
          res <- NULL
        }
        return(res)
        
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
      
    })
    
    
    # Hace el gráfico de densidad de variables númericas
    output$plot_density_poder <- renderEcharts4r({
      input$run_pp
      variable.num  <- input$sel_dens_pred
      idioma        <- codedioma$idioma
      variable.pred <- updateData$variable.predecir
      datos         <- updateData$datos
      
      tryCatch({
        n <- length(levels(datos[, variable.pred]))
        
        colores <- unname(sapply(1:n, function(i) 
          isolate(input[[paste0("ppColor", i)]])))
        
        if (ncol(var.numericas(datos)) >= 1) {
          cod <- paste0("e_numerico_dens(datos, '", variable.num,
                        "', '", variable.pred, "', label = '",tr("denspodlab",idioma) ,"' ))\n")
          cod  <- paste0("### denspred\n",cod)
          isolate(codedioma$code <- append(codedioma$code, cod))
          e_numerico_dens(datos, variable.num, variable.pred, label=tr("denspodlab", idioma), colores)
        }else{#No retorna nada porque el grafico de error es con PLOT no ECHARTS4R
          showNotification(paste0(tr("errornum",idioma)),
                           duration = 10,
                           type = "message")
          return(NULL)
        }
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
    
    # Hace el gráfico de poder predictivo categórico
    output$plot_dist_poder <- renderEcharts4r({
      input$run_pp
      variable.cat  <- input$sel_pred_cat
      idioma        <- codedioma$idioma
      variable.pred <- updateData$variable.predecir
      datos         <- updateData$datos
      
      tryCatch({
        if (ncol(var.categoricas(datos)) > 1) {
          cod <- paste0("e_categorico_dist(datos, '", variable.cat,
                        "', '", variable.pred, "', label = '",tr("distpodcat",idioma) ,"' ))\n")
          cod  <- paste0("### docpredcat\n",cod)
          n <- length(levels(datos[, variable.pred]))
          
          colores <- unname(sapply(1:n, function(i) 
            isolate(input[[paste0("ppColor", i)]])))
          
          isolate(codedioma$code <- append(codedioma$code, cod))
          e_categorico_dist(datos, variable.cat, variable.pred, 
                            label = tr("distpodcat",idioma),labels = c(tr("porcentaje", idioma),tr("cant", idioma) ), colores)
          
        }else{
          showNotification(paste0(tr("errorcat",idioma)),
                           duration = 10,
                           type = "message")
          return(NULL)
        }
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })
}

## To be copied in the UI
# mod_poder_pred_ui("poder_pred_ui_1")

## To be copied in the server
# mod_poder_pred_server("poder_pred_ui_1")
