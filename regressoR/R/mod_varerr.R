#' varerr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_varerr_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(id = "Boxvarerr",
                tabPanel(title = labelInput("varError"), value = "tabModelosComp", 
                         div(
                           col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                                     tags$div(class="multiple-select-var",
                                              selectInput(inputId = ns("ind.sel"),label = NULL,
                                                          choices =  "", width = "100%")))),
                           col_4(),
                           col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectMod"),class = "wrapper-tag"),
                                     tags$div(class="multiple-select-var",
                                              selectInput(inputId = ns("model.sel"),label = NULL,
                                                          choices =  "", width = "100%"))))),
                         div(col_12(withLoader(echarts4rOutput(ns('plot_comp'), height = "70vh"), 
                                              type = "html", loader = "loader4")))
                ))
  )
}

#' varerr Server Function
#'
#' @noRd 
mod_varerr_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list("knnl", "svml", "dtl", "rfl",  "bl", "rl", "rlr", "rd")
    names(nombres) <- tr(c("knnl", "svml", "dtl", "rfl", "bl", "rl", "rlr", "rd"),codedioma$idioma)
    select   <-  ifelse(!is.null(input$model.sel), nombres[1], input$model.sel)
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    updateSelectInput(session, "ind.sel", choices = indices, selected = 0)
    updateSelectInput(session, "model.sel", choices = nombres, selected = select)
  })
  
  # Update Plot ROC
  output$plot_comp <- renderEcharts4r({
    idioma        <- codedioma$idioma
    mdls          <- modelos2
    selected <- input$model.sel
    indice   <- input$ind.sel
    
    tryCatch({
      
      mdl <- switch (selected,
                     "knnl"  = mdls$knn,
                     "svml"  = mdls$svm,
                     "dtl"   = mdls$dt,
                     "rfl"   = mdls$rf,
                     "bl"    = mdls$boosting, 
                     "rl"    = mdls$rl, 
                     "rlr"   = mdls$rlr, 
                     "rd"   = mdls$rd)
      label <- switch (indice,
                       "0" = tr("RMSE",idioma),
                       "1" = tr("MAE",idioma),
                       "2" = tr("ER",idioma),
                       "3" = tr("correlacion",idioma)
      )
      if(is.null(mdl$mcs[[1]]))
        return(NULL)
      indices <- indices.comp(mdl$mcs,mdl$n)
      
      graf <- indices$grafico
      
      graf$value <- switch (indice,
                               "0" = indices$global,
                               "1" = graf$ea,
                               "2" = graf$er,
                               "3" = graf$corr
      )
      p <- ifelse(indice == "2", TRUE, FALSE)
      
      comp.lineas(graf, labels = c(label, tr("rep",idioma) ), percent = p)
    }, error = function(e) {
      showNotification(e, duration = 15, type = "error")
      return(NULL)
    })
  })
  
}

## To be copied in the UI
# mod_varerr_ui("varerr_ui_1")

## To be copied in the server
# callModule(mod_varerr_server, "varerr_ui_1")

