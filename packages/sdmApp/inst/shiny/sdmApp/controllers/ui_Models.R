output$ui_Models_main <- renderUI({
  out <- NULL
  val <- obj$cur_selection_results
  ## Categorical (defined in controller/ui_results_imputation.R)
  if (val=="btn_Models_results_1") {
    return(uiOutput("ui_bioclim"))
  }
  if (val=="btn_Models_results_2") {
    return(uiOutput("ui_domain"))
  }
  if (val=="btn_Models_results_3") {
    return( uiOutput("ui_mahal"))
  }
  if (val=="btn_Models_results_4") {
    return(uiOutput("ui_GLM"))
  }
  if (val=="btn_Models_results_5") {
    return(uiOutput("ui_MaxEnt"))
  }
  if (val=="btn_Models_results_6") {
    return( uiOutput("ui_RF"))
  }
  if (val=="btn_Models_results_7") {
    return(uiOutput("ui_SVM"))
  }
  if (val=="btn_Models_results_8") {
    return( uiOutput("ui_Combining"))
  }
})
output$ui_Models_sidebar_left <- renderUI({
  output$ui_sel_Models_btns <- renderUI({
    cc1 <- c("Bioclim","Domain","Mahalanobis distance")
    cc2 <- c("Generalized Linear Models")
    cc3 <- c("MaxEnt", "Random Forest","Support Vector Machines")
    cc4 <- c("Combining model")
    df <- data.frame(lab=c(cc1,cc2,cc3,cc4), header=NA)
    df$header[1] <- "Profile models"
    df$header[4] <- "Classical regression models"
    df$header[5] <- "Machine learning models"
    df$header[8] <- "Combining model predictions"
    out <- NULL
    for (i in 1:nrow(df)) {
      id <- paste0("btn_Models_results_",i)
      if (obj$cur_selection_results==id) {
        style <- "primary"
      } else {
        style <- "default"
      }
      if (!is.na(df$header[i])) {
        out <- list(out, fluidRow(column(12, h4(df$header[i]), align="center")))
      }
      out <- list(out, fluidRow(
        column(12, bsButton(id, label=df$lab[i], block=TRUE, size="extra-small", style=style))
      ))
    }
    out
  })
  # required observers that update the color of the active button!
  eval(parse(text=genObserver_menus(pat="btn_Models_results_", n=1:10, updateVal="cur_selection_results")))
  return(uiOutput("ui_sel_Models_btns"))
})
output$ui_Models_noproblem <- renderUI({
  return(list(
    noInputData(uri="ui_Models"),
    fluidRow(column(12, tags$br(), p(""), align="center"))
    #fluidRow(column(12, myActionButton("nodata_anonymize_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
  ))
})
output$ui_Models <- renderUI({
  if(length(load.occ$Pcol) == 0){
    return(uiOutput("ui_Models_noproblem"))}
  {
    fluidRow(
      #column(12,offset=0,radioButtons('choice_block', 'Please Choose your model technic', choices=c(No_sp_blocking="Modelling without spatial blocking",sp_blocking="Modelling with spatial blocking"), selected="Modelling without spatial blocking", inline=TRUE),align="center", class="wb-header" ),
      column(2, uiOutput("ui_Models_sidebar_left"), class="wb_sidebar"),
      column(10, uiOutput("ui_Models_main"), class="wb-maincolumn")
    )
  }
}
)
