###########################################"" Data Preparation#############
###############################################################"###########


output$ui_preparation_main <- renderUI({
  out <- NULL
  val <- obj$cur_selection_results
  if (val=="btn_preparation_results_1") {
    return(uiOutput("ui_view_species_data"))
  }
  if (val=="btn_preparation_results_2") {
    return(uiOutput("ui_correlation"))
  }
  if (val=="btn_preparation_results_3") {
    return(uiOutput("ui_enfa"))
  }
  if (val=="btn_preparation_results_4") {
    return(uiOutput("ui_spatial_auto_range"))
  }
  if (val=="btn_preparation_results_5") {
    return(uiOutput("ui_spatial_blocks"))
  }

})

output$ui_preparation_sidebar_left <- renderUI({
  output$ui_sel_preparation_btns <- renderUI({
    cc1 <- c("Summary")
    cc2 <- c("Correlation", "ENFA", "Spatial autocorrelation", "Spatial blocking")
    df <- data.frame(lab=c(cc1,cc2), header=NA)
    df$header[1] <- "View"
    df$header[2] <- "Spatial Analysis"
    out <- NULL
    for (i in 1:nrow(df)) {
      id <- paste0("btn_preparation_results_",i)
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
  eval(parse(text=genObserver_menus(pat="btn_preparation_results_", n=1:5, updateVal="cur_selection_results")))
  return(uiOutput("ui_sel_preparation_btns"))
})
output$ui_anonymize_noproblem <- renderUI({
  return(list(
    noInputData(uri="ui_preparation"),
    fluidRow(column(12, tags$br(), p(""), align="center"))
  ))
})
output$ui_preparation <- renderUI({
  if(length(load.occ$Pcol) == 0){
    return(uiOutput("ui_anonymize_noproblem"))}
  else{
    fluidRow(
      column(2, uiOutput("ui_preparation_sidebar_left"), class="wb_sidebar"),
      column(10, uiOutput("ui_preparation_main"), class="wb-maincolumn"))
  }
}
)

#########################################
