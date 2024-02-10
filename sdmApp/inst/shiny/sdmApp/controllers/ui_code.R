current_code <- reactive({
  code_ges <- c(obj$code)#, obj$code_read_and_modify, obj$code_setup, obj$code_anonymize)
  code_ges
})

output$current_code <- renderText({
  paste0("<pre class='r'><code class='r' id='codeout'>",paste(highr:::hi_html(current_code()), collapse="\n"),"</code></pre>")
})

output$ui_code_nodata <- renderUI({
  return(list(
    noInputData(uri="ui_code"),
    fluidRow(column(12, tags$br(), p(""), align="center"))
  ))
})
# GUI-output to view script
output$ui_code <- renderUI({
  if(length(load.occ$Pcol) == 0){
    return(uiOutput("ui_code_nodata"))}
  else{
  out <- fluidRow(
    column(12, h3("View the current generated script"), class="wb-header"),
    column(12, p("Browse and download the script used to generate your results. These can be used later as a reminder of what you did or entered into R from command-line to reproduce results."), class="wb-header-hint"),
    column(12, myActionButton("btn_save_script", "Save Script to File", btn.style="primary"), align="center"),
    column(12, tags$br(), uiOutput("current_code"))
  )

  out
  }
})
