source("ui.R")
server <- function(input = input, output = output, session = NULL) {
  
  dataInput <- reactive({
    switch(input$dataset,
         "NACurrent"   = readRDS(file = "NACurrent.RDS"),
         "NAConstant"  = readRDS(file = "NAConstant.RDS"),
         "GNICurrent"  = readRDS(file = "GNICurrent.RDS"),
         "GNIConstant" = readRDS(file = "GNIConstant.RDS")
         
         # "NACurrent"   = readRDS(file = system.file("extdata", "NACurrent.RDS",   package = "PakNAcc")),
         # "NAConstant"  = readRDS(file = system.file("extdata", "NAConstant.RDS",  package = "PakNAcc")),
         # "GNICurrent"  = readRDS(file = system.file("extdata", "GNICurrent.RDS",  package = "PakNAcc")),
         # "GNIConstant" = readRDS(file = system.file("extdata", "GNIConstant.RDS", package = "PakNAcc"))
    )
  })
  
  output$Data <- 
    DT::renderDT({
      DT::datatable(
        data  = dataInput(),
        filter = c("none", "bottom", "top")[3],
        options = list(pageLength = 50, autoWidth = TRUE)
      )
    })
  
  output$PivotTable <- renderRpivotTable({
    rpivotTable(
      data           = dataInput(),
      rows           = c("Quarter"),  
      cols           = c("Year"),
      aggregatorName = "Sum",
      vals           = "NA",
      rendererName   = "Area Chart",
      subtotals      = TRUE,
      width          = "10%",
      height         = "4000px"
    )
  })
}

shinyApp(ui = ui, server = server)