source("ui.R")

server <- function(input, output, session) {
    
    output$OutPakPC2023Pak <- 
      DT::renderDT({
          DT::datatable(
            data    = PakPC2023Pak,
            caption = htmltools::tags$caption(
                        style    = 'caption-side: top; text-align: center;',
                        'Table 1: ', 
                        htmltools::em('Number of households, Population & Average Annual Population Growth Rate by Region & Area')
                          )
            ) %>%
          formatCurrency(
            columns  = c('Households', 'Pop2023', 'Pop2017'),
            currency = "",
            interval = 3,
            mark     = ",",
            digits   = 0
          ) %>%
          formatCurrency(
            columns  = c('AHS', 'GR'),
            currency = "",
            digits   = 2
          )
        })
    
    output$OutPakPC2023PakDiv <- 
      DT::renderDT({
        DT::datatable(
            data  = PakPC2023PakDiv
          , caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center;',
            'Table 2: ', htmltools::em('Number of households, Population & Average Annual Population Growth Rate by Region & Division')
          )
        ) %>% 
          formatCurrency(
            columns  = c('Households', 'Pop2023', 'Pop2017')
            , currency = ""
            , interval = 3
            , mark     = ","
            , digits   = 0
          ) %>% 
          formatCurrency(
            columns  = c('AHS', 'GR')
            , currency = ""
            , digits   = 2
          )
      })
    
    output$OutPakPC2023PakDist <- 
      DT::renderDT({
        DT::datatable(
          data  = PakPC2023PakDist
          , caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: center;',
            'Table 3: ', htmltools::em('Number of households, Population & Average Annual Population Growth Rate by Region & District')
          )
        ) %>% 
          formatCurrency(
            columns  = c('Households', 'Pop2023', 'Pop2017')
            , currency = ""
            , interval = 3
            , mark     = ","
            , digits   = 0
          ) %>% 
          formatCurrency(
            columns  = c('AHS', 'GR')
            , currency = ""
            , digits   = 2
          )
      })
    
    Data2023Basic <- reactive({
      switch(input$dataset2023basic,
             "TABLE_01"             = PakPC2023::TABLE_01 , 
             "TABLE_02"             = PakPC2023::TABLE_02 ,
              "TABLE_03"            = PakPC2023::TABLE_03 ,
              "TABLE_04"            = PakPC2023::TABLE_04 , 
              "TABLE_05"            = PakPC2023::TABLE_05 , 
              "TABLE_06"            = PakPC2023::TABLE_06 , 
              "TABLE_07"            = PakPC2023::TABLE_07 , 
              "TABLE_08"            = PakPC2023::TABLE_08 , 
              "TABLE_09"            = PakPC2023::TABLE_09 , 
              "TABLE_10"            = PakPC2023::TABLE_10 , 
              "TABLE_11"            = PakPC2023::TABLE_11 ,  
              )
    })
    
    output$Out2023Basic <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2023Basic()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
    output$Pivot2023Basic <- renderRpivotTable({
      rpivotTable(
        data           = Data2023Basic(),
        rows           = c("PROVINCE"),  
        cols           = c("REGION"),
        aggregatorName = "Sum",
        vals           = "ALL_SEXES",
        exclusions     = list(REGION = list("OVERALL")),
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "100%", 
        height         = "500px",
        rendererOptions = list(
              table = list(digitsAfterDecimal = 0), 
              c3    = list(size = list(width = "600",height = "500")),
              d3    = list(size = list(width = "500", height = "500")) 
            )
        )
    })
    
    
    Data2023Edu <- reactive({
      switch(input$dataset2023edu,
              "TABLE_12"            = PakPC2023::TABLE_12 ,  
              "TABLE_13A"           = PakPC2023::TABLE_13A ,  
              "TABLE_13B"           = PakPC2023::TABLE_13B
             )
    })
    
    output$Out2023Edu <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2023Edu()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
    output$Pivot2023Edu <- renderRpivotTable({
      rpivotTable(
        data           = Data2023Edu(),
        rows           = c("PROVINCE"),  
        cols           = c(""),
        aggregatorName = "Sum",
        vals           = "ALL_SEXES_OVERALL",      
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "10%",
        height         = "4000px"
      )
    })
    
  
    Data2023Dis <- reactive({
      switch(input$dataset2023dis,
              "TABLE_16"            = PakPC2023::TABLE_16, 
              "TABLE_17"            = PakPC2023::TABLE_17
             )
    })
  
    output$Out2023Dis <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2023Dis()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
      output$Pivot2023Dis <- renderRpivotTable({
      rpivotTable(
        data           = Data2023Dis(),
        rows           = c("PROVINCE"),  
        cols           = c(""),
        aggregatorName = "Sum",
        vals           = "ALL_SEXES_OVERALL",      
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "10%",
        height         = "4000px"
      )
    })
    
    Data2023HC <- reactive({
      switch(input$dataset2023hc,
              "TABLE_20"            = PakPC2023::TABLE_20, 
              "TABLE_21"            = PakPC2023::TABLE_21, 
              "TABLE_22"            = PakPC2023::TABLE_22,
              "TABLE_23"            = PakPC2023::TABLE_23,
              "TABLE_24"            = PakPC2023::TABLE_24, 
              "TABLE_25"            = PakPC2023::TABLE_25
             )
    })
   
    output$Out2023HC <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2023HC()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
    output$Pivot2023HC <- renderRpivotTable({
      rpivotTable(
        data           = Data2023HC(),
        rows           = c("PROVINCE"),  
        cols           = c(""),
        aggregatorName = "Sum",
        vals           = "OVERALL",      
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "10%",
        height         = "4000px"
      )
    })
   
    Data2023Listing <- reactive({
      switch(input$dataset2023listing,
              "TABLE_26"            = PakPC2023::TABLE_26
             )
    })
    
     
    output$Out2023Listing <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2023Listing()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
    output$Pivot2023Listing <- renderRpivotTable({
      rpivotTable(
        data           = Data2023Listing(),
        rows           = c("PROVINCE"),  
        cols           = c("REGION"),
        aggregatorName = "Sum",
        vals           = "ALL_STRUCTURES", 
        exclusions     = list(REGION = list("OVERALL")),
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "10%",
        height         = "4000px"
      )
    })
    
    Data2023Misc <- reactive({
      switch(input$dataset2023misc,
             "PakPC2023Pak"         = PakPC2023::PakPC2023Pak,
             "PakPC2023PakDiv"      = PakPC2023::PakPC2023PakDiv,
             "PakPC2023PakDist"     = PakPC2023::PakPC2023PakDist
             )
    })
    
    output$Out2023Misc <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2023Misc()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
    
    output$Pivot2023Misc <- renderRpivotTable({
      rpivotTable(
        data           = Data2023Misc(),
        rows           = c("Region"),  
        cols           = c("Area"),
        aggregatorName = "Sum",
        vals           = "Pop2023",
        exclusions     = list(Region = list("Pakistan"), Area = list("All")),
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "10%",
        height         = "4000px"
      )
    })
  
    
    Data2017 <- reactive({
      switch(input$dataset2017,
             "PakPC2017Pak"         = PakPC2017::PakPC2017Pak,
             "PakPC2017Pakistan"    = PakPC2017::PakPC2017Pakistan, 
             "PakPC2017Punjab"      = PakPC2017::PakPC2017Punjab, 
             "PakPC2017Sindh"       = PakPC2017::PakPC2017Sindh,
             "PakPC2017KPK"         = PakPC2017::PakPC2017KPK, 
             "PakPC2017Balochistan" = PakPC2017::PakPC2017Balochistan, 
             "PakPC2017FATA"        = PakPC2017::PakPC2017FATA, 
             "PakPC2017Islamabad"   = PakPC2017::PakPC2017Islamabad,
             "PakPC2017Tehsil"      = PakPC2017::PakPC2017Tehsil,
             "PakPC2017City10"      = PakPC2017::PakPC2017City10
             )
    })
    
    
    output$Out2017 <- 
      DT::renderDT({
        DT::datatable(
          data  = Data2017()
        , filter = c("none", "bottom", "top")[3]
        , options = list(pageLength = 50, autoWidth = TRUE)
          )
      })
    
    output$Pivot2017 <- renderRpivotTable({
      rpivotTable(
        data           = Data2017(),
        rows           = c("AdminUnits"),  
        cols           = c("ResStatus"),
        aggregatorName = "Sum",
        vals           = "Pop2017",      
        rendererName   = "Table",
        subtotals      =  TRUE,
        width          = "10%",
        height         = "4000px"
      )
    })
}

shinyApp(ui = ui, server = server)