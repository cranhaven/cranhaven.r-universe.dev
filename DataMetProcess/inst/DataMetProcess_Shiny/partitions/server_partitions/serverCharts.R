serverCharts <- function(id, Daily, Monthly, Yearly){
  moduleServer(id,function(input,output,session){
    GraficoInput <- reactive({
      req(input$dataset)
      if(input$dataset=="Daily"){
        return(Daily())
      }
      if(input$dataset=="Monthly"){
        return(Monthly())
      }
      if(input$dataset=="Yearly"){
        return(Yearly())
      }
    })
    
    observeEvent(input$dataset,{
      output$colx <- renderUI({
        selectInput(NS(id,"colx"), "x axis variable:",
                    choices = colnames(GraficoInput()),
                    multiple = F
        )
      })
      output$coly <- renderUI({
        selectInput(NS(id,"coly"), "y axis variable:",
                    choices = colnames(GraficoInput()),
                    multiple = F,
                    selected = colnames(GraficoInput()[2])
        )
      })
    })
    
    
    
    Grafico <- reactive({
      req(input$colx)
      req(input$coly)
      req(input$Tipogra)
      
      if(input$labelx==""){
        lx <- input$colx
      }else{
        lx <- input$labelx
      }
      if(input$labely==""){
        ly <- input$coly
      }else{
        ly <- input$labely
      }
      
      gra <-
        plot_ly_fn(data = GraficoInput(),
                   x = input$colx,y = input$coly,
                   type = input$Tipogra,labelx=lx,labely=ly,
                   widthl = input$widthl,corl = input$corline,
                   corp = input$corpoint,corlp = input$corlp,
                   sizep = input$sizep,widthlp = input$widthlp,
                   corb = input$corbar,corlb = input$corlb,widthlb = input$widthlb
        )
      
      return(gra)
    })
    
    
    output$plotlygraf <- renderPlotly({
      Grafico()
    })
    
  })
}

