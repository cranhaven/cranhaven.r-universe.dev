serverDownInmet <- function(id){
  moduleServer(id,function(input,output,session){
    shinyjs::disable('Down_met')
    
    opcoes_down_ano <- reactive({
      return(list_inmet(input$AnosDown))
    })
    
    output$table_est <- renderReactable({
      reactable(
        opcoes_down_ano()[[2]],
        selection = "single",
        resizable = T,
        filterable = TRUE,
        showPageSizeOptions = T,
        defaultColDef = colDef(
          #minWidth = 150,
          style = list(border = "none"),
          align = "center",
          headerStyle = list(border = "none")
        ),
        theme = reactableTheme(
          style = list(fontFamily = "Helvetica, Segoe UI, Arial, sans-serif")
        ),
        paginationType = "jump",
        rowStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee',
    borderBottom: '1px solid #ffa62d',
    borderRadius: '0px'
    }}
  }"),
  columns = list(
    City = colDef(width = 300))
      )
    })
    
    selectid <- reactiveValues()
    
    select_row <- observe({
      selecionado <-
        opcoes_down_ano()[[2]][getReactableState("table_est",
                                                 name = "selected"),]
      if(nrow(selecionado)==0){
        shinyjs::disable('Down_met')
      }else{
        shinyjs::enable('Down_met')
      }
      selectid$id <- selecionado[5]
    })
    
    output$Down_met <- downloadHandler(
      filename = function(){
        opcoes_down_ano()[[1]][grep(selectid$id,
                                    opcoes_down_ano()[[1]][[1]]),]
      },
      content = function(file){
        filen <- opcoes_down_ano()[[1]][grep(selectid$id,
                                             opcoes_down_ano()[[1]][[1]]),]
        #file.copy(
        #unzip(zipfile = "./Downs/zip_inmet.zip",
        #files = filen,
        #     exdir = "Downs", overwrite = TRUE),file)
        #file.remove(paste0("./Downs/",filen))
        
        down_inmet(
          zipfile = opcoes_down_ano()[[3]], #nome do arquivo zip
          filen = filen, #nome do arquivo a ser salvado
          file = file, #local pra copiar se necessario, util para o app shiny,
          message = F
        )
        
      }
    )
  })
}