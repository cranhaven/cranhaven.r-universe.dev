
serverUpETo <- function(id){
  moduleServer(id,function(input,output,session){
    TableInput <- reactive({
      req(input$file1)
      tryCatch(
        {
          df <- read.table(input$file1$datapath,
                           header = T,
                           sep = input$sep,
                           quote = input$quote,
                           #skip=input$skiplines,
                           dec = input$dec,
                           check.names = F,
                           na.strings = input$na.values)
          
          colnames(df) <- utf8::utf8_encode(colnames(df))
          
          if(input$janitor){
            df <- janitor::clean_names(df)
          }
          
          df <- df[,colSums(is.na(df))<nrow(df)]
          
          df[1] <-
            as.Date(strptime(col_string(data = df),
                             input$dateFormats))
          
          return(df)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        },
        warning =function(e){
          stop(safeError(e))
        },
        finally = function(e){
          stop(safeError(e))
        }
      )
    })
    
    observeEvent(tryCatch(
      expr = {nrow(TableInput()) > 0},
      error = function(e) {
        if(is.null(input$file1)){
          pass = 1
        }else{
          showNotification("Ocorreu um erro na tabela de entrada.", type = "error")
        }
      }
    ),{
      tryCatch(
        expr = {if (nrow(TableInput()) > 0) {
          js$enableSubMenu('proc_evapo')
        }},
        error = function(e) {
          pass = 1
        }
      )
    })
    
    output$table_evapo <- renderReactable({
      pers_reactable(TableInput())
    })
    
    return(TableInput)
  })
}

