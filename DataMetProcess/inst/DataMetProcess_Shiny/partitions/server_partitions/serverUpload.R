serverUpload <- function(id){
  moduleServer(id,function(input,output,session){

    output$skiplines <- renderUI({
      numericInput(NS(id,"skiplines"),
                   "Ignored lines",
                   value=8,
                   min=0,
                   step = 1)
    })
    output$help_skip <- renderUI({
      helpText("Note: Number of lines skipped before starting to read the data (Inmet's default is 8). Set to 0 if the file starts directly with the header.")
    })

    TableInput <- reactive({
      req(input$file1)
      tryCatch(
        {
          df <- read.table(input$file1$datapath,
                           header = T,
                           sep = input$sep,
                           skip=input$skiplines,
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

          df <- adjustDate(
            data = df,
            col_date = colnames(df)[1],
            col_hour = colnames(df)[2],
            fuso = input$fuso
          )

          if(colSums(is.na(df[1]))==nrow(df)){
            stop("Error in date column")
          }
          df <-
            df %>%
            mutate_if(where((function(x)
              is.character(x) &&
                ! all((suppressWarnings(
                  is.na(as.numeric(x))))))),
              as.numeric)

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
          showNotification("An error occurred in the input table!", type = "error")
        }
      }
    ),{
      tryCatch(
        expr = {if (nrow(TableInput()) > 0) {
          js$enableSubMenu('process')
        }},
        error = function(e) {
          pass = 1
        }
      )
    })

    output$table_padrao <- renderReactable({
      reactable(
        mutate(TableInput(),!!colnames(TableInput())[1] :=
                 as.character(eval(parse(text = paste0(colnames(TableInput())[1]))))),
        resizable = T,
        filterable = TRUE,
        showPageSizeOptions = T,
        defaultColDef = colDef(
          #minWidth = 150,
          style = list(border = "none"),
          align = "center",
          headerStyle = list(border = "none")
        ),
        wrap = F,
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
  }")
      )
    })

    return(TableInput)

  })
}
