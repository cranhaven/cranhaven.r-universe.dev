#' gradientPickerD3_example
#'
#' Creates an example shiny app which include the gradientPickerD3 and a rendered table for gradientPickerD3 return value. By clicking the reload button new random ticks will be generated.
#'
#' @import shiny stats
#' @export
gradientPickerD3_example = function() {
  shinyApp(ui <- fluidPage(
    div(
      style = "width: 300px;",
      
      actionButton('btreload', 'reload'),
      gradientPickerD3Output('gpD3'),
      br(),
      tableOutput("text")
    )
    
  ),
  
  shinyServer(function(input, output) {
    payload <- list(
      colors = c("purple", "blue", "green", "yellow", "red"),
      ticks = c(-5, -2, 0, 2, 5)
    )
    output$gpD3 <- renderGradientPickerD3(gradientPickerD3(payload))
    
    observeEvent(input$btreload, {
      payload <- list(
        colors = c("purple", "blue", "green", "yellow", "red"),
        ticks = c(-5, stats::runif(1, -4, -1), 0, stats::runif(1, 1, 4), 5)
      )
      output$gpD3 <-
        renderGradientPickerD3(gradientPickerD3(payload))
      
    })
    
    observeEvent(input$gpD3_table, {
      output$text <- renderTable({
        df <-
          as.data.frame(matrix(
            unlist(input$gpD3_table),
            ncol = 3,
            byrow = TRUE
          ))
        colnames(df) <-  c('position', 'color', 'tick')
        return(df)
      })
    })
  }))
  
  
}
