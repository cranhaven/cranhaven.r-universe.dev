
### View the columns of the current table
###   Dependencies on specific packages: none.
###   No generated code.

page_contents <- list(

  ui = function() ..ui(page="contents", command=FALSE,
    fluidRow(
      column(width=2,radioButtons("contents.sort",..s2(.IGoR$Z$contents$sort),unname(.IGoR$Z$contents$columns))),
      column(width=10,tableOutput("contents"))
  ) ),


  server = function(input, output, session) {

    output$contents <- renderTable({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)) {
        df <- ..data(input)
        f <- Vectorize(function(nom) attr(df[[nom]],"label") %>% ifelse(is.null(.),NA,.))
        dt <- data.frame(seq_along(df),
                         colnames(df),
                         unlist(Map(function (x) toString(class(x)), df), use.names=FALSE),
                         f(colnames(df)))
        names(dt) <- .IGoR$Z$contents$columns
        dt[order(dt[[input$contents.sort]]),]
      }
    })
  }
)
