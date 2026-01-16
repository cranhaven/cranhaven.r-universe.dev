
### Update a set of columns in a copy of the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr, stringr, zoo (weak).

page_mutate2 <- list(

  ui = function() ..ui(page="mutate2", control=TRUE),


  server = function(input, output, session) {

    ..vServer(input,output,"mutate2")

    output$mutate2.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6, ..select.ui("mutate2", buttons.title=..s2(.IGoR$Z$mutate2$mutate2))),
          column(width=6,
            ..load.ui("mutate2",input$main.data),
            uiOutput("mutate2.how")
        ) )
    })

    classes <- function(input)
      ..data(input) %>% head(1) %>%
      select_at(..select.columns(input,output,"mutate2")) %>%
      Map(class, .)

    output$mutate2.how <- renderUI(
      if (!is.null(input$mutate2.type)) {
        v <- unique(classes(input))
        box(width='100%',
          column(width=4,
                 if (length(v)==0) tags$hr(.IGoR$Z$mutate2$msg.nop)
            else if (length(v)>1)  tags$hr(.IGoR$Z$mutate2$msg.mixed)
            else
              selectizeInput("mutate2.fun",..s1(.IGoR$Z$any$fun),
                choices=..Zrename(c(none.fun='',
                  if (v=="character")
                             c(iconv="c2c>>iconv",
                        str_to_upper="c0 : str_to_upper",
                        str_to_lower="c0 : str_to_lower",
                            str_sub2="c2n--str_sub",
                            str_sub1="c1n- str_sub",
                         str_extract="c1c< str_extract",
                         str_replace="c2c<=str_replace",
                          str_detect="c1c< str_detect",
                          str_length="c0 : str_length",
                            coalesce="p1c= coalesce")
            else if (v %in% c("numeric","integer"))
                          c(coalesce="p1n= coalesce",
                             .negate="r0 : funs(`-`)")
            else if (v=="logical")
                               c(not="r0 : funs(`!`)")
            else
                      c(as.character="f0 : as.character"),
                             na.locf="r0 : na.locf"))
          ) ),
          column(width=4,uiOutput("mutate2.arg1")),
          column(width=4,uiOutput("mutate2.arg2"))
        )
      }
    )

    output$mutate2.arg1 <- renderUI(
      if (!is.null(input$mutate2.type)&&(length(unique(classes(input)))==1))
        if (!is.null(input$mutate2.fun)&&(substr(input$mutate2.fun,2,2)>0))
          if (substr(input$mutate2.fun,3,3)=="c")
            textInput("mutate2.chr.arg1", ..s2(
                         if (substr(input$mutate2.fun,4,4)==">") .IGoR$Z$any$from
                    else if (substr(input$mutate2.fun,4,4)=="=") .IGoR$Z$any$by
                    else if (substr(input$mutate2.fun,4,4)=="<") .IGoR$Z$any$prx
                    else ""),
                    switch(substring(input$mutate2.fun,6),
                           iconv="850",
                           sprintf="<%5d>"
            )       )
          else
            numericInput("mutate2.num.arg1", ..s2(
                        if (substr(input$mutate2.fun,4,4)=="-") .IGoR$Z$any$from
                   else if (substr(input$mutate2.fun,4,4)=="=") .IGoR$Z$any$by
                   else ""),
                   switch(substring(input$mutate2.fun,6),
                          quantile=.5,
                          coalesce=0
            )     )
    )

    output$mutate2.arg2 <- renderUI(
      if (!is.null(input$mutate2.type)&&(length(unique(classes(input)))==1))
        if (!is.null(input$mutate2.fun)&&(substr(input$mutate2.fun,2,2)>1))
          if (substr(input$mutate2.fun,3,3)=="c")
            textInput("mutate2.chr.arg2", ..s2(
                         if (substr(input$mutate2.fun,5,5)==">") .IGoR$Z$any$into
                    else if (substr(input$mutate2.fun,5,5)=="=") .IGoR$Z$any$by
                    else ""),
                    if (substring(input$mutate2.fun,6)=="iconv") "UTF-8"
            )
          else
            numericInput("mutate2.num.arg2", ..s2(
                   if (substr(input$mutate2.fun,5,5)=="-") .IGoR$Z$any$to else ""),
                   NULL
           )
    )

    output$mutate2.command2 <- renderUI(
      ..textarea("mutate2", "mutate...(...)", 3,
        if (!is.null(input$mutate2.type)&&..isNotEmpty(input$mutate2.fun)
          &&(length(unique(classes(input)))==1)
           ) {
          ..command2(
            "mutate",
            if (input$mutate2.type==2)
              if (input$mutate2.drop)
                   glue("_if(Negate(is.{input$mutate2.class}), ")
              else glue("_if(is.{input$mutate2.class}, ")
            else
            if (input$mutate2.type==3)
              if (input$mutate2.drop)
                   "_at(c(), "
              else "_all("
            else glue("_at({..select(input,'mutate2',vars=TRUE)}, "),
            substring(input$mutate2.fun,6),
            if (substr(input$mutate2.fun,2,2)>0)
              if (substr(input$mutate2.fun,3,3)=="c")
                paste0(', "',if (..isNotEmpty(input$mutate2.chr.arg1)) input$mutate2.chr.arg1 else "",'"')
              else
              if (substr(input$mutate2.fun,3,3)=="n")
                paste0(", ",if (..isNotNA(input$mutate2.num.arg1)) input$mutate2.num.arg1 else "NA"),
            if (substr(input$mutate2.fun,2,2)>1)
              if (substr(input$mutate2.fun,3,3)=="c")
                paste0(', "',if (..isNotEmpty(input$mutate2.chr.arg2)) input$mutate2.chr.arg2 else "",'"')
              else
              if (substr(input$mutate2.fun,3,3)=="n")
                paste0(", ",if (..isNotNA(input$mutate2.num.arg2)) input$mutate2.num.arg2 else "NA"),
            ")"
          )
        }
    ) )

    observeEvent(input$mutate2.command2,
      ..try(input,output,"mutate2",
        function (x) {
          v <- classes(input)
          if (length(unique(v))==1) sprintf(.IGoR$Z$mutate2$msg.result,length(v),v[[1]])
        }
    ) )

  }
)
