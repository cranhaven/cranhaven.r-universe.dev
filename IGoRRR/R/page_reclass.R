
### Changes the type of columns of the current table
###   Dependencies in generated code: dplyr.

page_reclass <- list(

  ui = function() ..ui(page="reclass", control=TRUE),


  server = function(input, output, session) {

    ..vServer(input,output,"reclass")

    output$reclass.control <- renderUI({
      .IGoR$state$meta
      output$reclass.comment <- renderText("")
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6, ..select.ui("reclass",buttons.title=..s2(.IGoR$Z$reclass$reclass))),
          column(width=6,
            ..load.ui("reclass",input$main.data),
            box(width='100%',
              column(width=6,
                selectizeInput("reclass.class.out",..s2(.IGoR$Z$reclass$class),
                               choices=c('' %>% {names(.)<- .IGoR$Z$reclass$class.none;.},
                                       ..Znames("reclass","class",c("factor","as.character","as.double","as.integer","as.logical","as.Date")))
              ) ),
              column(width=6,
              uiOutput("reclass.parm"),
               checkboxInput("reclass.short",..s5(.IGoR$Z$reclass$short),TRUE)
        ) ) ) )
    })

    output$reclass.parm <- renderUI(
      if (..isEQ(input$reclass.class.out,"factor")
        &&!is.null(input$reclass.type)
        &&((input$reclass.type!=2)
         ||((input$reclass.type==2)
          &&((..isEQ(input$reclass.class,"character")&&!input$reclass.drop)
           ||(..isNE(input$reclass.class,"character")&&input$reclass.drop)))))
        checkboxInput("reclass.empty",..s5(.IGoR$Z$reclass$empty),TRUE)
      else
      if (..isEQ(input$reclass.class.out,"as.Date"))
        selectizeInput("reclass.format",..s2(.IGoR$Z$reclass$format), choices=..Zitems("reclass","formats"))
    )

    output$reclass.command2 <- renderUI(
      ..textarea("reclass", "mutate(column=as...(column))", 4,
        if (!is.null(input$reclass.type)&&..isNotEmpty(input$reclass.class.out)) {
          na <- if ((input$reclass.class.out=="factor")
                  &&((input$reclass.type!=2)
                   ||((input$reclass.type==2)
                    &&((..isEQ(input$reclass.class,"character")&&!input$reclass.drop)
                     ||(..isNE(input$reclass.class,"character")&&input$reclass.drop))))
                  &&..isTRUE(input$reclass.empty)) ", exclude=''" else ""
          ft <- if (..isEQ(input$reclass.class.out,"as.Date")&..isNE(input$reclass.format,"standard"))
            glue(", format=\"{input$reclass.format}\"")
          ..command2(
            "mutate",
            if (!..isTRUE(input$reclass.short)) {
              old <- ..name(..select.columns(input,output,"reclass"))
              if (length(old)==0) paste0("() # ",.IGoR$Z$reclass$nop)
              else glue("({..collapse0(paste0(old,'=',input$reclass.class.out,'(',old,na,ft,')'))})")
            }
            else
              paste0(
                if (input$reclass.type==2)
                  if (input$reclass.drop)
                       glue("_if(Negate(is.{input$reclass.class}), ")
                  else glue("_if(is.{input$reclass.class}, ")
                else
                if (input$reclass.type==3)
                  if (input$reclass.drop)
                       "_at(c(), "
                  else "_all("
                else glue("_at({..select(input,'reclass',vars=TRUE)}, "),
                input$reclass.class.out,
                na,
                ft,
                ")"
          )   )
        }
    ) )

    observeEvent(input$reclass.command2,
      ..try(input,output,"reclass",
        function(x) {
          a <- Map(class, ..data(input))
          a <- data.frame(names=names(a), classes=I(a))
          b <- Map(class, x)
          b <- data.frame(names=names(b), classes=I(b)) # may be longer than a if names are not reused
          d <- merge(a,b, by="names", all.y=TRUE)
          e <- mapply(identical, d$classes.y, d$classes.x)
          m <- length(e[!e])  # Changed columns
          if (m==0) .IGoR$Z$reclass$msg.nop
          else {
            f <- mapply(
                   function(old,new) ("factor" %in% old)                     # possibly length(old)>1
                                    &(new %not in% c("factor","character")), # allways length(new)==1
                   d$classes.x[!e], d$classes.y[!e]
                 )
            n <- length(f[f])   # Changed columns from factor to not factor nor character
            paste0(
              sprintf(.IGoR$Z$reclass$msg.result,m),
              if (n>0) paste0("\n",.IGoR$Z$reclass$msg.factor)
            )
          }
        }
    ) )

  }
)

