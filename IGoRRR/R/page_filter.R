
### Makes a table  form selected rows of the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr, stringr,

page_filter <- list(

  ui = function() ..ui(page="filter", control=TRUE),


  server = function(input, output, session) {

    ..aaServer(input,output,"filter", meta=FALSE)

    output$filter.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        tagList(
          fluidRow(
            column(width=6, box(width='50%', checkboxInput("filter.drop",..s4(.IGoR$Z$filter$drop), FALSE))),
            column(width=6, ..load.ui("filter"))
          ),
          box(width='100%',
            column(width=3, ..expr.type.ui("filter",..s2(.IGoR$Z$filter$filter))),
            column(width=3, uiOutput("filter.group")),
            column(width=6, uiOutput("filter.expr.what")
        ) ) )
    })

    output$filter.expr.what <- renderUI(
      if (!is.null(input$filter.type))
             if (input$filter.type==0) textInput("filter.where",..s1(.IGoR$Z$filter$where))
        else if (input$filter.type==1) numericInput("filter.no",..s2(.IGoR$Z$filter$no),1)
        else
          tagList(
            column(width=3,
              selectizeInput("filter.arg1",..s1(.IGoR$Z$any$var), choices=..column(input))
            ),
            column(width=5, uiOutput("filter.fun")),
            column(width=4, uiOutput("filter.arg2"))
    )     )

    funs <- read.table(header=TRUE, na.string='-', text =
"name   type stats operator arg2 fun
isEQ       b FALSE    TRUE     n ==
isTRUE     b FALSE       -     - -
and        b FALSE    TRUE     n &
or         b FALSE    TRUE     n |
isNA       b FALSE   FALSE     - is.na
isEQ       c FALSE    TRUE     c ==
belongsTo  c FALSE    TRUE     C %in%
startsWith c FALSE   FALSE     c startsWith
endsWith   c FALSE   FALSE     c endsWith
matches    c FALSE   FALSE     c str_detect
isNA       c FALSE   FALSE     - is.na
isEQ       n FALSE    TRUE     n ==
belongsTo  n FALSE    TRUE     N %in%
isGE       n FALSE    TRUE     n >=
isGT       n FALSE    TRUE     n >
isEQ.stat  n  TRUE    TRUE     f ==
isGT.stat  n  TRUE    TRUE     f >
isNA       n FALSE   FALSE     - is.na
isEQ       ? FALSE    TRUE     c ==
belongsTo  ? FALSE    TRUE     C %in%
isNA       ? FALSE   FALSE     - is.na") %>% mutate(id=row_number())

    funsMenu <- function(v,type) {
      t <- if (is.logical(v))   "b"
      else if (is.character(v)) "c"
      else if (is.numeric(v))   "n"
      else                      "?"
      a <- funs[funs$type==t,]
      if (type==3) a <- a[!a$stats & !is.na(a$arg2)]  # assisted between two columns
      b <- a$id
      names(b) <- a$name
      ..Zrename(b)
    }

    output$filter.fun <- renderUI(
      if (!is.null(input$filter.type)&&(input$filter.type>=2)
          &&..isNotEmpty(input$filter.arg1)) {
        c <- ..data(input)[[input$filter.arg1]]
        selectizeInput("filter.fun", ..s2(.IGoR$Z$any$operator),
          choices=funsMenu(c, input$filter.type)
        )
      }
    )

    output$filter.arg2 <- renderUI(
      if (!is.null(input$filter.type)&&(input$filter.type>=2)
          &&..isNotEmpty(input$filter.arg1)
          &&!is.null(input$filter.fun)) {
        fun <- as.integer(input$filter.fun)
        if (!is.na(funs$fun[fun])) {         # else no function expected
          t <- funs$arg2[fun]                # argument type
          if (!is.na(t))                     # else no argument expected
            if (input$filter.type==2)
              if (t=='f')
                selectizeInput("filter.arg2", ..s2(.IGoR$Z$filter$stat),
                  choices=..Zrename(c(mean="mean",
                                    median="median",
                                        q3="quantile,.75",
                                       p90="quantile,.90",
                                       max="max",
                                       min="min")))
              else
              if (t=='n')
                   numericInput("filter.arg2",..s2(.IGoR$Z$filter$value),0)
              else textInput("filter.arg2",if (t %in% c('C','N')) .IGoR$Z$filter$values else .IGoR$Z$filter$value)
            else
              if (input$filter.type==3)
                selectizeInput("filter.arg2",..s1(.IGoR$Z$any$var), choices=c(.IGoR$COLV,..columns(input$main.data)))
      } }
    )

    output$filter.group <- renderUI(
      if (!is.null(input$filter.type))
        if (((input$filter.type==0)&&..isNotEmpty(input$filter.where))
          || (input$filter.type==1)
          ||((input$filter.type==2)&&!is.null(input$filter.fun)
                                   &&..isEQ(funs$arg2[as.integer(input$filter.fun)],'f'))
         ) ..group.ui(input,"filter", box=FALSE)
        else
        if (length(input$filter.group)>0) shinyjs::hidden(textInput("filter.group","",""))
    )

    output$filter.command2<- renderUI(
      ..textarea("filter", "filter(condition)", 3,
        if (!is.null(input$filter.type))
          ..command2(
            ..group_by(input,"filter"),
            if ((input$filter.type==1)&&..isNotEmpty(input$filter.where)&&(nchar(msg<-..look(input$filter.where))>0))
              paste0(str_sub(msg,2),"\n   "),
            "filter(",
            {
              fun <- as.integer(input$filter.fun)
              f <- funs$fun[fun]  # function name
              drop <- ..isTRUE(input$filter.drop)
              arg1 <- ..name(input$filter.arg1)
              e <-   if ((input$filter.type==0)&&..isNotEmpty(input$filter.where)) # -- not assisted ----------------------
                       list(drop,TRUE,input$filter.where)
                     else
                     if ((input$filter.type==1)&&..isNotNA(input$filter.no))       #  -- row number -----------------------
                       list(FALSE,NA,glue("row_number(){if (drop) '!' else '='}={input$filter.no}"))
                     else
                     if ((input$filter.type>=2)                                    # -- not assisted ----------------------
                       &&..isNotEmpty(input$filter.arg1)
                       &&!is.null(input$filter.fun)
                       &&((input$filter.type==2)||..isNotEmpty(input$filter.arg2)))
                       if (is.na(f))                                          # - no function (logical column only) -------
                         list(drop,FALSE,arg1)
                       else {                                                 # - a function ------------------------------
                          a <- funs$arg2[fun]                                 # additional argument type
                         if (is.na(a)) list(drop,FALSE,glue("{f}({arg1})"))   # -- no argument ----------------------------
                         else {                                               # -- one argument ---------------------------
                           arg2 <- if (a=='f') {                              # --- argument is a statistical function ----
                             s <- str_split(input$filter.arg2,',')[[1]]
                             if (length(s)==1)
                                  glue("{s}({arg1})")
                             else glue("{s[1]}({arg1},{s[2]})")
                           } else if (input$filter.type==3) ..name(input$filter.arg2)
                             else input$filter.arg2
                           q <- if (input$filter.type==3) c('','')            # --- argument quoting ----------------------
                                else switch (a,
                                       c = c('"','"'),
                                       f =,
                                       n = c('',''),
                                       C =,
                                       N = c('c(',')'))
                           if (funs$operator[fun]) {                          # --- binary operator -----------------------
                             g <- if (drop) switch(f, "=="="!=", ">"="<=", ">="="<")
                             if (is.null(g))
                                  list(drop,TRUE,glue("{arg1} {f} {q[1]}{arg2}{q[2]}"))
                             else list(FALSE,NA, glue("{arg1} {g} {q[1]}{arg2}{q[2]}"))
                           } else                                              # --- function call -------------------------
                             list(drop,FALSE,glue("{f}({arg1},{q[1]}{arg2}{q[2]})"))
                      } } # e
              if (length(e)>0)                                           # negate result if required  ----------------
                if (e[[1]])
                  if (e[[2]]) glue("!({e[[3]]})") else glue("!{e[[3]]}")
              else e[[3]]
            },
            ')',
            ..ungroup(input,"filter")
    ) )   )

  }
)

