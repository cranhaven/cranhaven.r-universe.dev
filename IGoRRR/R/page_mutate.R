
### Add or create a column in a copy of the current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr, stringr, lubridate, zoo (weak).

page_mutate <- list(

  ui = function() ..ui(page="mutate", control=TRUE),


  server = function(input, output, session) {

    ..aServer(input,output,"mutate")

    # menuId   : menu result
    # menuName : initial name for menu, will be converted into plain text using the corresponding JSON entry
    # fun      : function to apply, - means specific handling in command2
    # arg0Type : input type
    # args     : additional args count
    # argsType : additional args type (- for any or not applicable, c)haracter, n)numeric), D)ate)
    # arg1     : title of 2nd arg field
    # arg2     : title of 3nd arg field
    # groups   : group_by meaningful?
    # narm     : -: not meaningful else default value
    functions <- read.table(text=
      "menuName     fun          arg0Type  args argsType arg1 arg2 groups narm
       identity     -            any       0    -        -    -    F      -
       first        first        any       0    -        -    -    T      -
       last         last         any       0    -        -    -    T      -
       lag          lag          any       0    -        -    -    T      -
       lead         lead         any       0    -        -    -    T      -
       na.locf      zoo::na.locf any       0    -        -    -    T      T
       iconv        iconv        character 1    c        <    -    F      -
       str_to_upper str_to_upper character 0    -        -    -    F      -
       str_to_lower str_to_lower character 0    -        -    -    F      -
       str_sub2     str_sub      character 2    n        <    |    F      -
       str_sub1     str_sub      character 1    n        <    -    F      -
       str_extract  str_extract  character 1    c        ?    -    F      -
       str_replace  str_replace  character 2    c        ?    =    F      -
       str_split    str_split    character 1    c        ?    -    F      -
       str_detect   str_detect   character 1    c        ?    -    F      -
       str_length   str_length   character 0    -        -    -    F      -
       ifelse       -            character 2    c        <    >    F      -
       coalesce     coalesce     character 1    c        =    -    F      -
       min          min          numeric   0    -        -    -    T      F
       max          max          numeric   0    -        -    -    T      F
       sum          sum          numeric   0    -        -    -    T      F
       mean         mean         numeric   0  -          -    -    T      F
       quantile     quantile     numeric   1    n        x    -    T      F
       ifelse       -            numeric   2    n        <    =    F      -
       coalesce     coalesce     numeric   1    n        =    -    F      -
       sprintf      -            numeric   1    c        x    -    F      -
       min          min          Date      0    -        -    -    T      F
       max          max          Date      0    -        -    -    T      F
       year         year         Date      0    -        -    -    F      -
       month        month        Date      0    -        -    -    F      -
       day          day          Date      0    -        -    -    F      -
       wday         wday         Date      0    -        -    -    F      -"
      ,header=TRUE, na.strings='-', stringsAsFactors=FALSE) %>%
      mutate(menuId=row_number())

    functionsMenu <- function(class) {
      f <- function(class) {
        df <- functions %>% filter(arg0Type %in% class)
        l <- df$menuId
        names(l) <- df$menuName
        ..Zrename(l)

      }

      if (class=="integer") class <- "numeric"
      # NB: First entry is empty, not 'none.fun'
      ..Zrename(list(none=c(none.fun=''),head.any=f("any"),head.class=f(class)))
    }

    fun.name     <- function() functions$menuName[as.integer(input$mutate.fun)]
    fun.fun      <- function() functions$fun[as.integer(input$mutate.fun)]
    fun.type     <- function() functions$arg0Type[as.integer(input$mutate.fun)]
    fun.args     <- function() functions$args[as.integer(input$mutate.fun)]
    fun.argsType <- function() functions$argsType[as.integer(input$mutate.fun)]
    fun.arg1     <- function() functions$arg1[as.integer(input$mutate.fun)]
    fun.arg2     <- function() functions$arg2[as.integer(input$mutate.fun)]
    fun.groups   <- function() functions$groups[as.integer(input$mutate.fun)]
    fun.narm     <- function() functions$narm[as.integer(input$mutate.fun)]

    output$mutate.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        tagList(
          fluidRow(
            column(width=6,
              box(width='100%',
                column(width=6, textInput("mutate.new", ..s2(.IGoR$Z$any$new.col), "mutate.new")),
                column(width=6, textInput("mutate.label", ..s3(.IGoR$Z$mutate$label), ''))
            ) ),
            column(width=6, ..load.ui("mutate", out=input$main.data))
          ),
          box(width='100%',
            column(width=3, ..expr.type.ui("mutate")),
            column(width=9, uiOutput("mutate.expr.what"))
        ) )
    })

    output$mutate.expr.what <- renderUI(
      if (length(input$mutate.type)>0)
             if (input$mutate.type==0) textInput("mutate.expr",..s1(.IGoR$Z$mutate$expr))
        else if (input$mutate.type==1)
          fluidRow(
            column(width=4),
            column(width=4,uiOutput("mutate.group"))
          )
        else if (input$mutate.type==2)
          tagList(
            fluidRow(
              column(width=4, selectizeInput("mutate.old", ..s1(.IGoR$Z$any$var), ..column(input))),
              column(width=4, uiOutput("mutate.group"))
            ),
            fluidRow(
              column(width=4, uiOutput("mutate.fun")),
              column(width=3, uiOutput("mutate.arg1")),
              column(width=3, uiOutput("mutate.arg2")),
              column(width=2,
                uiOutput("mutate.pipe"),
                uiOutput("mutate.narm")
              )
          ) )
        else if (input$mutate.type==3)
          tagList(
            column(width=4, selectizeInput("mutate.old1", ..s1(.IGoR$Z$any$var), ..column(input))),
            column(width=4,
              selectizeInput("mutate.op", ..s1(.IGoR$Z$any$operator),
                choices=..Zrename(c(none.op='',
                                       plus="+",
                                      minus="-",
                                      times="*",
                                  dividedBy="/",
                                        and="&",
                                         or="|",
                                       isEQ="==",
                                       isNE="!=",
                                       isGT=">",
                                       isGE=">=",
                                   pastedTo=" paste0"))
            ) ),
            column(width=4, selectizeInput("mutate.old2", ..s1(.IGoR$Z$any$var), ..column(input)))
          )
    )

    output$mutate.fun <- renderUI(
      if (..isEQ(input$mutate.type,2)&&..isNotEmpty(input$mutate.old))
        selectizeInput("mutate.fun", ..s1(.IGoR$Z$any$fun),
                        choices=functionsMenu(class(..data(input)[[input$mutate.old]]))
        )
    )

    output$mutate.arg1 <- renderUI(
      if (..isEQ(input$mutate.type,2)&&..isNotEmpty(input$mutate.old))
        if (..isNotEmpty(input$mutate.fun)&&(fun.args()>0))
          if (fun.argsType()=="c")
            textInput("mutate.chr.arg1", ..s2(
                   if (fun.arg1()=="<") .IGoR$Z$any$from
              else if (fun.arg1()=="=") .IGoR$Z$any$by
              else if (fun.arg1()=="?") .IGoR$Z$any$prx
              else ""),
              switch(fun.name(),
                iconv="850",
                sprintf="<%5d>"
            ) )
          else
            numericInput("mutate.num.arg1", ..s2(
                   if (fun.arg1()=="<") .IGoR$Z$any$from
              else if (fun.arg1()=="=") .IGoR$Z$any$by
              else ""),
              switch(fun.name(),
                quantile=.5,
                ifelse=,
                coalesce=0
            ) )
    )

    output$mutate.arg2 <- renderUI(
      if (..isEQ(input$mutate.type,2)&&..isNotEmpty(input$mutate.old))
        if (..isNotEmpty(input$mutate.fun)&&(fun.args()>1))
          if (fun.argsType()=="c")
            textInput("mutate.chr.arg2", ..s2(
                   if (fun.arg2()=="=") .IGoR$Z$any$by
              else if (fun.arg2()==">") .IGoR$Z$any$into
              else ""),
              if (substring(input$mutate.fun,6)=="iconv") "UTF-8"
            )
          else
            numericInput("mutate.num.arg2", ..s2(
                   if (fun.arg2()=="=") .IGoR$Z$any$by
              else if (fun.arg2()==">") .IGoR$Z$any$into
              else if (fun.arg2()=="|") .IGoR$Z$any$to
              else ""),
              NULL
            )
    )

    output$mutate.pipe <-renderUI(
      if (..isNotEmpty(input$mutate.old)&&..isNotEmpty(input$mutate.fun)&&(fun.name()!="identity"))
        checkboxInput("mutate.pipe", ..s5(.IGoR$Z$mutate$pipe), TRUE)
    )

    output$mutate.narm <- renderUI(
      if (..isNotEmpty(input$mutate.old)&&..isNotEmpty(input$mutate.fun)&&!is.na(fun.narm()))
        checkboxInput("mutate.narm", ..s5(.IGoR$Z$any$na.rm),TRUE)
    )

    output$mutate.group <- renderUI(
      if (!is.null(input$mutate.type))
        if (((input$mutate.type==0)&&..isNotEmpty(input$mutate.expr))
          || (input$mutate.type==1)
          ||((input$mutate.type==2)&&..isNotEmpty(input$mutate.old)&&..isNotEmpty(input$mutate.fun)&&fun.groups())
           ) ..group.ui(input,"mutate", box=FALSE)
    )

    label <- function() paste0('{',glue("attr(.${..name(input$mutate.new)},'label')<- {shQuote(input$mutate.label)}"),'; .}')

    output$mutate.command2 <- renderUI(
      ..textarea("mutate", "mutate(column=expression)", 3,
        if (!is.null(input$mutate.type)
          &&(((input$mutate.type==0)&&..isNotEmpty(input$mutate.expr))
           || (input$mutate.type==1)
           ||((input$mutate.type==2)&&..isNotEmpty(input$mutate.fun)&&..isNotEmpty(input$mutate.old))
           ||((input$mutate.type==3)&&..isNotEmpty(input$mutate.old1)&&..isNotEmpty(input$mutate.op)&&..isNotEmpty(input$mutate.old2))
           ))
          if (!is.null(input$mutate.label)
            &&(((input$mutate.type==0)&&(input$mutate.new==str_squish(input$mutate.expr)))
             ||((input$mutate.type==2)&&(fun.name()=="identity")&&(input$mutate.old==input$mutate.new))
             ))
            ..command2(label())
          else
            ..command2(
              if ((input$mutate.type==1)|((input$mutate.type==2)&&fun.groups())) ..group_by(input,"mutate"),
              "mutate(",
              ..name(input$mutate.new),
              " = ",
                   if (input$mutate.type==0) input$mutate.expr
              else if (input$mutate.type==1) "row_number()"
              else if (input$mutate.type==2) {
                old <- ..name(input$mutate.old)
                if (fun.name()=="identity") old
                else
                if (fun.name()=="ifelse")
                  if ((fun.type()=="character")&&!is.null(input$mutate.chr.arg1)&&!is.null(input$mutate.chr.arg2))
                    if (..isTRUE(input$mutate.pipe))
                         glue("{old} %>% ifelse(.==\"{input$mutate.chr.arg1}\",\"{input$mutate.chr.arg2}\",.)")
                    else glue("ifelse({old}==\"{input$mutate.chr.arg1}\",\"{input$mutate.chr.arg2}\",{input$mutate.old})")
                  else
                  if ((fun.type()=="numeric")&&!is.null(input$mutate.num.arg1)&&!is.null(input$mutate.num.arg2))
                    if (..isTRUE(input$mutate.pipe))
                         glue("{old} %>% ifelse(.=={input$mutate.num.arg1},{input$mutate.num.arg2},.)")
                    else glue("ifelse({old}=={input$mutate.num.arg1},{input$mutate.num.arg2},{input$mutate.old})")
                  else ""
              else
              if (fun.name()=="sprintf")
                if (!is.null(input$mutate.chr.arg1))
                  if (..isTRUE(input$mutate.pipe))
                       glue("{old} %>% sprintf(\"{input$mutate.chr.arg1}\",.)")
                  else glue("sprintf(\"{input$mutate.chr.arg1}\",{old})")
                else ""
              else
                paste0(
                  if (..isTRUE(input$mutate.pipe))
                       paste0(old," %>% ",fun.fun(),'(')
                  else paste0(fun.name(),'(',old),
                  if (fun.args()>0)
                    paste0(
                      if (..isTRUE(input$mutate.pipe)) '' else ',',
                      if ((fun.argsType()=="c")&&!is.null(input$mutate.chr.arg1))
                        paste0('"',input$mutate.chr.arg1,'"')
                      else
                      if ((fun.argsType()=="n")&&!is.null(input$mutate.num.arg1))
                        input$mutate.num.arg1,
                      if (fun.args()>1)
                        if ((fun.argsType()=="c")&&!is.null(input$mutate.chr.arg2))
                          paste0(',"',input$mutate.chr.arg2,'"')
                        else
                        if ((fun.argsType()=="n")&&!is.null(input$mutate.num.arg2))
                          paste0(',',input$mutate.num.arg2)
                    ),  # 2nd paste0
                 if (!is.na(fun.narm())&&..isNE(input$mutate.narm,fun.narm()))
                    paste0(
                      if (..isTRUE(input$mutate.pipe)&&(fun.args()==0)) '' else ', ',
                      glue("na.rm={input$mutate.narm}")
                    ),
                ')'
                )
              }
              else if (input$mutate.type==3)
                if (substr(input$mutate.op,1,1)==" ")
                     glue("{substring(input$mutate.op,2)}({..name(input$mutate.old1)},{..name(input$mutate.old2)})")
                else glue("{..name(input$mutate.old1)} {input$mutate.op} {..name(input$mutate.old2)}"),
              ')',
              if ((input$mutate.type==1)|((input$mutate.type==2)&&fun.groups()))..ungroup(input,"mutate"),
              if (..isNotEmpty(input$mutate.label)) paste0(NL,label())
    ) )   )

    # BUG: This is not correct if the name of column is directly modified in command box
    observeEvent({.IGoR$state$meta; input$mutate.command2},
      ..try(input,output,"mutate",
        function(x)
          paste(
            if (input$mutate.new %not in% ..columns(input$main.data)) ""
            else sprintf(.IGoR$Z$mutate$msg.duplicated,input$mutate.new),
            sprintf(.IGoR$Z$mutate$msg.result,input$mutate.new,class(x[[input$mutate.new]])),
            sep='\n'
    ) )   )

  }
)

