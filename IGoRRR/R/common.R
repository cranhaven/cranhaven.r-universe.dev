## Since 3.5, as_tibble clears class skim_df when it exists
as_tibble <- function(.data) if ("tbl_df" %in% class(.data)) .data else tibble::as_tibble(.data)


# Common data, initiated by init(), with some parts of it updated here
.IGoR <- new.env()


..image <- function(file) system.file("images", file, package="IGoRRR")


# Utilities for national language conversions -----------------------------

## Extract a list of sub items for an item of a page
..Zitems <- function(page, section) {
  l <- unlist(.IGoR$Z[[page]][[section]])
  setNames(names(l),l)
}

## Renames a list of items
..Znames <- function(page, item, items) {
  names(items)<- Vectorize(function(x) .IGoR$Z[[page]][[paste0(item,'.',x)]])(items)
  items
}

## Rename a list of functions
..Zrename <- function(items) {
  names(items)<- Vectorize(function (x) .IGoR$Z$any$fun.name[[x]])(names(items))
  items
}


# Utilities for table management ------------------------------------------

## Get the current table
..data <- function (input) get(input$main.data, envir=.IGoR$env)

## Get the list of tables from the global environment
..tables <- function () {
  t <- Vectorize(function(x) {
    y <- attr(get(x,.IGoR$env),"created")
    if (is.null(y)) 0 else y
  })
  
  l <- ls(envir=.IGoR$env)
  l <- unlist(Filter(function(x) is.data.frame(get(x,envir=.IGoR$env)), l))
  if (length(l)==0) l  # t' returns a list not a vector in that case!
  else l[order(t(l))]
}


## Update the list of tables known by IGoR
## Display the 'current table' menu
..renderTables <- function(input,output)
  output$main.data <- renderUI({
    .IGoR$tables <-..tables()
    selectizeInput("main.data", label = .IGoR$Z$all$main.data,
                   selected=if (length(input$main.data)>0) input$main.data,
                   choices = .IGoR$tables
    )
} )

## Update the list of know tables
## New tables are allways put at the end of list
..newTable <- function(input,output,
                       .table,
                       .select=FALSE) {
  a <- if (.table %in% .IGoR$tables) "updated" else "created"
  eval(parse(text=glue("attr({.table},'{a}')<- Sys.time()")),envir=.IGoR$env)
  if (a=="created") {
    .IGoR$tables <- ..tables()
    .IGoR$state$list <- Sys.time()
    output$main.data <- renderUI(
      selectizeInput("main.data", .IGoR$Z$all$main.data,
                     selected=if (.select) .table else input$main.data,
                     choices = .IGoR$tables
      )             )
} }

## Get the list of columns matching a given type from the current table
..columns <- function (.table,.class,.sort=TRUE) {
  .table <- get(.table, envir=.IGoR$env)
  if (!missing(.class)&&(length(.class)==1))
    if (.class=="numeric")  .class <- c("numeric","integer","Date")
  else if (.class=="discrete") .class <- c("factor","character","integer","logical","Date")
  else if (.class=="double")   .class <- "numeric"
  c <- names(if (missing(.class)) .table
             else {
               a <- Map(class,.table)
               b <- Map(function(x) if (length(intersect(x,.class))>0) x, a)
               Filter(Negate(is.null), b)
             }
  )
  if (.sort) c <- sort(c)
  names(c) <- c
  c
}

..column   <- function(input) c(.IGoR$COLV,..columns(input$main.data))
..discrete <- function(input,none=.IGoR$QALCOLV) c(none,..columns(input$main.data,"discrete"))
..numeric  <- function(input,none=.IGoR$NUMCOLV) c(none,..columns(input$main.data,"numeric"))



# Log management
..writeLog <- function(page,command,append=TRUE) {
  time <- Sys.time()
  .IGoR$log <-
      if (!append) data.frame(time=time,page=page,command=command,stringsAsFactors = FALSE)
      else  add_row(.IGoR$log,time=time,page=page,command=command)
}


# Syntax tools ------------------------------------------------------------

..look <- function(text) {
  x <- tryCatch({ z <- getParseData(parse(text=text, keep.source=TRUE))
                  z[z$token %in% c("OR2","AND2","EQ_ASSIGN"),"text"]
                },
                error=identity)
  if ((length(x)>0)&&!..isCondition(x)) sprintf(.IGoR$Z$all$msg.warning,..collapse(x)) else ""
}

## Widgets within dropdownButtons are not refreshed until they are shown,
## so the input files may conserve a value entered before the data table has changed and may lead to inconsistence.
## To bypass, we remember the name of the table and the columns change state when creating the widgets.
##  see the 'browse' page for examples of use.
..do.sync <- function (input,id) {
  .IGoR$sync[[id]] <- list(input$main.data, .IGoR$state$meta)
  id                                   # meant to be used as widget id
}
..is.sync <- function (input,id)
  identical(.IGoR$sync[[id]], list(input$main.data, .IGoR$state$meta))

..if.sync <- function (input,id) {
  x <- input[[id]]                 # in case of not refreshed, will cause to refresh
  if (..is.sync(input,id)) x       # if not refreshed returns NULL else its value
}

# server tools ------------------------------------------------------------

## Minimal server : command box with 1st line, create/replace button
..aServer <- function(input,output,page,
                      meta=TRUE) {         # will change table meta data?
  p <- function(item) paste0(page,".",item)
  i <- function(item) input[[p(item)]]
  
  ..rLogo(input,output,page)

  output[[p('command1')]] <- renderText(
    if (..isNotEmpty(input$main.data))
      .IGoR[[p('command1')]] <- glue("{make.names(i('out'))} <- {input$main.data} %>%")
  )

  observeEvent(i('load'),  isolate(..do2(input,output,page,meta=meta)))
}

## Minimal server with execution of command box
..aaServer <- function(input,output,page,
                       meta=TRUE)         # will change table meta data?
{
  ..aServer(input,output,page, meta)

  observeEvent({input[[paste0(page,".command2")]]; .IGoR$state$meta},
               ..try(input,output,page)
  )
}

## Minimal server with column selection
..vServer <- function(input,output,page) {

  ..aServer(input,output,page)

  ..output.select.what(input,output,page)
}

## Minimal server for pages that don't create a table
..bServer <- function(input,output,page) {
  p <- function(item) paste0(page,".",item)
  
  ..rLogo(input,output,page)

  output[[p('command1')]] <- renderText(
    if (..isNotEmpty(input$main.data))
      .IGoR[[p('command1')]] <- glue("{input$main.data} %>%")
  )
}

## Minimal server for joining tables ('join', 'fuzzyjoin')
..jServer <- function (input,output,page) {
  p <- function(item) paste0(page,".",item)

  ..aServer(input,output,page)

  output[[p('data')]]<- renderUI({
    .IGoR$state$list
    fluidRow(
      column(width=6, selectizeInput(p('data'), ..s1(.IGoR$Z$all$join.data), choices=c(.IGoR$TABLE,..tables()))),
      column(width=6, uiOutput(p('columns2')))
    )
  })

  output[[p('columns2')]]<- renderUI(
    if (..isNotEmpty(input[[p('data')]]))
      selectizeInput(p('columns2'), ..s1(.IGoR$Z$all$join.keys),
                     multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                     choices = ..columns(input[[p('data')]],"discrete")
  )  )

  output[[p('columns')]]<- renderUI({
    .IGoR$state$meta
    if (..isNotEmpty(input$main.data))
      fluidRow(
        column(width=6, ..s2(.IGoR$Z$all$main.data)),
        column(width=6, selectizeInput(p('columns'), ..s1(.IGoR$Z$all$join.keys),
                                       multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                                       choices = ..columns(input$main.data,"discrete")
        )     )         )
  })

}

## Minimal server for graphics
## Display is refreshed every time the command text change or the data change
..gServer <- function(input,output,page) {
  p <- function(item) paste0(page,".",item)
  i <- function(item) input[[p(item)]]

  ..bServer(input,output,page)
  
  output[[p('.plot')]] <- renderUI({
    h <- i('height')*100
    if (is.na(h)||(h<100)) h <- 400
    plotOutput(p('plot'), height=sprintf("%dpx",h))
  })
  
  observeEvent({i('command2'); .IGoR$state$data},
    output[[p('plot')]] <- renderPlot(
      if (..isNotEmpty(input$main.data)&&..isNotEmpty(i('command2')))
        ..do1(input,output,page,paste(input$main.data,'%>%',i('command2')))
  ) )

  ..saveButton(input,output,page)
}

## (graphics, 'tabular') button to select where to save results
## actual selected file is used in ..gSaveCmd
..saveButton <- function(input,output,page) 
  shinyFileSave(input, paste0(page,"SaveButton"), roots=.IGoR$config$volumes, defaultPath='', defaultRoot='home')

## (graphics, 'tabular') Widget to allow saving results
..save.ui <- function (page, .title=.IGoR$Z$all$graphics.save)
  box(width='100%',
      column(width=6, checkboxInput(paste0(page,".save"), .title, FALSE)),
      column(width=6, uiOutput(paste0(page,".save")))
  )

## (graphics) Add a widget displaying the label attribute of a column or its name
..output.gVarLabel <- function(input,output,page,var)
  output[[paste0(page,".",var,".label")]] <- renderUI({
    v <- input[[paste0(page,".",var)]]
    if (..isNotEmpty(input$main.data)&&..isNotEmpty(v))
      textInput(paste0(page,".",var,".label"),..s2(.IGoR$Z$any$title),{
        d <- ..data(input)
        l <- attr(d[[v]],'label')
        if (is.null(l)) v else l
      })
})

## (graphics) textInput widget with default text column label if some exists or column name if not
..gLabel.ui <- function(input,page,var) {
  v <- input[[paste0(page,".",var)]]
  d <- ..data(input)
  l <- attr(d[[v]],'label')
  textInput(paste0(page,".",var,".label"),.IGoR$Z$any$title,if (is.null(l)) v else l)
}

## command2 tool: Builds the additional arg to set a title from a column name or label or user text
## returns NULL if no customized label is required
..gLabel.arg <- function(input,page,
                             var,
                             name)
  if (..isNE(input[[paste0(page,'.',var,'.label')]],input[[paste0(page,'.',var)]]))
    paste0('\n     ',name,'=',shQuote(input[[paste0(page,'.',var,'.label')]]))

## command2 tool: Generate the call to 'gf_labs'
..gTitleCmd <- function(input,page,labels=NULL,X=FALSE,Y=FALSE) {
  i <- function(item) input[[paste0(page,".",item)]]

  l <-  c(if (..isNotEmpty(i('title')))    glue("title={shQuote(i('title'))}"),
          if (..isNotEmpty(i('subtitle'))) glue("subtitle={shQuote(i('subtitle'))}"),
          if (..isNotEmpty(i('source')))   glue("caption={shQuote(paste0('Source : ',i('source')))}"),
          if (X&&..isNE(i('X.label'),..nameg(i('X')))) glue("x={shQuote(i('X.label'))}"),
          if (Y&&..isNE(i('Y.label'),..nameg(i('Y')))) glue("y={shQuote(i('Y.label'))}"),
          labels)
  if (length(l)>0) paste0(NL,glue("gf_labs({paste(l,collapse=', ')})"))
}

## command2 tool: Generate the call to 'ggsave' from the checkbox and the selected file
..gSaveCmd <- function(input,page)
  if (..isTRUE(input[[paste0(page,".save")]])) {
    f <- parseSavePath(.IGoR$config$volumes,input[[paste0(page,"SaveButton")]])$datapath
    if (..isNotEmpty(f)) paste0(NL,"{",glue("ggsave(\"{f}\", device='png', plot=.)","; .}"))
  }


# Evaluation --------------------------------------------------------------

## Try to evaluate the contents of command2 alone
..try <- function (input,output,page
                   ,.fn=NULL,            # Additional function to apply to a valid result
                   .subset="head(1)") {  # The part of table to use for testing code
  
  output[[paste0(page,".preview")]] <- renderText("")
  
  output[[paste0(page,".comment")]] <- renderText({
    ok <- FALSE
    s <- input[[paste0(page,".command2")]]
    m <- if (..isNotEmpty(input$main.data)&&..isNotEmpty(s)&&(s!='   ')) {
      x <- tryCatch(
        eval(parse(text=glue("{input$main.data} %>% {.subset} %>%\n{s}")), envir=.IGoR$env),
        error=identity)
      if (!..isCondition(x)) {
        ok <- TRUE
        if (!is.null(.fn)) .fn(x) else ""
      }
      else
      if (x$message!="") x$message else toString(x)
    } 
    else ""
    if (ok) ..enableLoad(input,output,page) else ..disableLoad(output,page)
    m
  })
}

## Called by pages 'import' and 'create'
## Evaluates the command argument
..do <- function(input,output,page,
                 command)
  isolate({
    t <- make.names(input[[paste0(page,".out")]])
    x <- tryCatch(eval(parse(text=command), envir=.IGoR$env),
                  error=identity)
    if (..isCondition(x)) {
      output[[paste0(page,".preview")]] <- renderText("")
      output[[paste0(page,".comment")]] <- renderText(x$message)
    }
    else {
      if (page=="import")
        eval(parse(text=glue("attr({make.names(input$import.out)},'source')<- '{input$import.file}'")),
             envir=.IGoR$env)
      ..writeLog(page,command)
      if (t==input$main.data)
        .IGoR$state$meta <- Sys.time()
      ..newTable(input,output,t,.select=TRUE)
      d <- get(t, envir=.IGoR$env)
      output[[paste0(page,".preview")]] <- renderPrint(d %>% as_tibble() %>% print())
      output[[paste0(page,".comment")]] <- renderText(sprintf(.IGoR$Z$all$msg.result, t, nrow(d), ncol(d)))
      ..disableLoad(output,page)
    }
  })

# Called by pages that don't create a table
..do1 <- function(input,output,page,
                  command,
                  .fn=NULL) {
  x <- tryCatch(eval(parse(text=command), envir=.IGoR$env),
                error=identity)
  if (..isCondition(x)) {
    output[[paste0(page,".comment")]] <- renderText(x$message)
    NULL
  } else {
    ..writeLog(page,command)
    output[[paste0(page,".comment")]] <- renderText(if (is.null(.fn)) "" else .fn(x))
    x
  }
}

# Called by pages that create a table from another
..do2 <- function (input,output,page,
                   .source=TRUE,
                   meta=TRUE) {
  t <- make.names(input[[paste0(page,".out")]])
  a <- input[[paste0(page,".command2")]]
  c <- glue("{t} <- {input$main.data} %>% {a}")
  d <- eval(parse(text=c), envir=.IGoR$env)
  ..writeLog(page,c)
  if (!.source) eval(parse(text=glue("attr({t},'source')<- NULL")), envir=.IGoR$env)
  if (t==input$main.data)
      if (meta)
           .IGoR$state$meta <- Sys.time()
      else .IGoR$state$data <- Sys.time()
  else ..newTable(input,output,t)
  output[[paste0(page,".preview")]] <- renderPrint(d %>% as_tibble() %>% print())
  output[[paste0(page,".comment")]] <- renderText(
    sprintf(.IGoR$Z$all$msg.result, t, nrow(d), ncol(d))
  )
  ..disableLoad(output,page)
}

..disableLoad <- function(output,page) output[[paste0(page,".load")]] <- renderUI(NULL)
..enableLoad <- function(input,output,page) 
    output[[paste0(page,".load")]] <- renderUI(actionButton(paste0(page,".load"), ..buttonName(input,page)))

## Formatting of the command2 field
..textarea <- function (page,placeholder,rows,text)
  tagList(
    tags$style(type="text/css",
               "textarea {font-family: 'Courier New'; width: 100%; background: rgb(245,245,245); border-color: rgb(204,204,204); }"),
    tags$textarea(id=paste0(page,".command2"), spellcheck='false',
                  placeholder=placeholder, rows=rows,
                  text)
  )

## Widget to create the output table
..load.ui <- function(page, out=paste0(page,".out")) {
  p <- function(item) paste0(page,".",item)

  box(width='100%', NULL,
      column(width=8, textInput(p('out'), ..s2(.IGoR$Z$any$out), out)),
      column(width=4, uiOutput(p('load')))
  )
}

## Command block from server side
..rLogo <- function(input,output,page) {
  p <- function(item) paste0(page,".",item)
  i <- function(item) input[[p(item)]]

  output[[p("R")]] <- renderImage(list(src=..image("R_logo.png")), deleteFile = FALSE)

  observeEvent(i('copy'), {
    text <- paste(glue(.IGoR[[p('command1')]]),
                  i('command2'),
                  glue(.IGoR[[p('command3')]]),
                  sep='\n')
    text2 <- str_extract(text,"(?s)(?<= <- ).*") # Remove the table being created that could have a name of a function
    if (is.na(text2)) text2 <- text              # For pages that don't create tables

    code <- tryCatch(parse(text=text2, keep.source=TRUE), error=identity)
    clipr::write_clip(
      if (..isCondition(code))
        paste0('# ',.IGoR$Z$any$copy.invalid,'.\n', text)
      else {
        tmp  <- getParseData(code)
        funs <- tmp$text[tmp$token=="SYMBOL_FUNCTION_CALL"] # Keep symbols used in an explicit function call
        #This solution doesn't see, for example, 'str_to_upper' in 'rename_all(str_to_upper)'
        #Following code also searchs symbols alone but may be fooled by other names introduced by the user
        #Symbols corresponding to source columns are removed, but there may be other ones.
        #i.e. in 'page_labels' a labels table with a column code may be interpreted as shiny::code
        # cols <- colnames(..data(input))
        # funs <- unique(c(
        #   tmp$text[tmp$token=="SYMBOL_FUNCTION_CALL"], # Keep symbols used in an explicit function call
        #   setdiff(tmp$text[tmp$token=="SYMBOL"],cols)  # Remove known colum names from simple symbols
        # ))
        l <- Map(find,funs)                              # Get the list of packages or environments for symbols
        v <- unlist(Map(function(x) x[1],l))             # Keep only the first reference
        v <- v[str_detect(v,"^package:")]                # Keep only symbols from packages
        v <- sort(unique(str_replace(v,"^package:",""))) # Extract package names
        paste0('# ',.IGoR$Z$any$copy.require,': ',paste(v[v %not in% c("base","stats","utils")],collapse=", "),'.\n',
               text)
      })
  })
}

..command2 <- function (...) iconv(paste0('   ',...), from="UTF-8")


# command2 helpers (return strings) ---------------------------------------

## Get dplyr grouping
## Used by 'distinct', filter', 'mutate', 'summarise'
..group_by <- function(input,page) {
  group <- input[[paste0(page,".group")]]
  if (..isNotEmpty(group))
    paste0(glue("group_by({..collapse(group)})"),NL)
}

..ungroup  <- function(input,page,n=0) {
  group <- input[[paste0(page,".group")]]
  if ((length(group)>n)&&(nchar(group[1])>0))
    paste0(NL,"ungroup()")
}


## Get dplyr variables selection from select.ui
## Used by 'factor', 'gather', 'mutate2', 'rename', 'skim', 'summarise'
..select <- function(input,page,
                     vars=FALSE) {
  i <- function(item) input[[paste0(page,".",item)]]

  type    <- i('type')
  drop    <- i('drop')
  columns <- i('columns')
  pattern <- i('pattern')
  if (type==1)
    if (vars) ...input.columns(input,page)
  else
    if ((length(columns)==0)||is.null(drop)) ""
    else if (..isTRUE(drop))
              ..collapse0(paste0('-',..name(columns)))
         else ..collapse(columns)
  else
    if ((type>3)&&!is.null(pattern)) {
      f <- if (type==4) "starts_with"
      else if (type==5) "ends_with"
      else if (type==6) "contains"
      else              "matches"
      if (..isTRUE(drop)) f <- paste0('-',f)
      if (vars)
           glue("vars({f}({shQuote(pattern)}))")
      else glue("{f}({shQuote(pattern)})")
}   }

# UI tools ----------------------------------------------------------------
# Standardized UI for all pages
# NOTE: To the exception of 'mapsf' one (maps_w_trans.png) garatiously provided by Timthée Giraud on https://github.com/riatelab/mapsf/ ,
#   all icons are home made ones, transformed by https://tech-lagoon.com/imagechef/en/white-to-transparent

..ui <- function(..., page,
                icon=page,
                command=TRUE,
                graphics=FALSE,
                subtitle=TRUE,
                control=graphics,
                save=graphics|(page=="tabular"))
  div(id=paste0("div_",page),
    fluidRow(
      column(width=4,
        img(src=paste0("images/",icon,"_w_trans.png"), height = "48px"),
        h3(span(.IGoR$Z[[page]]$page.title, style="color: blue"))
      ),
      column(width=8,
        HTML(paste0("<p align='justify'>",paste(.IGoR$Z[[page]]$info,collapse=" ")))
    ) ),
    hr(),
    if (control) uiOutput(paste0(page,".control")),
    ...,
    if (command) ..commandBox(page),
    # Button is designed to be hidden, but when hidden, activating it activates the file dialog twice
    #    if (save) extendShinyjs(text = paste0("shinyjs.",page,"SaveButton=function(){ $('#",page,"SaveButton').click(); }")),
    if (save) shinySaveButton(paste0(page,"SaveButton"),
                label=if (graphics) .IGoR$Z$any$graphics.browse else .IGoR$Z$any$browse,
                title=if (graphics) .IGoR$Z$all$graphics.save.as else .IGoR$Z$tabular$save.as,
                filetype=if (graphics) list(png="png") else (html="html")),
    if (graphics)
       fluidRow(
        column(width=1,
          ..dropdownButton(page=paste0(page,"_titles"), width=NULL, title=.IGoR$Z$all$titles,
            textInput(paste0(page,".title"),..s4(.IGoR$Z$any$title),""),
            if (subtitle) textInput(paste0(page,".subtitle"),..s4(.IGoR$Z$all$subtitle),""),
            textInput(paste0(page,".source"),..s4(.IGoR$Z$all$source),""),
            ..hr(),
            sliderInput(paste0(page,".height"),..s2(.IGoR$Z$all$height),1,16,4)
          ),
          uiOutput(paste0(page,".dropdown"))
        ),
        column(width=11, uiOutput(paste0(page,"..plot")))
      )
    )

..dropdownButton <- function(..., page,
                             width="800px",
                             title=.IGoR$Z$all$graphics.options,
                             status="info")
  tagList(
    tags$style(HTML(paste0("#dropdown-menu-",page," {border: 1px solid blue ;background-color: #F0F0FF ;}"))),
    dropdownButton(inputId=page, width=width, tooltip=tooltipOptions(title=title), status=status,
                   circle=FALSE,
                   tags$em(title),
                   ..hr(),
                   ...),
    tags$br()
  )

## The command box : text, R logo and button for copying whole text
## Need to be completed by a 'command2' ui output item.
..commandBox <- function(page) {
  p <- function(item) paste0(page,".",item)

  tagList(
    box(width=12, collapsible=TRUE,
        column(width=2,
               imageOutput(p('R'), height='128px'),
               actionButton(p('copy'), .IGoR$Z$all$copy)
        ),
        column(width=10,
               verbatimTextOutput(p('command1')), # immutable command header
               uiOutput(p('command2')),           # mutable command text
               verbatimTextOutput(p('command3'))  # only for 'create'
        ) ),
    verbatimTextOutput(p('comment')),             # messages from execution of the command or of its test
    verbatimTextOutput(p('preview'))              # display of the command result
  )
}


## textInput widget with title before cell instead of above
..label.ui <- function(page,
                       var,
                       value,
                       title=..s2(.IGoR$Z$any$titleh),
                       suffix=".label") {
  id <- paste0(page,".",var,suffix)
  div_id <- str_replace_all(id,'\\.','_')
  tagList(
    tags$head(
      tags$style(type="text/css",
        paste0("#",div_id," label{ display: table-cell; text-align: center; vertical-align: middle; }
                #",div_id," .form-group { display: table-row;}"))),
    tags$div(id=div_id, textInput(id,title,value))
  )
}

## Columns selection box
## Used by 'browse', 'factor', 'gather', 'skim', 'mutate2', 'rename', 'select', "summarise'
..select.ui <- function(page, title=NULL, box=TRUE,
                        buttons.title=NULL, selected=1,
                        buttons.all=TRUE, buttons.class=TRUE, buttons.range=FALSE,
                        drop=TRUE) {
  f <- function ()
    tagList(
      fluidRow(
        column(width=6,
               radioButtons(paste0(page,".type"),buttons.title,
                            ..Znames("all","select",c(1,if (buttons.class) 2,if (buttons.all) 3,4:7,if (buttons.range) 0)),
                            selected=selected)),
        column(width=6,
               if (drop)
                 checkboxInput(paste0(page,".drop"),..s4(.IGoR$Z$any$drop),FALSE)
               else uiOutput(paste0(page,".drop")),
               uiOutput(paste0(page,".columns.what")),     # Auxiliary input field for type 1,2, 4:7 selections
               uiOutput(paste0(page,".columns.more"))      # Auxiliary input field ('select')
        ) ),
      verbatimTextOutput(paste0(page,".columns.why")) # Messages from type 7 selection
    )

  if (box) box(width='100%', title=title, f()) else f()
}

## Adds an auxiliary input field for column selection box
..output.select.what <- function(input,output,page,
                                 columns.all=FALSE) {
  p <- function(item) paste0(page,".",item)
  i <- function(item) input[[p(item)]]

  output[[p("columns.what")]] <- renderUI({
    type <- i('type')
    if (!is.null(type))
      if (type==0)
        fluidRow(
          column(width=6, numericInput(p('start'),..s2(.IGoR$Z$all$select.range.start),1)),
          column(width=6, numericInput(p('end'),  ..s2(.IGoR$Z$all$select.range.end),  ncol(..data(input))))
        )
      else
      if (type==1) {
        .IGoR$state$meta
        selectizeInput(p('columns'),"",
                       multiple = TRUE, options = list(placeholder = if (columns.all) .IGoR$Z$any$all else .IGoR$Z$any$cols),
                       choices = ..columns(input$main.data))

      }
      else
      if (type==2)
        selectizeInput(p('class'),"",
                       choices=..Znames("all","select.type",c("character","numeric","double","integer","logical","factor","Date")))
      else
      if (type>=4)
        textInput(p('pattern'),"")
  })
}

## Adds an the auxiliary input 'drop' checkbox for column selection box
..output.select.drop <- function(input,output,page) {
  p <- function(item) paste0(page,".",item)
  i <- function(item) input[[p(item)]]

  output[[p("drop")]] <- renderUI({
    type <- i('type')
    if (!is.null(type))
      if (((type==1)&&(length(i('columns'))>0))    # selection by list (allows no selection)
         ||(type==2)
        ||((type>=4)&&..isNotEmpty(i('pattern')))  # selection by name
         )
        checkboxInput(p('drop'),..s4(.IGoR$Z$any$drop),FALSE)
  })
}

## Widget for getting grouping columns
## NB: IGoR restricts available columns to those that are not of type 'double'
..group.ui <- function (input,page,
                        box=TRUE) {
  f <- function()
    selectizeInput(paste0(page,".group"), ..s3(.IGoR$Z$any$group),
                   multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                   choices = ..columns(input$main.data,"discrete"))

  if (box) box(width='100%', f()) else f()
}

## Widget to select mode of expression writing in 'filter', 'mutate'
..expr.type.ui <- function (page,
                            title=NULL)
  radioButtons(paste0(page,".type"),title,..Znames("all","expr",0:3))


# Server tools ------------------------------------------------------------

## Get the list of selected columns
..select.columns <- function(input,output,page) {
  p <- function(item) paste0(page,".",item)
  i <- function(item) input[[p(item)]]

  type    <- i('type')
  pattern <- i('pattern')
  why <- ""
  l0 <- ..columns(input$main.data,.sort=FALSE)
  l1 <-
    if (is.null(type)) l0
  else
    if (type==1) i('columns')
  else
    if ((type==2)&&!is.null(i('class')))
      ..columns(input$main.data, i('class'), .sort=FALSE)
  else
    if (type==3) l0
  else
    if ((type>3)&&!is.null(pattern)) {
      f <- if (type==4) startsWith
      else if (type==5) endsWith
      else if (nchar(pattern)==0) function (x,y) TRUE
      else if (type==6)           function(x,y) grepl(y,x,fixed=TRUE)
      else                        function(x,y) grepl(y,x)
      tryCatch(l0[f(l0,pattern)],
               error=function(e) {why <<- e$message; character(0)}
      )
    }
  output[[p('columns.why')]] <- renderText(why)
  if (..isTRUE(i('drop'))) setdiff(l0, l1) else l1
}

## Get the list of selected columns, from the columns menu only
...input.columns <- function(input,page) {
  i <- function(item) input[[paste0(page,".",item)]]

  columns <- i('columns')
  ..collapse3(
    if (i('drop'))
         setdiff(..columns(input$main.data), columns)
    else columns
  )
}


## Name of the button that will create/replace the table
..buttonName <- function(input,page)
  if (make.names(input[[paste0(page,".out")]]) %not in% .IGoR$tables) .IGoR$Z$any$create else .IGoR$Z$any$replace


# Some html utilities ----------------------------------------------------

..hr <- function() hr(style='border:0; margin:0; width:100%; height:1px; background:blue;')

..s1 <- function(s) strong(span(s, style='color:red'))  # mandatory field without default setting
..s2 <- function(s) strong(span(s, style='color:blue')) # mandatory field with default setting
..s3 <- function(s) em(s)                               # control field with default setting
..s4 <- function(s) em(span(s, style='color:blue'))     # optional field without default setting
..s5 <- function(s) span(s, style='color:blue')         # optional field with default setting


# String tools ------------------------------------------------------------

NL <- ' %>%\n   '




