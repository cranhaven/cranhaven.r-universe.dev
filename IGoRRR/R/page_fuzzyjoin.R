
### Merge the current table with another on some special condition
###   Dependencies on specific packages: none.
###   Dependencies in generated code: fuzzyjoin.

page_fuzzyjoin <- list(

  ui = function() ..ui(page="fuzzyjoin", icon="join",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("fuzzyjoin.columns"),
          uiOutput("fuzzyjoin.data"),
          uiOutput("fuzzyjoin.fun")
      ) ),
      column(width=6,
        ..load.ui("fuzzyjoin"),
        box(width='100%',
          radioButtons("fuzzyjoin.type", ..s2(.IGoR$Z$join$type),
                       ..Znames("join","type",c("inner","left","right","full","anti","semi"))
        ) )
  ) ) ),


  server = function(input, output, session) {

  ..jServer(input,output,"fuzzyjoin")

    is.string <- function(theMainData)
      ("character" %in% class(
         if (theMainData) ..data(input)[[input$fuzzyjoin.columns]]
         else             get(input$fuzzyjoin.data,envir=.IGoR$env)[[input$fuzzyjoin.columns2]]
      ))

    strings <- function(l) {
      n <- names(l)
     names(n) <- l
      n
    }

    output$fuzzyjoin.fun <- renderUI(
      if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1))
        list(
          fluidRow(
            column(width=6, radioButtons("fuzzyjoin.funType",..s2(.IGoR$Z$fuzzyjoin$fun),strings(.IGoR$Z$fuzzyjoin$type))),
            column(width=6, uiOutput("fuzzyjoin.stringfun"))
          ),
          uiOutput("fuzzyjoin.dist")
        )
      else
      if ((length(input$fuzzyjoin.columns)>0)&&(length(input$fuzzyjoin.columns2)>0))
        textInput("fuzzyjoin.fun",
                  ..s1(
                    paste0(
                      .IGoR$Z$fuzzyjoin$condition,
                      ..collapse(vars("x",input$fuzzyjoin.columns,input$fuzzyjoin.columns2)),
                      .IGoR$Z$fuzzyjoin$and,
                      .collapse(vars("y",input$fuzzyjoin.columns2,input$fuzzyjoin.columns)),
                      "."
    )   )         ) )

    output$fuzzyjoin.stringfun <- renderUI(
      if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)
         &&is.string(TRUE)&&is.string(FALSE))
        if (..isEQ(input$fuzzyjoin.funType,"dist"))
          selectizeInput("fuzzyjoin.dist","",choices=strings(.IGoR$Z$fuzzyjoin$method))
        else
        if (..isEQ(input$fuzzyjoin.funType,"menu"))
          selectizeInput("fuzzyjoin.fun","",choices=strings(.IGoR$Z$fuzzyjoin$funs))
        else textInput("fuzzyjoin.fun","")
      else textInput("fuzzyjoin.fun",..s1(.IGoR$Z$fuzzyjoin$fun))
    )

    output$fuzzyjoin.dist <- renderUI(
      if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)
          &&is.string(TRUE)&&is.string(FALSE)
          &&..isEQ(input$fuzzyjoin.funType,"dist")
          &&(length(input$fuzzyjoin.dist)>0))
        list(
          fluidRow(
            column(width=6, textInput("fuzzyjoin.dist.col",..s2(.IGoR$Z$fuzzyjoin$dist.col),"fuzzyjoin.dist")),
            column(width=6, numericInput("fuzzyjoin.dist.max",..s2(.IGoR$Z$fuzzyjoin$dist.max), NA))
          ),
          uiOutput("fuzzyjoin.dist.parm")
    )   )

    output$fuzzyjoin.dist.parm <- renderUI(
      if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)
          &&is.string(TRUE)&&is.string(FALSE)
          &&..isEQ(input$fuzzyjoin.funType,"dist")
          &&(length(input$fuzzyjoin.dist)>0))
        if (input$fuzzyjoin.dist %in% c("qgram","cosine"))
            column(width=3,numericInput("fuzzyjoin.parm.q", ..s2(.IGoR$Z$fuzzyjoin$parm.q), 1))
        else
        if (input$fuzzyjoin.dist=="jw")
          fluidRow(
            column(width=3,numericInput("fuzzyjoin.parm.p",  ..s2(.IGoR$Z$fuzzyjoin$parm.p),  0)),
            column(width=3,numericInput("fuzzyjoin.parm.bt", ..s2(.IGoR$Z$fuzzyjoin$parm.bt), 0))
          )
        else
        if (input$fuzzyjoin.dist %in% c("osa","dl"))
          fluidRow(
            column(width=3,numericInput("fuzzyjoin.parm.w.d", ..s2(.IGoR$Z$fuzzyjoin$parm.w.d), 1)),
            column(width=3,numericInput("fuzzyjoin.parm.w.i", ..s2(.IGoR$Z$fuzzyjoin$parm.w.i), 1)),
            column(width=3,numericInput("fuzzyjoin.parm.w.s", ..s2(.IGoR$Z$fuzzyjoin$parm.w.s), 1)),
            column(width=3,numericInput("fuzzyjoin.parm.w.t", ..s2(.IGoR$Z$fuzzyjoin$parm.w.t), 1))
    )     )

    vars <- function(t,l,l2) ifelse(l %in% l2,paste0(l,".",t),l)

    expr <- function(expr,lx,ly) {
      f <- function(t,v) substitute(t[,v],list(t=t,v=v))
      g <- function(t,l,l2) {
        m <- if (length(l)==1) list(t)
             else Map(function (x) f(t,x),l)
        names(m) <- vars(as.character(t),l,l2)
        m
      }
      e <- tryCatch(parse(text=expr),error=identity)
      if (is(e,"condition")) .IGoR$Z$any$error
      else {
        l <- append(g(quote(x),lx,ly), g(quote(y),ly,lx))
        deparse(
          eval(
            substitute(
              substitute(e, l),
              list(e=e[[1]],l=l)
          ) ),
          width.cutoff=500
        )
    } }

    output$fuzzyjoin.command2 <- renderUI(
      ..textarea("fuzzyjoin", "fuzzy_join(table,mode,columns,function)", 4,
        if (..isNotEmpty(input$fuzzyjoin.data)
          &&(length(input$fuzzyjoin.columns)>0)
          &&(length(input$fuzzyjoin.columns2)>0)
          &&!is.null(input$fuzzyjoin.type)
           ) {
          key <- "fuzzy"
          parm <- ""
          col <- ""
          max <- ""
          if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)) {
            by <- glue("by=c(\"{input$fuzzyjoin.columns}\"=\"{input$fuzzyjoin.columns2}\")")
            if (is.string(TRUE)&&is.string(FALSE))
              if (length(input$fuzzyjoin.funType)==0) key <- "" # Not ready yet
              else # --- 1-1 strings column, distance ---------------------------
              if (input$fuzzyjoin.funType=="dist")
                if (length(input$fuzzyjoin.dist)==0) key <- "" # Not ready yet
                else {
                  key <- "stringdist"
                  fun <- glue("method=\"{input$fuzzyjoin.dist}\"")
                  parm <-
                    if (input$fuzzyjoin.dist %in% c("osa","dl"))
                       glue("weight=c(d={input$fuzzyjoin.parm.w.d}, i={input$fuzzyjoin.parm.w.i}, s={input$fuzzyjoin.parm.w.s}, t={input$fuzzyjoin.parm.w.t})")
                    else
                    if (input$fuzzyjoin.dist=="jw")
                       glue("p={input$fuzzyjoin.parm.p}, bt={input$fuzzyjoin.parm.bt}")
                    else
                    if (input$fuzzyjoin.dist %in% c("qgram","cosine"))
                      glue("q={input$fuzzyjoin.parm.q}")
                    else ''
                  if (..isNotEmpty(parm)) parm <- paste0(', ',parm)
                  if (..isNotEmpty(input$fuzzyjoin.dist.col))
                    col<- paste0(',\n     ',glue("distance_col=\"{make.names(input$fuzzyjoin.dist.col)}\""))
                  if (..isNotNA(input$fuzzyjoin.dist.max))
                    max<- paste0(',\n     ',glue("max_dist={input$fuzzyjoin.dist.max}"))
                }
              else # --- 1-1 strings column, function from menu or input --------
                if (!..isNotEmpty(input$fuzzyjoin.fun)) key <- "" # Not ready yet
                else {
                  f <- input$fuzzyjoin.fun
                       if (f=="contains") f <- "function(x,y) grepl(y,x,fixed=TRUE)"
                  else if (f=="example")  f <- "function(x,y) (x==y)|((x=='20')&(y %in% c('2A','2B')))"
                  fun <- glue("match_fun={f}")
                }
            else # ----- 1-1 nonstrings column ----------------------------------
                if (!..isNotEmpty(input$fuzzyjoin.fun)) key <- "" # Not ready yet
                else fun <- glue("match_fun={input$fuzzyjoin.fun}")
          }
          else # ------- n-m columns --------------------------------------------
          if (!..isNotEmpty(input$fuzzyjoin.fun)) key <- "" # Not ready yet
          else {
            by <- glue("multi_by=list(x={..collapse2(input$fuzzyjoin.columns)},y={..collapse2(input$fuzzyjoin.columns2)}),")
            fun <-paste0(
              "multi_match_fun=function (x,y) ",
               expr(input$fuzzyjoin.fun,..name(input$fuzzyjoin.columns),..name(input$fuzzyjoin.columns2))
            )
          }
          if (key!="")
            ..command2(
              key,
              glue("_join({input$fuzzyjoin.data}, mode=\"{input$fuzzyjoin.type}\""),
              ',\n     ',by,
              ',\n     ',fun,
              parm,
              max,
              col,
              ')'
            )
        }
    ) )

    observeEvent(input$fuzzyjoin.command2, ..try(input,output,"fuzzyjoin"))

  }
)


