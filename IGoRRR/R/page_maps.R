
### Display statistical maps about a column in a 'sf' data.frame.
###   Dependencies on specific packages: sf.
###   Dependencies in generated code: mapsf.

page_maps <- list(

  ui = function() ..ui(page="maps", graphics=TRUE, subtitle=FALSE),


  server = function(input, output, session) {

    ..gServer(input,output,"maps")

    `%intersects%` <- function(e1,e2) length(intersect(e1,e2))>0

    hasType <- function(theColName, theClasses) {
      d <- ..data(input)
      class(d[[theColName]]) %intersects% theClasses
    }

    if1 <- function(theTest,theGlue) eval(substitute(if (theTest) glue(theGlue) else ""),envir=parent.frame())

    # List of all shp tables of same projection system than current table
    maps <- function () {
      epsg <- sf::st_crs(..data(input))$epsg
      if (is.na(epsg)) ""
      else {
        f <- Vectorize(
               function(x) {
                 df <- get(x, envir=.IGoR$env)
                 "sf" %in% class(df) && sf::st_crs(df)$epsg==epsg
               })
        l <- ls(envir=.IGoR$env)
        c("",l[f(l)])
    } }

    # List of all columns except geometry
    cols <- function() {
      d <- ..data(input)
      c <- Map(function(x) if ("sfc" %not in% class(d[[x]])) x,colnames(d))
      c <- Filter(Negate(is.null),c)
      names(c) <- c
      c
    }

    THEMES = c("default", "brutal", "ink", "dark", "agolalight", "candy", "darkula", "iceberg", "green", "nevermind", "jsk", "barcelona")
    COLORS = c('',"black","white","grey","blue","lightblue","green","red")
    PALETTES = c("Mint", "Orange->Red"="OrRd","Yellow->Orange->Red"="YlOrRd",
                 "Spectral","Red->Yellow->Green"="RdYlGn","Red->Yellow->Blue"="RdYlBu")
    nl = "\n    "


    output$maps.control<- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data)
        &&..isNotNA(crs<-sf::st_crs(..data(input))$input)
         )
        fluidRow(
          column(width=8,
            box(width='100%',
              fluidRow(
                column(width=5, selectizeInput("maps.X", ..s1(.IGoR$Z$any$var), c('',cols()))),
                column(width=5, uiOutput("maps.X.label")),
                column(width=2, crs)
              ),
              fluidRow(
                column(width=3, uiOutput("maps.labels.var")),
                column(width=3, uiOutput("maps.labels.table")),
                column(width=3, uiOutput("maps.labels.col")),
                column(width=3, uiOutput("maps.labels.cex"))
            ) ),
            uiOutput("maps.layers")
          ),
          column(width=4,
            box(width='100%',
              fluidRow(
                column(width=6, ..s2(.IGoR$Z$maps$theme)),
                column(width=6, selectizeInput("maps.theme", NULL, THEMES))
            ) ),
            uiOutput("maps.type"),
            uiOutput("maps.save")
       )  )
    })

    output$maps.save <- renderUI({
      page <- "maps"
      f <- parseSavePath(.IGoR$config$volumes,input[[paste0(page,"SaveButton")]])$datapath
      if (..isNotEmpty(f))
        box(width='100%',
          column(width=6, checkboxInput(paste0(page,".save"), ..s3(.IGoR$Z$all$graphics.save), FALSE))
        )
    })

    output$maps.labels.var <- renderUI(
      if (..isNotEmpty(input$maps.labels.table))
        selectizeInput("maps.labels.var",..s2(.IGoR$Z$maps$labels.var),
          c(.IGoR$CHRCOLV,
            ..columns(if (input$maps.labels.table=='.') input$main.data else input$maps.labels.table, "character")))
    )

    output$maps.labels.table <- renderUI(
      if (..isNotEmpty(input$main.data))
        selectizeInput("maps.labels.table",..s2(.IGoR$Z$maps$labels.table),
          c('.',
            if (..isNE(input$maps.add1,.IGoR$Z$maps$none)) input$maps.add1,
            if (..isNE(input$maps.add2,.IGoR$Z$maps$none)) input$maps.add2
    )   ) )

    output$maps.labels.col <- renderUI(
      if (..isNotEmpty(input$maps.labels.var))
        selectizeInput("maps.labels.col", ..s2(.IGoR$Z$maps$labels.col), COLORS)
    )

    output$maps.labels.cex <- renderUI(
      if (..isNotEmpty(input$maps.labels.var))
      sliderInput("maps.labels.cex",..s2(.IGoR$Z$maps$labels.cex), min=1, max=5, value=1, step=.5)
    )

    output$maps.layers <- renderUI(
      if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$maps.X))
        box(width='100%', collapsible=TRUE,
          column(width=9,
            fluidRow(
              column(width=1),
              column(width=4, ..s2(.IGoR$Z$maps$add)),
              column(width=2, ..s2(.IGoR$Z$maps$color)),
              column(width=2, ..s2(.IGoR$Z$maps$border)),
              column(width=3, ..s2(.IGoR$Z$maps$lwd))
            ),
            fluidRow(
              column(width=1, ..s2("0")),
              column(width=4, checkboxInput ("maps.add0", ..s4(.IGoR$Z$maps$current), FALSE)),
              column(width=2, selectizeInput("maps.col0", '', COLORS)),
              column(width=2, selectizeInput("maps.bdr0", '', COLORS)),
              column(width=3, sliderInput("maps.lwd0", NULL, min=1, max=3, value=1, step=.5))
            ),
            fluidRow(
              column(width=1, ..s2("1")),
              column(width=4, selectizeInput("maps.add1", NULL, maps(), selected=NULL)),
              column(width=2, selectizeInput("maps.col1", NULL, COLORS)),
              column(width=2, selectizeInput("maps.bdr1", NULL, COLORS)),
              column(width=3, sliderInput("maps.lwd1", NULL, min=1, max=3, value=1, step=.5))
            ),
            fluidRow(
              column(width=1, ..s2("2")),
              column(width=4, selectizeInput("maps.add2", NULL, maps(), selected=NULL)),
              column(width=2, selectizeInput("maps.col2", NULL, COLORS)),
              column(width=2, selectizeInput("maps.bdr2", NULL, COLORS)),
              column(width=3, sliderInput("maps.lwd2", NULL, min=1, max=3, value=1, step=.5))
          ) ),
          column(width=3, uiOutput("maps.order"))
    )   )

    output$maps.order <- renderUI(
      if (..isNotEmpty(input$main.data)&&..isNotEmpty(input$maps.X))
        rank_list(
          text = ..s2(.IGoR$Z$maps$order),
          labels = c(if (..isTRUE(input$maps.add0)) "0",
                     .IGoR$Z$maps$analysis,
                     if (..isNotEmpty(input$maps.add1)) "1",
                     if (..isNotEmpty(input$maps.add2)) "2"),
          input_id= "maps.order"
    )   )

    output$maps.type <- renderUI(
      if (..isNotEmpty(input$maps.X))
        if (hasType(input$maps.X,c("numeric","integer")))
          box(width='100%',
            column(width=6, radioButtons("maps.type", ..s2(.IGoR$Z$maps$type),
                                         ..Znames("maps","type1",c("choro","prop")))),
            column(width=6, uiOutput("maps.breaks"),
                            selectizeInput("maps.legend",..s2(.IGoR$Z$maps$legend), ..Zitems("maps","legends"))
          ) )
        else
        if (hasType(input$maps.X,c("factor","logical","character")))
          box(width='100%',
            column(width=6, radioButtons("maps.type", ..s2(.IGoR$Z$maps$type),
                                         ..Znames("maps","type2",c("typo","symb")))),
            column(width=6, selectizeInput("maps.legend",..s2(.IGoR$Z$maps$legend), ..Zitems("maps","legends"))
    )     ) )

    output$maps.breaks <- renderUI(
      if (..isNotEmpty(input$maps.X)&&..isEQ(input$maps.type,"choro"))
        list(
          radioButtons("maps.breaks", ..s2(.IGoR$Z$maps$breaks), unname(.IGoR$Z$maps$breaks.methods)),
          numericInput("maps.nbreaks", ..s2(.IGoR$Z$maps$nbreaks), NA),
          selectizeInput("maps.pal", ..s2(.IGoR$Z$maps$pal), PALETTES),
          checkboxInput("maps.borders", ..s3(.IGoR$Z$maps$borders), TRUE)
    )   )

    ..output.gVarLabel(input,output,"maps","X")

    output$maps.command2 <- renderUI(
      ..textarea("maps", "map_...", 5,
        if (..isNotEmpty(input$main.data)&&..isNotNA(sf::st_crs(..data(input))$input)) {
          theme  <- if1(..isNE(input$maps.theme,"default"), "mapsf::mf_theme(\"{input$maps.theme}\")")
          labels <- if (..isNE(input$maps.labels.var,.IGoR$CHRCOLV)&&..isNotEmpty(input$maps.labels.table)) {
            labels.cex <- if1(..isNE(input$maps.labels.cex,1),     ", cex={input$maps.labels.cex}")
            labels.col <- if1(..isNotEmpty(input$maps.labels.col), ", col=\"{input$maps.labels.col}\"")
            glue("mapsf::mf_label({input$maps.labels.table}, var=\"{input$maps.labels.var}\"{labels.cex}{labels.col})")
          } else ""
          l <- c(if (..isNotEmpty(input$maps.title))  glue("title={shQuote(input$maps.title)}"),
                if (..isNotEmpty(input$maps.source)) glue("credits={shQuote(paste0('Source : ',input$maps.source))}")
               )
          layout <- if1(length(l)>0, "mapsf::mf_layout({..collapse0(l)})")
          save <- if1(..isTRUE(input$maps.save),
                      "mapsf::mf_export(.,filename=\"{parseSavePath(.IGoR$config$volumes,input$mapsSaveButton)$datapath}\")")
          ..command2(
            if (..isNotEmpty(input$maps.X) && ..isNotEmpty(input$maps.type)) {
              breaks  <- if1(input$maps.type=="choro" && ..isNotEmpty(input$maps.breaks) && input$maps.breaks!="quantile",
                            ", breaks=\"{input$maps.breaks}\"")
              nbreaks <- if1(input$maps.type=="choro" && ..isNotNA(input$maps.nbreaks), ", nbreaks={input$maps.nbreaks}")
              pal     <- if1(input$maps.type=="choro" && ..isNE(input$maps.pal,"Mint"), ", pal=\"{input$maps.pal}\"")
              borders <- if1(input$maps.type=="choro" && ..isFALSE(input$maps.borders), ", border=NA")
              legend  <- if1(..isNE(input$maps.X.label,..name(input$maps.X)),           ", leg_title={shQuote(input$maps.X.label)}")
              legpos  <- if1(..isNE(input$maps.legend,"default"),                        ", leg_pos=\"{input$maps.legend}\"")
              layers <- list()
              layers <-
                if (length(input$maps.add0)>0)
                  mapply(
                    function(x,i,b,w)
                      if (..isNotEmpty(x)) {
                        i <- if (..isNotEmpty(i)) paste0('"',i,'"') else "NA"
                        b <- if (..isNotEmpty(b)) paste0('"',b,'"') else "NA"
                        w <- if (..isNotNA(w)) paste0(", lwd=",w) else ''
                        glue("mapsf::mf_base({x}, col={i}, border={b}{w}")
                      },
                    c(if (input$maps.add0) '.' else '',
                                       input$maps.add1, input$maps.add2),
                    c(input$maps.col0, input$maps.col1, input$maps.col2),
                    c(input$maps.bdr0, input$maps.bdr1, input$maps.bdr2),
                    c(input$maps.lwd0, input$maps.lwd1, input$maps.lwd2)
                  ) %>% Filter(Negate(is.null),.)

              layers <- append(layers,
                glue("mapsf::mf_{input$maps.type}(., var=\"{input$maps.X}\"{breaks}{nbreaks}{pal}{borders}{legend}{legpos}"))
              if (..isEQ(length(input$maps.order),length(layers))) layers[order(input$maps.order)] <- layers
              layers <- paste0(layers,", add=",c("FALSE",rep("TRUE",length(layers)-1)),")")
              f <- (nchar(save)>0)||(nchar(theme)>0)||(nchar(labels)>0)||(nchar(layout)>0)||(length(layers)>1)
              paste0(
                if (f) "{\n    ",
                  paste(
                    c(if (nchar(save)>0)   save,
                      if (nchar(theme)>0)  theme,
                      layers,
                      if (nchar(labels)>0) labels,
                      if (nchar(layout)>0) layout,
                      if (nchar(theme)>0)  "mapsf::mf_theme(\"default\")",
                      if (nchar(save)>0)   "dev.off()"
                    ),
                    collapse=nl
                  ),
                  if (f) "\n   }"
              )
            }
            else {
              f <- (nchar(save)>0)||(nchar(theme)>0)||(nchar(labels)>0)||(nchar(layout)>0)
              paste0(
                if (f) "{\n    ",
                paste(
                  c(if (nchar(save)>0)   save,
                    if (nchar(theme)>0)  theme,
                    glue("mapsf::mf_base(.)"),
                    if (nchar(labels)>0) labels,
                    if (nchar(layout)>0) layout,
                    if (nchar(theme)>0)  "mapsf::mf_theme(\"default\")",
                    if (nchar(save)>0)   "dev.off()"
                  ),
                  collapse=nl
                ),
                if (f) "\n   }"
             )
           }
         )
       }
   ) )
  }
)
