
### Export tables in various file formats
###   Dependencies on specific packages: none.
###   Dependencies in generated code: rio.

page_export <- list(

  ui = function() ..ui(page="export",
    fluidRow(
      column(width=6,
        box(width='100%',
          radioButtons("export.type",NULL,..Znames("export","type",c("export","excel","rdata"))),
          uiOutput("export.control")
        ),
        uiOutput("export.parms")
      ),
      column(width=6,
        box(width='100%',
          fluidRow(
            column(width=8, uiOutput("export.file")),
            column(width=4,
              uiOutput("export.files"),
              uiOutput("export.load"))
  )   ) ) ) ),


  server = function(input, output, session) {

    ..rLogo(input,output,"export")

    output$export.control <- renderUI(
      if (input$export.type!="export")
       tagList(
         selectizeInput("export.tables",
                     ..s1(if (input$export.type==2) .IGoR$Z$export$excel.tables else .IGoR$Z$export$rdata.tables),
                     multiple=TRUE,  options = list(placeholder = .IGoR$Z$any$all),
                     choices=.IGoR$tables),
       if (input$export.type=="excel") checkboxInput("export.names",..s4(.IGoR$Z$export$excel.names),FALSE)
  ) )

    output$export.files <- renderUI(
      if (..isNotEmpty(input$main.data))
        if (input$export.type=="export")
          shinySaveButton("export", .IGoR$Z$any$browse, .IGoR$Z$export$choose,
            filename=input$main.data,
            filetype=list("R Data Serialization"="RDS",
                            "Fast serialization"="fst",
                                       "feather"="feather",
                                "Excel Open XML"="xlsx",
                                         "DBase"="dbf",
                                          "Calc"="ods",
                        "Comma Separated Values"="csv",
                                          "JSON"="json",
						                    "APACHE parquet"="parquet"))
        else
        if (input$export.type=="excel")
          shinySaveButton("export", .IGoR$Z$any$browse, .IGoR$Z$export$choose, filetype=list("Excel Open XML"="xlsx"))
        else
        if (input$export.type=="rdata")
          shinySaveButton("export", .IGoR$Z$any$browse, .IGoR$Z$export$rdata.choose, filetype=list("R Data"="RData"))
    )

    output$export.parms <- renderUI(
      if (..isFile(input$export.file))
        switch(..pathExt(input$export.file),
          fst = box(width='100%', sliderInput("export.fst.compress", ..s2(.IGoR$Z$export$fst.compress), 0, 100, 50)),
          csv = box(width='100%', HTML(.IGoR$Z$export$csv.function),
                  column(width=6, radioButtons("export.csv.sep", ..s5(.IGoR$Z$export$csv.sep),
                                               choices=c(.IGoR$Z$export$csv.sep.comma,.IGoR$Z$export$csv.sep.semicolon))),
                  column(width=6, checkboxInput("export.csv.quote", ..s5(.IGoR$Z$export$csv.quote), FALSE)))
    )   )

    observe({
      volumes <- .IGoR$config$volumes
      shinyFileSave(input, "export", roots = volumes, defaultPath='', defaultRoot='home')
      fileinfo <- parseSavePath(volumes, input$export)

      output$export.file <- renderUI(
        textInput("export.file",..s1(.IGoR$Z$export$path),fileinfo$datapath))
    })

    output$export.command1 <- renderText(
      if (..isNotEmpty(input$main.data))
        .IGoR$export.command1 <-
          if (input$export.type=="export")
            glue("{input$main.data} %>%")
          else {
            l <- if (length(input$export.tables)==0) .IGoR$tables else input$export.tables
            glue("c({..collapse1(l)}) %>%")
          }
    )

    output$export.command2 <- renderUI(
      ..textarea("export", "export(path,parms)", 3,
        if (!is.null(input$export.type)&&..isNotEmpty(input$export.file))
          ..command2(
            if (input$export.type=="export")  # - save using rio -----------------------------------------------------------
              paste0(
                glue("export(file=\"{input$export.file}\""),
                switch(..pathExt(input$export.file),
                  fst = if (..isNE(input$export.fst.compress,50)) glue(", compress={input$export.fst.compress}"),
                  csv = paste0(
                        if (..isEQ(input$export.csv.sep,.IGoR$Z$export$csv.sep.semicolon)) ", sep=';'",
                        if (..isTRUE(input$export.csv.quote)) ", quote=TRUE")
                ),
                ")"
              )
            else
            if (input$export.type=="excel") { # - save as Excel sheets -----------------------------------------------------
              l <- if (length(input$export.tables)==0) .IGoR$tables else input$export.tables
              # When generated code is manually changed, an invalid file type cause a strange message:
              # 'x' is not a data.frame or matrix
              t <- if (!str_detect(input$export.file,"\\.xlsx$")) ".xlsx" else ""
              paste0(
                "Map(get,.)",NL,              # -- make sheet names explicit so user change them
                if (..isTRUE(input$export.names)) paste0("{",glue("names(.)<- c({..collapse1(l)})"),"; .}",NL),
                glue("export(file=\"{input$export.file}{t}\")")
              )
            }
            else
            if (input$export.type=="rdata") { # - save as RData ------------------------------------------------------------
              t <- if (!str_detect(input$export.file,"\\.RData$")) ".RData" else ""
              glue("save(list=., file=\"{input$export.file}{t}\")")
            }
    ) )   )

    observeEvent(input$export.command2, {
      if (..isNotEmpty(input$export.file))
        output$export.load <- renderUI(
          actionButton("export.load",
            if (input$export.type=="export") .IGoR$Z$export$export
            else
            if (input$export.type=="excel")  .IGoR$Z$export$excel
            else                             .IGoR$Z$export$rdata
        ) )
      else output$export.comment <- renderText("")
    })

    observeEvent(input$export.load,
      isolate({
        ..do1(input,output,"export", paste0(.IGoR$export.command1,input$export.command2),
          function (x) {
            # Protect against a manual input of the file name within the command box
            f <- str_extract(isolate(input$export.command2),"(?<=file=([\"'])).*?(?=\\1)")
            sprintf(.IGoR$Z$export$msg.result, f, file.size(f))
          }
        )
        shinyjs::disable("export.load")
      })
    )

  }
)
