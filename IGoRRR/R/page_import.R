
### Imports files of various types
###   Dependencies on specific packages (weak): readxl, readODS, data.table, fst, feather.
###   Dependencies in generated code: rio, readxl, readODS, fst, feather.

page_import <- list(

  ui = function() ..ui(page="import",
    fluidRow(
     column(width=6,
        box(width='100%',
          column(width=9, uiOutput("import.file")),
          column(width=3, shinyFilesButton("import", label=.IGoR$Z$any$browse, title=.IGoR$Z$import$choose, multiple=FALSE))
      )),
      column(width=6, uiOutput("import.out"))
    ),
    uiOutput("import.parms")
  ),


  server = function(input, output, session) {

    ..rLogo(input,output,"import")

    .IGoR$import.command1 <- "{make.names(input$import.out)} <-"

    expr <- function(.data, fst=TRUE) {
      x <- if (fst) input$import.fst.expr else input$import.feather.expr
      e <- tryCatch(parse(text=x),error=identity)
      if (is(e,"condition")) .IGoR$Z$any$error
      else {
        n <- if (fst) metadata_fst(input$import.file)$columnNames
             else attr(feather_metadata(input$import.file)$types,'names')
        l <- as.list(parse(text=paste0(.data,"$",..name(n))))
        names(l)<- n
        paste(deparse(do.call(substitute,list(e[[1]],l)),width.cutoff = 130L),
              collapse='\n')
    }  }

    observe({
      shinyFileChoose(input, "import",
                      roots = .IGoR$config$volumes,
                      filetypes=c('csv', 'psv',
                 "Excel open XML"='xlsx',
                                  'xls','dbf','json',
                           "Calc"="ods",
                            "SAS"='sas7bdat',
                                  'fst','feather',
                                  'rds','rData','rda',
                                  'shp',
                                  'parquet'))
      fileinfo <- parseFilePaths(.IGoR$config$volumes, input$import)

      output$import.file <- renderUI(
        textInput("import.file", ..s1(.IGoR$Z$any$path), fileinfo$datapath)
      )
    })

    output$import.out <- renderUI(
      if (..isFile(input$import.file))
        box(width='100%',
          column(width=8, {
            type <- ..pathExt(input$import.file)
            if (is.na(type)) NULL  # Occurs at dot when typing in the file name
            else
            if (type=="rdata") 
                 selectizeInput("import.out",..s2(.IGoR$Z$import$rdata.load), choices=..RDataContents(input$import.file))
            else textInput("import.out", ..s2(.IGoR$Z$any$out),
                            str_extract(input$import.file,"(?<=/)[^/]*(?=\\.[^.]+$)"))
          }),
          column(width=4, uiOutput("import.load"))
    )   )

    encoding.ui <- function () selectizeInput("import.encoding",..s2(.IGoR$Z$import$encoding), choices=c("","UTF-8"))

    output$import.parms <- renderUI(
      if (..isFile(input$import.file)) {
        type <- ..pathExt(input$import.file)
        if (!is.na(type)) # Occurs at dot when typing in the file name
          box(width='100%',
            if (type=="parquet")
              fluidRow(
                column(width=6,
                  selectizeInput("import.parquet.columns", ..s2(.IGoR$Z$import$vars),
                                 multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                                 choices = sort(names(arrow::open_dataset(input$import.file)))
                )),
                column(width=6,
                  uiOutput("import.parquet.parms"),
                  radioButtons("import.parquet.filter",.IGoR$Z$import$parquet.filter,
                               ..Znames("import","parquet.filter",c("all","nrow","where")),
                               inline=TRUE)
                ))
            else
            if (type=="sas7bdat")
              if (packageVersion("haven")>="2.2.0")
                fluidRow(
                  column(width=6,
                     selectizeInput("import.sas.columns", ..s2(.IGoR$Z$import$vars),
                                    multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                                    choices = sort(colnames(read_sas(input$import.file,n_max=0)))
                  )),
                  column(width=3, numericInput("import.sas.nrows", ..s2(.IGoR$Z$import$nrows), Inf)),
                  column(width=3, encoding.ui())
                )
              else
                fluidRow(
                  column(width=9),
                  column(width=3, encoding.ui())
                )
          else
          if (type=="csv")
            list(
              fluidRow(
                column(width=6,
                       selectizeInput("import.csv.columns", ..s2(.IGoR$Z$import$vars),
                                      multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                                      choices = sort(colnames(data.table::fread(input$import.file,nrows=0))))),
                column(width=3, numericInput("import.csv.nrows", ..s2(.IGoR$Z$import$nrows), Inf)),
                column(width=3, encoding.ui())
              ),
              fluidRow(
                column(width=3, checkboxInput("import.csv.chars",..s5(.IGoR$Z$import$csv.chars),FALSE)),
                column(width=3, uiOutput("import.csv.dec")),
                column(width=6)
              ))
          else
          if (type=="psv")
            checkboxInput("import.psv.names",..s5(.IGoR$Z$import$header), TRUE)
          else
          if (type=="dbf")
            checkboxInput("import.dbf",..s5(.IGoR$Z$any$stringsAsFactors), FALSE)
          else
          if (type %in% c("xls","xlsx"))
            fluidRow(
              column(width=6, selectizeInput("import.xls.sheet", ..s2(.IGoR$Z$import$xls.sheet),
                                             readxl::excel_sheets(input$import.file))),
              column(width=4, radioButtons("import.xls.type","",..Znames("import","xls.type",c("skip","insee"))),
                              uiOutput("import.xls.names")),
              column(width=2, uiOutput("import.xls.skip"))
            )
          else
          if (type=="ods")
            fluidRow(
              column(width=2, radioButtons("import.ods.type",.IGoR$Z$import$ods.type, ..Znames("import","ods.type",c("one","all")))),
              column(width=3, uiOutput("import.ods.sheet")),
              column(width=2, checkboxInput("import.ods.names", ..s5(.IGoR$Z$import$header), TRUE)),
              column(width=2, radioButtons("import.ods.filter",.IGoR$Z$import$ods.filter, ..Znames("import","ods.filter",c("skip","range")))),
              column(width=3, uiOutput("import.ods.parms"))
            )
          else
          if (type=="fst")
            fluidRow(
              column(width=6,
                 selectizeInput("import.fst.columns", ..s2(.IGoR$Z$import$vars),
                               multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                               choices = sort(metadata_fst(input$import.file)$columnNames))
              ),
              column(width=6,
                uiOutput("import.fst.parms"),
                radioButtons("import.fst.filter",.IGoR$Z$import$fst.filter,
                             ..Znames("import","fst.filter",c("all","range","where")),
                             inline=TRUE)
            ) )
          else
          if (type=="feather")
            fluidRow(
              column(width=6,
                selectizeInput("import.feather.columns", ..s2(.IGoR$Z$import$vars),
                               multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                               choices = sort(attr(feather_metadata(input$import.file)$types,'names'))
              ) ),
              column(width=6,
                 uiOutput("import.feather.parms"),
                 checkboxInput("import.feather.filter",..s3(.IGoR$Z$import$where),FALSE)
              ) )
		      else
		      if (type=="shp")
		        fluidRow(
		          column(width=9),
		          column(width=3, selectizeInput("import.shp.encoding",..s2(.IGoR$Z$import$encoding),
		                                         choices=c("","WINDOWS-1252")))
		        )
    )})

    output$import.csv.dec <- renderUI(
      if (..isFALSE(input$import.csv.chars))
        checkboxInput("import.csv.dec", ..s5(.IGoR$Z$import$csv.dec), FALSE)
    )

    output$import.xls.names <- renderUI(
      if (..isEQ(input$import.xls.type,"skip"))
        checkboxInput("import.xls.names", ..s5(.IGoR$Z$import$header), TRUE)
    )

    output$import.xls.skip <- renderUI(
      if (..isFile(input$import.file)) {
        type <- ..pathExt(input$import.file)
        if ((type %in% c("xlsx","xls"))&&..isEQ(input$import.xls.type,"skip"))
          numericInput("import.xls.skip", "", NA)
      }
    )

    output$import.ods.sheet <- renderUI(
      if (..isFile(input$import.file)) {
        type <- ..pathExt(input$import.file)
        if (type=="ods")
          if (..isEQ(input$import.ods.type,"one"))
            selectizeInput("import.ods.sheet","",readODS::ods_sheets(input$import.file))
          else
          if (..isEQ(input$import.ods.type,"all"))
            list(
              checkboxInput("import.ods.all.skip", ..s5(.IGoR$Z$import$ods.all.skip), FALSE),
              textInput("import.ods.all.name", ..s5(.IGoR$Z$import$ods.all.name), "import.sheet")
            )
      }
    )

    output$import.ods.parms <- renderUI(
      if (..isFile(input$import.file)) {
        type <- ..pathExt(input$import.file)
        if (type=="ods")
          if (..isEQ(input$import.ods.filter,"skip"))
            numericInput("import.ods.skip","",NA)
          else
          if (..isEQ(input$import.ods.filter,"range"))
            textInput("import.ods.range","","")
      }
    )

    output$import.fst.parms <- renderUI(
      if (!is.null(input$import.fst.filter))
        if (input$import.fst.filter=="range")
          fluidRow(
            column(width=6, numericInput("import.fst.from",..s2(.IGoR$Z$import$fst.from),1)),
            column(width=6, numericInput("import.fst.to",  ..s2(.IGoR$Z$import$fst.to),metadata_fst(input$import.file)$nrOfRows))
          )
        else
        if (input$import.fst.filter=="where")
          textInput("import.fst.expr",..s2(.IGoR$Z$import$expr))
    )
    
    output$import.parquet.parms <- renderUI(
      if (!is.null(input$import.parquet.filter))
        if (input$import.parquet.filter=="nrow")
          numericInput("import.parquet.nrow",..s2(.IGoR$Z$import$parquet.nrow),10)
      else
        if (input$import.parquet.filter=="where")
          textInput("import.parquet.where",..s2(.IGoR$Z$import$expr))
    )
    
    output$import.feather.parms <- renderUI(
      if (..isTRUE(input$import.feather.filter))
        textInput("import.feather.expr",..s3(.IGoR$Z$import$expr))
    )

    output$import.command1 <- renderText(if (..isFile(input$import.file)) glue(.IGoR$import.command1))

    output$import.command2 <- renderUI(
      ..textarea("import", "import(parms)", 6,
        if (..isFile(input$import.file)) {
          type <- ..pathExt(input$import.file)
          if (is.na(type)) NULL  # Occurs at dot when typing in the file name
          else
          if ((type=="parquet")&&
              ((length(input$import.parquet.columns)>0)
               || ..isEQ(input$import.parquet.filter,"nrow")
               ||(..isEQ(input$import.parquet.filter,"where")&&..isNotEmpty(input$import.parquet.where))
             ))
            ..command2(
              glue("arrow::open_dataset(\"{input$import.file}\")"),NL,
              if (..isEQ(input$import.parquet.filter,"nrow"))
                paste0(glue("head({input$import.parquet.nrow})"),NL)
              else
              if (..isEQ(input$import.parquet.filter,"where")&&..isNotEmpty(input$import.parquet.where))
                paste0(glue("filter({input$import.parquet.where})"),NL),
              if (length(input$import.parquet.columns)>0)
                paste0(glue("select({..collapse(input$import.parquet.columns)})"),NL),
              "collect()"
            )
          else
          if ((type=="fst")
              &&..isEQ(input$import.fst.filter,'where')&&..isNotEmpty(input$import.fst.expr))
            ..command2(      # --- Use 'fst' connection -------------------
              glue("fst(\"{input$import.file}\")"),NL,
              glue(".[{expr('.',TRUE)},{..collapse2(input$import.fst.columns)}]"), ..look(input$import.fst.expr),
              if (length(input$import.fst.columns)==1) # PB fst 0.8.8 drop=FALSE ne marche pas
                paste0(NL,glue("data.frame({input$import.fst.columns}=., stringsAsFactors=FALSE)"))
            )
          else
          if ((type=="feather")&&..isNotEmpty(input$import.feather.expr))
            ..command2(      # --- Use 'feather' connection --------------
              glue("feather(\"{input$import.file}\")"),NL,
              glue(".[{expr('.',FALSE)},{..collapse2(input$import.feather.columns)}]"), ..look(input$import.feather.expr)
            )
          else
          if (type=="shp") {
            encoding <- if (..isNotEmpty(input$import.shp.encoding)) glue(", options = \"ENCODING={input$import.shp.encoding}\"") else ""
            ..command2(
              glue("sf::st_read(\"{input$import.file}\"{encoding})")
            )
          }
          else
          if ((type %in% c("xlsx","xls"))&&..isEQ(input$import.xls.type,"insee"))
            ..command2(      # --- from insee.fr : get column labels ----
              paste0("(function (file, sheet) {\n",
                     "     df <- import(file,  sheet=sheet, skip=5)\n",
                     "     Map(function(x) attr(df[[x[2]]],'label')<<- x[1],\n",
                     "         import(file, sheet=sheet, skip=4, n_max=2, col_names=FALSE))\n",
                     "     df\n",
                     "   })",
                     glue("(\"{input$import.file}\""),
                     if (..isNE(input$import.xls.sheet,1)) glue(", \"{input$import.xls.sheet}\""),
                     ")"
            ) )
          else
          if (type=="ods")
            ..command2(
              if (..isEQ(input$import.ods.type,"one"))
                paste0(
                  glue("read_ods(\"{input$import.file}\""),  # 'sheet' parameter doesn't work with 'import'
                  if (..isNE(input$import.ods.sheet,1))     glue(", sheet=\"{input$import.ods.sheet}\""),
                  if (..isFALSE(input$import.ods.names))    ", col_names=FALSE",
                  if (..isNotNA(input$import.ods.skip))     glue(", skip={input$import.ods.skip}"),
                  if (..isNotEmpty(input$import.ods.range)) glue(", range=\"{input$import.ods.range}\""),
                  ")"
                )
              else
              if (..isEQ(input$import.ods.type,"all"))
                paste0(
                  "(function (file)\n",
                  "      ods_sheets(file)",
                  if (..isTRUE(input$import.ods.all.skip)) "[-1]",
                  " %>%\n",
                  "      Map(function(x)\n",
                  "        read_ods(file, sheet=x",
                  if (..isFALSE(input$import.ods.names))    ", col_names=FALSE",
                  if (..isNotNA(input$import.ods.skip))     glue(", skip={input$import.ods.skip}"),
                  if (..isNotEmpty(input$import.ods.range)) glue(", range=\"{input$import.ods.range}\""),
                  ") %>%\n",
                  "        mutate(",..name(input$import.ods.all.name),"=x) %>%\n",
                  "        select(",..name(input$import.ods.all.name),",everything()), .) %>%\n",
                  "      Reduce(bind_rows, .)\n",
                  "   )(\"",input$import.file,"\")"
                )
            )
          else # --- Calls to 'rio::import' --------------------------------------------------------------------------
            ..command2(
              glue("import(\"{input$import.file}\""),
              switch(type,
                sas7bdat =
                  paste0(
                    if (..isNotEmpty(input$import.sas.columns))  glue(", col_select={..collapse2(input$import.sas.columns)}"),
                    if (..isNE(input$import.sas.nrows,Inf))      glue(", n_max={input$import.sas.nrows}"),
                    if (..isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\"")
                  ),
                csv      =
                  paste0(
                    if (..isNotEmpty(input$import.csv.columns))  glue(", select={..collapse2(input$import.csv.columns)}"),
                    if (..isNE(input$import.csv.nrows,Inf))      glue(", nrows={input$import.csv.nrows}"),
                    if (..isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\""),
                    if (..isFALSE(input$import.csv.chars)&&..isTRUE(input$import.csv.dec)) ", dec=','",
                    if (..isTRUE(input$import.csv.chars)) ", colClasses=\"character\""
                  ),
                psv      = if (..isTRUE(input$import.psv.names)) ", header=TRUE",
                dbf      = glue(", as.is={..isFALSE(input$import.dbf)}"), # PB rio 0.5.16 default should be TRUE
                xls      =,
                xlsx     =
                  paste0(
                    if (..isNE(input$import.xls.sheet,1)) glue(", sheet=\"{input$import.xls.sheet}\""),
                    if (..isFALSE(input$import.xls.names)) ", col_names=FALSE",
                    if (..isNotNA(input$import.xls.skip)) glue(", skip={input$import.xls.skip}")
                  ),
                fst      =
                  paste0(
                    if (!is.null(input$import.fst.columns)) glue(", columns={..collapse2(input$import.fst.columns)}"),
                    if (..isEQ(input$import.fst.filter,"range")&&!is.null(input$import.fst.from)) glue(", from={as.character(input$import.fst.from)}"),
                    if (..isEQ(input$import.fst.filter,"range")&&!is.null(input$import.fst.to))   glue(", to={as.character(input$import.fst.to)}")
                  ),
				        feather  =
				          if (!is.null(input$import.feather.columns)) glue(", columns={..collapse2(input$import.feather.columns)}"),
				        json     = "",
				        rds      = "",
                rdata    = glue(", which=\"{input$import.out}\"")),
              ")"
            )
          }
    )   )

    observeEvent({input$import.command2
                  input$import.out},
      if (..isNotEmpty(input$import.command2))
        ..enableLoad(input,output,"import")
      else {
        output$import.comment <- renderText("")
        output$import.preview <- renderText("")
        ..disableLoad(output,"import")
      }
    )

    observeEvent(input$import.load,
      ..do(input,output,"import",
        paste0(glue(.IGoR$import.command1),' ',input$import.command2)
    ) )

  }
)
