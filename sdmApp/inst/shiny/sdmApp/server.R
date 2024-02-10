library(shiny)

shinyServer(function(session, input, output) {
  wd <- setwd(getShinyOption(".appDir", getwd()))
  on.exit(setwd(wd))

  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local=TRUE)
  }
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })

  myActionButton <- function(inputId, label, btn.style="", css.class="") {
    if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
      btn.css.class <- paste("btn", btn.style, sep="-")
    } else {
      btn.css.class <- ""
    }
    tags$button(id=inputId, type="button", class=paste("btn action-button", btn.css.class, css.class, collapse=" "), label)
  }

  ################################
  genObserver_menus <-
    function(pat="btn_results_", n=1, updateVal) {
      res <- paste0('observeEvent(input$',pat,n,', {
                  curid <- "',pat,n,'"
                  nn <- names(input)
                  nn <- nn[grep("',pat,'",nn)]
                  nn <- setdiff(nn, curid)
                  for (btnid in nn) {
                  updateButton(session, btnid, style="default")
                  }
                  obj$',updateVal,' <- "',pat,n,'"
                  updateButton(session, curid, style="primary")
  });
                  ')
      res
    }

  ###########################
  noInputData <- function(prefix="btn_a_micro_", uri) {
    #btn <- myActionButton(paste0(prefix, uri), label=("Load data"), "primary")
    fluidRow(
      column(12, h3("No environmental variables or occurrence data available!"), class="wb-header"),
      column(12, p("Go to the Data Upload tab to load  data"), class="wb-header-hint"))
    # ,
    #   column(12, p("Go back to the Data Upload tab by clicking the button below and load  data."), align="center"),
    #   column(12, div(btn, align="center")))
  }
  ###########################"
  summarise_fold<-function(sb){
    records<-sb$records
    records$fold<-1:nrow(records)
    records <- records[,c(5,1,2,3,4)] %>%
      dplyr::mutate(Percentage= round((test_0 + test_1)*100/(test_0 + test_1+train_1 + train_0),digits = 0))
    return(records)
  }
  ################################
  ######################"
  # data <- reactiveValues(Env = stack(), Occ = data.frame(), dir = getwd(), ESDM = NULL, esdms = list(), Stack = NULL)
  # load.var <- reactiveValues(factors = c(), formats = c(), norm = TRUE,  vars = list())
  # working.directory <- system.file("extdata", package = "sdmApp")
  #example = system.file("extdata", package = "sdmApp")
  if(Sys.info()[['sysname']] == 'Linux') {
    shinyFileChoose(input, 'envfiles', session=session,
                    roots=c(wd = working.directory,
                            #example = example,
                            home = '/home',
                            root = '/'),
                    filetypes=c('',"grd", "tif", "asc","sdat", "rst", "nc", "tif", "envi", "bil", "img"))
  } else if (Sys.info()[['sysname']] == 'Windows') {
    d = system('wmic logicaldisk get caption', intern = TRUE)
    disks = c()
    for(i in 2:(length(d)-1)){
      disks = c(disks, substr(d[i],1,2))
    }
    names(disks) = disks
    shinyFileChoose(input, 'envfiles', session=session,
                    roots=c(wd = working.directory,
                            #example = example,
                            disks),
                    filetypes=c('',"grd", "tif", "asc","sdat", "rst", "nc", "tif", "envi", "bil", "img"))
  } else {
    shinyFileChoose(input, 'envfiles', session=session,
                    roots = c(wd = working.directory,
                              #example = example,
                              home = '/user',
                              root = '/'),
                    filetypes=c('',"grd", "tif", "asc","sdat", "rst", "nc", "tif", "envi", "bil", "img"))
  }
  observeEvent(input$envfiles,{
    if(!is.integer(input$envfiles)){
      load.var$vars = lapply(input$envfiles$files, function(x) x[[length(x)]])
      names(load.var$vars) <- unlist(load.var$vars)
    }
  })

  output$factors <- renderUI({
    selectInput('factors', 'Categorical', load.var$vars, multiple = TRUE, selectize = TRUE)
  })
  observeEvent(input$load, {
    validate(
      need(length(load.var$vars) > 0, 'Choose environment variable files first !')
    )
    if(Sys.info()[['sysname']] == 'Linux') {
      path = switch(input$envfiles$root,
                    'wd' = working.directory,
                    'example' = example,
                    'home' = '/home',
                    'root' = '/')
    } else if (Sys.info()[['sysname']] == 'Windows') {
      path = switch(input$envfiles$root,
                    'wd' = working.directory,
                    'example' = example,
                    input$envfiles$root)
    } else {
      path = switch(input$envfiles$root,
                    'wd' = working.directory,
                    'example' = example,
                    'home' = '/home',
                    'root' = '/')
    }
    for(i in 2:(length(input$envfiles$files[[1]]))-1){
      path = paste0(path, '/', input$envfiles$files[[1]][i])
    }
    load.var$formats = c()
    for (i in seq_len(length(load.var$vars))) {
      format = paste0('.',strsplit(load.var$vars[[i]], '.', fixed = TRUE)[[1]][2])
      if (!(format %in% load.var$formats)) {load.var$formats = c(load.var$formats, format)}
    }

    a = try(withProgress(message = 'Variables loading',
                         load_var(path,
                                  files = unlist(load.var$vars),
                                  format = load.var$formats,
                                  Norm = FALSE,
                                  tmp = FALSE,
                                  categorical = load.var$factors,
                                  verbose = FALSE,
                                  GUI = TRUE)))
    if(inherits(a, 'try-error')){
      output$Envbug <- renderUI(p('Environmental variables loading failed, please check your inputs and try again'))
    } else {
      output$Envbug <- renderUI(p())
      data$Env = a
      for (i in seq_len(length(load.var$vars))) {
        names(data$Env)[i] = strsplit(load.var$vars[[i]], '.', fixed = TRUE)[[1]][1]
      }
      output$layerchoice <- renderUI({
        selectInput('layer', 'Variable', as.list(names(data$Env)), multiple = FALSE, selectize = TRUE)

      })
      width <- reactive({
        input$fig_width
      })
      height <- reactive({
        input$fig_height
      })
      # width_download <- reactive({
      #   input$fig_width_download
      # })
      # height_download <- reactive({
      #   input$fig_height_download
      # })
      string_code <- reactive({
        p <- paste("sdmApp_RasterPlot(map)")
        p <- paste(p, "+ scale_fill_","gradientn", "(name = 'Value',  colours = rev(terrain.colors(10)))",
                   sep = "")
        #p <- paste("+ theme(plot.title = element_text(hjust = 0.5, size = 10))")
        if (input$label_axes)
          p <- paste(p, "+ labs(x = 'input$lab_x', y = 'input$lab_y')")
        if (input$add_title)
          p <- paste(p, "+ ggtitle('input$title')")
        if (input$adj_leg == "Change legend")
          p <- paste(p, "+ scale_fill_","gradientn", "(name = 'input$leg_ttl',  colours = rev(terrain.colors(10)))",
                     sep = "")
        # if (input$adj_col)
        #   p <- paste(p, "+ scale_fill_","gradientn", "(name = 'input$leg_ttl',  colours = rev(terrain.colors(10)))",
        #     sep = "")
        p <- paste(p, "+", input$theme)
        if (input$adj_fnt_sz || input$adj_fnt || input$rot_txt ||
            input$adj_leg != "Keep legend as it is" ||
            input$adj_grd) {
          p <- paste(p, paste(" + theme(\n    ",
                              "plot.title = element_text(hjust = 0.5, size = 10),\n    ",
                              if (input$adj_fnt_sz)
                                "axis.title = element_text(size = input$fnt_sz_ttl),\n    ",
                              if (input$adj_fnt_sz)
                                "axis.text = element_text(size = input$fnt_sz_ax),\n    ",
                              if (input$adj_fnt)
                                "text = element_text(family = 'input$font'),\n    ",
                              if (input$rot_txt)
                                "axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
                              if (input$adj_leg == "Remove legend")
                                "legend.position = 'none',\n    ",
                              if (input$adj_leg == "Change legend")
                                "legend.position = 'input$pos_leg',\n    ",
                              if (input$grd_maj)
                                "panel.grid.major = element_blank(),\n    ",
                              if (input$grd_min)
                                "panel.grid.minor = element_blank(),\n    ",
                              ")", sep = ""), sep = "")
        }
        p <- stringr::str_replace_all(p, c(`input\\$lab_x` = as.character(input$lab_x),
                                  `input\\$lab_y` = as.character(input$lab_y),
                                  `input\\$title` = as.character(input$title),
                                  `input\\$palet` = as.character(input$palet),
                                  `input\\$fnt_sz_ttl` = as.character(input$fnt_sz_ttl),
                                  `input\\$fnt_sz_ax` = as.character(input$fnt_sz_ax),
                                  `input\\$font` = as.character(input$font),
                                  `input\\$leg_ttl` = as.character(input$leg_ttl),
                                  `input\\$pos_leg` = as.character(input$pos_leg))
        )
        p <- stringr::str_replace_all(p, ",\n    \\)", "\n  \\)")
        p
      })
      output$env <- renderPlot(width = width, height = height,{
        if(!is.null(input$layer)){
          i = as.numeric(which(as.list(names(data$Env)) == input$layer))
          if(data$Env[[i]]@data@isfactor) {
            map = !as.factor(data$Env[[i]])
          } else {
            map = data$Env[[i]]
          }
          a =try(eval(parse(text = string_code())))
          load.occ$plot<-a
          if(inherits(a, 'try-error')){
            output$Envbugplot <- renderUI(p('Can not plot this raster! Please verify it and try again.'))
          }
          else{
            output$Envbugplot <- renderUI(p())
            a
          }
        }
      })


    }
    updateTabItems(session, "actions", selected = "newdata")
  })
  # Occurrences loading
  #load.occ <- reactiveValues(columns = c())
  #load.occ <- reactiveValues()
  observeEvent(input$file_type,{
    if(input$file_type=="text"){
      load.occ$type_file=c('',"csv", "txt")}
    else {
      if(input$file_type=="Excel"){
        load.occ$type_file=c('',"xlsx", "xls")
      }
      else{
        if(input$file_type=="SPSS"){
          load.occ$type_file=c('',"sav", "zsav","por")}
        else{
          if(input$file_type=="Stata"){
            load.occ$type_file=c('',"dta")}
          else{if(input$file_type == "SAS"){load.occ$type_file=c('',"sas7bdat")}}
        }
      }

    }
    if(Sys.info()[['sysname']] == 'Linux') {
      shinyFileChoose(input, 'Occ', session=session,
                      roots = c(wd = working.directory,
                                #example = example,
                                home = '/home',
                                root = '/'),
                      filetypes=load.occ$type_file)
    } else if (Sys.info()[['sysname']] == 'Windows') {
      d = system('wmic logicaldisk get caption', intern = TRUE)
      disks = c()
      for(i in 2:(length(d)-1)){
        disks = c(disks, substr(d[i],1,2))
      }
      names(disks) = disks
      shinyFileChoose(input, 'Occ', session=session,
                      roots = c(wd = working.directory,
                                #example = example,
                                disks),
                      filetypes=load.occ$type_file)
    } else {
      shinyFileChoose(input, 'Occ', session=session,
                      roots = c(wd = working.directory,
                                #example = example,
                                home = '/user',
                                root = '/'),
                      filetypes=load.occ$type_file)
    }
  })
  ###################################
  observeEvent(input$Occ, {
    if(!is.integer(input$Occ)) {
      file = paste0(switch(input$Occ$root,
                           'wd' = working.directory,
                           'example' = example,
                           'home' = '/home',
                           'root' = '/',
                           input$Occ$root), '/', paste0(unlist(input$Occ$files[[1]])[-1], collapse = '/'))
      if(input$file_type=="text"){
        load.occ$columns = names(read.csv2(file))
        load.occ$df_occ<-read.csv2(file)
        observeEvent(input$sep, {
          if(!is.integer(input$Occ)) {
            file = paste0(switch(input$Occ$root,
                                 'wd' = working.directory,
                                 'example' = example,
                                 'home' = '/home',
                                 'root' = '/',
                                 input$Occ$root), '/', paste0(unlist(input$Occ$files[[1]])[-1], collapse = '/'))
            load.occ$columns = names(read.csv2(file, sep = input$sep, nrows = 0))
            load.occ$df_occ<-read.csv2(file, sep = input$sep, nrows = 0)
          }
        })
        observeEvent(input$Occ, {
          if(!is.integer(input$Occ)) {
            file = paste0(switch(input$Occ$root,
                                 'wd' = working.directory,
                                 'example' = example,
                                 'home' = '/home',
                                 'root' = '/',
                                 input$Occ$root), '/', paste0(unlist(input$Occ$files[[1]])[-1], collapse = '/'))
            load.occ$columns = names(read.csv2(file, sep = input$sep, nrows = 0))
            load.occ$df_occ<-read.csv2(file, sep = input$sep, nrows = 0)

          }
        })
      }
      else if (input$file_type == "Excel") {
        a = try(withProgress(message = 'Loading Excel occurence data...',
                             as.data.frame(read_excel(file))))
        if(inherits(a, 'try-error')){
          output$Envbug_occ <- renderUI(p('Excel occurence data loading failed, please check your inputs and try again'))
        }
        else{
          output$Envbug_occ <- renderUI(p())
          load.occ$columns <- names(a)
          load.occ$df_occ<-a
        }
      }
      else if (input$file_type == "SPSS") {
        a = try(withProgress(message = 'Loading SPSS occurence data...',
                             as.data.frame(read_sav(file))))
        if(inherits(a, 'try-error')){
          output$Envbug_occ <- renderUI(p('SPSS occurence data loading failed, please check your inputs and try again'))
        }
        else{
          output$Envbug_occ <- renderUI(p())
          load.occ$columns <- names(a)
          load.occ$df_occ<-a
          }
      }
      else if (input$file_type == "Stata") {
        a = try(withProgress(message = 'Loading Stata occurence data...',
                             as.data.frame(read_dta(file))))
        if(inherits(a, 'try-error')){
          output$Envbug_occ <- renderUI(p('Stata occurence data loading failed, please check your inputs and try again'))
        }
        else{
          output$Envbug_occ <- renderUI(p())
          load.occ$columns <- names(a)
          load.occ$df_occ<-a
        }
      }
      else if (input$file_type == "SAS") {
        a = try(withProgress(message = 'Loading SAS occurence data...',
                             as.data.frame(read_sas(file))))
        if(inherits(a, 'try-error')){
          output$Envbug_occ <- renderUI(p('SAS occurence data loading failed, please check your inputs and try again'))
        }
        else{
          output$Envbug_occ <- renderUI(p())
          load.occ$columns <- names(a)
          load.occ$df_occ<-a
        }
      }
    }
  })

  ##############################

  ####################################################""
  output$Xcol <- renderUI({selectInput('Xcol', 'Longitude (X)', load.occ$columns, multiple = FALSE)})
  observeEvent(input$Xcol,{
    load.occ$Ycolumns<-setdiff(load.occ$columns,input$Xcol)
    output$Ycol <- renderUI({selectInput('Ycol', 'Latitude (Y)', load.occ$Ycolumns, multiple = FALSE)})
    observeEvent(input$Ycol,{
      load.occ$Pcol<-setdiff(load.occ$Ycolumns,input$Ycol)
      output$Pcol <- renderUI({selectInput('Pcol', 'Specie column', load.occ$Pcol, multiple = FALSE)})
    })
  })
  observeEvent(input$load2, {
    validate(
      need(length(data$Env@layers) > 0, 'You need to load environmental variables!'),
      need(length(input$Occ) > 0, 'Choose occurrence data !')
    )
    load.occ$select<-load.occ$df_occ[,c(input$Xcol,input$Ycol,input$Pcol)]
    load.occ$lon<-input$Xcol
    load.occ$lat<-input$Ycol
    load.occ$spec_select<-input$Pcol

  })

  ################
  occ_data_df = reactive({
    datatable(load.occ$df_occ,
              rownames = FALSE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=20)
    )
  })
  #, options = list(scrollX=TRUE, lengthMenu=list(c(10, 25, 100, -1), c('10', '20', '100', 'All')), pageLength=25), filter="top", rownames=FALSE
  output$occ <- DT::renderDataTable({
    occ_data_df()
  })

  sac<-reactive({
    a = try(withProgress(message = 'Spatial Autorange',
                         blockCV::spatialAutoRange(rasterLayer = data$var_auto,
                                                   doParallel = T,
                                                   plotVariograms = TRUE,
                                                   showPlots = FALSE)))
    load.occ$sac<-a
    load.occ$sac
    })

  Cor_plotInput <- function(){
    p1 =   ggcorrplot::ggcorrplot(mat(),ggtheme = ggplot2::theme_gray,
                                  hc.order = TRUE,
                                  type = "lower",
                                  p.mat = p.mat(),
                                  colors = c("#6D9EC1", "white", "#E46726"))

    return(p1)
  }


  enfa_plotInput <- function(){
    glc <- glc()

    mod.enfa <- mod.enfa()
    p1=CENFA::scatter(x = mod.enfa,yax=as.numeric(input$number_spec),y = glc,n=nlayers(data$Env),p=1)
    return(p1)
  }

  ## a plot function
  plotInput <- function(){
      a <- load.occ$plot
      if(inherits(a, 'try-error')){
        return(NULL)
      }
      else{
        return(a)
      }
  }

  plotInput_variogram <- function(){
    a <- data$variogram
    if(inherits(a, 'try-error')){
      return(NULL)
    }
    else{
      return(a)
    }
  }

  plotInput_barchart <- function(){
    a <- data$barchart
    if(inherits(a, 'try-error')){
      return(NULL)
    }
    else{
      return(a)
    }
  }

  plotInput_mapplot <- function(){
    a <- data$mapplot
    if(inherits(a, 'try-error')){
      return(NULL)
    }
    else{
      return(a)
    }
  }

  plotInput_bioclim <- function(){
    a = model$bioclim
    if(inherits(a, 'try-error')){
      return(NULL)
    }
    else{
      return(a)
    }
  }

  map_bioclim <- function(){
    a <- load.occ$Bioclim
    if(inherits(a, 'try-error')){
      return(NULL)
    }
    else{
      return(a)
    }
  }

  ev_bioclim <- function(){
    a <- data$ev
    if(inherits(a, 'try-error')){
      return(NULL)
    }
    else{
      return(a)
    }
  }
})
