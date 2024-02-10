######
output$ui_view_species_data <- renderUI({
  ###############
  ###species selected
  occ_data_select_df = reactive({
    datatable(load.occ$select,
              rownames = FALSE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=10)
    )
  })

  output$occ_data_select <- DT::renderDataTable({
    occ_data_select_df()
  })

  ###############  function ######################
  # all variables available in the input data set
  allVars <- reactive({
    inp <- load.occ$select
    if (is.null(inp)) {
      return(NULL)
    }
    cn <- colnames(inp)
    cl <- sapply(1:ncol(inp), function(x) {
      class(inp[[x]])
    })
    names(cn) <- paste0(cn," (",cl,")")
    cn
  })
  ###############  end function ######################
  ###############  function ######################
  dataTypes <- reactive({
    inputdata <- load.occ$select
    if (is.null(inputdata)) {
      return(NULL)
    }
    cn <- colnames(inputdata)
    cl <- sapply(1:ncol(inputdata), function(x) {
      class(inputdata[[x]])
    })
    cl
  })

  output$SpeciesTable <- DT::renderDataTable({
    ############# function ########
    sdmData <- reactive({
      inputdata <- load.occ$select
      if (is.null(inputdata)) {
        return(NULL)
      }
      vars <- allVars()
      df <- data.frame(
        "Variable Name"=vars
      )
      df$nrCodes <- sapply(inputdata, function(x) { length(unique(x))} )
      df$nrNA <- sapply(inputdata, function(x) { sum(is.na(x))} )
      df$min<-sapply(inputdata, function(x) { min(x,na.rm = TRUE)} )
      df$max<-sapply(inputdata, function(x) { max(x,na.rm = TRUE)} )
      colnames(df) <- c("Variable name",  "Number of levels", "Number of missing","minimum","maximum")
      rownames(df) <- NULL
      df
    })
    datatable(sdmData(),
              rownames = FALSE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=10)
    )
  })

  output$sumlayers<-DT::renderDataTable({
    summrize_rasters<-reactive({
      df<-data.frame("Layers name"=names(data$Env),"Minimum"=minValue(data$Env),"Maximum"=maxValue(data$Env))
      rownames(df) <- NULL
      df
    })
    datatable(summrize_rasters(),
              rownames = FALSE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=10)
    )
  })

  txt_species_data <- paste0("The loaded dataset  consists of",code(nrow(load.occ$select)),"observations and ",code(ncol(load.occ$select)),"variables. ")
  txt_rasters_info<-paste0("You have" ,code(raster::nlayers(data$Env)),"layers.The extent is xmin=",code(raster::extent(data$Env)@xmin),",xmax=",code(raster::extent(data$Env)@xmax),",ymin=",code(raster::extent(data$Env)@ymin),",ymax=",code(raster::extent(data$Env)@ymax))
  fluidRow(
    mainPanel(width = 8, tabsetPanel(type = "tabs",
                                     tabPanel("Environmental data summary",
                                              p(HTML(txt_rasters_info)),
                                              dataTableOutput("sumlayers")),
                                     tabPanel("Occurence data summary",
                                              p(HTML(txt_species_data)),
                                              dataTableOutput("occ_data_select")
                                     ),
                                     tabPanel("Occurence data summary",
                                              dataTableOutput("SpeciesTable")
                                     )



    ),
    id = "tabs")
  )


})
