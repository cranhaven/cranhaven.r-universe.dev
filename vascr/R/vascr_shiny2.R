#' Vascr UI setup
#'
#' @returns A UI page for shiny presenting vascr data analysis
#' 
#' @noRd
#'
#' @examples
vascr_ui = function(){
  
  # Bind tags to something to keep cmd check happy
  tags = NULL
  tags
  
    test_page = function(pages)
    {
      paste("input.nav === '", pages, "'", sep = "", collapse = " | ")
    }
    
    cpan = function(output, pages){
      shiny::conditionalPanel(test_page(pages), shiny::uiOutput(output))
    }
    
    # nd = function()
    # {
    #   bslib::card("No data",  shiny::actionButton("load_default", "Load the default growth.df dataset"))
    # }
    
    
    ui <- 
      shiny::tagList(# spinner css
      #' tags$head(
      #'   tags$style(shiny::HTML("
      #'       #loadmessage {
      #'       position:fixed; z-index:8; top:50%; left:50%; padding:10px;
      #'       text-align:center; font-weight:bold; color:#000000; background-color:#CCFF66;
      #'       }
      #' 
      #'       .loader {
      #'       position:fixed; z-index:8; border:16px solid #999999;
      #'       border-top: 16px solid #8B0000; border-radius: 50%;
      #'       width: 120px; height: 120px; top:45%; left:45%;
      #'       animation: spin 2s linear infinite;
      #'       }
      #' 
      #'       .prevent_click{
      #'       position:fixed;
      #'       z-index:9;
      #'       width:100%;
      #'       height:100vh;
      #'       background-color: transpare'nt;
      #'       }
      #' 
      #'       @keyframes spin {
      #'       0% { transform: rotate(0deg); }
      #'       100% { transform: rotate(360deg); }
      #'       }"))
      #' ),

            
      bslib::page_navbar(
        
      # display load spinner when shiny is busy
        
      shiny::conditionalPanel(
        condition = "$(\'html\').hasClass(\'shiny-busy\')",
        shiny::div(class = "loader"),
        shiny::div(class = "prevent_click")
      ),

      theme = bslib::bs_theme() %>% bslib::bs_add_rules("#shiny-notification-success_box {background-color:rgb(103, 194, 58);}
                                          #loadmessage {
            position:fixed; z-index:8; top:50%; left:50%; padding:10px;
            text-align:center; font-weight:bold; color:#000000; background-color:#CCFF66;
            }

            .loader {
            position:fixed; z-index:8; border:16px solid #999999;
            border-top: 16px solid #8B0000; border-radius: 50%;
            width: 120px; height: 120px; top:45%; left:45%;
            animation: spin 2s linear infinite;
            }

            .prevent_click{
            position:fixed;
            z-index:9;
            width:100%;
            height:100vh;
            background-color: transparent;
            }

            @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
            }"),
      
      shinyjs::useShinyjs(),
      title = "vascr dashboard",
      id = "nav",
      sidebar =   bslib::sidebar(
        cpan("select_unit", c("line", "anova", "cc")),
        cpan("select_frequency", c("line", "anova", "cc")),
        cpan("select_sample", c("line", "anova", "cc")),
        cpan("select_time_single", c("anova")),
        cpan("select_reference", c("anova", "cc")),
        cpan("select_level", c("line", "cc")),
        cpan("select_experiment", c("qc")),
        cpan("qc_wells", c("qc")),
        cpan("select_normalise", c("line")),
        cpan("add_row", c("label"))
      ),
    
      bslib::nav_panel("Import data", shiny::uiOutput("import_controls", fill = "item")),
      bslib::nav_panel("Edit labels", bslib::card(DT::DTOutput("map")), value = "label"),
      bslib::nav_panel("Resample time", bslib::card(shiny::textOutput("original_times"), shiny::uiOutput("resample_controls")), shiny::plotOutput("resample_graph"), shiny::plotOutput("resample_graph_range"), value = "resample"),
      bslib::nav_panel("QC", bslib::card(shiny::plotOutput("plot_qc")), value = "qc"),
      bslib::nav_panel("Line graph", bslib::card(shiny::plotOutput("plot_line")), value = "line"),
      bslib::nav_panel("ANOVA", bslib::card(shiny::plotOutput("plot_ANOVA")), value = "anova"),
      bslib::nav_panel("Cross Correlation", bslib::card(shiny::plotOutput("plot_cc")), value = "cc"),
      bslib::nav_panel("Export", bslib::card(shiny::uiOutput("export_controls"))),
      bslib::nav_panel("Log", bslib::card(shiny::verbatimTextOutput("log")), value = "log"),
      fillable = c("label","qc", "line", "anova", "cc")
    )
    )
    
    return(ui)
}


#' Create the vascr server for shiny
#'
#' @param data.df The vascr dataset to be imported on load
#' 
#' @importFrom ggplot2 geom_text theme_void
#' @importFrom dplyr full_join mutate filter group_by summarise tribble
#' @importFrom utils read.csv2
#' @importFrom glue glue
#' @importFrom rlang .data
#' 
#' @noRd
#'
#' @returns A shiny server function for vascr
#'
#' @examples
#' vascr_serve(growth.df)
#' 
vascr_serve  = function (data.df)
{


server <- function(input, output) {

  options(shiny.maxRequestSize=1000*1024^2)


  l = shiny::reactiveVal({})


  vascr_log = function(l, string)
  {
    tolog = paste(l(), string, "\n")
    print(tolog)
  }

  setuplog = shiny::observe({

  l(vascr_log(l, "# Setup the dataset"))
  l(vascr_log(l, "data.df = vascr_blank_df()"))
  setuplog$destroy()
  })



  raw_dat = shiny::reactiveVal({
    vascr::growth.df %>% dplyr::mutate(Excluded = FALSE) %>% dplyr::select(-"Sample", -"SampleID", -"Excluded", -"cells", -"line") %>% dplyr::filter(FALSE)  
  })
  
  platemap = shiny::reactiveVal({
    vascr_map_template()
  })


  # uniques = shiny::reactive({
  #   uni = list()
  # 
  #   unit = unique(dat()$Sample)
  # 
  #   return(uni)
  #   })

  # Generate floating UI

  output$select_unit = shiny::renderUI(shiny::selectInput("unit", "Select unit", choices = unique(dat()$Unit)))
  
  shiny::observeEvent(input$unit, {
  output$select_frequency = shiny::renderUI(shiny::selectInput("frequency", "Select Frequency", choices = unique((dat() %>% filter(.data$Unit == input$unit))$Frequency)))
  })
  
  
  output$select_sample = shiny::renderUI(shiny::checkboxGroupInput("sample", "Select sample", choices = unique(dat()$Sample), selected = unique(dat()$Sample)))
  output$select_time_single = shiny::renderUI(shiny::selectInput("time_single", "Select time", choices = unique(dat()$Time)))
  output$select_normalise = shiny::renderUI(shiny::selectInput("normalise", "Select time to normalise to", choices = c("none",unique(dat()$Time))))
  output$select_reference = shiny::renderUI(shiny::selectInput("reference", "Select reference", choices = c("none", as.character(unique(dat()$Sample)))))
  output$select_level = shiny::renderUI(shiny::selectInput("level", "Select level", choices = c("summary", "experiments", "wells")))



  #///////////////////////////// Import Data /////////////////////////////////////

  output$import_controls = shiny::renderUI({
    shiny::tagList(
      bslib::card(
      bslib::card_header("Import default"),
      bslib::card_body(
        shiny::actionButton("load_default", "Load the default growth.df dataset")),
      ),
      bslib::card(
        bslib::card_header("Import from instrument"),
        bslib::card_body(
               shiny::selectInput(
                "import_instrument",
                "Select Instrument",
                c("ECIS", "xCELLigence", "cellZscope")
              ),
            shiny::fileInput("raw", "Upload a raw file"),
            shiny::fileInput("model", "Upload a modeled file"),
            shiny::textInput("experiment_name", "Experiment Name"),
            shiny::actionButton("run_import", "Run Import")
             )) ,
      bslib::card(
        bslib::card_header("Import previous data"),
        bslib::card_body(
                shiny::fileInput("load_previous", "Upload a previous vascr file"))
           )
    )
    })

 shiny::observeEvent(input$run_import, {

    l(vascr_log(l, "setup"))

    log1 = glue("vascr_import({input$raw$name},
                      {input$raw$name},
                      {input$model$name},
                      {input$experiment_name})")

    print(log1)

    importing = vascr_import(input$import_instrument,
                             input$raw$datapath,
                            input$model$datapath,
                            input$experiment_name) %>% mutate(Sample = "Not Set")


    # Merge the datasets
    current_data = raw_dat()
    exp_moving = unique(importing$Experiment)

    print(exp_moving)

    if (exp_moving %in% unique(current_data$Experiment))
    {
      current_data = current_data %>% dplyr::filter(!.data$Experiment == exp_moving)
      shiny::showNotification(glue("Experiment {exp_moving} already imported, overwriting"),
                       type = "warning")
    }

    to_output = rbind(current_data, importing)
    raw_dat(to_output)

    l(vascr_log(l, "# Import a dataset"))
    tolog = paste("imported.df = vascr_import(instrument = '",input$import_instrument,"', raw = '",input$raw$name,"', modeled = '",input$model$name,"', map  = '",input$platemap$name,"', experiment = '",input$platemap$name,"')", sep = "")
    l(vascr_log(l, tolog))
    l(vascr_log(l, "data.df = vascr_combine(data.df, imported.df)"))

    shinyjs::reset("raw")
    shinyjs::reset("model")
    shinyjs::reset("experiment_name")
    shinyjs::reset("platemap")



  })

# TODO Fix this so import works again
 
  # shiny::observeEvent(input$load_previous, {
  #   req(input$load_previous)
  #   load_in = read.csv2(input$load_previous$datapath)
  #   print(load_in)
  #   all_data(load_in)
  # })

  shiny::observeEvent(input$load_default, {
    
    raw_dat(vascr::growth.df %>% mutate(Excluded = FALSE) %>% select(-"Sample", -"SampleID", -"Excluded", -"cells", -"line") %>% dplyr::filter(!is.na(.data$Value)))
    platemap(vascr_regenerate_map(vascr::growth.df%>% mutate(Excluded = "no")))
    vascr_notify("success", "Default data loaded")

  })
  
  named_dat = shiny::reactive({
    raw_dat() %>% vascr_apply_map(platemap())
  })
  
  dat = shiny::reactive({
    
    if(is.null(input$resample_n))
    {
      npoints = 40
    } else
    {
      npoints = input$resample_n
    }
    
    toreturn = named_dat()  %>% vascr_resample_time(npoints)
    return(toreturn)
  })

  # ///////////// Re-sample time


  output$resample_controls = shiny::renderUI(
    shiny::sliderInput("resample_n", "Number of points to resample", min = 0, max = vascr_find_count_timepoints(raw_dat()), value = vascr_find_count_timepoints(raw_dat()))
  )

  # output$original_times = shiny::renderText(paste(unique(raw_dat()$Time, collapse = ",")))

  output$resample_graph = shiny::renderPlot(vascr_plot_resample(named_dat() , newn = input$resample_n))

  output$resample_graph_range = shiny::renderPlot(vascr_resample_time(named_dat() %>% vascr_plot_line()))

  

  
  #////////////////////////// Plate map import

  # output$label_active_expt = shiny::renderUI({
  #   shiny::selectInput("active_expt_select",
  #               "Active Experiment",
  #               choices = unique(dat()$Experiment))
  # })
  # 
  # 
  # 
  # platemap = shiny::reactive({
  #   raw_dat(raw_dat() %>% mutate(Experiment = str_replace_all(Experiment, " ", "_")))
  # 
  #   dat() %>% vascr_subset(time = min(dat()$Time)) %>%
  #     select("Experiment", "Well", "Sample", "SampleID", "Excluded") %>% distinct() %>%
  #     group_by(Experiment, Sample, SampleID, Excluded) %>%
  #     summarise(Well = paste(Well, collapse = " "))
  # })


  # 
  # shiny::observeEvent(input$nav, {
  # 
  #   if(input$nav == "label")
  #   {
  #       localmap = platemap()
  #       print(localmap)
  #       c_platemap(platemap())
  #   } else if(nrow(c_platemap())>0)
  #   {
  #     updatedpm = c_platemap() %>% separate_longer_delim("Well", delim = " ") %>%
  #       separate_longer_delim("Experiment", delim = " ")
  # 
  # 
  #     updateddf = raw_dat() %>% mutate(Sample = NULL, SampleID = NULL, Excluded = NULL) %>%
  #       full_join(updatedpm, by = join_by(Well, Experiment)) %>%
  #       ungroup()
  # 
  #     raw_dat(updateddf)
  #   }
  # })

  
  

  #   # Auxiliary function
  # shinyInput <- function(FUN, len, id, ...) {
  #   inputs <- character(len)
  #   for (i in seq_len(len)) {
  #     inputs[i] <- as.character(FUN(paste0(id, i), ...))
  #   }
  #   inputs
  # }
  # 
  


  output$map <- DT::renderDT({
    platemap(platemap() %>% dplyr::mutate(Experiment = as.character(.data$Experiment), SampleID = as.character(.data$SampleID)))
      DT::datatable(platemap(), editable = TRUE, escape = F, options = list(pageLength = nrow(platemap()))) %>%
      DT::formatStyle('Sample', backgroundColor = DT::styleEqual(unique(platemap()$Sample), vascr_gg_color_hue(length(unique(platemap()$Sample)))))%>%
      DT::formatStyle('Experiment', backgroundColor = DT::styleEqual(unique(platemap()$Experiment), vascr_gg_color_hue(length(unique(platemap()$Experiment %>% as.character())))))
  }, server = FALSE)
    

  shiny::observeEvent(input$add_row_btn, {
    platemap(dplyr::add_row(platemap() %>% ungroup()))
     
  })
  
  output$add_row = shiny::renderUI({shiny::tagList(shiny::actionButton("add_row_btn", "Add another row"), 
                                            shiny::actionButton("deleteRows", "Delete Row"), 
                                            shiny::actionButton("dupRows", "Duplicate Row"))})

  shiny::observeEvent(input$map_cell_edit, {
    #get values
    info = input$map_cell_edit
    selectedrow = as.numeric(info$row)
    selectedcol = as.numeric(info$col)
    k = info$value %>% as.character()
    
    sc_name = colnames(platemap())[selectedcol]
    
    if(sc_name == "Experiment"){
      k = vascr_find_experiment(raw_dat(), k)
      k = as.character(k)
      print(k)
    }
    

    #write values to reactive

    updatedpm = platemap()
    updatedpm[selectedrow, selectedcol] = k

    platemap(updatedpm)

  })

  
  shiny::observeEvent(input$deleteRows,{
    
    if (!is.null(input$map_rows_selected)) {
      
      platemap( platemap()[-as.numeric(input$map_rows_selected),])
    }
  })
  
  shiny::observeEvent(input$dupRows,{
    
    if (!is.null(input$map_rows_selected)) {
      
      platemap(rbind(platemap(), platemap()[as.numeric(input$map_rows_selected),]))
    }
  })


  ### QC ################################################################

  selected_expt = shiny::reactiveVal(c("testing"))

  output$select_experiment = shiny::renderUI(shiny::selectInput("experiment", "Select experiment", 
                                                                choices = unique(raw_dat()$Experiment), 
                                                                selected = selected_expt()))


  deviation = shiny::reactive({
    dat() %>%
      vascr_subset(unit = "R", frequency = 4000, remove_excluded = FALSE) %>%
      vascr_summarise_deviation() %>%
      group_by(.data$Well, .data$Experiment) %>%
      summarise(max = max(.data$Median_Deviation, na.rm = TRUE))
  })


# Generate check boxes when the experiment is selected
shiny::observeEvent(input$experiment, {
  
        output$qc_wells = shiny::renderUI({
          already_chosen = unique((dat() %>% dplyr::filter(.data$Excluded == "yes"))$Well)
      
          print(input$experiment)
          
          devs = (deviation() %>% dplyr::filter(.data$Experiment == input$experiment))
          
          print("devs")
          print(devs)
      
          names = as.list(paste0(
            devs$Well,
            ifelse(
              devs$max > 0.2,
              "<span style = 'color:red'>   &#9888;</span>",
              ""
            )
          ))
          names = lapply(names, shiny::HTML)
      
              shiny::checkboxGroupInput(
                "qc_wells",
                "Exclude wells",
                choiceNames = names,
                choiceValues = devs$Well,
                selected = already_chosen
          
              )
        })
})


  grid_data = shiny::reactive({
    shiny::req(input$experiment)
    
    dat() %>%
      vascr_subset(
        unit = "R",
        frequency = 4000,
        experiment = input$experiment
      ) %>%
      vascr_resample_time(npoints = min(
        40,
        vascr_find_count_timepoints(dat())
      ))
  })

  
  
  output$plot_qc = shiny::renderPlot({
    vascr_plot_grid(grid_data())
  })


  shiny::observeEvent(input$qc_wells,{
    updatedat = platemap() %>% 
      separate_longer_delim(.data$Well, delim = " ") %>% 
      mutate(Excluded = ifelse(.data$Well %in% input$qc_wells & .data$Experiment %in% input$experiment, "yes", "no")) %>%
      group_by(.data$Experiment, .data$Sample, .data$SampleID, .data$Excluded) %>%
      summarise(Well = paste(.data$Well, collapse = " "))
    platemap(updatedat)
    print(platemap())
  })


  # Plotted outputs //////////////////////////////////////////////



  # Generate the plotted outputs
  protect = function(plot)
  {
    if (nrow(dat())==0)
    {
      return(ggplot() + geom_text(aes(x = 0, y = 0, label = "No Data")) + theme_void())
    } else
    {
      return(plot)
    }
  }


  shiny::observe({

    normtime = if(isTRUE(input$normalise =="none"))
    {
      normtime = NULL
    } else
    {
      normtime = as.numeric(input$normalise)
    }

      output$plot_line <- shiny::renderPlot(protect(dat() %>% vascr_subset(unit = input$unit, frequency = input$frequency) %>%
                                       dplyr::filter(.data$Excluded == "no") %>%
                                       dplyr::filter(.data$Sample %in% input$sample) %>%
                                       vascr_normalise(normtime) %>%
                                       vascr_summarise(level = input$level) %>%
                                       vascr_plot_line())) 

      # print(.Last.value)
  })


 plot_ANOVA_generator = shiny::reactive({
  dat() %>%
     dplyr::filter(.data$Excluded == "no") %>%
     dplyr::filter(.data$Sample %in% input$sample) %>%
     vascr_plot_anova(unit = input$unit, frequency = input$frequency,
     time = as.numeric(input$time_single),
      reference = input$reference)
   
 })
  
 output$plot_ANOVA <- shiny::renderPlot(plot_ANOVA_generator())


  output$plot_cc <- shiny::renderPlot(dat() %>%
                                        dplyr::filter(.data$Excluded == "no") %>%
                                        dplyr::filter(.data$Sample %in% input$sample) %>%
                                        vascr_subset(unit = input$unit, frequency = input$frequency) %>%
                                        vascr_plot_cc_stats(unit = input$unit, frequency = input$frequency, reference = input$reference)
                                  )

  # Exporting data
  ####################### Export ####################################

  output$export_controls = shiny::renderUI({
    shiny::tagList(
        shiny::downloadButton('downloadDataR', 'Save data'),
        shiny::downloadButton('downloadDataCSV', 'Download vascr dataframe'),
        shiny::downloadButton('downloadDataP', 'Download prism compatable xlsx spreadsheet'),
        shiny::actionButton("return_to_r", "Return values to R"))
  })

  output$downloadDataR <- shiny::downloadHandler(
    filename = function() {
      paste("vascr_output", format(Sys.time()), '.RData', sep='')
    },
    content = function(con) {
      savedata = dat()
      save(savedata, file = con)
    }
  )

  output$downloadDataCSV <- shiny::downloadHandler(
    filename = function() {
      paste("vascr_output", format(Sys.time()), '.csv', sep='')
    },
    content = function(con) {
      utils::write.csv2(dat(), con)
    }
  )

  output$downloadDataP <- shiny::downloadHandler(
    filename = function() {
      paste("vascr_output_prism", format(Sys.time()), '.xlsx', sep='')
    },
    content = function(con) {
      vascr_export_prism(dat(), con)
    }
  )

  shiny::observeEvent(input$return_to_r , {
    shiny::stopApp(dat())
  })


  ######## Logs

  output$log = shiny::renderText(l())

}

return(server)

}


#' Launch the vascr UI
#'
#' @returns A shinyApp to work with vascr data
#' 
#' @param data.df Data to preload into shiny app
#' 
#' @export
#' 
#' @importFrom rlang check_installed
#' 
#' @examples
#'  if(interactive()){
#'    vascr_shiny()
#'  }
vascr_shiny = function(data.df)
{
  # Web packages do not ship with shiny by default, hence offer to install if required
  rlang::check_installed(pkg = c("shiny", "bslib", "DT", "shinyjs"), reason ="to run the web technologies required to render the vascr UI. You must install these packages to continue to the vascr UI")
  
  # Launch the UI
  shiny::shinyApp(vascr_ui, vascr_serve(data.df))
}

# runApp(vascr_shiny())
