server_fct <- function(input, output, session){
  ## build fixes : start ##
  Target.ID <- NULL
  Genotype <- NULL
  locus <- NULL
  accept <- NULL
  p_value <- NULL
  selected_ <- NULL
  meta <- NULL
  pop <- NULL
  . <- NULL
  lat <- NULL
  lon <- NULL
  aims_example <- NULL
  genotype <- NULL
  ## build fixes : end ##
  if(!exists("db_list")) db_list <- get("db_list", envir = -2)
  if(!exists("reporting_panel")) reporting_panel <- get("reporting_panel", envir = -2)
  
  observeEvent(input$analyse, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("analyse", result())
  })
  
  ## USER INTERFACE
  output$analysis <- renderUI({
    res <- result()
    prof <- Profile()
    dat <- Dataset()
    if(is.null(res)){
      if(is.null(dat) || nrow(dat) == 0){
        fluidPage(
          h3("Upload valid AIMs profile"),
          HTML("<p>Upload valid file: It must contain a column  
             containing marker (locus) names, and a column with the genotypes as e.g. 
             <tt>AA</tt> or <tt>AC</tt></p>"),
          HTML("You can download a sample file showing the necessary columns and data structure here:"),
          downloadLink("download_sample", "Download")
        )
      }
      else{
        ## Make column suggestions
        guess_locus_text <- and_text(pre = "<p>Suggestions for <b>locus column</b>: ", x = columns$locus_guess, post = "</p>")
        guess_genotype_text <- and_text(pre = "<p>Suggestions for <b>genotype column</b>: ", x = columns$genotype_guess, post = "</p>")
        #
        fluidPage(
          fluidRow(h3("Uploaded file")),
          fluidRow(
            HTML("<p>The first few rows of the uploaded file are shown below</p>"),
            HTML(guess_locus_text),
            HTML(guess_genotype_text)
          ),
          fluidRow(
            renderDT({
              datatable(dat, options(dom = "t"))
            })
          )
        )
      }
    }
    else{
      fluidPage(
        tags$head(tags$style(paste0(".modal-lg{ width: ", 2*session$clientData$output_barplot_width,"px}"))),
        ## tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: none !important; font-weight: bold;}')),
        fluidRow(HTML(paste0("<p><b>Analysis of file:</b> ",input$profile_file$name,":")),
                 actionLink("show_profile", label = "Show profile"),
                 icon("new-window", lib = "glyphicon"),
                 HTML("</p>")
                 ),
        fluidRow(tags$h2("Graphics")),
        fluidRow(
          column(width = 6, uiOutput("barplot_panel") ),
          column(width = 6, uiOutput("map_panel") )
        ),
        fluidRow(tags$h2("Tables")),
        fluidRow(DTOutput("result_table")),
        fluidRow(tags$h2("Likelihood ratios")),
        fluidRow(
          column(width = 6, DTOutput("lr_list")),
          column(width = 6, uiOutput("LRplot_panel") )
        )
      )
    }
  })
  
  ## 
  
  output$download_sample <- downloadHandler(
    filename <- function(){
      paste("aims_example", "csv", sep = ".")
    },
    contentType = "text/csv",
    content = function(file) {
      aims_example <- read_csv(system.file("deployable_app", "aims_example.csv", package = "genogeographer"), col_types = "cc")
      write_csv(aims_example, path = file)
    }
  )
  
  ## REACTIVES
  
  observeEvent(input$show_profile, {
    A1 <- NULL
    A2 <- NULL
    profile <- Profile() %>% 
      profile_AA_x0(df = db$db, select = c("locus", "A1", "A2"), keep_dropped = TRUE)
    profile_drop <- profile$profile_drop %>% 
      mutate(genotype = paste0(A1, A2)) %>% select(locus, genotype)
    profile_x0 <- profile$profile_x0 %>% 
      mutate(genotype = paste0(A1, A2)) %>% select(locus, genotype)
    showModal(modalDialog(
      title = paste("Uploaded profile:",input$profile_file$name),
      size = "m",
      h3("Analysed loci"),
      helpText("The loci below has been included in the analysis."),
      renderDT(profile_x0 %>% 
               DT::datatable(rownames=FALSE, filter = "bottom", selection = 'none',
                             extensions = 'Buttons', 
                             options = list(
                               dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               lengthMenu = list(c(10, 25, 50, 100, -1),
                                                 c("10", "25", "50", "100", "All"))
                               )
                             )
               ),
      h3("Dropped loci"),
      helpText(paste0("The loci below has been excluded from the analysis.\n
               Either because of state 'NN', locus not in '",input$snp_set,"' 
                      or other typing error (e.g. different reference allele).")),
      renderDT(profile_drop %>% 
                 DT::datatable(rownames=FALSE, filter = "bottom", selection = 'none',
                               extensions = 'Buttons', 
                               options = list(
                                 dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10, 25, 50, 100, -1),
                                                   c("10", "25", "50", "100", "All"))
                               )
                 )
      ),
      footer = modalButton("Close"),
      easyClose = TRUE
      ))
    })
  
  ### ANALYSIS
  
  observeEvent(input$reset,{
    runjs("history.go(0)")
  })
  
  output$uploaded_profile <- renderTable({
    if (is.null(input$profile_file)) return(NULL)
    Profile()
  })
  
  db <- reactiveValues(db = NULL)
  barplot_selected <- reactiveValues(which = NULL)
  bar_ranges <- reactiveValues(x = NULL, y = NULL)
  map_ranges <- reactiveValues(x = NULL, y = NULL)
  LR_lists <- reactiveValues(pop = NULL, meta = NULL)
  columns <- reactiveValues(locus = "Target.ID", locus_guess = NULL,
                            genotype = "Genotype", genotype_guess = NULL)
  
  output$column_panel <- renderUI({
    sel_locus <- input$col_locus
    sel_genotype <- input$col_genotype
    opt_locus <- columns$locus
    opt_genotype <- columns$genotype
    ## if(!is.null(sel_locus)) opt_genotype <- opt_genotype[!grepl(sel_locus, opt_genotype)]
    verticalLayout(
      selectInput(inputId = "col_locus", label = "Locus column:", choices = opt_locus, multiple = FALSE, selected = sel_locus),
      selectInput(inputId = "col_genotype", label = "Genotype column:", choices = opt_genotype, multiple = FALSE, selected = sel_genotype)
    )
  })
  
  
  observe({
    db$db <- db_list[[ifelse(is.null(input$snp_set), 1, input$snp_set)]]
    barplot_selected$which <- NULL
  })
  
  output$dbs <- renderUI({
    selectInput(inputId = "snp_set", label = "Select frequency database:", choices = names(db_list))
  })
  
  
  output$fileUploaded <- reactive({
    return(nrow(Dataset())>0)
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  Dataset <- reactive({
    if(is.null(input$profile_file)) { # User has not uploaded a file yet
      return(tibble())
    }
    ## data <- read.csv(input$profile_file$datapath, header = TRUE, stringsAsFactors = FALSE) %>% as_tibble()
    # browser()
    ext <- rio::get_ext(input$profile_file$datapath)
    if(grepl("xls", ext)) data <- rio::import(input$profile_file$datapath) %>% as_tibble()
    else data <- rio::import(input$profile_file$datapath, header = TRUE) %>% as_tibble()
    ## data %>% head() %>% print()
    ## Remove NA columns
    data <- data %>% select(which(map_lgl(.x = ., .f = ~ !all(is.na(.x)))))
    ndata <- names(data)
    ## Update columns:
    columns$locus <- ndata
    columns$genotype <- ndata
    ## 
    columns$locus_guess <- nullset(ndata, map_lgl(data, function(x) all(grepl(pattern = "^rs.*", x))))
    columns$genotype_guess <- nullset(ndata, map_lgl(data, function(x) all(nchar(x)<=2) & all(unlist(strsplit(paste(x),"")) %in% c("A", "C", "G", "T", "N", "-"))))
    ##
    updateSelectInput(session, inputId = "col_locus", selected = columns$locus_guess[1])
    updateSelectInput(session, inputId = "col_genotype", selected = columns$genotype_guess[1])
    ## return read data
    data
  })
  
  Profile <- reactive({
    profile <- Dataset()
    if(is.null(profile) | nrow(profile) == 0) return(NULL) ## No profile
    ##
    nprofile <- names(profile)
    col_return <- FALSE
    if(!(input$col_locus %in% nprofile)){ 
      updateSelectInput(session, inputId = "col_locus", selected = NULL) 
      col_return <- TRUE
      }
    if(!(input$col_genotype %in% nprofile)){ 
      updateSelectInput(session, inputId = "col_genotype", selected = NULL) 
      col_return <- TRUE
    }
    if(col_return) return(NULL)
    ##
    if(input$col_locus == input$col_genotype) return(NULL)
    profile <- profile %>% 
      select_(locus = paste0("'",input$col_locus,"'"), genotype = paste0("'",input$col_genotype,"'")) %>%
      extract(col = genotype, into = c("A1","A2"), regex = "(.{1})(.{1})", remove = TRUE) %>%
      arrange(locus)
    ## "-" fix
    profile <- profile %>% filter(!is.na(A1), !is.na(A2))
    ## Validate input columns
    OK_locus <- all(grepl("^rs[0-9].*", profile$locus))
    DNA_bases <- c("A", "C", "G", "T", "N", "-")
    OK_genotype <- all(c(profile$A1 %in% DNA_bases), c(profile$A2 %in% DNA_bases))
    if(!OK_locus | !OK_genotype) profile <- NULL
    ## 
    profile
  })
  
  result <- eventReactive(list(input$profile_file,input$analyse),{
    bar_ranges$x <- NULL
    bar_ranges$y <- NULL
    profile <- Profile()
    if(is.null(profile)) return(NULL)
    # print("OK - pre profile")
    # browser()
    profile <- profile %>% as_tibble() %>% profile_AA_x0(df = db$db)
    tilt_ <- if(is.null(input$tilt)) FALSE else (input$tilt == "adjust")
    # print("OK - pre genogeo")
    result_pop <- genogeo(profile = profile, df = db$db, CI = input$CI/100, min_n = input$min_n, tilt = tilt_)
    ### Fixed clusters based on STRUCTURE analysis
    result_meta <- genogeo(profile = profile, df = db$db, CI = input$CI/100, min_n = input$min_n, grouping = "meta", tilt = tilt_)
    result_list <- list(pop = result_pop, meta = result_meta)
    ## ADMIXTURE
    admix_ <- if(is.null(input$admix)) FALSE else (input$admix == "admix")
    if(!admix_) result_admix <- NULL
    else{
      result_admix <- profile_admixture(x0 = profile, df = db$db, grouping = input$meta, hyp = NULL)
      result_list[[input$meta]] <- add_results(result_list[[input$meta]], result_admix)
    }
    ##
    result_list
  })
  
  # admixture <- eventReactive(list(input$profile_file,input$analyse, input$admix),{
  #   admix_ <- if(is.null(input$admix)) FALSE else (input$admix == "admix")
  #   if(!admix_) return(NULL)
  #   profile <- Profile()
  #   if(is.null(profile)) return(NULL)
  #   # print("OK - pre profile")
  #   # browser()
  #   profile <- profile %>% as_tibble() %>% profile_AA_x0(df = db$db)
  #   result_admix <- profile_admixture(x0 = profile, df = db$db, grouping = input$meta, hyp = NULL)
  #   result_admix
  # })
  
  
  output$side_pvalue <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    largest_p_value <- res[[input$meta]] %>% filter(accept)
    if(nrow(largest_p_value)==0) largest_p_value_text <- paste0("All ",ifelse(input$meta == "meta", "meta-", ""),"populations rejected")
    else{
      largest_p_value <- largest_p_value %>% top_n(n = 1, wt = p_value) %>% select(p_value, 2) %>% mutate(p_value = round(p_value, 3))
      largest_p_value_ <- paste0(largest_p_value$p_value, " (",largest_p_value[[2]],")")
      largest_p_value_text <- paste0("<b>DB-score (largest p-value):</b><br/>", largest_p_value_)
    }
    verticalLayout(
      h4("Reporting"),
      HTML(paste0("<p>",largest_p_value_text,"</p>"))
    )
  })
  
  ### MAP
  
  output$map_panel <- renderUI({
    div(style = paste0("position:relative; ",
                       "height: ",0.8*session$clientData$output_barplot_width,"px;"),
        withSpinner(plotOutput("map",
                               dblclick = "map_dblclick",
                               brush = brushOpts(id = "map_brush", 
                                                 delay = 800, 
                                                 resetOnNew = TRUE, 
                                                 direction = "xy"),
                               hover = hoverOpts(id = "map_hover", delay = 100, delayType = "debounce"),
                               click = clickOpts(id = "map_click"),
                               width = "100%"), type = 4),
        uiOutput("hover_map"))
  })
  
  observeEvent(input$map_click, {
    res <- result()
    if(is.null(res)) return(NULL)
    clicked_pop <- nearPoints(res[[input$meta]], input$map_click, threshold = 10, maxpoints = 1)[[1]]
    if(length(clicked_pop)==0) return(NULL)
    ## Already selected
    lr_pop <- input[[paste0("lr_",input$meta)]]
    if(is.null(lr_pop)) return(NULL)
    ## Remove
    if(clicked_pop %in% lr_pop) lr_pop <- lr_pop[lr_pop!=clicked_pop]
    ## Add
    else lr_pop <- c(lr_pop,clicked_pop)
    updateSelectizeInput(session, paste0("lr_",input$meta), selected = lr_pop)
  })
  
  ### BARPLOT
  
  output$barplot_panel <- renderUI({
    div(style = paste0("position:relative; ",
                       "height: ",0.8*session$clientData$output_barplot_width,"px;"),
        withSpinner(plotOutput("barplot", 
                   dblclick = "barplot_dblclick",
                   brush = brushOpts(id = "barplot_brush", 
                                     delay = 800, 
                                     resetOnNew = TRUE, 
                                     direction = "xy"), 
                   hover = hoverOpts(id = "barplot_hover", delay = 100, delayType = "debounce"),
                   width = "100%"), type = 4), 
        uiOutput("hover_barplot"))
  })
  
  observeEvent(input$barplot_brush, {
    res <- result()
    if(is.null(res)) return(NULL)
    barplot_selected$which <- brushedPoints(df = res[[input$meta]], brush = input$barplot_brush, allRows = TRUE) %>% 
      select(1, selected_)
  })
  
  ## TABLES
  
  output$result_table <- renderDT({
    res <- result()[[input$meta]]
    ## admix_res <- admixture()
    if(is.null(barplot_selected$which)) return(result_table(res, lr_listed = input[[paste0("lr_",input$meta)]]))
    result_table(res %>% inner_join(barplot_selected$which, by = names(res)[1]), .filter = "selected_", lr_listed = input[[paste0("lr_",input$meta)]])
  })
  
  ## LR calculations and controls
  
  output$LR_select <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    meta_pvalue <- res$meta %>% filter(accept) %>% pull(meta)
    pop_pvalue <- res$pop %>% filter(accept) %>% pull(pop)
    verticalLayout(
      checkboxGroupInput(inputId = "LR_accept", 
                         label = "LRs with two rejected populations:", 
                         choiceNames = list("Allow (LRs may be misleading)"), choiceValues = list("allow")),
      conditionalPanel("input.meta == 'meta'",
                       selectizeInput(inputId = "lr_meta", 
                                      label = "Metapopulations for LR", 
                                      choices = paste(res$meta$meta), 
                                      selected = meta_pvalue, ## unique(c(meta_pvalue,input$lr_meta)),
                                      multiple = TRUE,
                                      options = list(plugins = list("remove_button", "drag_drop")), 
                                      width = "100%")
      ),
      conditionalPanel("input.meta == 'pop'",
                       selectizeInput(inputId = "lr_pop", 
                                      label = "Populations for LR", 
                                      choices = paste(res$pop$pop),
                                      selected = pop_pvalue, ## unique(c(pop_pvalue, input$lr_pop)),
                                      multiple = TRUE,
                                      options = list(plugins = list("remove_button", "drag_drop")), 
                                      width = "100%")
      )
    )
  })
  
  LR_Table <- reactive({
    accepted_ <- if(is.null(input$LR_accept)) FALSE else (input$LR_accept == "allow")
    res <- result()[[input$meta]]
    list(
      res = res, 
      LR_Tab = LR_table(result_df = res, lr_populations = input[[paste0("lr_",input$meta)]], CI = input$CI/100, only_accepted = !accepted_)
      )
  })
  
  output$lr_list <- renderDT({
    LR_Tab <- LR_Table()
    LR_list(result = LR_Tab$res, LR_tab = LR_Tab$LR_Tab)
  })
  
  observeEvent(input$lr_pop,{ ## Can't delete most probable (based on z_score)
    res <- result()
    if(is.null(res)) return(NULL)
    max_score_pop <- res$pop %>% filter(accept) %>% top_n(n = 1, wt = p_value) %>% pull(var = 1) ## Highest p-value
    lr_pop <- unique(c(input$lr_pop, max_score_pop))
    updateSelectizeInput(session, "lr_pop", selected = lr_pop)
  })
  
  observeEvent(input$lr_meta,{ ## Can't delete most probable (based on z_score)
    res <- result()
    if(is.null(res)) return(NULL)
    max_score_meta <- res$meta %>% filter(accept) %>% top_n(n = 1, wt = p_value) %>% pull(var = 1) ## Highest p-value
    lr_meta <- unique(c(input$lr_meta, max_score_meta))
    updateSelectizeInput(session, "lr_meta", selected = lr_meta)
  })
  
  # observeEvent(input$lr_list_rows_current,{
  #   cat(paste0("Current LR rows: ",paste0(input$lr_list_rows_current, collapse = " "),"\n"))
  # })
  
  # observeEvent(input$result_table_rows_selected,{ ## Can't delete most probable (based on z_score)
  #   cat(paste0("Selected table rows: ",paste0(input$result_table_rows_selected, collapse = " "),"\n"))    # res <- result()
  # })
  
  ##
  
  ## PLOTS

  LR_plot <- function(...) NULL
  
  plot_LR <- reactive({
    LR_Tab <- LR_Table()
    if(is.null(LR_Tab$LR_Tab) | nrow(LR_Tab$LR_Tab) == 0) return(NULL)
    LR_plot(result = LR_Tab$res, LR_list = LR_Tab$LR_Tab, rows = input$lr_list_rows_current, theme_ = theme_bw(base_size = 18))
  })
  
  output$LRplot <- renderPlot({
    plot_LR()
  },height = function() {
    scale <- length(input$lr_list_rows_current)
    # 0.8*max(10, scale)/10*session$clientData$output_LRplot_width
    scale*45 + 200
  })
  
  output$LRplot_panel <- renderUI({
    div(style = paste0("position:relative; ",
                       "height: ",0.8*session$clientData$output_LRplot_width,"px;"),
        withSpinner(plotOutput("LRplot", width = "100%"), type = 4))
  })
  
  plot_bars <- reactive({
    res <- result()
    if(is.null(res)) return(NULL)
    return(error_bar_plot(data = res[[input$meta]]) + coord_cartesian(xlim = bar_ranges$x, ylim = bar_ranges$y))
  })
  
  observeEvent(input$barplot_dblclick, {
    brush <- input$barplot_brush
    if (!is.null(brush)) {
      bar_ranges$x <- c(brush$xmin, brush$xmax)
      bar_ranges$y <- c(brush$ymin, brush$ymax)
      barplot_selected$which <- NULL
    } else {
      if(is.null(bar_ranges$x)){
        showModal(
          modalDialog(title = "Error bar plot", size = "l", 
                      easyClose = TRUE, footer = modalButton("Close"),
                      fluidPage(div(style = paste0("position:relative; ",
                                 "height: ",2*0.8*session$clientData$output_barplot_width,"px;"),
                      plotOutput("barplotPopup", width = paste0(1.8*session$clientData$output_barplot_width,"px"))
                      ))
          )
        )
      }
      else{
        bar_ranges$x <- NULL
        bar_ranges$y <- NULL
        barplot_selected$which <- NULL
      }
    }
  })
  
  output$barplotPopup <- renderPlot({ 
    plot_bars() + theme_bw(base_size = 18)
  },height = function() {
    0.8*session$clientData$output_barplotPopup_width
  })
  
  output$barplot <- renderPlot({ 
    plot_bars() + theme_bw(base_size = 18)
  },height = function() {
    0.8*session$clientData$output_barplot_width
  })
  
  plot_map <- reactive({
    res <- result()
    if(is.null(res)) return(NULL)
    ## SELECTED POINTS IN BAR-PLOT
    res_ <- res[[input$meta]]  
    if(is.null(barplot_selected$which)) res_ <- res_ %>% mutate(selected_ = FALSE)
    else res_ <- inner_join(res_, barplot_selected$which, by = names(res_)[1])
    ## LR populations
    if(is.null(input[[paste0("lr_", input$meta)]])) res_ <- res_ %>% mutate(LR_listing = "No")
    else res_ <- res_ %>% mutate(LR_listing = ifelse(.[[1]] %in% input[[paste0("lr_",input$meta)]], "Yes", "No"))
    ## 
    res_ <- subset(res_, !(is.na(lat) | is.na(lon))) ## Discard pops with no location
    map_plot(res_)
  })
  
  observeEvent(input$map_dblclick, {
    brush <- input$map_brush
    if (!is.null(brush)) {
      map_ranges$x <- c(brush$xmin, brush$xmax)
      map_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      if(is.null(map_ranges$x)){
        showModal(
          modalDialog(title = "Map plot",size = "l", 
                      easyClose = TRUE, footer = modalButton("Close"),
                      fluidPage(div(style = paste0("position:relative; ",
                                                   "height: ",2*0.8*session$clientData$output_map_width,"px;"),
                      plotOutput("map_popup", paste0(1.8*session$clientData$output_barplot_width,"px"))
                      ))
          )
        )
      }
      else{
        map_ranges$x <- NULL
        map_ranges$y <- NULL
      }
    }
  })
  
  output$map_popup <- renderPlot({
    plot_map() + theme_bw(base_size = 18)
  },height = function() {
    0.8*session$clientData$output_map_popup_width
  })
  
  output$map <- renderPlot({
    plot_map() +  ## $plot
      coord_cartesian(xlim=map_ranges$x, ylim=map_ranges$y, expand = FALSE) + 
      theme_bw(base_size = 18)
  },height = function() {
    0.8*session$clientData$output_map_width
  })
  
  ## ADDITIONAL PLOTS
  
  output$hover_map <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    ## bg_colours <- bar_colour(res[,c("logP","accept",names(res)[1])], alpha = 0.1)
    hover <- input$map_hover
    if(is.null(hover)) return(NULL)
    point <- nearPoints(res[[input$meta]], hover, threshold = 5, maxpoints = 1)
    tool_tip(hover = hover, point = point) ## , bg = bg_colours[paste(point[[1]][1])]
  })
  
  output$hover_barplot <- renderUI({
    res <- result()
    if(is.null(res)) return(NULL)
    hover <- input$barplot_hover
    if(is.null(hover)) return(NULL)
    point <- nearPoints(res[[input$meta]], hover, threshold = 5, maxpoints = 1)
    tool_tip(hover = hover, point = point)
  })
  
  ### RETURN pdf REPORT
  # 
  output$report_panel <- renderUI({
    res <- result()
    if(!reporting_panel) return(verticalLayout())
    if(is.null(res)) return(verticalLayout())
    if(!requireNamespace("tidyverse")) return(verticalLayout(helpText("Needs `tidyverse` for generating report!")))
    verticalLayout(
      h4("Report"),
      textInput(inputId = "name", label = "Name of analyst", width = "100%", 
                placeholder = "Name as to appear in report", value = input$name),
      radioButtons(inputId = 'format', label = 'Report format', choices = c('PDF', 'HTML', 'Word'),
                   inline = TRUE, selected = input$format),
      withBusyIndicatorUI(downloadButton(outputId = "report_download", label = paste0("Download (",input$format,")"), class = "btn-primary")),
      hr()
    )
  })
  # 

  output$report_download = downloadHandler(
##  filename = "test.html",
    filename = function(){
      paste(sub("\\.[[:alnum:]]*.$","",input$profile_file$name), sep =".",
            switch(
              input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
    },
    content = function(file) {
      ## withProgress(message = "Generating report..", {
      src <- list(rmd_file = normalizePath(system.file("deployable_app", "aims_report.Rmd", package = "genogeographer")))
      ## cat(file = stderr(), src$rmd_file, "\n")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      cat(file = stderr(), owd, "\n")
      on.exit(setwd(owd))
      file.copy(src$rmd_file, 'aims_report.Rmd', overwrite = TRUE)
      out <- rmarkdown::render(input = 'aims_report.Rmd', clean = TRUE,
                               output_format = switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ),
        params = list(
          set_file = input$profile_file$name,
          set_author = input$name,
          set_output = input$format
          )
        )
      ## cat(file = stderr(), "File start", "\n")
      ## out <- render('aims_report.Rmd', clean = TRUE)
      ## cat(file = stderr(), "File end", "\n")
      file.rename(out, file)
    }
    )
  # pdf REPORT
}