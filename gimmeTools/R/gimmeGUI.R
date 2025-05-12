#' @title Graphical User Interface for the gimme R Package
#' @name gimmeGUI
#' @description A graphical user interface for the gimme R package. Easily facilitates the specification of directories, custom subgroups, fixed and freed paths for estimation, and various other options.
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @import shiny 
#' @import miniUI
#' @import rstudioapi
#' @importFrom rhandsontable rhandsontable rHandsontableOutput hot_to_r renderRHandsontable hot_col
#' @importFrom shinyWidgets prettyRadioButtons prettyCheckbox updateSwitchInput switchInput 
#' @importFrom rintrojs introBox introjs introjsUI
#' @importFrom magrittr %>% 
#' @importFrom tools file_path_sans_ext
#' @importFrom easycsv choose_dir
#' @importFrom utils read.table 
#' @examples 
#' \dontrun{
#' gimmeGUI()
#' }
#' @references Lane, S.T. & Gates, K.M. (2017). Automated selection of robust
#' individual-level structural equation models for time series data.
#' Structural Equation Modeling, 24, 768-782.
#' @references Gates, K.M. & Molenaar, P.C.M. (2012). Group search algorithm
#' recovers effective connectivity maps for individuals
#' in homogeneous and heterogeneous samples. NeuroImage, 63, 310-319.
#' @export
NULL

gimmeGUI <- function(){
  
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  introjsUI(),
  fluidRow(
    column(4, 
           introBox(
             h2("gimmeGUI"),
             data.step = 1, 
             data.intro = "Welcome to the gimmeGUI! 
             This quick-start guide will walk you through the use of 
             gimme.")
           ),
    column(4, br(), 
           actionButton("btn", h4("Getting Started Guide")), align = "left")
           ),
  br(),
  fluidRow( 
    column(4, 
           tabsetPanel(
             tabPanel("Main Options",
                      br(),
                      introBox(
                        selectInput("func", "Select gimme function", 
                                    c("gimmeSEM", "indSEM", "aggSEM"), 
                                    selected = "gimmeSEM"), 
                        data.step = 2, 
                        data.intro = "Here, you select the function you will use for your
                        analysis. gimmeSEM looks for both group and 
                        individual-level relationships. indSEM uses no shared 
                        information, and arrives at 
                        individual-level models only.
                        aggSEM concatenates all individuals and arrives at 
                        one model."),
                      introBox(
                        actionButton("inputdir", "Select Input Directory"), 
                        actionButton("outdir", "Select Output Directory"),
                        helpText("Depending on your operating system (PC, Mac, Linux), the file browser may open
                                 behind other active windows on your machine."),
                        data.step = 3, 
                        data.intro = "Here, select the location of your input and output
                        directories. The input directory should contain
                        a data file for each individual, where the columns 
                        represent variables and the rows represent time points 
                        (e.g., individual scans in fMRI or individual measurement 
                        occasions in daily diary studies). The output directory
                        should be empty."),
                      hr(),
                      introBox(
                        prettyRadioButtons("sep", "File delimiter", 
                                           c(
                                             "Comma Separated (.csv)" = "csv",
                                             "Tab Separated (.txt)" = "tab",
                                             "Space Separated (.txt)" = "space"
                                           ), icon = icon("check")),
                        data.step = 4, 
                        data.intro = "gimme allows comma separated, tab separated, 
                        and space separated files. Select
                        which is appropriate for your data."),
                      introBox(
                        prettyRadioButtons("header",
                                           "Do you have variable names in the first row of
                                           your data files?",
                                           c("Yes", "No"),
                                           selected = "Yes", inline = TRUE,
                                           icon = icon('check')),
                        data.step = 5, 
                        data.intro = "If your data files have a header row, 
                        select 'Yes.' Otherwise, select 'No', and default 
                        variable names will be assigned automatically."),
                      introBox(
                        prettyRadioButtons("ar", "Freely estimate autoregressive paths?", 
                                           c("Yes (Recommended)", "No"), 
                                           selected = "Yes (Recommended)", inline = TRUE,
                                           icon = icon("check")),
                        data.step = 6, 
                        data.intro = "We recommend freely estimating autoregressive paths. 
                        By selecting 'Yes', all autoregressive relationships (e.g., 
                        a variable X predicts itself at the next time point)
                        will be included 
                        at the start of estimation, and will stay in all models."),
                      introBox(
                        prettyRadioButtons("plots", "Automatically generate plots?",
                                           c("Yes (Recommended)", "No"),
                                           selected = "Yes (Recommended)", inline = TRUE,
                                           icon = icon("check")),
                        data.step = 7, 
                        data.intro = "Free plots! Automatically generated in qgraph for the 
                        sample, each individual, and each 
                        subgroup (where applicable)."),
                      introBox(
                        conditionalPanel(condition = "input.func == 'gimmeSEM'", 
                                         prettyRadioButtons("sub", "Subgroup individuals?", 
                                                            c("Yes", "No"), 
                                                            selected = "Yes", 
                                                            inline = TRUE,
                                                            icon = icon("check"))),
                        data.step = 8, 
                        data.intro = "If 'Yes', gimme will automatically subgroup individuals
                        based on similarities in their group and individual 
                        processes using walktrap algorithm. Option only available
                        for gimmeSEM, not indSEM or aggSEM.")
                      ),
             tabPanel("Advanced Options",
                      br(),
                      conditionalPanel(condition = "input.func == 'gimmeSEM'", 
                                       #  wellPanel(
                                       introBox(sliderInput("groupcutoff",
                                                            "Group Cutoff:",
                                                            min = 0,
                                                            max = 1,
                                                            value = .75),
                                                data.step = 10, 
                                                data.intro = "Adjust this cutoff to change 
                                                the proportion of individuals 
                                                for whom a path must be significant
                                                in order for it to exist in the group-level model.",
                                                div(id = "grouptext", 
                                                    helpText("The group cutoff sets the percentage
                                                             of individuals for whom a path must 
                                                             be significant in order for it to be
                                                             added to the group-level model."))),
                                       #    )
                                       hr()
                                                    ), 
                      conditionalPanel(
                        condition = "input.sub == 'Yes'",
                        # wellPanel(
                        introBox(
                          conditionalPanel(
                            condition = "input.func == 'gimmeSEM'", 
                            sliderInput("subcutoff",
                                        "Subgroup Cutoff:",
                                        min = 0,
                                        max = 1,
                                        value = .5),
                            div(id = "subtext", 
                                helpText("The subgroup cutoff sets the 
                                         percentage of individuals within a
                                         subgroup for whom a path must be 
                                         significant in order for it to be 
                                         added to the subgroup-level 
                                         model.")
                                ),
                            hr()
                                ), 
                          data.step = 11, 
                          data.intro = "Adjust this cutoff to change the proportion of 
                          individuals for whom a path must be significant in 
                          order for it to exist in the subgroup-level model."),
                        #  ),
                        conditionalPanel(
                          condition = "input.func == 'gimmeSEM'", 
                          introBox(
                            switchInput(inputId = "confsub2", 
                                        label = "<b>Confirmatory Subgroups</b>", 
                                        labelWidth = 225, 
                                        onLabel = "Add", 
                                        offLabel = "Save",
                                        value = FALSE,
                                        width = "100%",
                                        disabled = TRUE),
                            data.step = 12, 
                            data.intro = "Here, the user may specify 
                            confirmatory, a priori subgroup 
                            assignments. With this option, 
                            the subgroups are fixed, and the 
                            algorithm will search for paths
                            common to members of each 
                            subgroup after the
                            sample-level (group-level) 
                            search.",
                            div(id = "confsubtext", 
                                helpText("Confirmatory subgroups can be 
                                         specified if the researcher has an a priori
                                         subgroup structure in mind. Use the above switch to 
                                         toggle between 'Add' and 'Save.'"))),
                          hr()
                                )
                            ),
                      #  wellPanel(
                      introBox(
                        switchInput(inputId = "paths2", 
                                    label = "<b>Confirmatory Paths</b>", 
                                    labelWidth = 225, 
                                    onLabel = "Add", 
                                    offLabel = "Save",
                                    value = FALSE,
                                    width = "100%",
                                    disabled = TRUE),
                        data.step = 13, 
                        data.intro = "Here, the user may specify confirmatory paths. 
                        These paths will be estimated for all individuals,
                        and will not be removed during any stage of the 
                        search procedure.",
                        div(id = "pathtext", 
                            helpText("Add confirmatory paths if there are relationships 
                                     that you want to be freely estimated for all 
                                     individuals. Use the above switch to 
                                     toggle between 'Add' and 'Save.'"))),
                      hr(),
                      #  ),
                      introBox(
                        switchInput(inputId = "respaths2", 
                                    label = "<b>Restricted Paths</b>", 
                                    labelWidth = 225, 
                                    onLabel = "Add", 
                                    offLabel = "Save",
                                    value = FALSE,
                                    width = "100%",
                                    disabled = TRUE),
                        data.step = 14, 
                        data.intro = "Here, the user may specify restricted paths. These
                        paths will be fixed to zero for all individuals, 
                        and will not be added during any stage of the 
                        search procedure. ",
                        div(id = "respathtext", 
                            helpText("Set restricted paths if there are relationships that 
                                     you do not want to be freely estimated for all 
                                     individuals. Use the above switch to 
                                     toggle between 'Add' and 'Save.'")))
                            ),
             wellPanel(
               introBox(
                 downloadButton("download", "Download R Script"), 
                 data.step = 15, 
                 data.intro = "Finally, download the R script to your computer! 
                 You can then run the program from the complete 
                 script.")
               ) 
               )
             ),
    column(8,
           tabsetPanel(
             tabPanel("Syntax",
                      tags$style(type = 'text/css', 
                                 '#syntax {background-color: #F5F5F5; border-color: "black"; 
                                 color:"#000000"; font-size:14px; font-family:"Lucida Console", 
                                 monospace}'), 
                      tags$style("word-wrap: break-word;"),
                      wellPanel(htmlOutput("syntax")),
                      # wellPanel(style = "background-color: #d3f0fd;",
                      #           span(textOutput("message"), style = "font-size:16px")),
                      conditionalPanel(condition = "output.syntax",
                                       introBox(
                                         uiOutput("advopt"),
                                         data.step = 9,
                                         data.intro = "Select this box to include advanced options in the syntax. 
                                         These options are not necessary, but are available to the interested
                                         user. Here, you will find the ability to specify
                                         custom subgroups, specify fixed or freed paths for
                                         estimation, and change group and subgroup cutoffs."
                                       )),
                      extendShinyjs(text = jscode, functions = c("closeWindow")),
                      # actionBttn(inputId = "close", label = tags$b("Click to close GUI when done"), 
                      #            style = "fill", color = "primary")
                      actionButton("close", tags$b("Click to close GUI when done"))
                                       ),
             tabPanel("Advanced Inputs: Custom Subgroups",
                      br(),
                      conditionalPanel(
                        condition = "input.func != 'gimmeSEM'",
                        HTML("Custom subgroups are not available with the indSEM
                             or aggSEM functions.")
                        ),
                      conditionalPanel(
                        condition = "output.confsubstatus == true", 
                        div(id = "confpathtext", 
                            HTML("If confirmatory subgroups are 
                                 desired, enter an integer value, 
                                 beginning at 1, for each individual
                                 in the sample. When you are finished, toggle
                                 the Confirmatory Subgroups button back to 'Save.'<br>")),
                        rHandsontableOutput("tbl_confsub")),
                      conditionalPanel(
                        condition = "output.confsubstatus == false && input.func == 'gimmeSEM'",
                        HTML("To get started, select an input directory under <b>Main Options</b>
                             and toggle the <b>Confirmatory Subgroups</b> switch to <b>Add</b> under
                             <b>Advanced Options</b>.")
                        )
                        ),
             tabPanel("Advanced Inputs: Paths",
                      conditionalPanel(
                        condition = "output.pathstatus == false",
                        br(),
                        HTML("To get started, select an input directory under <b>Main Options</b>
                             and toggle the <b>Confirmatory Paths</b> switch or 
                             <b>Restricted Paths</b> switch to <b>Add</b> under <b>Advanced Options</b>.")
                        ),
                      conditionalPanel(condition = "output.confpathstatus",
                                       br(),
                                       HTML("Select paths below that you want to be 
                                            <font color='#000000'><b>freely estimated</b></font>
                                            for all individuals. 
                                            These paths will be freed at the start of estimation. 
                                            The rows represent outcomes and the columns 
                                            represent predictors.<br><br>"),
                                       HTML("Below is the <strong>lagged</strong> matrix.<br>"),
                                       rHandsontableOutput("path_mat_lag"), 
                                       HTML("<br>Below is the <strong>contemporaneous</strong> matrix.<br>"),
                                       rHandsontableOutput("path_mat_con")
                                       ),
                      br(),
                      conditionalPanel(condition = "output.respathstatus",
                                       HTML("Select paths below that you want to be 
                                            <font color='#000000'><b>fixed to zero</b></font>
                                            for all individuals. 
                                            These paths will 
                                            <font color='#000000'><b>not</b></font>
                                            be freed during the search. 
                                            The rows represent outcomes and the columns 
                                            represent predictors.<br><br>"),
                                       HTML("Below is the <strong>lagged</strong> matrix.<br>"),
                                       rHandsontableOutput("path_mat_lag_restrict"),
                                       HTML("<br>Below is the <strong>contemporaneous</strong> matrix.<br>"),
                                       rHandsontableOutput("path_mat_con_restrict")
                                       )
                      )
                      )
                        )
                      )
                      )


server <- function(input, output, session) {
  
  store <- reactiveValues(data         = NULL, 
                          paths        = "NULL", 
                          paths2       = "NULL",
                          infile       = NULL, 
                          outfile      = NULL, 
                          pathsshow    = TRUE, 
                          respathsshow = TRUE,
                          pathsshow    = TRUE,
                          respathsshow = TRUE,
                          showconfsub = FALSE,
                          showpathtext = FALSE,
                          showrespathtext = FALSE,
                          confsub      = NULL)
  
  output$pathstatus <- reactive({
    store$showpathtext == TRUE | store$showrespathtext == TRUE
  })
  outputOptions(output, "pathstatus", suspendWhenHidden = F)
  
  output$confsubstatus <- reactive({
    store$showconfsub == TRUE
  })
  outputOptions(output, "confsubstatus", suspendWhenHidden = F)
  
  output$confpathstatus <- reactive({
    store$showpathtext == TRUE
  })
  outputOptions(output, "confpathstatus", suspendWhenHidden = F)
  
  output$respathstatus <- reactive({
    store$showrespathtext == TRUE
  })
  outputOptions(output, "respathstatus", suspendWhenHidden = F)
  
  output$advopt <- renderUI({
    prettyCheckbox("advopt", "Include Advanced Options in Syntax",
                   value = ifelse(input$btn != 0 | store$showconfsub | store$showpathtext | store$showrespathtext, TRUE, FALSE),
                   icon = icon("check"))
  })
  
  observeEvent(input$inputdir, { 
    store$infile <- choose_dir()
  })
  
  observeEvent(input$outdir, { 
    store$outfile <- choose_dir()
  })
  
  store$func <- reactive({input$func})
  
  ## Confirmatory subgrouping code ---------------------------------------------
  
  observeEvent(req(input$confsub2), {# executes only when true
    if (is.null(store$confsub)){
      df_confsub = data.frame(ID    = file_path_sans_ext(list.files(store$infile)),
                              group = character(length(list.files(store$infile))))
      store$df_confsub  <- df_confsub
      store$showconfsub <- TRUE
    } else if (!is.null(store$confsub)){
      store$df_confsub <- store$final_confsub
    }
  })
  
  
  output$tbl_confsub <- renderRHandsontable({
    if (store$showconfsub){
      if (input$confsub2){
        rhandsontable(store$df_confsub) %>% hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, allowInvalid = TRUE)
      } else if (!input$confsub2){
        rhandsontable(store$df_confsub) %>% hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)
      } 
    } else NULL
  })
  
  observeEvent(!input$confsub2, {
    if (!is.null(input$tbl_confsub)){
      df_temp       <- hot_to_r(input$tbl_confsub)
      if (is.na(sum(as.numeric(as.character(df_temp[,2]))))){
        store$confsub <- NULL
      } else store$confsub <- TRUE
      store$df_confsub    <- df_temp
      store$final_confsub <- store$df_confsub
    }
  })
  
  observeEvent(!is.null(store$infile), {
    updateSwitchInput(
      session = session,
      inputId = "confsub2",
      disabled = FALSE
    )
  }, ignoreInit = T)
  
  observeEvent(!is.null(store$infile), {
    updateSwitchInput(
      session = session,
      inputId = "paths2",
      disabled = FALSE
    )
  }, ignoreInit = T)
  
  observeEvent(!is.null(store$infile), {
    updateSwitchInput(
      session = session,
      inputId = "respaths2",
      disabled = FALSE
    )
  }, ignoreInit = T)
  
  ## End confirmatory subgrouping ----------------------------------------------
  
  ## Adding confirmatory paths ---------------------------------------------------
  output$path_mat_lag <- renderRHandsontable({
    if (input$paths2 & store$showpathtext){
      rhandsontable(store$mat_lag, 
                    rowHeaders = rownames(store$mat_lag), 
                    rowHeaderWidth = 150)
    } else if (!input$paths2 & store$showpathtext){
      rhandsontable(store$mat_lag, 
                    rowHeaders = rownames(store$mat_lag), 
                    rowHeaderWidth = 150, readOnly = TRUE)
    } else NULL
  })
  
  output$path_mat_con <- renderRHandsontable({
    if (input$paths2 & store$showpathtext){
      rhandsontable(store$mat_con, 
                    rowHeaders = rownames(store$mat_con), 
                    rowHeaderWidth = 150)
    } else if (!input$paths2 & store$showpathtext){
      rhandsontable(store$mat_con, 
                    rowHeaders = rownames(store$mat_con), 
                    rowHeaderWidth = 150, readOnly = TRUE)
    } else NULL
  })
  
  observeEvent(input$paths2, {
    if (input$sep == "csv") {
      sep <- ","
    } else if (input$sep == "tab") {
      sep = "/t"
    } else if (input$sep == "space") {
      sep = ""
    }
    
    header   <- ifelse(input$header == "Yes", TRUE, FALSE)
    
    if (!is.null(store$infile) & !store$showpathtext){
      # read in first data file, obtain column names for matrix
      dat1 <- read.table(list.files(store$infile, full.names = TRUE)[1L], 
                         header = header, sep = sep)
      
      store$mat_lag <- gen_path_mat(dat1, diag = TRUE)
      store$mat_con <- gen_path_mat(dat1, diag = FALSE)

      store$showpathtext <- TRUE
    } 
  })
  
  observeEvent(!input$paths2, {
    if (!is.null(store$infile) & !is.null(input$path_mat_lag) & !is.null(input$path_mat_con)){
      pc_mat  <- hot_to_r(input$path_mat_con)
      store$mat_con <- pc_mat
      pc_ind <- which(pc_mat, arr.ind = TRUE)
      print(pc_ind)
      
      p1 <- ifelse(nrow(pc_ind) != 0, 
                   paste0(rownames(pc_mat)[pc_ind[,1]], " ~ ", 
                          colnames(pc_mat)[pc_ind[,2]], 
                          collapse = paste0(",<br>", 
                                            paste0(rep("&nbsp", 
                                                       (nchar("paths") +
                                                          nchar(" = ") + 9)), 
                                                   collapse = ""))),
                   NA)
      # different formatting for the download.R script
      p1plain <- ifelse(nrow(pc_ind) != 0, 
                        paste0(paste0(rownames(pc_mat)[pc_ind[,1]], " ~ ", 
                                      colnames(pc_mat)[pc_ind[,2]]), 
                               collapse = ",\n                 "),
                        NA)
      
      pl_mat <- hot_to_r(input$path_mat_lag)
      store$mat_lag <- pl_mat
      pl_ind <- which(pl_mat, arr.ind = TRUE)
      
      p2 <- ifelse(nrow(pl_ind) != 0, 
                   paste0(rownames(pl_mat)[pl_ind[,1]], " ~ ", 
                          paste0(colnames(pl_mat)[pl_ind[,2]], "lag"),
                          collapse = paste0(",<br>", 
                                            paste0(rep("&nbsp", 
                                                       (nchar("paths") + 
                                                          nchar(" = ") + 9)), 
                                                   collapse = ""))),
                   NA)
      
      p2plain <- ifelse(nrow(pl_ind) != 0, 
                        paste0(rownames(pl_mat)[pl_ind[,1]], " ~ ", 
                               paste0(colnames(pl_mat)[pl_ind[,2]], "lag"), 
                               collapse = ",\n                 "),
                        NA)
      
      if (!is.na(p1) & !is.na(p2)){
        paths <- paste0(p1, paste0(",<br>", 
                                   paste0(rep("&nbsp", 
                                              (nchar("paths") + 
                                                 nchar(" = ") + 9)), 
                                          collapse = "")), 
                        p2)
        
        paths2 <- paste0(p1plain, ",\n                 ", p2plain)
        
      } else if (is.na(p1) & !is.na(p2)){
        paths  <- p2
        paths2 <- p2plain
      } else if (!is.na(p1) & is.na(p2)){
        paths  <- p1
        paths2 <- p1plain
      } else if (is.na(p1) & is.na(p2)){
        paths  <- NULL
        paths2 <- NULL
      }
      
      store$pathsshow <- FALSE
      store$confpaths <- paths
      store$confpaths2 <- paths2
    } else {
      NULL
    }
  })
  
  ## End adding confirmatory paths ---------------------------------------------
  
  ## Restricted paths---------------------------------------------------------
  
  observeEvent(input$respaths2, {
    if (input$sep == "csv") {
      sep <- ","
    } else if (input$sep == "tab") {
      sep = "/t"
    } else if (input$sep == "space") {
      sep = ""
    }
    
    header   <- ifelse(input$header == "Yes", TRUE, FALSE)
    
    if (!is.null(store$infile) & !store$showrespathtext){
      # read in first data file, obtain column names for matrix
      dat1 <- read.table(list.files(store$infile, full.names = TRUE)[1L], 
                         header = header, sep = sep)
      
      store$res_mat_lag <- gen_path_mat(dat1, diag = TRUE)
      store$res_mat_con <- gen_path_mat(dat1, diag = FALSE)

      store$showrespathtext <- TRUE
    } 
  })
  
  output$path_mat_lag_restrict <- renderRHandsontable({
    if (input$respaths2 & store$showrespathtext){
      rhandsontable(store$res_mat_lag, 
                    rowHeaders = rownames(store$res_mat_lag), 
                    rowHeaderWidth = 150)
    } else if (!input$respaths2 & store$showrespathtext){
      rhandsontable(store$res_mat_lag, 
                    rowHeaders = rownames(store$res_mat_lag), 
                    rowHeaderWidth = 150, readOnly = TRUE)
    } else NULL
  })
  
  output$path_mat_con_restrict <- renderRHandsontable({
    if (input$respaths2 & store$showrespathtext){
      rhandsontable(store$res_mat_con, 
                    rowHeaders = rownames(store$res_mat_con), 
                    rowHeaderWidth = 150)
    } else if (!input$respaths2 & store$showrespathtext){
      rhandsontable(store$res_mat_con, 
                    rowHeaders = rownames(store$res_mat_con), 
                    rowHeaderWidth = 150, readOnly = TRUE)
    } else NULL
  })
  
  observeEvent(!input$respaths2, {
    if (!is.null(store$infile) & !is.null(input$path_mat_lag_restrict) & !is.null(input$path_mat_con_restrict)){
      pc_mat  <- hot_to_r(input$path_mat_con_restrict)
      store$res_mat_con <- pc_mat
      pc_ind <- which(pc_mat, arr.ind = TRUE)
      print(pc_ind)
      
      p1 <- ifelse(nrow(pc_ind) != 0, 
                   paste0(rownames(pc_mat)[pc_ind[,1]], " ~ 0*", 
                          colnames(pc_mat)[pc_ind[,2]], 
                          collapse = paste0(",<br>",
                                            paste0(rep("&nbsp", 
                                                       (nchar("paths") + 
                                                          nchar(" = ") + 9)), 
                                                   collapse = ""))),
                   NA)
      
      p1plain <- ifelse(nrow(pc_ind) != 0, 
                        paste0(rownames(pc_mat)[pc_ind[,1]], " ~ 0*", 
                               colnames(pc_mat)[pc_ind[,2]], 
                               collapse = ",\n                 "),
                        NA)
      
      pl_mat <- hot_to_r(input$path_mat_lag_restrict)
      store$res_mat_lag <- pl_mat
      pl_ind <- which(pl_mat, arr.ind = TRUE)
      
      p2 <- ifelse(nrow(pl_ind) != 0, 
                   paste0(rownames(pl_mat)[pl_ind[,1]], 
                          " ~ 0*", 
                          paste0(colnames(pl_mat)[pl_ind[,2]], "lag"),
                          collapse = paste0(",<br>", 
                                            paste0(rep("&nbsp", 
                                                       (nchar("paths") + 
                                                          nchar(" = ") + 9)), 
                                                   collapse = ""))),
                   NA)
      
      p2plain <- ifelse(nrow(pl_ind) != 0, 
                        paste0(rownames(pl_mat)[pl_ind[,1]], " ~ 0*",
                               paste0(colnames(pl_mat)[pl_ind[,2]], "lag"),
                               collapse = ",\n                 "),
                        NA)
      
      if (!is.na(p1) & !is.na(p2)){
        respaths <- paste0(p1, 
                           paste0(",<br>", 
                                  paste0(rep("&nbsp", 
                                             (nchar("paths") +
                                                nchar(" = ") + 9)), 
                                         collapse = "")), 
                           p2)
        respaths2 <- paste0(p1plain, ",\n                 ", p2plain)
      } else if (is.na(p1) & !is.na(p2)){
        respaths <- p2
        respaths2 <- p2plain
      } else if (!is.na(p1) & is.na(p2)){
        respaths <- p1
        respaths2 <- p1plain
      } else if (is.na(p1) & is.na(p2)){
        respaths <- NULL
        respaths2 <- NULL
      }
      
      store$respathsshow <- FALSE
      store$respaths     <- respaths
      store$respaths2    <- respaths2
    }
  })
  
  ## End restricted paths ----------------------------------------------------
  
  output$pathsshow <- reactive({
    return(store$pathsshow)
  })
  
  output$respathsshow <- reactive({
    return(store$respathsshow)
  })
  
  observeEvent(input$btn, {
    updateSelectInput(session, 
                      "func", 
                      choices =  c("gimmeSEM", "indSEM", "aggSEM"))
  })
  
  ## start processing the output syntax -------------------------------------
  
  syntax <- renderText({ 
    # this if statement gives the renderUI statement time to process
    if (!is.null(input$advopt)){ 
      store$func <- input$func
      
      if (!is.null(store$confpaths) & !is.null(store$respaths)) {
        store$paths <- paste0("'", store$confpaths, 
                              paste0(",<br>", 
                                     paste0(rep("&nbsp", 
                                                (nchar("paths") + 
                                                   nchar(" = ") + 9)),
                                            collapse = "")),
                              store$respaths, "'")
        
        store$paths2 <- paste0("'", 
                               store$confpaths2,
                               ",\n                 ", 
                               store$respaths2, "'")
        
      } else if (is.null(store$confpaths) & !is.null(store$respaths)) {
        store$paths <- paste0("'", store$respaths, "'")
        store$paths2 <- paste0("'", store$respaths2, "'")
      } else if (!is.null(store$confpaths) & is.null(store$respaths)) {
        store$paths <- paste0("'", store$confpaths, "'")
        store$paths2 <- paste0("'", store$confpaths2, "'")
      }
      
      
      if (input$sep == "csv") {
        sep <- '","'
      } else if (input$sep == "tab") {
        sep = "\"/t\""
      } else if (input$sep == "space") {
        sep = '""'
      }
      
      header   <- ifelse(input$header == "Yes", TRUE, FALSE)
      subgroup <- ifelse(input$sub == "Yes", TRUE, FALSE)
      plots    <- ifelse(input$plots == "Yes (Recommended)", TRUE, FALSE)
      ar       <- ifelse(input$ar == "Yes (Recommended)", TRUE, FALSE)
      
      if (!is.null(store$vec_confsub)) {
        brks <- c(rep(",", (length(store$vec_confsub) - 1)), "")
        brks[seq(5, length(brks), 5)] <- paste0(",<br>", 
                                                paste0(rep("&nbsp", 14), 
                                                       collapse = ""))
      }
      
      if (!is.null(store$confsub)){
        sub_text <- paste0("data.frame(id = c(", 
                           paste0('"',store$final_confsub$ID,'"', collapse = ","), 
                           "),\n", "grp = c(",
                           paste0(store$final_confsub$group, collapse = ","),
                           "))")
      } else {
        sub_text <- "NULL"
      }
      
      
      if ((store$paths) == "NULL"){
        pathsColor <- "#a22f5f"
      } else {
        pathsColor <- "#ed4a0d"
      }
      
      # if ((store$confsub) == "NULL"){
      if (is.null(store$confsub)){
        subColor <- "#a22f5f"
      } else {
        subColor <- "#ed4a0d"
      }
      
      if (input$advopt) { 
        HTML(
          paste0("<span style=\"color:#355e3b\">&nbsp# The R code below can be 
                 copied and pasted directly into R to run gimme.</span><br>",
                 "<span style=\"color:#000000\">", 
                 "&nbspfit <- ", store$func, "(", "<br>",
                 paste0("&nbsp&nbspdata&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                        "        = "), 
                 "<span style=\"color:#ed4a0d\">",'"', 
                 gsub("\\\\", "/", store$infile), '"',  "</span>",
                 ",", "<br>",
                 paste0("&nbsp&nbspout&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                        "         = "),
                 "<span style=\"color:#ed4a0d\">",'"', 
                 gsub("\\\\", "/", store$outfile), '"', "</span>",
                 ",","<br>",
                 paste0("&nbsp&nbspsep&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",
                        "         = "), 
                 "<span style=\"color:#ed4a0d\">", sep, "</span>",
                 ",", "<br>",
                 "&nbsp&nbspheader&nbsp&nbsp&nbsp&nbsp&nbsp      = ", 
                 "<span style=\"color:#a22f5f\">", header, "</span>",
                 ",", "<br>",
                 "&nbsp&nbspar&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp = ", 
                 "<span style=\"color:#a22f5f\">", ar, "</span>",
                 ",", "<br>",
                 "&nbsp&nbspplot&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp       = ", 
                 "<span style=\"color:#a22f5f\">", plots, "</span>", 
                 ",", "<br>",
                 ifelse(input$func == "gimmeSEM", 
                        paste0("&nbsp&nbspsubgroup&nbsp&nbsp&nbsp    = ", 
                               "<span style=\"color:#a22f5f\">", 
                               subgroup, "</span>", 
                               ",", "<br>"), ""),
                 ifelse(input$func == "gimmeSEM" & subgroup == TRUE, 
                        paste0("<span style=\"color:#355e3b\">&nbsp#
                               Advanced Options</span><br>",
                               "&nbsp&nbspconfirm_subgroup&nbsp= ", 
                               "<span style=\"color:", 
                               subColor, "\">", sub_text, "</span>", 
                               ",", "<br>"), ""),
                 "<span style=\"color:#000000\">", 
                 "&nbsp&nbsppaths &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp= ", 
                 "<span style=\"color:", pathsColor, "\">", 
                 store$paths, "</span>", 
                 ifelse(input$func == "gimmeSEM", paste0(",", "<br>"), ""),
                 ifelse(input$func == "gimmeSEM", 
                        paste0("&nbsp&nbspgroupcutoff = ", 
                               "<span style=\"color:#a22f5f\">", 
                               input$groupcutoff, "</span>"), 
                        ""),
                 ifelse(input$func == "gimmeSEM" & subgroup == TRUE,  
                        paste0(",", "<br>"), "<br>"),
                 ifelse(input$func == "gimmeSEM" & subgroup == TRUE, 
                        paste0("&nbsp&nbspsubcutoff&nbsp&nbsp&nbsp= ", 
                               "<span style=\"color:#a22f5f\">", 
                               input$subcutoff, "</span>", "<br>"), 
                        ""),
                 "&nbsp)", "</span>"
          )
        )
      } else {
        HTML(
          paste0("<span style=\"color:#355e3b\">&nbsp# The R code below can be 
                 copied and pasted directly into R to run gimme.</span><br>",
                 "<span style=\"color:#000000\">", 
                 "&nbspfit <- ", store$func, "(", "<br>",
                 "&nbsp&nbspdata&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp        = ", 
                 "<span style=\"color:#ed4a0d\">",'"', 
                 gsub("\\\\", "/", store$infile), '"',  "</span>",
                 ",", "<br>",
                 "&nbsp&nbspout&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp         = ",
                 "<span style=\"color:#ed4a0d\">",'"', 
                 gsub("\\\\", "/", store$outfile), '"', "</span>",
                 ",","<br>",
                 "&nbsp&nbspsep&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp         = ", 
                 "<span style=\"color:#ed4a0d\">", sep, "</span>",
                 ",", "<br>",
                 "&nbsp&nbspheader&nbsp&nbsp&nbsp&nbsp&nbsp      = ", 
                 "<span style=\"color:#a22f5f\">", header, "</span>",
                 ",", "<br>",
                 "&nbsp&nbspar&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp = ", 
                 "<span style=\"color:#a22f5f\">", ar, "</span>",
                 ",", "<br>",
                 "&nbsp&nbspplot&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp       = ", 
                 "<span style=\"color:#a22f5f\">", plots, "</span>", 
                 ifelse(input$func == "gimmeSEM", paste0(",", "<br>"), 
                        "<span style=\"color:white\">,<br>)</span>"),
                 ifelse(input$func == "gimmeSEM", 
                        paste0("&nbsp&nbspsubgroup&nbsp&nbsp&nbsp    = ", 
                               "<span style=\"color:#a22f5f\">",
                               subgroup, "</span>", 
                               "</span><br>"), ""),
                 "<span style=\"color:000000\">&nbsp)</span>"
          )
        )
      }
      
    }
  }
  )
  
  textsyntax <- renderText({
    sep = NULL
    header = NULL
    if (input$sep == "csv"){
      sep <- '","'
    } else if (input$sep == "tab"){
      sep = "\"/t\""
    } else if (input$sep == "space"){
      sep = '""'
    }
    
    header   <- ifelse(input$header == "Yes", TRUE, FALSE)
    subgroup <- ifelse(input$sub == "Yes", TRUE, FALSE)
    plots    <- ifelse(input$plots == "Yes (Recommended)", TRUE, FALSE)
    ar       <- ifelse(input$ar == "Yes (Recommended)", TRUE, FALSE)
    
    #if (store$confsub != "NULL"){
    if (!is.null(store$confsub)){
      sub_text <- paste0("data.frame(id = c(", 
                         paste0('"',store$final_confsub$ID,'"', collapse = ","), 
                         "),\n", "grp = c(",
                         paste0(store$final_confsub$group, collapse = ","),
                         "))")
    } else {
      sub_text <- "NULL"
    }
    
    printme <- paste0(
      "# R script generated by gimmeGUI on ", Sys.time(), 
      "\n", "\n",
      "fit <- ", store$func, "(", "\n",
      "  data        = ", '"', store$infile, '"', ",", "\n",
      "  out         = ", '"', store$outfile, '"', ",", "\n",
      "  sep         = ", sep, ",", "\n",
      "  header      = ", header, ",", "\n", 
      "  ar          = ", ar, ",", "\n", 
      "  plot        = ", plots, ",", "\n", 
      if (store$func == "gimmeSEM") paste0("  subgroup    = ", 
                                           subgroup, ",", "\n"), 
      "# Advanced Options ", "\n",
      if(store$func == "gimmeSEM" & subgroup == TRUE){
        paste0("  confirm_subgroup = ", sub_text, 
               ",", "\n")
      }, 
      "  paths       = ", store$paths2, 
      if (store$func == "gimmeSEM") paste0(","), 
      "\n", 
      if (store$func == "gimmeSEM") paste0("  groupcutoff = ", 
                                           input$groupcutoff),
      if (subgroup == TRUE) paste0(",", "\n"), 
      if (store$func == "gimmeSEM" & subgroup == TRUE) {
        paste0("  subcutoff   = ", input$subcutoff, "\n")
      }, 
      ")"
    )
    
    printme
    
  })
  
  
  output$syntax <- reactive({
    syntax()
  })
  
  observeEvent(input$close, {
    #jscode <- "shinyjs.closeWindow = function() { window.close(); }"
    shinyjs::js$closeWindow()
    stopApp()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste('gimme R Script', '.R', sep = '') },
    content = function(file){
      writeLines(textsyntax(), file)
    }
  )
  
  observeEvent(input$btn, {
    introjs(
      session,
      events = list(
        "onchange" = I(
          "if (this._currentStep == 0) {
          $('a[data-value=\"Advanced Options\"]').removeClass('active');
          $('a[data-value=\"Main Options\"]').addClass('active');
          $('a[data-value=\"Main Options\"]').trigger('click');
          $('a[data-value=\"Advanced Inputs: Custom Subgroups\"]').removeClass('active');
          $('a[data-value=\"Advanced Inputs: Paths\"]').removeClass('active');
          $('a[data-value=\"Syntax\"]').addClass('active');
          $('a[data-value=\"Syntax\"]').trigger('click');
  }
          if (this._currentStep == 9) {
          $('a[data-value=\"Main Options\"]').removeClass('active');
          $('a[data-value=\"Advanced Options\"]').addClass('active');
          $('a[data-value=\"Advanced Options\"]').trigger('click');
          }
          if (this._currentStep == 14) {
          $('a[data-value=\"Advanced Options\"]').removeClass('active');
          $('a[data-value=\"Main Options\"]').addClass('active');
          $('a[data-value=\"Main Options\"]').trigger('click');
          }"
        )
        )
        )
})
  
  session$onSessionEnded(stopApp)
  
}

runGadget(ui, server, viewer = browserViewer())
}


