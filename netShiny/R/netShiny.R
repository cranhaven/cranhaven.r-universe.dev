#' This function launches the netShiny Shiny app.
#'
#' @param Net.obj A list of (sparse) matrices corresponding to the networks that need to be visualized. Net.obj can also be a list of dataframes with data to be used to reconstruct networks.  Or, Net.obj can be a combination of (sparse) matrices and dataframes. If items in list are names, these names will be used, otherwise automatic names will be generated.
#' @param mapping A dataframe containing order for each node. There should be a column with the names of the nodes and a column with the corresponding group that the nodes belong to. The app will automatically choose the column representing the grouping of the nodes by looking at the first two columns, and choosing the column with the less number of factor levels as the columns containing the grouping of the nodes.
#' @param resamples If an user has resampling information corresponding to the networks to be visualized the user can also include this in the function, which will incorporate it into the app.
#' @return A Shiny app.
#' @details This function opens the shiny app, netShiny. All of the arguments in netShiny are optional, so netShiny can be called without any arguments. Users are prompted with a series of modal dialogs after running the netShiny function. The first modal dialog gives users the possibility to upload files to the app and show the dataframes that already uploaded in a datatable. Users can choose files which contain information to reconstruct networks from them. The next modal dialog let users reconstruct networks using the dataframes that were uploaded. netShiny uses the functions netphenogeno and selectnet from the package netgwas for graph structure learning from non-Gaussian data. The next modal let users optionally choose a file containing the ordering of the nodes. If a dataframe containing the ordering of the nodes was already passed to mapping argument, this modal will visualize this in a datatable. The last modal let users choose the mode they want the app to run in, GxE (Genetic-by-Environment) or general mode. In GxE mode the language used in netShiny is more Genetic-by-Environment related. Users need to input the number of traits if GxE mode is chosen, and optionally, manually input a grouping for the traits.
#' @examples
#' if (interactive()) {
#'     netShiny()
#' }

#' @author
#' Rocherno de Jongh and Pariya Behrouzi \cr
#' Maintainer: Rocherno de Jongh \email{rocherno.dejongh@@hotmail.com}
#' @references
#' Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
#' @seealso \code{\link[netgwas]{netphenogeno}}, \code{\link[netgwas]{selectnet}}
#' @export
#' @import shiny shinyBS shinydashboard
#' @importFrom magrittr %>%
netShiny <- function(Net.obj = NULL,
                     mapping = NULL,
                     resamples = NULL){

  future::plan(future.callr::callr)

  #Check if argument passed are defined
  if (!missing(Net.obj)) {
    string_net.obj <- deparse(substitute(Net.obj))
    if (!exists(string_net.obj)) stop(paste0("Object '", string_net.obj, "' not found"))

  }
  if (!missing(mapping)) {
    string_mapping <- deparse(substitute(mapping))
    if (!exists(string_mapping)) stop(paste0("Object '", string_mapping, "' not found"))
  }
  if (!missing(resamples)) {
    string_resamples <- deparse(substitute(resamples))
    if (!exists(string_resamples)) stop(paste0("Object '", string_resamples, "' not found"))
  }

  sett_nms <- NULL
  all_node_nms <- NULL
  to_reconstruct <- FALSE
  df_settings <- NULL

  if(!is.null(Net.obj)){
    if(is.null(names(Net.obj))){
      message("Names not provided, default names will be used")
      sett_nms <- sprintf("Environment %s", 1:length(Net.obj))
      names(Net.obj) <- sett_nms
    }
    else{
      sett_nms <- names(Net.obj)
    }
    for(i in 1:length(Net.obj)){
      if(is.null(dimnames(Net.obj[[i]])[[1]])){
        if(is.null(dimnames(Net.obj[[i]])[[2]])){
          dimnames(Net.obj[[i]]) <- list(paste("Node", 1:nrow(Net.obj[[i]])), paste("Node", 1:nrow(Net.obj[[i]])))
        }
        else{
          dimnames(Net.obj[[i]])[[1]] <- dimnames(Net.obj[[i]])[[2]]
        }
      }
      else if(is.null(dimnames(Net.obj[[i]])[[2]])){
        dimnames(Net.obj[[i]])[[2]] <- dimnames(Net.obj[[i]])[[1]]
      }
    }
    n_nodes <- max(unlist(sapply(Net.obj, function(x) dim(x)[[1]])))
    all_node_nms <- unique(unlist(lapply(Net.obj, function(x) dimnames(x)[[2]])))
    for(i in 1:length(Net.obj)){
      curr_nm <- sett_nms[i]
      if(Matrix::isSymmetric(Net.obj[[curr_nm]])){
        Net.obj[[curr_nm]] <- as.matrix(Net.obj[[curr_nm]])
        Net.obj[[curr_nm]] <- Matrix::Matrix(Net.obj[[curr_nm]], sparse = TRUE)
      }
      else{
        to_reconstruct <- TRUE
        df_settings[[curr_nm]] <- data.frame(Net.obj[[curr_nm]])
        Net.obj[[curr_nm]] <- NULL
      }
    }
  }

  sidebar <- shinydashboard::dashboardSidebar(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
      ".sidebar {
            height: 95vh; overflow-y: auto;
          }"
      ))
    ),
    shinyWidgets::setSliderColor(rep("#007c00", 3), c(1, 2, 3)),
    shinyjs::useShinyjs(),
    shinydashboard::sidebarMenu(id = "tabs",
                                shinydashboard::menuItem("Networks", tabName = "networks", icon = shiny::icon("diagram-project")),
                                shinydashboard::menuItem("Summary Statistics", tabName = "summ_stats", icon = shiny::icon("stats", lib = "glyphicon")),
                                shinydashboard::menuItem("Weights Analysis", tabName = "net", icon = shiny::icon("chart-line")),
                                shinydashboard::menuItem("Centrality Measures", tabName = "centrality", icon = shiny::icon("align-center")),
                                shinydashboard::menuItem("Net Diffs", tabName = "differences", icon = shiny::icon("compress")),
                                shinydashboard::menuItem("Community Detection", tabName = "comm_detect", icon = shiny::icon("chart-bar")),
                                shinydashboard::menuItem("Uncertainty Check", tabName = "unc_check", icon = shiny::icon("chart-area")),
                                shinydashboard::menuItem("Manual", tabName = "manual", icon = shiny::icon("circle-info")),
                                shiny::sliderInput(inputId = "cor_t", "Partial Correlations Traits", min = 0, max = 1, value = 0),
                                shiny::sliderInput(inputId = "cor_m", "Partial Correlations Markers", min = 0, max = 1, value = 0),
                                shiny::selectInput("net1", "Left Panel", sett_nms, selected = sett_nms[1]),
                                shiny::selectInput("net2", "Right Panel", sett_nms, selected = sett_nms[2]),
                                shiny::selectInput("sets_selin", "Operation", c("Union", "Intersection", "Complement"), selected = "Union"),
                                shiny::selectInput("marker", "Markers", all_node_nms),
                                shinyWidgets::materialSwitch(inputId = "diff_switch", label = "All Differences", width = "100%"),
                                shinyWidgets::searchInput(inputId = "meas_butt", label = "Add Statistic", placeholder = "func, arg1; func2", btnSearch = shiny::icon("magnifying-glass"), btnReset = shiny::icon("xmark"), width = "100%"),
                                shinyBS::bsTooltip(id = "meas_butt", title = "Add arguments for function by separting by commas, add addtional function by separating by ;", options = list(hover = "auto")),
                                shiny::tags$head(
                                  shiny::tags$style(shiny::HTML('#subgraph{color:black; background-color:#F5F2F2}
                                  #customize{color:black; background-color:#F5F2F2}
                                  #print_network{color:black; background-color:#F5F2F2}
                                  #refresh{color:black; background-color:#F5F2F2}'))
                                ),
                                shiny::splitLayout(cellWidths = c("28%", "28%", "28%"),
                                                   shinyWidgets::actionBttn("subgraph", label = "subnet", style = "simple", size = "sm", block = TRUE),
                                                   shinyWidgets::actionBttn("print_network", label = NULL, icon = shiny::icon("print"), style = "simple", size = "sm", block = TRUE),
                                                   shinyWidgets::actionBttn("customize", label = "", icon = icon("gears"), style = "simple", size = "sm", block = TRUE)
                                )

    )
  )

  shinyWidgets::useSweetAlert()

  body <- shinydashboard::dashboardBody(
    shiny::tags$head(shiny::tags$style(".modal{ overflow-y:auto}")),
    shiny::tags$head(shiny::tags$style(type = "text/css", ".modal-backdrop { z-index: -1}")),
    shiny::tags$head(shiny::tags$style(shiny::HTML(".main-sidebar { font-size: 18px; }"))),
    custombsModal("modalStartup_step1", "Files for Network Reconstruction", "nothin", size = "large",
                  shiny::fileInput("files_upload", "Choose Files",
                                   multiple = TRUE,
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv',
                                     '.xls',
                                     '.xlsx'
                                   )),
                  shiny::tags$hr(),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::checkboxInput("header", "Header", TRUE)),
                    shiny::column(width = 3, shiny::radioButtons("sep", "Separator",
                                                                 choices = c(Comma = ",",
                                                                             Semicolon = ";",
                                                                             Tab = "\t"),
                                                                 selected = ",")),
                    shiny::column(width = 3, shiny::radioButtons("quote", "Quote",
                                                                 choices = c(None = "",
                                                                             "Double Quote" = '"',
                                                                             "Single Quote" = "'"),
                                                                 selected = '"')),
                    shiny::column(width = 3, shiny::textInput("exc_columns", "Columns To Exclude (separate by space)",
                                                              value = ""))
                  ),
                  shiny::tags$hr(),
                  shiny::selectInput("currFile", "File", choices = NULL),
                  DT::dataTableOutput("tbl"),
                  shiny::fluidRow(
                    shiny::column(width = 12, offset = 10,
                                  shiny::actionButton(inputId = "nextButton_startup", label = "Next")
                    )
                  ),
                  shiny::tags$head(shiny::tags$style("#modalStartup_step1 .modal-footer{ display:none}"))),
    custombsModal("modalStartup_reconstruction", "Arguments for Network Reconstruction", "nextButton_startup", size = "large",
                  shiny::selectInput("file_names", "Data Files", choices = NULL),
                  shinyWidgets::awesomeCheckbox("adv_op", "Show advanced options", value = FALSE),
                  shiny::tags$hr(),
                  shiny::fluidRow(
                    shiny::column(width = 6,
                                  shiny::tags$h3("Arguments for Network Reconstruction", id = "net_recon_tag"),
                                  shinyWidgets::pickerInput(inputId = "net_method_start", label = "method", choices = c("Gibbs sampling" = "gibbs",
                                                                                                                        "approximation method" = "approx",
                                                                                                                        "nonparanormal" = "npn"), selected = "npn"),
                                  shiny::textInput(inputId = "net_rho_start", label = "rho (decreasing sequence of non-negative numbers that control the sparsity level)", value = "NULL"),
                                  shiny::numericInput(inputId = "net_n.rho_start", label = "n.rho (number of regularization parameters)", value = 10),
                                  shiny::numericInput(inputId = "net_rho.ratio_start", label = "rho.ratio (distance between the elements of rho sequence)",
                                                      value = 0.3, min = 0, max = 1, step = 0.1),
                                  shiny::textInput(inputId = "net_ncores_start", label = "ncores (number of cores to use for the calculations)", value = "1"),
                                  shiny::numericInput(inputId = "net_em.iter_start", label = "em.iter (number of EM iterations)", value = 5),
                                  shiny::numericInput(inputId = "net_em.tol_start", label = "em.tol (criteria to stop the EM iterations)",
                                                      value = 0.001, min = 0.001, max = 1, step = 0.001)
                    ),
                    shiny::column(width = 6,
                                  shiny::tags$h3("Arguments for Selecting Optimal Network", id = "sel_net_tag"),
                                  shiny::textInput(inputId = "sel_opt.index_start", label = "opt.index (manually choose an optimal graph from the graph path)", value = "NULL"),
                                  shinyWidgets::pickerInput(inputId = "sel_criteria_start", label = "criteria (model selection criteria)", choices = c("ebic", "aic"), selected = "ebic"),
                                  shiny::numericInput(inputId = "sel_ebic.gamma_start", label = "ebic.gamma (tuning parameter for ebic)",
                                                      value = 0.5, min = 0, max = 1, step = 0.1),
                                  shiny::textInput(inputId = "sel_ncores_start", label = "ncores (number of cores to use for the calculations)", value = "1")

                    )
                  ),
                  shiny::tags$hr(),
                  shiny::fluidRow(
                    shiny::column(width = 2,
                                  shiny::actionButton(inputId = "prevButton_reconstruction", label = "Previous")
                    ),
                    shiny::column(width = 2, offset = 8,
                                  shiny::actionButton(inputId = "startup_run", label = "Run", style = "color: #fff; background-color: #007c00")
                    )
                  ),
                  shiny::tags$head(shiny::tags$style("#modalStartup_reconstruction .modal-footer{ display:none}"))),
    ##NOTHING 2 NEEDS TO BE REMOVED
    custombsModal("startup_mapping", "File for Mapping of Nodes", "nothing2", size = "large",
                  shiny::fileInput("mapping_upload", "Choose File for Mapping",
                                   multiple = FALSE,
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv',
                                     '.xls',
                                     '.xlsx'
                                   )),
                  shiny::tags$hr(),
                  shiny::fluidRow(
                    shiny::column(width = 3, shiny::checkboxInput("header_mapping", "Header", TRUE)
                    ),
                    shiny::column(width = 3, shiny::radioButtons("sep_mapping", "Separator",
                                                                 choices = c(Comma = ",",
                                                                             Semicolon = ";",
                                                                             Tab = "\t"),
                                                                 selected = ",")),
                    shiny::column(width = 3, shiny::radioButtons("quote_mapping", "Quote",
                                                                 choices = c(None = "",
                                                                             "Double Quote" = '"',
                                                                             "Single Quote" = "'"),
                                                                 selected = '"')),
                    shiny::column(width = 3, shiny::textInput("exc_columns_mapping", "Columns To Exclude (separate by space)",
                                                              value = ""))
                  ),
                  shiny::tags$hr(),
                  DT::dataTableOutput("tbl_mapping"),
                  shiny::fluidRow(
                    shiny::column(width = 2,
                                  shiny::actionButton(inputId = "prevButton_mapping", label = "Previous")
                    ),
                    shiny::column(width = 2, offset = 8,
                                  shiny::actionButton(inputId = "nextButton_mapping", label = "Next")
                    )
                  ),
                  shiny::tags$head(shiny::tags$style("#startup_mapping .modal-footer{ display:none}"))),
    custombsModal("data_settings", "Options for Mapping of Nodes", "nextButton_mapping", size = "large",
                  shinyWidgets::switchInput("gxe_mode", label = "GxE Mode", labelWidth = "80px", onLabel = "YES", offLabel = "NO", value = TRUE),
                  shiny::textInput("net_names", label = "Environment Names:", value = NULL),
                  shiny::textInput("trait_types", "Put trait groups separated by semicolons (;)"),
                  shiny::actionButton("trait_create_group", "Create"),
                  shiny::uiOutput("trait_inputs"),
                  shiny::fluidRow(
                    shiny::column(width = 2,
                                  shiny::actionButton(inputId = "prevButton_data_settings", label = "Previous")
                    ),
                    shiny::column(width = 2, offset = 8,
                                  shiny::actionButton(inputId = "finish_data_settings", label = "Done")
                    )
                  ),
                  shiny::tags$head(shiny::tags$style("#data_settings .modal-footer{ display:none}"))
    ),
    shiny::tags$head(shiny::tags$style(shiny::HTML('
              /* logo */
        .skin-blue .main-header .logo {
          background-color: #306ea1;
        }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
          background-color: #306ea1;
        }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
          background-color: #306ea1;
        }

        /* main sidebar */
        .skin-blue .main-sidebar {
          background-color: #306ea1;
        }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: #F5F2F2;
        }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
          background-color: #007c00;
          color: #000000;
        }

        /* other links in the sidebarmenu when hovered */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          background-color: #9acd32;
        }

        /* toggle button when hovered  */
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #007c00;
        }

        /* body */
        .content-wrapper, .right-side {
          background-color: #F5F2F2;
        }

        .multicol {
           -webkit-column-count: 3; /* Chrome, Safari, Opera */
           -moz-column-count: 3;    /* Firefox */
           column-count: 3;
           -moz-column-fill: auto;
           -column-fill: auto;
        }

       .shiny-split-layout > div {
          overflow: visible;
       }

    '))),
    shinydashboard::tabItems(

      #Networks tab
      shinydashboard::tabItem(tabName = "networks",
                              shiny::tabsetPanel(id = "tabs_net_mat",
                                                 shiny::tabPanel("Networks", shiny::splitLayout(cellWidths = c("50%", "50%"),
                                                                                                   style = "padding: 0px",
                                                                                                   cellArgs = list(style = "border-right: 1px solid silver"),
                                                                                                   shinycssloaders::withSpinner(visNetwork::visNetworkOutput("network_proxy_nodes", height = "1000px"), color = "#007c00", size = 2),
                                                                                                   shinycssloaders::withSpinner(visNetwork::visNetworkOutput("network_proxy_nodes_2", height = "1000px"), color = "#007c00", size = 2))
                                                 ),
                                                 shiny::tabPanel("Matrices",
                                                                 shinyWidgets::dropMenu(
                                                                   shinyWidgets::actionBttn(inputId = "mat_action", icon = shiny::icon("gear"), style = "material-circle", color = "default", size = "sm"),
                                                                   shinyWidgets::pickerInput(inputId = "mat_sel", label = "Choose Networks", choices = sett_nms, selected = sett_nms, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                                                   hideOnClick = TRUE),
                                                                 shiny::fillPage(
                                                   tags$style(type = "text/css", "#mat_plots {height: calc(100vh - 100px) !important;}"),
                                                   shinycssloaders::withSpinner(plotly::plotlyOutput("mat_plots", width = "100%", height = "100%"), color = "#007c00", size = 2)
                                                 ))

                              )

      ),

      #Summary Statistics tab
      shinydashboard::tabItem(tabName = "summ_stats",
                              shiny::tabsetPanel(id = "tabs_summ_stats",
                                                 shiny::tabPanel("Metrics", shinycssloaders::withSpinner(shiny::plotOutput("summary_statistics", height = "750px"), color = "#007c00", size = 2)),
                                                 shiny::tabPanel(title = shiny::textOutput("title_par_cors"),
                                                                 shinyWidgets::dropdownButton(
                                                                   shiny::tags$h3("Plot Settings"),
                                                                   shiny::numericInput(inputId = "par_cor_bins", label = "Number of Bins", min = 0, value = 15),
                                                                   circle = TRUE, status = "primary", icon = shiny::icon("gear"),
                                                                   tooltip = shinyWidgets::tooltipOptions(title = "Click to change plot's settings")
                                                                 ),
                                                                 shinycssloaders::withSpinner(shiny::plotOutput("par_cors", height = "750px"), color = "#007c00", size = 2))
                              )

      ),

      #Community Detection tab
      shinydashboard::tabItem(tabName = "comm_detect",
                              shiny::tabsetPanel(id = "tabs_comm_detect",
                                                 shiny::tabPanel("Communities", shiny::splitLayout(cellWidths = c("50%", "50%"),
                                                                                                   style = "padding: 0px",
                                                                                                   cellArgs = list(style = "border-right: 1px solid silver"),
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput("comms_plot", height = "1000px"), color = "#007c00", size = 2),
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput("comms_plot2", height = "1000px"), color = "#007c00", size = 2))
                                                 ),
                                                 shiny::tabPanel("Modularity",  shinycssloaders::withSpinner(shiny::plotOutput("mods"), color = "#007c00", size = 2))

                              )
      ),

      #Tab for Uncertainty check
      shinydashboard::tabItem(tabName = "unc_check",
                              shinyWidgets::dropdown(
                                shiny::fluidRow(
                                  shiny::column(width = 6,
                                                shiny::tags$h3("Arguments for netphenogeno"),
                                                shinyWidgets::pickerInput(inputId = "net_method", label = "method", choices = c("gibbs", "approx",  "npn"), selected = "npn"),
                                                shiny::textInput(inputId = "net_rho", label = "rho (decreasing sequence of non-negative numbers that control the sparsity level)", value = "NULL"),
                                                shiny::numericInput(inputId = "net_n.rho", label = "n.rho (number of regularization parameters)", value = 10),
                                                shiny::numericInput(inputId = "net_rho.ratio", label = "rho.ratio (distance between the elements of rho sequence)", value = 0.3),
                                                shiny::textInput(inputId = "net_ncores", label = "ncores (number of cores to use for the calculations)", value = "1"),
                                                shiny::numericInput(inputId = "net_em.iter", label = "em.iter (number of EM iterations)", value = 5),
                                                shiny::numericInput(inputId = "net_em.tol", label = "em.tol (criteria to stop the EM iterations)", value = 0.001),
                                                shinyWidgets::actionBttn(inputId = "unc_check_run", label = "Run", style = "jelly", color = "danger")
                                  ),
                                  shiny::column(width = 6,
                                                shiny::tags$h3("Arguments for selectnet"),
                                                shiny::textInput(inputId = "sel_opt.index", label = "opt.index (manually choose an optimal graph from the graph path)", value = "NULL"),
                                                shinyWidgets::pickerInput(inputId = "sel_criteria", label = "criteria (model selection criteria)", choices = c("ebic", "aic"), selected = "ebic"),
                                                shiny::numericInput(inputId = "sel_ebic.gamma", label = "ebic.gamma (tuning parameter for ebic)", value = 0.5),
                                                shiny::textInput(inputId = "sel_ncores", label = "ncores (number of cores to use for the calculations)", value = "1"),
                                                shiny::tags$h4("Bootsrap Method Parameters"),
                                                shiny::numericInput(inputId = "unc_check_n", label = "Number of resamples", value = 10)

                                  )
                                ),
                                circle = TRUE, status = "primary", icon = shiny::icon("gear"), inputId = "dropdown_res",
                                tooltip = shinyWidgets::tooltipOptions(title = "Click to plot's settings")
                              ),
                              shinycssloaders::withSpinner(
                                plotly::plotlyOutput("unc_check_plot", height = "750px", width = "100%"),
                                color = "#007c00", size = 2
                              )
      ),

      #Netphenogeno tab
      shinydashboard::tabItem(tabName = "net",
                              shinycssloaders::withSpinner(
                                plotly::plotlyOutput("net_plot", width = "100%", height = "750px"),
                                color = "#007c00", size = 2
                              )

      ),

      #Centrality Measures Plots
      shinydashboard::tabItem(tabName = "centrality",
                              shinyWidgets::dropdownButton(
                                shiny::tags$h3("Plot Settings"),
                                shinyWidgets::materialSwitch(inputId = "cen_meas_col_switch", label = "Color by Group", value = TRUE, status = "primary"),
                                shiny::selectInput(inputId = "cen_meas_leg_pos", label = "Legend Position", choices = c("right", "left", "bottom", "top"), selected = "bottom"),
                                circle = TRUE, status = "primary", icon = shiny::icon("gear"),
                                tooltip = shinyWidgets::tooltipOptions(title = "Click to plot's settings")
                              ),
                              shinycssloaders::withSpinner(
                                shiny::plotOutput("centrality_plots", height = "750px"),
                                color = "#007c00", size = 2
                              )
      ),

      #Manual
      #shinydashboard::tabItem(tabName = "manual",
      #                        shinydashboard::box(
      #                         style='width:800px;overflow-x: scroll;height:90vh;overflow-y: auto;',
      #                         shiny::includeMarkdown("vignettes/netShiny-Manual.Rmd")
      #                        )
      #),

      #Differences Tab
      shinydashboard::tabItem(tabName = "differences",
                              shiny::tabsetPanel(id = "tab_differences",
                                                 shiny::tabPanel("Difference Networks",
                                                                 shinycssloaders::withSpinner(visNetwork::visNetworkOutput("diff_nets", height = "750px"), color = "#007c00", size = 2)),
                                                 shiny::tabPanel("Difference Table",
                                                                 shinycssloaders::withSpinner(DT::dataTableOutput("diff_table"), color = "#007c00", size = 2)),
                                                 shiny::tabPanel("Network Distances",
                                                                 shiny::fluidRow(
                                                                   shinydashboard::box(title = shiny::textOutput("title_dist_table"), width = 12, solidHeader = TRUE, status = "primary",
                                                                                       shinyWidgets::dropdownButton(
                                                                                         shiny::tags$h3("Table Settings"),
                                                                                         shinyWidgets::radioGroupButtons(inputId = "mat_type_table", label = "", choices = list("Adjacency", "Weighted"), selected = "Adjacency", justified = TRUE),
                                                                                         shiny::selectInput(inputId = "dist_meas_table", label = "Distance Measure", choices = c("Euclidean", "Manhattan", "Canberra"), selected = "Euclidean"),
                                                                                         circle = TRUE, status = "primary", icon = shiny::icon("gear"), size = "sm", width = "300px",
                                                                                         tooltip = shinyWidgets::tooltipOptions(title = "Click to plot's settings")
                                                                                       ),
                                                                                       shinycssloaders::withSpinner(shiny::uiOutput("distances_table"), color = "#007c00", size = 2)
                                                                   )
                                                                 ),
                                                                 shiny::fluidRow(
                                                                   shinydashboard::box(title = shiny::textOutput("title_dist_plot"), width = 12, solidHeader = TRUE, status = "primary",
                                                                                       shinyWidgets::dropdownButton(
                                                                                         shiny::tags$h3("Plot Settings"),
                                                                                         shinyWidgets::radioGroupButtons(inputId = "mat_type_plot", label = "", choices = c("Adjacency", "Weighted"), selected = "Adjacency", justified = TRUE),
                                                                                         shiny::selectInput(inputId = "dist_meas_plot", label = "Distance Measure", choices = c("Euclidean", "Manhattan", "Canberra"), selected = "Euclidean", multiple = TRUE),
                                                                                         shiny::checkboxInput(inputId = "log_check", label = "Log Scale", value = FALSE),
                                                                                         circle = TRUE, status = "primary", icon = shiny::icon("gear"), size = "sm", width = "300px",
                                                                                         tooltip = shinyWidgets::tooltipOptions(title = "Click to plot's settings")
                                                                                       ),
                                                                                       shinycssloaders::withSpinner(shiny::plotOutput("distances_plot"), color = "#007c00", size = 2)
                                                                   )
                                                                 )
                                                 ),
                                                 shiny::tabPanel("Venn Diagram",
                                                                 shinyWidgets::dropMenu(
                                                                   shinyWidgets::actionBttn(inputId = "venn_diag", icon = shiny::icon("gear"), style = "material-circle", color = "default", size = "sm"),
                                                                   shiny::tags$h3("Choose Networks"),
                                                                   shinyWidgets::switchInput(inputId = "venn_opt", onLabel = "Nodes", offLabel = "Edges", value = TRUE),
                                                                   shinyWidgets::pickerInput(inputId = "venn_diag_sel", label = "Choose Networks", choices = sett_nms, selected = sett_nms, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                                                   hideOnClick = TRUE
                                                                 ),
                                                                 shinycssloaders::withSpinner(shiny::plotOutput("venn_diag", height = "750px"), color = "#007c00", size = 2)),
                                                 shiny::tabPanel("Nodes Sets",
                                                                 shinycssloaders::withSpinner(DT::dataTableOutput("diff_sets"), color = "#007c00", size = 2))
                              )
      )
    )
  )

  # Put them together into a dashboardPage
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "netShiny",
                                    shiny::tags$li(shiny::actionLink("settings_button", label = "", icon = shiny::icon("gear")),
                                                   class = "dropdown"), titleWidth = 100),
    sidebar,
    body
  )

  server <- function(input, output, session) {
    shinyjs::useShinyjs()

    if(is.null(resamples)){
      shinyjs::hide(selector = "a[data-value='unc_check']")
    }

    vals <- shiny::reactiveValues(tree_root = NULL,
                                  rseed = 6708,
                                  n_traits = NULL,
                                  sett_names = sett_nms,
                                  trt_types = NULL,
                                  map_nodes = NULL,
                                  node_names = all_node_nms,
                                  coords = NULL,
                                  networks = Net.obj,
                                  mode = "gxe",
                                  subgraph_nodes = NULL,
                                  nodes_to_change = NULL,
                                  color_custom = NULL,
                                  layout = "Automatic",
                                  roundness = 0,
                                  cluster_algs = "Fast Greedy",
                                  hide_iso_markers = TRUE,
                                  hide_iso_traits = FALSE,
                                  hide_iso_nodes = TRUE,
                                  trait_nodes = NULL)

    # Show the model on start up ...
    if(is.null(Net.obj) || isTRUE(to_reconstruct)){
      if(isTRUE(to_reconstruct)){
        shiny::isolate(shiny::updateSelectInput(inputId = "file_names", choices = names(df_settings), selected = names(df_settings)[[1]]))
        shiny::isolate(shiny::updateSelectInput(inputId = "currFile", choices = names(df_settings), selected = names(df_settings)[[1]]))
        #####DISABLE THE BUTTONS FOR HEADER, QUOTE, AND SEP, AND ENABLE THEM IFF USER UPLOAD NEW FILES#####
      }
      else{
        shinyjs::disable("nextButton_startup")
      }
      shinyBS::toggleModal(session = session, modalId = "modalStartup_step1", toggle = "open")
    }
    else{
      shinyBS::toggleModal(session = session, modalId = "startup_mapping", toggle = "open")
      shiny::isolate(shiny::updateTextInput(inputId = "net_names", value = paste(vals$sett_names, collapse = ",")))
    }

    if(isFALSE(to_reconstruct)){
      shinyjs::disable("dropdown_res")
    }

    #reactive value that reacts when when either the data for environments are changed, or
    #the threshold values are changed, or that the layout structure is changed
    #This controls the coordinates for the left panel network, which will help us match the
    #right panel network
    shiny::observeEvent(c(input$color_apply,
                          input$font_apply,
                          input$size_apply,
                          input$cor_t,
                          input$cor_m,
                          input$net1,
                          input$net2,
                          vals$roundness,
                          input$ok_sel_nodes,
                          vals$tree_root,
                          vals$rseed,
                          vals$sett_names,
                          vals$map_nodes,
                          vals$hide_iso_markers,
                          vals$hide_iso_traits), {
                            shiny::validate(shiny::need(shiny::isTruthy(vals$networks), "Nothing Loaded in Yet"))
                            lay <- shiny::repeatable(rngfunc = get_coords, seed = vals$rseed)
                            vals$coords <- lay(vals, input)
                          })

    #Evertime a node is clicked, change to the bootstraps tab with
    #the information for the clicked trait/marker
    shiny::observeEvent({input$click}, {
      shinydashboard::updateTabItems(session, "tabs", selected = "net")
      nz_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      shiny::updateSelectInput(session, "marker", selected = input$click, choices = c(input$click, nz_nodes))
    }, ignoreInit = TRUE)

    shiny::observeEvent({input$refresh}, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      vals$rseed <- sample(1:1000, 1)
    }, ignoreInit = TRUE)

    shiny::observeEvent({input$print_network}, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shinyscreenshot::screenshot(selector = ".content")

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_button, {
      if(!shiny::isTruthy(vals$networks)){
        shinyBS::toggleModal(session = session, modalId = "modalStartup_step1", toggle = "open")
      }
      else{
        shinyBS::toggleModal(session = session, modalId = "data_settings", toggle = "open")
      }
    })

    #This code makes sure that layout is changed accordingly to layout chosen
    shiny::observeEvent({input$layout}, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      vals$layout <- input$layout
      if(input$layout == "Tree"){
        shinyjs::show(id = "roots")
        shinyjs::show(id = "tree_layout_text")
        shinyjs::show(id = "ok_tree")

      }
      else{
        shinyjs::hide(id = "roots")
        shinyjs::hide(id = "tree_layout_text")
        shinyjs::hide(id = "ok_tree")
        vals$tree_root <- c(" ", vals$tree_root)
      }

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$roundness, {
      vals$roundness <- input$roundness
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cluster_algs, {
      vals$cluster_algs <- input$cluster_algs
    }, ignoreInit = TRUE)

    #When ok is clicked on the popup, check if input is valid
    #then change the layout of the netowork to tree, else show another
    #popup saying that input was invalid
    shiny::observeEvent(input$ok_tree, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      rts_inp <- strsplit(input$roots, "\\s+")[[1]]
      ind <- !anyNA(as.numeric(rts_inp))
      if(ind){
        rts_inp <- vals$node_names[as.numeric(rts_inp)]
      }

      if(anyNA(rts_inp) | !all(rts_inp %in% vals$node_names)){
        shiny::showNotification("Error Choosing Root Nodes: Invalid Format", type = "error", duration = NULL)
      }
      else if(length(rts_inp) == 0){
        shiny::showNotification("Error Choosing Root Nodes: Invalid Format", type = "error", duration = NULL)
      }
      else{
        vals$tree_root <- rts_inp
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shiny::isolate(shiny::updateSelectInput(session, "layout", selected = "Automatic"))
      shiny::removeModal()
    }, ignoreInit = TRUE)

    #This hides the controls for when the tab is not the netoworks tab
    #And shows them when we switch back to the networks tab
    shiny::observeEvent(c(input$tabs, input$tabs_summ_stats, input$tabs_net_mat, input$tab_differences, input$tabs_comm_detect), {
      controls <- c("cor_t", "cor_m", "net1", "net2", "layout", "roundness", "refresh", "print_network", "subgraph",
                    "sets_selin", "marker", "meas_butt", "cluster_algs", "customize", "diff_switch")

      if(input$tabs == "networks"){
        shiny::updateSelectInput(session, "net1", label = "Left Panel")
        shiny::updateSelectInput(session, "net2", label = "Right Panel")
        if(input$tabs_net_mat == "Networks"){
          sapply(controls, shinyjs::show)
          sapply(c("sets_selin", "marker", "meas_butt", "cluster_algs", "diff_switch"), shinyjs::hide)
          if(vals$mode != "gxe"){
            shinyjs::hide("cor_m")
          }
        }
        else {
          sapply(controls, shinyjs::hide)
        }

      }

      else if (input$tabs == "manual") {
        sapply(controls, shinyjs::hide)
      }

      else if(input$tabs == "summ_stats"){
        if(input$tabs_summ_stats == "Metrics"){
          sapply(controls, shinyjs::hide)
          sapply(c("cor_t", "subgraph", "meas_butt"), shinyjs::show)
        }
        else{
          sapply(controls, shinyjs::hide)
          sapply(c("cor_t", "subgraph"), shinyjs::show)
        }
        if(vals$mode == "gxe"){
          shinyjs::show("cor_m")
        }
      }

      else if(input$tabs == "net"){
        sapply(controls, shinyjs::hide)
        sapply("marker", shinyjs::show)
        shinyjs::enable("marker")
      }

      else if(input$tabs == "unc_check"){
        sapply(controls, shinyjs::hide)
        sapply("marker", shinyjs::show)
        if(is.null(bootstrap_dat$dat_bootstrap)){
          shinyjs::disable("marker")
        }
      }

      else if(input$tabs == "centrality"){
        sapply(controls, shinyjs::hide)
        sapply(c("cor_t", "subgraph"), shinyjs::show)
        if(vals$mode == "gxe"){
          shinyjs::show("cor_m")
        }
      }

      else if(input$tabs == "differences"){
        shiny::updateSelectInput(session, "net1", label = "Network 1")
        shiny::updateSelectInput(session, "net2", label = "Network 2")

        if(input$tab_differences == "Nodes Sets"){
          sapply(controls, shinyjs::hide)
          sapply(c("cor_t", "subgraph", "net1", "net2", "sets_selin"), shinyjs::show)
          if(vals$mode == "gxe"){
            shinyjs::show("cor_m")
          }
        }
        else if(input$tab_differences == "Network Distances" || input$tab_differences == "Venn Diagram"){
          sapply(controls, shinyjs::hide)
        }
        else if (input$tab_differences == "Difference Networks") {
          sapply(controls, shinyjs::hide)
          sapply(c("net1", "net2", "diff_switch"), shinyjs::show)
        }
        else{
          sapply(controls, shinyjs::hide)
          sapply(c("net1", "net2"), shinyjs::show)
        }
      }

      else if(input$tabs == "comm_detect"){
        if(input$tabs_comm_detect == "Modularity"){
          sapply(controls, shinyjs::hide)
          sapply(c("cor_t", "subgraph", "meas_butt"), shinyjs::show)
          if(vals$mode == "gxe"){
            shinyjs::show("cor_m")
          }
        }
        else{
          shiny::updateSelectInput(session, "net1", label = "Left Panel")
          shiny::updateSelectInput(session, "net2", label = "Right Panel")

          sapply(controls, shinyjs::hide)
          sapply(c("cor_t", "subgraph", "net1", "net2", "cluster_algs", "layout", "roundness", "print_network", "customize"), shinyjs::show)
          if(vals$mode == "gxe"){
            shinyjs::show("cor_m")
          }
        }
      }
    })

    shiny::observeEvent(input$subgraph, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      if(vals$mode == "gxe"){
        dialog_title <- "Select Markers/Traits for Subgraph"
      }
      else{
        dialog_title <- "Select Nodes for Subgraph"
      }
      shiny::showModal(shiny::modalDialog(
        size = "l",
        list(shiny::h3(dialog_title),
             shiny::tags$div(align = 'left',
                             class = 'multicol',
                             shinyWidgets::prettyCheckboxGroup(inputId  = "nodes_subgraph",
                                                               label    = NULL,
                                                               choices  = all_nodes,
                                                               selected = vals$subgraph_nodes,
                                                               shape = "curve",
                                                               icon = shiny::icon("check"),
                                                               status = "primary",
                                                               inline   = FALSE))),
        footer = shiny::tagList(
          shiny::actionButton("reset_sel_nodes", "Reset"),
          shiny::modalButton("Cancel"),
          shiny::actionButton("ok_sel_nodes", "OK")
        )
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$customize, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      shiny::showModal(shiny::modalDialog(
        size = "s",
        list(shiny::h3("Customize Networks"),
             shinyBS::bsCollapse(id = "collapseCustomize", open = "Layout",
                                 shinyBS::bsCollapsePanel("Nodes",
                                                          shinyWidgets::multiInput(inputId = "nodes_to_change", label = "Nodes to Customize:", choices = all_nodes, choiceNames = all_nodes, selected = vals$nodes_to_change),
                                                          shiny::splitLayout(cellWidths = rep("33%", 3),
                                                                             shiny::actionButton(inputId = "clear_nodes", label = "Clear All"),
                                                                             if (vals$mode == "gxe") {
                                                                               shiny::actionButton(inputId = "all_traits", label = "All Traits")
                                                                             } else {
                                                                               shiny::actionButton(inputId = "all_nodes", label = "All Nodes")
                                                                             },
                                                                             if (vals$mode == "gxe") shiny::actionButton(inputId = "all_markers", label = "All Markers")
                                                          ),
                                                          shiny::splitLayout(
                                                            colourpicker::colourInput("color_custom", "Node Color:", allowTransparent = TRUE, closeOnClick = TRUE, value = vals$color_custom),
                                                            shiny::div(style = "margin-top: 25px;", shiny::actionButton(inputId = "color_apply", label = "apply"))
                                                          ),
                                                          shiny::splitLayout(
                                                            shiny::numericInput(inputId = "size_custom", label = "Node Size:", value = NULL, min = 0),
                                                            shiny::div(style = "margin-top: 25px;", shiny::actionButton(inputId = "size_apply", label = "apply"))
                                                          ),
                                                          shiny::splitLayout(
                                                            shiny::numericInput(inputId = "font_custom", label = "Font Size:", value = NULL, min = 0),
                                                            shiny::div(style = "margin-top: 25px;", shiny::actionButton(inputId = "font_apply", label = "apply"))
                                                          ),
                                                          shiny::actionButton(inputId = "reset_custom", label = "Reset to Default"),
                                                          shiny::hr(),
                                                          shinyWidgets::materialSwitch(inputId = "hide_iso_markers",label = "Hide Isolated Markers", value = shiny::isolate(vals$hide_iso_markers), status = "success", right = TRUE),
                                                          shinyWidgets::materialSwitch(inputId = "hide_iso_traits", label = "Hide Isolated Traits", value = shiny::isolate(vals$hide_iso_traits), status = "success", right = TRUE),
                                                          shinyjs::hidden(shinyWidgets::materialSwitch(inputId = "hide_iso_nodes",label = "Hide Isolated Nodes", value = shiny::isolate(vals$hide_iso_nodes), status = "success", right = TRUE)),
                                                          style = "primary"),
                                 shinyBS::bsCollapsePanel("Edges",
                                                          shiny::sliderInput("roundness", "Edge Curviness", min = 0, max = 1, value = vals$roundness),
                                                          style = "primary"),
                                 shinyBS::bsCollapsePanel("Layout",
                                                          shiny::selectInput("cluster_algs", "Clustering Algorithm", c("Fast Greedy", "Edge Betweenness"), selected = vals$cluster_algs),
                                                          shiny::selectInput("layout", "Layout Algorithm",
                                                                             c("Automatic", "Circle", "Fruchterman-Reingold", "Grid.2D", "Kamada-Kawai" = "kk", "Tree"),
                                                                             selected = vals$layout),
                                                          shiny::textInput(inputId = "roots", label = "Choose node(s) to make root", placeholder = "Use node\'s name or node\'s index", value = paste0(vals$tree_root)),
                                                          shiny::p(id = "tree_layout_text", "If choosing multiple roots, separate by space"),
                                                          shiny::actionButton(inputId = "ok_tree", label = "OK"),
                                                          shiny::actionButton(inputId = "refresh", label = "Change Seed"),
                                                          style = "primary")
             )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      ))

      if(vals$mode == "gxe"){
        shinyjs::show(id = "hide_iso_traits")
        shinyjs::show(id = "hide_iso_markers")
        shinyjs::hide(id = "hide_iso_nodes")
      }
      else{
        shinyjs::hide(id = "hide_iso_traits")
        shinyjs::hide(id = "hide_iso_markers")
        shinyjs::show(id = "hide_iso_nodes")
      }

      if(vals$layout == "Tree"){
        shinyjs::show(id = "roots")
        shinyjs::show(id = "tree_layout_text")
        shinyjs::show(id = "ok_tree")
      }
      else{
        shinyjs::hide(id = "roots")
        shinyjs::hide(id = "tree_layout_text")
        shinyjs::hide(id = "ok_tree")
      }

      if(input$tabs != "comm_detect"){
        shinyjs::hide("cluster_algs")
      }
      else{
        shinyjs::hide("heading_cpanel0721075")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(c(input$hide_iso_markers, input$hide_iso_traits, input$hide_iso_nodes), {
      vals$hide_iso_markers <- input$hide_iso_markers
      vals$hide_iso_traits <- input$hide_iso_traits
      vals$hide_iso_nodes <- input$hide_iso_nodes
    })

    shiny::observeEvent(input$color_apply, {
      if(shiny::isTruthy(input$nodes_to_change) && shiny::isTruthy(input$color_custom)){
        vec_change <- which(vals$map_nodes$node %in% input$nodes_to_change)
        vals$map_nodes$node_color[match(input$nodes_to_change, vals$map_nodes$node)] <- rep(input$color_custom, length(input$nodes_to_change))
      }
    })

    shiny::observeEvent(input$size_apply, {
      if(shiny::isTruthy(input$nodes_to_change) && shiny::isTruthy(input$size_custom)){
        if(!shiny::isTruthy(vals$map_nodes$node_size)){
          if(vals$mode == "gxe"){
            vals$map_nodes$node_size <- ifelse(vals$map_nodes$node %in% vals$trait_nodes, 30, 20)
          }
          else{
            vals$map_nodes$node_size <- rep(25, nrow(vals$map_nodes))
          }
        }
        vals$map_nodes$node_size[match(input$nodes_to_change, vals$map_nodes$node)] <- rep(input$size_custom, length(input$nodes_to_change))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$font_apply, {
      if(shiny::isTruthy(input$nodes_to_change) && shiny::isTruthy(input$font_custom)){
        if(!shiny::isTruthy(vals$map_nodes$font_size)){
          vals$map_nodes$font_size <- rep(17, length(vals$map_nodes$node))
        }
        vals$map_nodes$font_size[match(input$nodes_to_change, vals$map_nodes$node)] <- rep(input$font_custom, length(input$nodes_to_change))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_nodes, {
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      vals$nodes_to_change <- vector()
      shinyWidgets::updateMultiInput(session = session, inputId = "nodes_to_change", label = "Nodes to Customize", choices = all_nodes, selected = character(0))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$all_nodes, {
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      shinyWidgets::updateMultiInput(session = session, inputId = "nodes_to_change", label = "Nodes to Customize", choices = all_nodes, selected = all_nodes)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$all_traits, {
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      sel_nodes <- c(vals$trait_nodes, input$nodes_to_change)
      shinyWidgets::updateMultiInput(session = session, inputId = "nodes_to_change", label = "Nodes to Customize", choices = all_nodes, selected = sel_nodes)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$all_markers, {
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      sel_nodes <- c(setdiff(vals$node_names, vals$trait_nodes), input$nodes_to_change)
      shinyWidgets::updateMultiInput(session = session, inputId = "nodes_to_change", label = "Nodes to Customize", choices = all_nodes, selected = sel_nodes)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$reset_custom, {
      vals$map_nodes$node_color <- vals$original_colors
      vals$map_nodes$node_size <- NULL
      vals$map_nodes$font_size <- NULL

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$ok_sel_nodes, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      vals$subgraph_nodes <- input$nodes_subgraph
      shiny::removeModal()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$reset_sel_nodes, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      all_nodes <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
      shinyWidgets::updatePrettyCheckboxGroup(session, inputId = "nodes_subgraph", label = NULL, choices = all_nodes, selected = NULL)
    }, ignoreInit = TRUE)

    #output for the left panel network
    output$network_proxy_nodes <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(shiny::isTruthy(vals$networks), "Nothing Loaded in Yet"))
      if(vals$mode == "gxe") shiny::validate(shiny::need(!is.null(vals$n_traits), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(shiny::isTruthy(vals$map_nodes$node), "Getting proper data"))
      mat <- getNZ(vals = vals, input = input, mat = vals$networks[[input$net1]])
      mat <- get_subnet(vals = vals, mat = mat)
      shiny::validate(shiny::need(length(mat@x) > 0, 'No Connections'))
      g <- get_g_complete(vals = vals, mat = mat)
      lay <- get_igraph_lay(vals = vals, input = input, mat = mat)
      vis_net <- get_vis_net(vals = vals, input = input, mat = mat, g = g, lay = lay)
      vis_net
    })

    #Output for the right panel network
    output$network_proxy_nodes_2 <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(shiny::isTruthy(vals$networks), "Nothing Loaded in Yet"))
      if(vals$mode == "gxe") shiny::validate(shiny::need(!is.null(vals$n_traits), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(shiny::isTruthy(vals$map_nodes$node), "Getting proper data"))
      mat <- getNZ(vals = vals, input = input, mat = vals$networks[[input$net2]])
      mat <- get_subnet(vals = vals, mat = mat)
      shiny::validate(shiny::need(length(mat@x) > 0, 'No Connections'))
      g <- get_g_complete(vals = vals, mat = mat)
      lay <- get_igraph_lay(vals = vals, input = input, mat = mat)
      vis_net <- get_vis_net(vals = vals, input = input, mat = mat, g = g, lay = lay)
      vis_net
    })

    output$mat_plots <- plotly::renderPlotly({
      if(vals$mode == "gxe") shiny::validate(shiny::need(!is.null(vals$n_traits), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      p <- get_mat_plots(vals = vals, input = input)

      plotly::layout(p, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
    })

    output$summary_statistics <- shiny::renderPlot({
      if(vals$mode == "gxe") shiny::validate(shiny::need(!is.null(vals$n_traits), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      p <- get_summ_stats_plot(vals = vals, input = input)
      p

    })

    #Output for the partial correlations plot
    output$par_cors <- shiny::renderPlot({
      if(vals$mode == "gxe") shiny::validate(shiny::need(!is.null(vals$n_traits), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      if(shiny::isTruthy(input$par_cor_bins)){
        p <- get_par_cor_plot(vals = vals, input = input)
        p
      }
    }, bg = "#F5F2F2")

    output$title_par_cors <- shiny::renderText({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      if(vals$mode == "gxe"){
        paste0("Partial Correlations")
      }
      else{
        paste0("Weights")
      }
    })

    #Output plot for weights analysis tab
    output$net_plot <- plotly::renderPlotly({
      if(vals$mode == "gxe") shiny::validate(shiny::need(!is.null(vals$n_traits), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      p <- get_weights_analysis_plot(vals = vals, input = input)
      gp <- plotly::ggplotly(p, tooltip = c("text"))

      if(is.null(vals$map_nodes)){
        for(i in 1:length(gp$x$data)){
          text <- gp$x$data[[i]]$text
          text <- gsub(".{2}$", "", gsub("Chromosome:.+", "", text))
          gp$x$data[[i]]$text <- text
        }
      }
      plotly::layout(gp, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
    })

    output$centrality_plots <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      if(shiny::isTruthy(vals$map_nodes)){
        shinyjs::show("cen_meas_col_switch")
      }
      else{
        shinyjs::hide("cen_meas_col_switch")
      }
      p <- create_central_meas(vals = vals, input = input)
      p
    }, bg = "#F5F2F2")

    output$diff_sets <- DT::renderDataTable({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      dt <- get_diff_sets(vals = vals, input = input)
      dt
    })

    output$diff_nets <- visNetwork::renderVisNetwork({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(input$net1 != input$net2, "Same Networks Selected"))
      if (input$diff_switch) {
        net <- get_dif_net_compl(vals = vals, input = input)
      } else {
        net <- get_dif_net(vals = vals, input = input)
      }

      net
    })

    output$diff_table <- DT::renderDataTable({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      dt <- get_diff_table(vals = vals, input = input)
      dt
    })

    output$distances_table <- shiny::renderTable({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      dist_mat <- apply_mat_dist_list(vals = vals, input = input)
      dist_mat
    }, rownames = TRUE, na = "")

    output$title_dist_table <- shiny::renderText({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      paste0("Network Distances Table", " (", input$mat_type_table, ", ",  input$dist_meas_table, ")")
    })

    output$distances_plot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(length(input$dist_meas_plot) > 0, "Choose a distance measure"))
      p <- apply_mat_dist_list(vals = vals, input = input, for_plot = TRUE)
      p

    })

    output$venn_diag <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(length(input$venn_diag_sel) >= 2, "Not Enough Networks Chosen"))
      p <- get_venn_diag(vals = vals, input = input)
      p

    }, bg = "transparent")

    output$title_dist_plot <- shiny::renderText({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      paste0("Network Distances Plot", " (", input$mat_type_plot, ", ",  paste(input$dist_meas_plot, collapse = "/"), ")")
    })

    output$mods <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      p <- modularity_plot(vals = vals, input = input)
      p
    }, bg = "transparent")

    output$comms_plot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      comm_det <- shiny::repeatable(rngfunc = comm_detection_plot, seed = vals$rseed)
      p <- comm_det(vals = vals, input = input, setting = input$net1)

    }, bg = "transparent")

    output$comms_plot2 <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      comm_det <- shiny::repeatable(rngfunc = comm_detection_plot, seed = vals$rseed)
      p <- comm_det(vals = vals, input = input, setting = input$net2)
      p
    }, bg = "transparent")

    output$unc_check_plot <- plotly::renderPlotly({
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shiny::validate(shiny::need(!is.null(bootstrap_dat$dat_bootstrap), "No Resampling Data"))
      p <- bootstrap_func(vals = vals, mkr = input$marker, new_res = bootstrap_dat$dat_bootstrap)
      shinyjs::enable("marker")
      p
    })

    shiny::observeEvent(input$unc_check_run, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = "unc_check_confirm",
        title = "Run Resampling?",
        text = shiny::tags$b(
          "Are arguments chosen correctly?",
          style = "color: #FA5858;"
        ),
        btn_labels = c("Cancel", "Run"),
        btn_colors = c("#00BFFF", "#FE2E2E"),
        type = "warning",
        closeOnClickOutside = FALSE,
        html = TRUE
      )
    }, ignoreInit = TRUE)

    bootstrap_dat <- shiny::reactiveValues(dat_bootstrap = resamples)
    promise_boot <- shiny::reactiveValues(promise_dat = NULL)

    shiny::observeEvent(input$unc_check_confirm, {
      shiny::validate(shiny::need(!is.null(vals$networks), "Nothing Loaded in Yet"))
      inva(TRUE)
      if(isTRUE(input$unc_check_confirm)){
        n_res <- input$unc_check_n
        nr <- nrow(uploadedFiles$files[[1]])
        len_f <- length(uploadedFiles$files)
        prog_inc <- len_f * n_res

        shinyjs::disable("dropdown_res")
        new_l <- vector("list", len_f)
        for(i in 1:length(new_l)){
          new_l[[i]] <- vector("list", n_res)
        }
        progress <- ipc::AsyncProgress$new(message = "Boostrapping Procedure")
        new_rec <- NULL
        l_args <- get_args_recon(input = input, start_up = FALSE)

        list_sett <- lapply(uploadedFiles$files, function(df) replicate(n_res, df[sample(1:nr, size = nr, replace = TRUE), ], simplify = FALSE))
        promise_boot$promise_dat <- future::future({
          for(i in 1:len_f){
            for(j in 1:n_res){
              net_rec <- netgwas::netphenogeno(data = list_sett[[i]][[j]],
                                               method = l_args[["net_method"]],
                                               rho = l_args[["net_rho"]],
                                               n.rho = l_args[["net_n.rho"]],
                                               rho.ratio = l_args[["net_rho.ratio"]],
                                               ncores = l_args[["net_ncores"]],
                                               em.iter = l_args[["net_em.iter"]],
                                               em.tol = l_args[["net_em.tol"]],
                                               verbose = FALSE
              )
              new_l[[i]][[j]] <- netgwas::selectnet(netgwas.obj = net_rec,
                                                    opt.index = l_args[["sel_opt.index"]],
                                                    criteria = l_args[["sel_criteria"]],
                                                    ebic.gamma = l_args[["sel_ebic.gamma"]],
                                                    ncores = l_args[["sel_ncores"]],
                                                    verbose = FALSE)$par.cor
              progress$inc(1/prog_inc)
            }
          }
          progress$close() # Close the progress bar
          new_l
        })

        promises::then(
          promise_boot$promise_dat,
          onFulfilled = function(value) {
            bootstrap_dat$dat_bootstrap <- value
          },
          onRejected = NULL
        )
      }

      return(NULL)
    }, ignoreInit = TRUE)

    shiny::observe({
      if(inva()){
        shiny::invalidateLater(1000)
      }
      if(!is.null(promise_boot$promise_dat)){
        if(isTRUE(future::resolved(promise_boot))){
          inva(FALSE)
          shinyjs::enable("dropdown_res")
        }
      }
    })

    inva <- shiny::reactiveVal(FALSE)

    uploadedFiles <- shiny::reactiveValues(files = df_settings,
                                           header = NULL,
                                           sep = NULL,
                                           quote = NULL,
                                           exc_columns = NULL)

    sett_upFiles <- function(name){
      uploadedFiles$header[[name]] <- input$header
      uploadedFiles$sep[[name]] <- input$sep
      uploadedFiles$quote[[name]] <- input$quote
      uploadedFiles$exc_columns[[name]] <- col_to_exclude(vec = input$exc_columns, warn = TRUE)
    }

    shiny::observeEvent(input$files_upload ,{
      shiny::isolate(shiny::updateSelectInput(inputId = "currFile", choices = "", selected = NULL))
      shiny::isolate(shiny::updateSelectInput(inputId = "file_names", choices = "", selected = NULL))
      shiny::isolate(shiny::updateTextInput(session = session, inputId = "exc_columns", label = "Columns To Exclude (separate by space)", value = ""))

      lst <- list()
      names_files <- vector(mode = "character")

      for(i in 1:length(input$files_upload[, 1])){
        t_name <- input$files_upload[[i, 'name']]
        if(get_ext(t_name) %nin% c("xlsx", "csv", "xls", "txt")){
          shiny::showNotification(paste(t_name, "is not in any of the supported formats", sep = " "), type = "warning", duration = NULL)
          next
        }

        names_files <- append(names_files, t_name)

        if(get_ext(t_name) == "xlsx" || get_ext(t_name) == "xls"){
          uploadedFiles$files[[t_name]] <- readxl::read_excel(path = input$files_upload[[i, 'datapath']])
          sett_upFiles(t_name)
        }
        else{
          uploadedFiles$files[[t_name]] <- utils::read.csv(input$files_upload[[i, 'datapath']], row.names = NULL)
          sett_upFiles(t_name)
        }
      }

      if(shiny::isTruthy(names_files)){
        shiny::updateSelectInput(inputId = "currFile", choices = names_files, selected = names_files[1])
        all_names <- names(uploadedFiles$files)
        shiny::updateSelectInput(inputId = "file_names", choices = all_names, selected = all_names[1])
        shiny::isolate(shiny::updateTextInput(session = session, inputId = "exc_columns", label = "Columns To Exclude (separate by space)", value = ""))
        shinyjs::enable(id = "nextButton_startup")
      }
    })

    shiny::observeEvent(c(input$header, input$sep, input$quote, input$exc_columns),{
      shiny::req(shiny::isTruthy(uploadedFiles$files) && length(uploadedFiles$files) > 0)
      ind <- which(input$files_upload[['name']] == input$currFile)
      if(shiny::isTruthy(ind)){
        if(get_ext(input$currFile) == "xlsx" || get_ext(input$currFile) == "xls"){
          uploadedFiles$files[[input$currFile]] <- readxl::read_excel(path = input$files_upload[[ind, 'datapath']], col_names = input$header)
        }
        else{
          uploadedFiles$files[[input$currFile]] <- utils::read.csv(input$files_upload[[ind, 'datapath']], row.names = NULL,
                                                                   header = input$header,
                                                                   sep = input$sep,
                                                                   quote = input$quote)
        }
      }
      col_excl <- col_to_exclude(input$exc_columns)
      uploadedFiles$files[[input$currFile]] <- check_cols_to_excl(session = session, df = uploadedFiles$files[[input$currFile]], vec = col_excl)
      sett_upFiles(input$currFile)
    }, ignoreInit = TRUE)

    output$tbl <- DT::renderDataTable({
      shiny::req(shiny::isTruthy(uploadedFiles$files) && length(uploadedFiles$files) > 0)
      shiny::updateCheckboxInput(inputId = "header", value = uploadedFiles$header[[input$currFile]])
      shiny::updateRadioButtons(inputId = "sep", selected = uploadedFiles$sep[[input$currFile]])
      shiny::updateRadioButtons(inputId = "quote", selected = uploadedFiles$quote[[input$currFile]])
      shiny::updateTextInput(inputId = "exc_columns", value = paste(as.character(uploadedFiles$exc_columns[[input$currFile]]), collapse = " "))


      DT::datatable(
        utils::head(uploadedFiles$files[[input$currFile]], 10),
        filter = 'none', extensions = c('Scroller'),
        options = list(scrollY = 250,
                       scrollX = 650,
                       deferRender = TRUE,
                       scroller = TRUE, #paging = TRUE, pageLength = 25,
                       fixedColumns = TRUE),
        rownames = FALSE)
    })

    output$tbl_mapping <- DT::renderDataTable({
      shiny::req(shiny::isTruthy(input$mapping_upload) || shiny::isTruthy(mapping))

      if(shiny::isTruthy(input$mapping_upload)){
        mapping_file <- input$mapping_upload
        if(get_ext(mapping_file$name) %nin% c("xlsx", "csv", "xls", "txt")){
          shiny::showNotification(paste(mapping_file$name, "is not in any of the supported formats", sep = " "), type = "warning")
          shiny::req(isTRUE(FALSE))
        }

        if(get_ext(mapping_file$name) == "xlsx" || get_ext(mapping_file$name) == "xls"){
          mapping <- readxl::read_excel(path = mapping_file$datapath, col_names = input$header_mapping)
        }
        else{
          mapping <- utils::read.csv(file = mapping_file$datapath, row.names = NULL,
                                     header = input$header_mapping,
                                     sep = input$sep_mapping,
                                     quote = input$quote_mapping)
        }
      }

      col_excl <- col_to_exclude(input$exc_columns_mapping)
      mapping <- check_cols_to_excl(session = session, df = mapping, vec = col_excl)
      if(ncol(mapping) > 2 && is.null(vals$mapping)){
        shiny::showNotification("Only the first two columns will be used", type = "warning")
      }

      if(ncol(mapping) < 2){
        shiny::showNotification("Columns has less than two columns", type = "error")
        shiny::req(isTRUE(FALSE))
      }

      else{
        vals$map_nodes_init <- mapping
      }

      DT::datatable(
        vals$map_nodes_init,
        filter = 'none', extensions = c('Scroller'),
        options = list(scrollY = 250,
                       scrollX = 650,
                       deferRender = TRUE,
                       scroller = TRUE, #paging = TRUE, pageLength = 25,
                       fixedColumns = TRUE),
        rownames = FALSE
      )
    })


    shiny::observeEvent(input$nextButton_startup, {
      shinyBS::toggleModal(session = session, modalId = "modalStartup_step1", toggle = "close")
    }, ignoreInit = TRUE)

    listenNetArgs <- shiny::reactive({
      list(input$net_rho_start, input$net_n.rho_start, input$net_rho.ratio_start, input$net_ncores_start, input$net_em.iter_start, input$net_em.tol_start,
           input$sel_opt.index_start, input$sel_criteria_start, input$sel_ebic.gamma_start, input$sel_ncores_start,
           input$net_method_start, input$modalStartup_reconstruction)
    })

    shiny::observeEvent(listenNetArgs(), {
      if(is.null(vals$start_up_args)){
        for(i in 1:length(input$files_upload[, 1])){
          t_name <- input$files_upload[[i, 'name']]
          vals$start_up_args[[t_name]] <- get_args_recon(input = input, start_up = TRUE)
        }
      }
      else{
        vals$start_up_args[[input$file_names]] <- get_args_recon(input = input, start_up = TRUE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file_names, {
      shiny::req(shiny::isTruthy(vals$start_up_args))
      shinyWidgets::updatePickerInput(session = session, inputId = "net_method_start", label = "method", choices = c("Gibbs sampling" = "gibbs", "approximation method" = "approx", "nonparanormal" = "npn"),
                                      selected = vals$start_up_args[[input$file_names]][["net_method_start"]])
      shiny::updateTextInput(session = session, inputId = "net_rho_start", label = "rho (decreasing sequence of non-negative numbers that control the sparsity level)",
                             value = ifelse(is.null(vals$start_up_args[[input$file_names]][["net_rho_start"]]), "NULL", vals$start_up_args[[input$file_names]][["net_rho_start"]]))
      shiny::updateNumericInput(session = session, inputId = "net_n.rho_start", label = "n.rho (number of regularization parameters)",
                                value = vals$start_up_args[[input$file_names]][["net_n.rho_start"]])
      shiny::updateNumericInput(session = session, inputId = "net_rho.ratio_start", label = "rho.ratio (distance between the elements of rho sequence)", min = 0, max = 1, step = 0.1,
                                value = vals$start_up_args[[input$file_names]][["net_rho.ratio_start"]])
      shiny::updateTextInput(session = session, inputId = "net_ncores_start", label = "ncores (number of cores to use for the calculations)",
                             value = as.character(vals$start_up_args[[input$file_names]][["net_ncores_start"]]))
      shiny::updateNumericInput(session = session, inputId = "net_em.iter_start", label = "em.iter (number of EM iterations)",
                                value = vals$start_up_args[[input$file_names]][["net_em.iter_start"]])
      shiny::updateNumericInput(session = session, inputId = "net_em.tol_start", label = "em.tol (criteria to stop the EM iterations)", min = 0.001, max = 1, step = 0.001,
                                value = vals$start_up_args[[input$file_names]][["net_em.tol_start"]])

      shiny::updateTextInput(session = session, inputId = "sel_opt.index_start", label = "opt.index (manually choose an optimal graph from the graph path)",
                             value = ifelse(is.null(vals$start_up_args[[input$file_names]][["sel_opt.index_start"]]), "NULL", vals$start_up_args[[input$file_names]][["sel_opt.index_start"]]))
      shinyWidgets::updatePickerInput(session = session, inputId = "sel_criteria_start", label = "criteria (model selection criteria)", choices = c("ebic", "aic"),
                                      selected = vals$start_up_args[[input$file_names]][["sel_criteria_start"]])
      shiny::updateNumericInput(session = session, inputId = "sel_ebic.gamma_start", label = "ebic.gamma (tuning parameter for ebic)", min = 0, max = 1, step = 0.1,
                                value = vals$start_up_args[[input$file_names]][["sel_ebic.gamma_start"]])
      shiny::updateNumericInput(session = session, inputId = "sel_ncores_start", label = "ncores (number of cores to use for the calculations)",
                                value = as.character(vals$start_up_args[[input$file_names]][["sel_ncores_start"]]))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$startup_run, {
      if(!shiny::isTruthy(uploadedFiles$files)){
        shiny::showNotification("No Datafiles To Run", type = "warning")
        shinyBS::toggleModal(session = session, modalId = "modalStartup_reconstruction", toggle = "close")
        shinyBS::toggleModal(session = session, modalId = "startup_mapping", toggle = "open")
      }
      shiny::req(shiny::isTruthy(uploadedFiles$files))
      args_net <- paste("net_", args_netphenogeno[-1], "_start", sep = "")
      args_sel <- paste("sel_", args_selectnet, "_start", sep = "")
      args_all <- c(args_net, args_sel, "sel_net_tag", "startup_run", "prevButton_reconstruction")
      vals$node_names <- unique(unlist(lapply(uploadedFiles$files, colnames)))
      sapply(c(args_all, "net_method_start", "file_names", "adv_op"), shinyjs::disable)

      vals$networks <- perform_startup_recon(val_nets = vals$networks, files = uploadedFiles$files, l_args = vals$start_up_args)

      if(length(vals$networks) > 0){
        vals$sett_names <- names(vals$networks)
        shiny::isolate(shiny::updateTextInput(inputId = "net_names", value = paste(vals$sett_names, collapse = ",")))
        shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "mat_sel", choices = vals$sett_names, selected = vals$sett_names))
        shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "venn_diag_sel", choices = vals$sett_names, selected = vals$sett_names))
        shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "mat_sel", choices = vals$sett_names, selected = vals$sett_names))
        shiny::isolate(shiny::updateTextInput(inputId = "net_names", value = paste(vals$sett_names, collapse = ",")))
        shinyBS::toggleModal(session = session, modalId = "modalStartup_reconstruction", toggle = "close")
        shinyBS::toggleModal(session = session, modalId = "startup_mapping", toggle = "open")
        shinyjs::enable("dropdown_res")
      }
      sapply(c(args_all, "net_method_start", "file_names", "adv_op"), shinyjs::enable)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$nextButton_mapping, {
      vals$map_nodes <- vals$map_nodes_init
      shinyBS::toggleModal(session = session, modalId = "startup_mapping", toggle = "close")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$finish_data_settings, {
      shiny::req(shiny::isTruthy(vals$networks))
      net_names <- trimws(strsplit(input$net_names, split = ",")[[1]])
      vect_err <- vector(mode = "character")
      #Check if number of network names passed is correct
      if(length(net_names) > length(vals$networks)){
        vect_err <- append(vect_err, "Too many network names given")
      }
      else if (length(net_names) < length(vals$networks)){
        vect_err <- append(vect_err, "Too few network names given")
      }

      else if(isTRUE(input$gxe_mode) && !shiny::isTruthy(counter_trait_types$types)){
        vect_err <- append(vect_err, "No traits given")
      }
      if(length(vect_err) == 0){
        tlist <- vector("list")
        trt_typs <- data.frame("node" = character(), "node_group" = character())
        for (t in counter_trait_types$types) {
          if (!shiny::isTruthy(input[[t]])) {
            if (isTRUE(input$gxe_mode)) {
              vect_err <- append(vect_err, paste0("For trait type ", t, ", no nodes were selected"))
            } else {
              vect_err <- append(vect_err, paste0("For group ", t, ", no nodes were selected"))
            }

          }
          tlist[[t]] <- input[[t]]
        }
        trt_typs <- data.frame("node_group" = rep(names(tlist), lengths(tlist)), "node" = unlist(tlist))
      }
      #Check if we have any error and show a notification about the error(s)
      if(length(vect_err) != 0){
        shiny::showNotification(paste(vect_err, collapse = "\n"), type = "error")
      }
      #If no errors detected, put the all values in required format
      else{
        shiny::isolate(vals$sett_names <- net_names)
        shiny::isolate(names(vals$networks) <- net_names)
        if(vals$mode == "gxe"){
          shiny::isolate(vals$n_traits <- length(counter_trait_types$types))
          shiny::isolate(vals$trait_nodes <- unlist(tlist))
        }
        shiny::updateSelectInput(session, "net1", choices = vals$sett_names, selected = vals$sett_names[1])
        shiny::updateSelectInput(session, "net2", choices = vals$sett_names, selected = vals$sett_names[2])
        vals$map_nodes <- map_nodes_to_group(vals = vals, input = input, trt_typs = trt_typs)
        vals$map_nodes <- complete_df(vals = vals)
        vals$original_colors <- vals$map_nodes$node_color
        av_mks <- unique(unlist(lapply(vals$networks, get_nz_nodes, vals = vals, input = input)))
        av_mks <- av_mks[order(match(av_mks, vals$node_names))]
        shiny::updateSelectInput(session, "marker", choices = av_mks)
        shinyBS::toggleModal(session = session, modalId = "data_settings", toggle = "close")
        shinyjs::disable("gxe_mode")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$prevButton_reconstruction, {
      shinyBS::toggleModal(session = session, modalId = "modalStartup_reconstruction", toggle = "close")
      shinyBS::toggleModal(session = session, modalId = "modalStartup_step1", toggle = "open")
    }, ignoreInit = TRUE)


    shiny::observeEvent(input$prevButton_mapping, {
      shinyBS::toggleModal(session = session, modalId = "startup_mapping", toggle = "close")
      shinyBS::toggleModal(session = session, modalId = "modalStartup_reconstruction", toggle = "open")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$prevButton_data_settings, {
      shinyBS::toggleModal(session = session, modalId = "data_settings", toggle = "close")
      shinyBS::toggleModal(session = session, modalId = "startup_mapping", toggle = "open")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$adv_op, {
      args_net <- names(formals(netgwas::netphenogeno))[-c(1, 2)]
      args_net <- paste("net_", args_net, "_start", sep = "")
      args_sel <- names(formals(netgwas::selectnet))[-1]
      args_sel <- paste("sel_", args_sel, "_start", sep = "")
      args_all <- c(args_net, args_sel, "sel_net_tag")
      if(isTRUE(input$adv_op)){
        sapply(args_all, shinyjs::show)
      }
      else{
        sapply(args_all, shinyjs::hide)
      }
    })

    shiny::observeEvent(input$gxe_mode, {
      shiny::req(shiny::isTruthy(vals$networks))
      if(isFALSE(input$gxe_mode)){
        shiny::isolate(shiny::updateTextInput(session = session, inputId = "trait_types", label = "Put groups separated by semicolons (;)", value = input$trait_types))
        vals$mode <- "general"
        nm <- trimws(strsplit(input$net_names, split = ",")[[1]])
        if(setequal(nm, sprintf("Environment %s", 1:length(vals$networks)))){
          names(vals$networks) <- sprintf("Network %s", 1:length(vals$networks))
          vals$sett_names <- sprintf("Network %s", 1:length(vals$networks))
          shiny::isolate(shiny::updateTextInput(inputId = "net_names", label = "Network Names", value = paste(vals$sett_names, collapse = ", ")))
          shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "venn_diag_sel", choices = vals$sett_names, selected = vals$sett_names))
          shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "mat_sel", choices = vals$sett_names, selected = vals$sett_names))
        }
        shiny::isolate(shinyjs::hide("cor_m"))
        nets <- lapply(vals$networks, function(x){diag(x) <- 0; x})
        nets <- lapply(nets, Matrix::drop0, tol = 0, is.Csparse = TRUE)
        max_slider <- round(max(unlist(lapply(nets, max))), 2)
        shiny::isolate(shiny::updateSliderInput(session = session, inputId = "cor_t", min = 0, max = max_slider, label = "Weights Values"))
        shiny::isolate(shiny::updateSelectInput(session = session, inputId = "marker", label = "Node"))
      }
      else{
        shiny::isolate(shiny::updateTextInput(session = session, inputId = "trait_types", label = "Put trait groups separated by semicolons (;)", value = input$trait_types))
        vals$mode <- "gxe"
        nm <- trimws(strsplit(input$net_names, split = ",")[[1]])
        if(setequal(nm, sprintf("Network %s", 1:length(vals$networks)))){
          names(vals$networks) <- sprintf("Environment %s", 1:length(vals$networks))
          vals$sett_names <- sprintf("Environment %s", 1:length(vals$networks))
          shiny::isolate(shiny::updateTextInput(inputId = "net_names", label = "Environment Names", value = paste(vals$sett_names, collapse = ", ")))
          shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "venn_diag_sel", choices = vals$sett_names, selected = vals$sett_names))
          shiny::isolate(shinyWidgets::updatePickerInput(session = session, inputId = "mat_sel", choices = vals$sett_names, selected = vals$sett_names))
        }
        shiny::isolate(shinyjs::show("cor_m"))
        shiny::isolate(shiny::updateSliderInput(session = session, inputId = "cor_t", min = 0, max = 1, label = "Partial Correlations Traits"))
        shiny::isolate(shiny::updateSelectInput(session = session, inputId = "marker", label = "Marker"))
      }
    })

    #To store observers
    obs_trait_List <- reactiveValues()

    counter_trait_types <- shiny::reactiveValues(types = NULL)

    observeEvent(input$trait_create_group, {
      counter_trait_types$types <- trimws(strsplit(input$trait_types, split = ";")[[1]])
    })

    output$trait_inputs <- renderUI({
      shiny::req(shiny::isTruthy(counter_trait_types$types))
      buttons <- as.list(1:length(counter_trait_types$types))
      buttons <- lapply(buttons, function(i) {
        btName <- counter_trait_types$types[[i]]
        if (is.null(obs_trait_List[[btName]])) {
          obs_trait_List[[btName]] <- shiny::observeEvent(input[[btName]], {
            all_chosen <- NULL
            diff_nms <- setdiff(counter_trait_types$types, btName)
            for (b in counter_trait_types$types) {
              all_chosen <- union(all_chosen, input[[b]])
            }
            for (b in diff_nms) {
              sel <- input[[b]]
              choices <- setdiff(vals$node_names, setdiff(all_chosen, sel))
              shinyWidgets::updateMultiInput(session = session, inputId = b, choices = choices, selected = sel)
            }
          })
        }
        shinyWidgets::multiInput(inputId = btName, label = btName, choices = vals$node_names)
      }
      )
    })

  }

  shiny::shinyApp(ui,
                  server
  )

}
