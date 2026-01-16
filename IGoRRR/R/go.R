go <- function(envir=.GlobalEnv, examples=TRUE) {
  
  language <- "FR" # The only possibility!

  init(envir,examples,language) # libraries and global data

  # Enable access to images from html
  shiny::addResourcePath(prefix = "images",
                         directoryPath = system.file("images", package="IGoRRR"))

# UI ----------------------------------------------------------------------

  ui <- shinydashboard::dashboardPage(
    skin = "blue",

    header = shinydashboard::dashboardHeader(
      title = "I Go R"
      ,tags$li(class = "dropdown", em(glue('"{.IGoR$Z$version}" -- {utils::packageVersion("IGoRRR")}({language})')))
      ,uiOutput("main.auto")
    ),

    sidebar = shinydashboard::dashboardSidebar(
      tags$head(
        tags$style(HTML("
                      .main-sidebar {
                      font-family : Arial;
                      }
                      .content-wrapper {
                      background-color: linen;
                      font-family : Arial;
                      }
                      "))),
      imageOutput("main.igor",height='140px'),
      tags$div(id = "loading", tags$script('$("#loading").hide()')),
      uiOutput("main.data"),
      do.call(sidebarMenu,
        append(list(id = "menu", width = "400"),
          map2(unname(.IGoR$config$menus),names(.IGoR$config$menus),
               function(l,n)
                 do.call(menuItem,
                   append(.IGoR$Z$dashboard[[n]],
                          map(l, function(x) menuSubItem(.IGoR$Z[[x]]$menu.title,tabName=x))
      ) ) )      )       )
    ),

    shinydashboard::dashboardBody(
      div(id = "form",
        # Following javascript code is imported from https://colinfay.me/watch-r-shiny/
        # Don't re-use or modify without contacting the author at contact@colinfay.me 
        # License : MIT
        tags$script(
          'function checkifrunning() {
          var is_running = $("html").attr("class").includes("shiny-busy");
          if (is_running){
          $("#loading").show()
          } else {
          $("#loading").hide()
          }
          };
          setInterval(checkifrunning, 1000)'
        ),
        tags$style(
          "#loading {
          display: inline-block;
          border: 3px solid #f3f3f3;
          border-top: 3px solid #3498db;
          border-radius: 50%;
          width: 50px;
          height: 50px;
          animation: spin 1s ease-in-out infinite;
          }

          @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
          }"
        ),
        # Launch the user interfaces for all the pages present in config
        do.call(tabItems,
         map(unlist(.IGoR$config$menus, use.names=FALSE),
             function(x) tabItem(x, (get(paste0("page_",x))$ui)())
   ) ) ) )
  )

# Server ------------------------------------------------------------------

  server <- function(input, output,session) {
    
    session$onSessionEnded(shiny::stopApp)
    
    output$main.igor <- renderImage(
      list(src=..image("jll.png"), height = "230px"),
      deleteFile = FALSE
    )
    
    output$main.data <- ..renderTables(input,output)

    state <- reactiveValues()
    .IGoR$state  <- state
    .IGoR$state$data <- Sys.time() # will change every time contents of current table changes
    .IGoR$state$meta <- Sys.time() # will change every time structure of current table changes
    .IGoR$state$list <- Sys.time() # will change every time the list of current tables changes
    
    # .IGoR$auto <- TRUE
    # autoInvalidate <- reactiveTimer(10000)
    # observe({
    #   autoInvalidate()
    #   output$main.auto <- renderText(as.character(as.integer(.IGoR$auto <- !.IGoR$auto)))
    # })
  
    # Launch the servers for all the pages present in config
    walk(unlist(.IGoR$config$menus, use.names=FALSE),
         function(x) (get(paste0("page_",x))$server)(input,output,session))

  }

# Launch application ------------------------------------------------------
  # Remember the current settings
  old <- options()
  
  # The width of the display of tibble previews (should be in config?)
  options(width=200)
  # Deactivates sequence esc[..m used by tibble output in previews
  options(crayon.enabled = FALSE)
  
  # Restores options when 'go' terminates
  on.exit(options(old))
  
  app <- shiny::shinyApp(ui, server, options=list(launch.browser=TRUE))
  print(app) # Application is launched here!
  invisible(NULL)

}
