## ---- eval = FALSE------------------------------------------------------------
#  library(crunchy)
#  ui <- fluidPage(
#      includeHTML("dashboard")
#  )
#  
#  server <- function(input, output, session) {}
#  
#  # Run the application
#  shinyApp(ui = ui, server = server)

## ---- eval = FALSE------------------------------------------------------------
#  ui <- fluidPage(
#      uiOutput("dashboard")
#  )

## ---- eval = FALSE------------------------------------------------------------
#  server <- function(input, output, session) {
#      user <- shinyUser()
#      output$dashboard <- renderUI({
#          user()
#          includeHTML("acme_dashboard.html")
#      })
#  }

## ----eval = FALSE-------------------------------------------------------------
#  server <- function(input, output, session) {
#      user <- shinyUser()
#      output$dashboard <- renderUI({
#          current_user <- email(user())
#          if (grepl("acme.com$", current_user)) {
#              includeHTML("acme_dashboard.html")
#          } else if (grepl("globex.com$", current_user)) {
#              includeHTML("globex_dashboard.html")
#          } else {
#              h1("You do not have access to this dashboard, please contact your dataset administrator.")
#          }
#      })
#  }

## ----eval = FALSE-------------------------------------------------------------
#  library(crunchy)
#  
#  ui <- fluidPage(
#      actionButton('refresh', 'Refresh Dashboard'),
#      textOutput("last_render"),
#      uiOutput("dashboard")
#  )
#  
#  server <- function (input, output, session) {
#      file <- reactivePoll(1000, session,
#          checkFunc = function () {
#              file.info("acme_dashboard.html")$ctime
#          },
#          valueFunc = function () {
#              list(
#                  path = "acme_dashboard.html",
#                  time = Sys.time()
#              )
#          }
#      )
#      user <- shinyUser()
#      observeEvent(input$refresh, rmarkdown::render("acme_dashboard.Rmd"))
#      output$last_render <- renderText(paste("Last Render:", file()$time))
#      output$dashboard <- renderUI({
#          user()
#          includeHTML(file()$path)
#      })
#  }
#  
#  shinyApp(ui = ui, server = server)
#  

