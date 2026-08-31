## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cognitoR)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  your_ui = function() {
#      fluidPage(
#                # Load UI logout
#                logout_ui("logout"),
#                # Load UI Cognito.
#                cognito_ui("cognito"),
#                # Output to show some content.
#                uiOutput("content"))
#  }

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  your_server = function(input, output, session) {
#  
#      # Call Cognito module. ####
#      cognitomod <- callModule(cognito_server, "cognito")
#  
#      # Call Logout module ####
#      logoutmod <- callModule(logout_server,
#                              "logout",
#                              reactive(cognitomod$isLogged),
#                              sprintf("You are logged in as '%s'", cognitomod$userdata$email))
#  
#      # To Click on button logout of logout module, call logout in cognito module. ####
#      observeEvent(logoutmod(),{
#        cognitomod$logout()
#      })
#  
#      # Check if user is already logged, and load your content. ####
#      observeEvent(cognitomod$isLogged, {
#        if (cognitomod$isLogged) {
#          # User is logged
#          userdata <- cognitomod$userdata
#  
#          output$content <- renderUI({
#            # Load your content here
#          })
#        }
#      })
#  
#    }
#  )

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  shinyApp(
#    ui = your_ui(),
#    server = your_server()
#  )

