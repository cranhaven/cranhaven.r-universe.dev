## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=F---------------------------------------------------------
library(shiny.reglog)

## ----RegLogServer_assignement_example, eval = F-------------------------------
#    RegLogServer <- RegLogServer$new(
#      dbConnector = dbConnector,
#      mailConnector = mailConnector)

## ----is_logged, eval = F------------------------------------------------------
#  # observe the reactiveVal change
#  observeEvent(RegLogServer$is_logged, {
#  
#    # if contains TRUE: user is currently logged-in
#    if (RegLogServer$is_logged()) {
#      showModal(modalDialog(title = "You are logged in!"))
#      # if contains FALSE: user isn't logged-in
#    } else {
#      showModal(modalDialog(title = "You are not logged in!"))
#    }
#  })

## ----uuid_example-------------------------------------------------------------
uuid::UUIDgenerate()

## ----user_id, eval = F--------------------------------------------------------
#  RegLogServer$user_id()

## ----user_mail, eval = F------------------------------------------------------
#  RegLogServer$user_mail()

## ----account_id, eval=F-------------------------------------------------------
#  RegLogServer$account_id()

## ----user_logout, eval = F----------------------------------------------------
#  # if you create an actionButton "logout_button" in your UI, you can observe
#  # its action and logout after press:
#  observeEvent(input$logout_button, {
#    RegLogServer$logout()
#  })

## ----eval=F-------------------------------------------------------------------
#  # you can observe some kind of event to trigger the send
#  observeEvent(input$send_custom_email, {
#  
#    # as the username and email will be acquired from RegLogServer,
#    # it is best to make sure that the user is logged-in
#    req(RegLogServer$is_logged())
#  
#    message_to_send <- RegLogConnectorMessage(
#      type = "custom_mail",
#      # name your process in some unique way - it will be tracked by the app
#      # and saved into logs
#      process = "attachement_mail!",
#      # username and email can be gotten from RegLogServer
#      username = RegLogServer$user_id(),
#      email = RegLogServer$user_mail(),
#      # we can specify the subject and body of message ourselves
#      mail_subject = "Custom message with attachement",
#      # it's best for the body to contain html code
#      mail_body = "<p>This is a custom message send from my App</p>
#                   <p>It is completely optional, but that kind of message can also
#                      contain an attachment!</p>",
#      # optionally: attachment
#      mail_attachement = "files/myplot.png"
#    )
#  })

## ---- eval = F----------------------------------------------------------------
#  logs_df <- RegLogServer$get_logs()

## ---- eval = F----------------------------------------------------------------
#      # initialize new RegLogServer object
#  RegLog <- RegLogServer$new(
#    # assign created dbConnector and mailConnector
#    dbConnector = dbConnector,
#    mailConnector = mailConnector,
#    # replace default title and body of `login_success` modalDialog
#    custom_txts = list(
#      login_success_t = "Welcome!",
#      login_success_b = "Hi! It's nice to see you logged into our ShinyApp!"
#    )
#  )

## ---- eval = F----------------------------------------------------------------
#  # initialize new RegLogServer object
#  RegLog <- RegLogServer$new(
#    # assign created dbConnector and mailConnector
#    dbConnector = dbConnector,
#    mailConnector = mailConnector,
#    # inhibit default 'login_success' and 'register_success' modals
#    use_modals = list(
#      login_success = FALSE,
#      register_success = FALSE
#    )
#  )

## ---- eval = F----------------------------------------------------------------
#  # examples with default values
#  options("RegLogServer.logs" = 1)
#  options("RegLogServer.logs_to_database" = 0)

## ----eval = F-----------------------------------------------------------------
#  RegLog <- RegLogServer$new(
#    dbConnector = somedbConnector,
#    mailConnector = RegLogConnector$new()
#  )

