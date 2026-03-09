## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(shiny.reglog)

## ----preview_of_message-------------------------------------------------------
message <- 
  RegLogConnectorMessage(
    type = "test",
    dataframe = mtcars,
    numbers = runif(10, 0, 100),
    logcontent = paste0("I contain data.frame and random numbers"))

str(message)

## ----login_message------------------------------------------------------------
login_message <- 
  RegLogConnectorMessage(
    type = "login",
    username = "Whatever",
    password = "&f5*MSYj^niDt=V'3.[dyEX.C/")

str(login_message)

## ----register_message---------------------------------------------------------
register_message <- 
  RegLogConnectorMessage(
    type = "register",
    username = "IAmNewThere",
    email = "something@new.com",
    password = "veryHardP422w0rd!")

str(register_message)

## ----credsEdit message--------------------------------------------------------
credsEdit_message <- 
  RegLogConnectorMessage(
    type = "credsEdit",
    account_id = 1,
    password = "&f5*MSYj^niDt=V'3.[dyEX.C/",
    new_username = "Whenever",
    new_email = "edited@email.com",
    new_password = "veryHardP422w0rd!")

str(credsEdit_message)

## ----resetPass_generate_message-----------------------------------------------
resetPass_generate_message <- 
  RegLogConnectorMessage(
    type = "resetPass_generate",
    username = "Whatever")

str(resetPass_generate_message)

## ----resetPass_confirm_message------------------------------------------------
resetPass_confirm_message <-
  RegLogConnectorMessage(
    type = "resetPass_confirm",
    username = "Whatever",
    reset_code = "4265417643",
    password = "veryHardP422w0rd!")

str(resetPass_confirm_message)

## ----mail_reglogconnectormessage----------------------------------------------
resetPass_mail_message <- 
  RegLogConnectorMessage(
    type = "reglog_mail",
    process = "resetPass",
    username = "Whatever",
    email = "edited@email.com",
    app_name = "RegLog Nice ShinyApp",
    app_address = "https://reglog.nice.com",
    reset_code = "4265417643")

str(resetPass_mail_message)

## ----custommailmessage--------------------------------------------------------
message_to_send <- RegLogConnectorMessage(
    type = "custom_mail",
    process = "attachment_mail",
    username = "Whatever",
    email = "edited@email.com",
    mail_subject = "Custom message with attachement",
    mail_body = "<p>This is a custom message send from my App</p>
                 <p>It is completely optional, but that kind of message can also
                    contain an attachment!</p>",
    mail_attachement = "files/myplot.png"
  )

## ----create_new_sheet, eval=F-------------------------------------------------
#  # create new sheet to the googlesheet
#  googlesheets4::write_sheet(
#    ss = gsheet_ss,
#    sheet = "SES_results",
#    # append 0-row data.frame to create the "schema" for the sheet
#    data = data.frame(timestamp = as.character(NA),
#                      user_id = as.character(NA),
#                      score = as.numeric(NA))[-1,]
#  )

## ----SES_results_write, eval=F------------------------------------------------
#  write_SES_handler <- function(self, private, message) {
#  
#    googlesheets4::sheet_append(
#      # ID of the connected googlesheet is stored inside private of the
#      # RegLogGsheetConnector
#      ss = private$gsheet_ss,
#      sheet = "SES_results",
#      data = data.frame(
#        # db_timestamp creates nicely formatted and interpretable by most
#        # databases current time
#        timestamp = db_timestamp(),
#        # user ID and score should be received inside received message
#        user_id = message$data$user_id,
#        score = message$data$score
#        ))
#  
#    return(RegLogConnectorMessage(type = "write_SES",
#                                  success = TRUE))
#  
#  }

## ----SES_results_read, eval=F-------------------------------------------------
#  read_SES_handler <- function(self, private, message) {
#  
#    # read all results
#    SES_results <- googlesheets4::read_sheet(
#      ss = private$gsheet_ss,
#      sheet = "SES_results",
#      col_types = "ccn")
#  
#    # get the lastest result for the current user
#    SES_results <- SES_results |>
#      dplyr::filter(user_id == message$data$user_id) |>
#      dplyr::arrange(dplyr::desc(timestamp)) |>
#      dplyr::slice_head()
#  
#    # return the RegLogConnectorMessage with the score if available
#    if (nrow(SES_results) == 1) {
#      return(RegLogConnectorMessage(type = "read_SES",
#                                    success = TRUE,
#                                    score = SES_results$score))
#    } else {
#      return(RegLogConnectorMessage(type = "read_SES",
#                                    success = FALSE))
#    }
#  }

## ----everything_in_motion, eval=F---------------------------------------------
#  # create and assign RegLogServer object
#  RegLog <- RegLogServer$new(
#    # create googlesheet connector
#    dbConector = RegLogGsheetConnector$new(
#      # provide correct googlesheet ID
#      gsheet_ss = gsheet_ss,
#      # provide handlers in a named list. Names will be used to choose on basis
#      # of received RegLogConnectorMessage which function to use
#      custom_handlers = list(write_SES = write_SES_handler,
#                             read_SES = read_SES_handler)
#    ),
#    # provide some mailConnector with all needed data
#    mailConnector = mailConnector
#  )
#  
#  # create an event to write the data to the database: there actionButton will
#  # trigger it
#  observeEvent(input$write_ses_result, {
#  
#    # make sure the inputs are provided
#    req(input$SES_1, input$SES_2, input$SES_3, input$SES_4, input$SES_5,
#        input$SES_6, input$SES_7, input$SES_8, input$SES_9, input$SES_10)
#  
#    # get the score by summing all raw scores of items
#    score <- sum(input$SES_1, input$SES_2, input$SES_3, input$SES_4, input$SES_5,
#                 input$SES_6, input$SES_7, input$SES_8, input$SES_9, input$SES_10)
#  
#    # send message to the dbConnector's listener
#    RegLog$dbConnector$listener(
#      RegLogConnectorMessage(
#        # specify correct type - the same as the name of the handler
#        type = "write_SES",
#        # get required user ID from the RegLog object
#        user_id = RegLog$user_id(),
#        score = score))
#  })
#  
#  # create an event to read the data from the database: eg. another actionButton
#  observeEvent(input$read_last_ses_result, {
#  
#    # send correct message to the dbConnector's listener
#    RegLog$dbConnector$listener(
#      RegLogConnectorMessage(
#        type = "read_SES",
#        user_id = RegLog$user_id())
#    )
#  })
#  
#  # assign the retrieved data: eg. to the reactive
#  
#  SES_result <- reactive(
#    # retrieved data will be available in `message()` field of RegLog object
#    received_message <- RegLog$message()
#    # make sure to only process correct type of message
#    req(received_message$type == "read_SES")
#  
#    if (!is.null(score)) {
#      # get the score if there was any saved in the database
#      received_message$data$score
#    })

