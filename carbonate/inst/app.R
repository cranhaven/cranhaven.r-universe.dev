td <- file.path(tempdir(),'carbonshiny')
dir.create(td)
carb <- carbonate::carbon$new(silent_yml = TRUE)
carb$download_path <- td
v <- names(carb$.__enclos_env__$private$var_names)

# Define the UI
ui <- miniUI::miniPage(
  miniUI::gadgetTitleBar("Carbonate Shiny App"),
  
  miniUI::miniContentPanel(
    
    shiny::fillRow(
                   fillCol(
                     column(width = 12,
                     shiny::h2('Script'),
                     shiny::actionButton(inputId = 'get',label = 'Fetch from carbon.js'),
                     shiny::hr(),
                       shinyAce::aceEditor(
                         height = '300px',
                         outputId = "myEditor",
                         wordWrap = TRUE,
                         value = rstudioapi::getSourceEditorContext()$selection[[1]]$text,
                         mode = "r",
                         theme = "ambiance",
                         placeholder = 'Enter Code Here ...',
                         fontSize = 10
                       )
                   )),
                   fillCol(
                     column(width = 12,
                      shiny::h2('Images'),
                      shiny::fileInput("local", NULL, accept = c("image/png")
                      ),
                      shiny::hr(),
                      slickR::slickROutput('carbons'))
                     ),
                   fillCol(
                     column(12,
                            shiny::h2('Twitter'),
                            shiny::actionButton(inputId = 'post',label = 'Post to Twitter'),
                            shiny::hr(),
                            shiny::textInput(inputId = 'reply_status_id',label = NULL,placeholder = 'Enter reply status id'),
                            shiny::uiOutput('chars'),
                            shiny::textAreaInput(
                              inputId = 'status',
                              label = sprintf('Tweet Status: Posting as @%s', 
                                              Sys.getenv('TWITTER_SCREEN_NAME'))
                              )
                     ))
                   )
    
))


# Define the server code
server <- function(session, input, output) {

  shiny::observeEvent(input$myEditor,{
    carb$code <- input$myEditor
  })
  
  shiny::observeEvent(c(input$local,input$get),{
    
      output$carbons <- slickR::renderSlickR({
    
        imgs <- list.files(td,full.names = TRUE)
        opts <- slickR::settings(adaptiveHeight = TRUE)
        
        if(length(imgs)>1){
          opts <- slickR::settings(adaptiveHeight = TRUE, dots = TRUE)
        }
            
        slickR::slickR(imgs,slideId = 'me') + opts
          
      })
    
  })
  
  observeEvent(input$local,{
    inFile <- input$local
    idx <- length(list.files(td,pattern = '^local')) + 1
    if(!is.null(inFile$datapath)){
      file.copy(inFile$datapath,file.path(td,glue::glue('local_{idx}.png')))
    }
  })
  
  shiny::observeEvent(input$get,{
    
    carb$carbonate(
      file = glue::glue('img_{length(list.files(td)) + 1}.png'),
      path = td
    )

  })
  
  reply_handles <- shiny::eventReactive(input$reply_status_id,{
    
    if(!nzchar(input$reply_status_id))
      return('')
    
    reply <- rtweet::lookup_statuses(input$reply_status_id)
    
    handles <- trimws(
        gsub(pattern = "(@\\w+\\s)(*SKIP)(*FAIL)|.",
             replacement =  "", 
             x = reply[['text']], 
             perl = TRUE)
      )
    
    handles <- strsplit(handles,'\\s')[[1]]
    
    sender <- glue::glue("@{reply$screen_name}")
    whoami <- glue::glue("@{Sys.getenv('TWITTER_SCREEN_NAME')}")
    
    handles <- c(sender, handles)
    handles <- setdiff(handles, whoami)
    handles <- paste0(unique(handles),collapse = ' ')
    
    if(!length(handles))
      handles <- ''

    handles
    
  })
  
  shiny::observeEvent(c(input$status,input$reply_status_id),{
    
    carb$tweet_status <- glue::glue('{reply_handles()} {input$status}')
  
    output$chars <- shiny::renderUI({
      shiny::h6(glue::glue('Characters: {nchar(carb$tweet_status) -1 }'))
    })
      
  })
  
  shiny::observeEvent(input$post,{
    
    carb$rtweet(media = carb$carbons,
                media_format = 'png',
                in_reply_to_status_id = input$reply_status_id)

  })
  
  shiny::observeEvent(input$cancel, {
    unlink(td,recursive = TRUE,force = TRUE)
    shiny::stopApp(invisible())
  })
  
  shiny::observeEvent(input$done, {
    unlink(td,recursive = TRUE,force = TRUE)
    shiny::stopApp(invisible())
  })
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)