
# Define server logic 
function(input, output, session) {
  
  # Reload app if disconnected
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # Reload app button
  observeEvent(input$reload,session$reload())
  
  # On session end
  session$onSessionEnded(stopApp)
  
  # Upload message
  observeEvent(input$file, {
    showModal(modalDialog(
      title = "Reading Data", "Please Wait", 
      footer = NULL,
      fade = FALSE,
      easyClose = TRUE,
    ))
    Sys.sleep(2)
  }, priority=100)
  
  
  # Upload data
  datainput <- reactive({ 
    
    ###############
    # Validations
    ###############
    
    validate(need(input$file$datapath != "", "Please upload a CSV file."))
    
    validate(need(tools::file_ext(input$file$datapath) == "csv", "Error. Not a CSV file. Please upload a CSV file."))
    
    
    if (input$fencoding == "unknown"){
      
      validate(need(try(datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=".", encoding = "unknown", 
                                            data.table = FALSE, na.strings = "")),
                    "Error. File cannot be read. Please check that the file is not empty, fully whitespace, or skip has been set after the last non-whitespace."))
      
      validate(need(tryCatch(datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=".", encoding = "unknown", 
                                           data.table = FALSE, na.strings = ""), warning=function(w) {}),
                    "Error. The file cannot be read unambigously. Check the characters for the field separator, quote or decimal. Remove blank lines. "
                    ))

      validate(need(try(iconv(colnames(datainput1), guess_encoding(input$file$datapath)[[1]][1], "UTF-8")),
                        "Error. Encoding cannot be converted. Please revise your data or try other upload options."))
      
               
      validate(need(try(sapply(datainput1[, sapply(datainput1, is.character)], function(col) iconv(col, guess_encoding(input$file$datapath)[[1]][1], "UTF-8"))),
                        "Error. Encoding cannot be converted. Please revise your data or try other upload options."))
      
    }
    
   if (input$fencoding == "UTF-8"){
      
      validate(
       need(guess_encoding(input$file$datapath)[[1]][1] %in% c("UTF-8","ASCII") & 
               guess_encoding(input$file$datapath)[[2]][1] > 0.9,
             "Error. The file is probably not UTF-8 encoded. Please convert to UTF-8 or try the automatic encoding option.")
      )
     
      validate(need(try(datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=".", encoding = "UTF-8", 
                                  data.table = FALSE, na.strings = "")), "Error. File cannot be read. Please check that the file is not empty, fully whitespace, or skip has been set after the last non-whitespace."))
      
      
      validate(need(tryCatch(datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=".", encoding = "unknown", 
                                                 data.table = FALSE, na.strings = ""), warning=function(w) {}),
                    "Error. The file cannot be read unambigously. Check the characters for the field separator, quote or decimal. Remove blank lines. "
      ))

   }
    
   

   if (is.null(input$file))
      return(NULL)
    
    
    ###############
    # Datainput code
    ################
    
    return(tryCatch(
      
      
      if (input$fencoding == "UTF-8" & input$decimal == "auto"){ 
        
        datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=".", encoding = "UTF-8", data.table = FALSE, na.strings = "")
        
        # Probably comma as decimal
        colnames <- sapply(datainput1, function(col) is.numeric(col) & Negate(is.integer)(col))
        if (sum(colnames) == 0L){
          
          datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=",", encoding = "UTF-8", data.table = FALSE, na.strings = "")
          datainput1
          
        } else {datainput1}
        
      } else if (input$fencoding == "UTF-8" & input$decimal != "auto") {
        
        datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=input$decimal, encoding = "UTF-8", data.table = FALSE, na.strings = "")
        datainput1
        
        
      } else if (input$fencoding == "unknown" &  input$decimal == "auto"){
        
        enc_guessed <- guess_encoding(input$file$datapath)
        enc_guessed_first <- enc_guessed[[1]][1]
        datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=".", encoding = "unknown", data.table = FALSE, na.strings = "")
        
        # Probably comma as decimal
        colnames <- sapply(datainput1, function(col) is.numeric(col) & Negate(is.integer)(col))
        if (sum(colnames) == 0L){
          
          datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec=",", encoding = "unknown", data.table = FALSE, na.strings = "")
          colnames(datainput1) <- iconv(colnames(datainput1), enc_guessed_first, "UTF-8")
          col_names <- sapply(datainput1, is.character)
          datainput1[ ,col_names] <- sapply(datainput1[, col_names], function(col) iconv(col, enc_guessed_first, "UTF-8"))
          datainput1
          
        } else {
          
          colnames(datainput1) <- iconv(colnames(datainput1), enc_guessed_first, "UTF-8")
          col_names <- sapply(datainput1 , is.character)
          datainput1[ ,col_names] <- sapply(datainput1[, col_names], function(col) iconv(col, enc_guessed_first, "UTF-8"))
          datainput1}
        
      } else {
        
        enc_guessed <- guess_encoding(input$file$datapath)
        enc_guessed_first <- enc_guessed[[1]][1]
        datainput1 <- fread(input$file$datapath, header = "auto", sep="auto", dec = input$decimal, encoding = "unknown", data.table = FALSE, na.strings = "")
        colnames(datainput1) <- iconv(colnames(datainput1), enc_guessed_first, "UTF-8")
        col_names <- sapply(datainput1, is.character)
        datainput1[ ,col_names] <- sapply(datainput1[, col_names], function(col) iconv(col, enc_guessed_first, "UTF-8"))
        datainput1
        
      }
      
      ,error=function(e) stop(safeError(e))
      
    ))
    
    
  })
  
  
  # Row limits 
  observe({
    
    req(input$file, datainput())
    
    removeModal()
    
    
    if (nrow(datainput()) > 10000){
      showNotification("Maximum sample size exceeded. ", duration=30)
      Sys.sleep(5)
      session$close()
    }
    
    if (nrow(datainput()) < 3){
      showNotification("Error: Minimum 3 observations required. ", duration=30)
      Sys.sleep(5)
      session$close()
    }
    
    
  })
  
  
  
  
  # Select Variables
  output$selection1 <- renderUI({
    
    req(datainput())
    
    removeModal()
    
    chooserInput("selection1", "Available", "Selected",
                 colnames(datainput()), c(), size = 15, multiple = TRUE)
    
  })
  
  
  # Stop if column names not distinct or if too many columns selected
  observe({
    
    req(input$file, datainput())
    
    removeModal()
    
    if (length(unique(input$selection1$left)) != length(input$selection1$left)){
      showNotification("Error: The columns names are not distinct. Rename columns and restart the app.", duration=30)
      Sys.sleep(5)
      session$close()
    }
    
    
    if (length(input$selection1$right) > 25 ){
      showNotification("Maximum number of columns exceeded. For more contact: support@statsomat.com", duration=30)
      Sys.sleep(5)
      session$close()
    }
    
  })
  

  # This creates a short-term storage location for a filepath 
  report <- reactiveValues(filepath = NULL) 
  
  # Render report
  observeEvent(input$generate, {
    
    req(input$file, datainput(), input$selection1$right)
  
    src0 <- normalizePath('report_kernel.Rmd') 
    src1 <- normalizePath('report.Rmd')
    src4 <- normalizePath('references.bib')
    src5 <- normalizePath('report_code_unknown.Rmd') 
    src6 <- normalizePath('report_code_common.Rmd') 
    src8 <- normalizePath('FiraSans-Bold.otf')
    src9 <- normalizePath('FiraSans-Regular.otf')
    
    # Temporarily switch to the temp dir
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src0, 'report_kernel.Rmd', overwrite = TRUE)
    file.copy(src1, 'report.Rmd', overwrite = TRUE)
    file.copy(src4, 'references.bib', overwrite = TRUE)
    file.copy(src5, 'report_code_unknown.Rmd', overwrite = TRUE)
    file.copy(src6, 'report_code_common.Rmd', overwrite = TRUE)
    file.copy(src8, 'FiraSans-Bold.otf', overwrite = TRUE)
    file.copy(src9, 'FiraSans-Regular.otf', overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    enc_guessed <- guess_encoding(input$file$datapath)
    enc_guessed_first <- enc_guessed[[1]][1]
    
    params <- list(data = datainput(), filename=input$file, fencoding=input$fencoding, decimal=input$decimal, enc_guessed = enc_guessed_first, 
                   vars1 = input$selection1$right, continuity=input$continuity)
   
    
    
    tryCatch({
      
      withProgress(message = 'Please wait, the Statsomat app is computing. This may take a while.', value=0, {
        
        for (i in 1:15) {
          incProgress(1/15)
          Sys.sleep(0.25)
          
        }
      
        if (input$rcode == "Data Analysis Report (PDF)"){
          
          tmp_file <- render('report.Rmd', pdf_document(latex_engine = "xelatex"),
                        params = params,
                        envir = new.env(parent = globalenv())
          )
          
        } else {
        
          tmp_file <- render('report_code_unknown.Rmd', html_document(),
                             params = params,
                             envir = new.env(parent = globalenv())
                             )
          
        }
    
        report$filepath <- tmp_file 
    
      })
      
      showNotification("Now you can download the file.", duration=20)
      
    },
    
    error=function(e) {
      # Report not available 
      showNotification("Something went wrong. Please contact the support@statsomat.com. ",duration=20)
      }
    )
    
  })
  
  
  # Enable downloadbutton 
  observe({
    req(!is.null(report$filepath))
    session$sendCustomMessage("check_generation", list(check_generation  = 1))
  })
  

  
  
   
  # Download report  
  output$download <- downloadHandler(
    
    filename = function() {
      
      if (input$rcode == "Data Analysis Report (PDF)"){
        paste('MyReport',sep = '.','pdf')
      } else {
        paste('MyCode',sep = '.','html')
      }
    },
    
    content = function(file) {

      file.copy(report$filepath, file)
         
    }
  )
  
  
}