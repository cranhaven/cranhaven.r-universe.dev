shiny::shinyServer(function(input,output,session) {
  values <- reactiveValues()

  #--------------# Excel data #---------------#
  observe({
    inFile <- input$file1
    if (is.null(inFile)) {
      return (NULL)
    }
    else {#Set up the selection for the worksheet names within the selected file
      filePath <- inFile$datapath
      selectionWorksheet <- sort( unique(readxl::excel_sheets(inFile$datapath)))
      updateSelectInput(session, "worksheet", choices = selectionWorksheet)
    }
  })

  observe({
    if(is.null(input$file1)) {
      shinyjs::disable("getData")
    }

    else {shinyjs::enable("getData")
    }
  })

  observeEvent(input$getData, {# Get the data from the spreadsheet worksheets
    datxl <- readxl::read_excel (filePath, sheet=input$worksheet)
    values$datxl <- as.data.frame(unclass(datxl), stringsAsFactors = TRUE)
    datxl <- values$datxl %>% group_by(News)
    NamesDiarios <- as.data.frame(datxl$Newspapers)
    values$outxl <- table(NamesDiarios)
  })
##---------------scraping--------
  observeEvent(input$runsearch, {
    req(input$search)
    if(input$language == "spanish"){
      url1 <- URLencode(paste0("https://www.google.com/search?q=",
                               gsub(" ","+",input$search),
                               "&hl=es-419&tbm=nws&start="))
    news_array <- c()
    diarios_array <- c()
    for(i in 0:input$numberpag){
        page <- i * 10
        new_url <- paste(url1,toString(page),sep="")
        reddit_wbpg <- read_html(new_url)
        (diario<-reddit_wbpg %>%
            html_nodes('.BNeawe.UPmit') %>%
            html_text())

        (Noticias<-reddit_wbpg %>%
            html_nodes('.zBAuLc.l97dzf') %>%
            html_text())
        news_array <- append(news_array, Noticias)
        diarios_array <- append(diarios_array, diario)
      }
      #creacion de tabla de Datos
      Diario<-data.frame(Diario=diarios_array)
      Diario$Diario<-str_trim(Diario$Diario,side = "both")
      Diario$Diario<-str_to_lower(Diario$Diario,locale = "es")
      Diario$Diario<-chartr('áéíóúñ','aeioun',Diario$Diario)
      Noticia<-data.frame(Noticia=news_array)
      Noticia$Noticia<-str_to_lower(Noticia$Noticia,locale = "es")
      Noticia$Noticia<-chartr('áéíóúñ','aeioun',Noticia$Noticia)
      Data<-cbind(ID=row.names(Diario),Diario, Noticia)
      values$Data <- Data %>% group_by(Noticia) %>% filter (! duplicated(Noticia))
      NamesDiarios<- as.data.frame(Data$Diario)
      values$out <- table(NamesDiarios)
      #View(values$Data)
    }
    else {url1 <- URLencode(paste0("https://www.google.com/search?q=",
                                   gsub(" ","+",input$search),
                                   "&hl=en-US&gl=US&ceid=US%3Aen&tbm=nws&start="))
    news_array <- c()
    diarios_array <- c()
    for(i in 0:input$numberpag){
      page <- i * 10
      new_url <- paste(url1,toString(page),sep="")
      reddit_wbpg <- read_html(new_url)
      (diario<-reddit_wbpg %>%
          html_nodes('.BNeawe.UPmit') %>%
          html_text())

      (Noticias<-reddit_wbpg %>%
          html_nodes('.zBAuLc.l97dzf') %>%
          html_text())
      news_array <- append(news_array, Noticias)
      diarios_array <- append(diarios_array, diario)
    }
    #creacion de tabla de Datos
    Diario<-data.frame(Diario=diarios_array)
    Diario$Diario<-str_trim(Diario$Diario,side = "both")
    Diario$Diario<-str_to_lower(Diario$Diario,locale = "es")
    Diario$Diario<-chartr('áéíóúñ','aeioun',Diario$Diario)
    Noticia<-data.frame(Noticia=news_array)
    Noticia$Noticia<-str_to_lower(Noticia$Noticia,locale = "es")
    Noticia$Noticia<-chartr('áéíóúñ','aeioun',Noticia$Noticia)
    Data<-cbind(ID=row.names(Diario),Diario, Noticia)
    values$Data <- Data %>% group_by(Noticia) %>% filter (! duplicated(Noticia))
    NamesDiarios<- as.data.frame(Data$Diario)
    values$out <- table(NamesDiarios)
    #View(values$Data)
    }
    beepr::beep(2)
  })



  #tabla resumen
  output$sum <-  DT::renderDT({
    if(input$load =="null"){
      return (NULL)
      }
    else if(input$load =="import"){
    req(values$outxl)
      tabla <- data.frame(values$outxl)%>%
      arrange(desc(Freq))}
    else if (input$load =="load"){
      req(values$out)
             tabla <- data.frame(values$out)%>%
      arrange(desc(Freq))}
    DT::datatable(data = tabla,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    buttons = c('pageLength',
                                'copy',
                                'csv',
                                'excel',
                                'pdf',
                                'print'),
                    pagelength = 5,
                    lengthMenu = list(c(5, 25, 100, -1),
                                      c('5', '25', '100','All'))))
  })

#tabla de noticas por diario
  output$text <-  DT::renderDT({

    if(input$load =="null"){
      return (NULL)
    }
    else if(input$load =="import"){
      req(values$datxl)
      tabla <- data.frame(values$datxl)}
    else if (input$load =="load"){
      req(values$Data)
      tabla <- data.frame(values$Data)}

    DT::datatable(data =  tabla, extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    buttons = c('pageLength',
                                'copy',
                                'csv',
                                'excel',
                                'pdf',
                                'print'),
                    pagelength = 5,
                    lengthMenu = list(c(5, 25, 100, -1),
                                      c('5', '25', '100','All'))))
    })

  ### Permitir que este disponible el nombre del periodico
  observe({
    req(values$Data)
    dat <-data.frame(values$out)
  lista <- unique(dat[,1])
    updateSelectizeInput(
      session = session,
      inputId = "selectnews",
      label = "Select/deselect all options",
      choices = c(as.character(lista)),
      server = TRUE
      # multiple = TRUE
    )
  })

  # Realizar proceso de creacion de la matrix dtm
  observeEvent(input$dtm.update, {
    #req(input$selectnews)
    if(is.null(input$selectnews)) {return(shinyalert::shinyalert("Oops!",
                                                    "Something went wrong.
                                                    please select at least
                                                    one newspaper",
                                                    type = "error"))}

    .data <- dplyr::filter(values$Data, Diario %in% c(input$selectnews))
    #View(.data)
    diario <- .data
    values$diario <- data.frame(diario =.data$Diario)
    #View(values$diario)
    stp <- unlist(strsplit(input$stopwords,","))
    stp <- trimws(stp)
    cpus <- parallel::detectCores()
    ngram <- as.integer(input$ngrams)
    Stemm <- trimws(input$Stemm)
    odtm <- textmineR::CreateDtm(
      doc_vec = .data$Noticia,
      doc_names = .data$ID,
      ngram_window = c(1,ngram),
      lower = FALSE,
      remove_punctuation = FALSE,
      remove_numbers = FALSE,
      cpus = cpus)
    if(input$checkStemming){
      dtm <- textmineR::CreateDtm(
        doc_vec = .data$Noticia,
        doc_names = .data$ID,
        ngram_window = c(1,ngram),
        stopword_vec = c(stopwords::stopwords(input$Language),letters,stp),
        lower = TRUE,
        remove_punctuation = TRUE,
        remove_numbers = input$removenumber,
        stem_lemma_function = function(x) SnowballC::wordStem(x, Stemm),
        cpus = cpus)}
    else{dtm <- textmineR::CreateDtm(
      doc_vec = .data$Noticia,
      doc_names = .data$ID,
      lower = TRUE,
      stopword_vec = c(stopwords::stopwords(input$Language),letters,stp),
      ngram_window = c(1,ngram),
      remove_punctuation = TRUE,
      remove_numbers = input$removenumber,
      cpus = cpus)
    }

    values$dtm <- quanteda::as.dfm(dtm)
    CONVERT <- quanteda::convert(
      values$dtm,
      to = "topicmodels")
    values$dtmt <- removeSparseTerms(CONVERT,
                                sparse= input$sparce)
    values$dtmF <-  chinese.misc::m3m(values$dtmt,
                                 to="dgCMatrix")
     Original <- dim(odtm)
    Without_Sparsity <- dim(values$dtm)
    Final  <- dim (values$dtmt)
    values$dimen <- rbind(Original,
                     Final)
    colnames (values$dimen) <- c ("document", "term")
    values$tf_mat <- textmineR::TermDocFreq(dtm = values$dtmF)
    values$freq <- colSums(as.matrix(values$dtmF))  #
    values$wf <- data.frame(word=names(values$freq), freq=values$freq)
    beepr::beep(2)
  })

  ###_ DTM xl_ inicio#####
  observeEvent(input$dtmexcel.update, {
    req(values$datxl)
    .data <- data.frame(values$datxl)
    View(.data)
    stp <- unlist(strsplit(input$stopwordsxl,","))
    stp <- trimws(stp)
    cpus <- parallel::detectCores()
    ngram <- as.integer(input$ngramsxl)
    Stemm <- trimws(input$Stemmxl)


    odtm <- textmineR::CreateDtm(
      doc_vec = .data$News,
      doc_names = .data$ID,
      ngram_window = c(1,ngram),
      lower = FALSE,
      remove_punctuation = FALSE,
      remove_numbers = FALSE,
      cpus = cpus)
    if(input$checkStemming){
      dtm <- textmineR::CreateDtm(
        doc_vec = .data$News,
        doc_names = .data$ID,
        ngram_window = c(1,ngram),
        stopword_vec = c(stopwords::stopwords(input$Languagexl),letters,stp),
        lower = TRUE,
        remove_punctuation = TRUE,
        remove_numbers = input$removenumberxl,
        stem_lemma_function = function(x) SnowballC::wordStem(x, Stemm),
        cpus = cpus)}
    else{dtm <- textmineR::CreateDtm(
      doc_vec = .data$News,
      doc_names = .data$ID,
      lower = TRUE,
      stopword_vec = c(stopwords::stopwords(input$Languagexl),letters,stp),
      ngram_window = c(1,ngram),
      remove_punctuation = TRUE,
      remove_numbers = input$removenumberxl,
      cpus = cpus)
    }


    values$dtmxl <- quanteda::as.dfm(dtm)

    CONVERT <- quanteda::convert(
      values$dtmxl,
      to = "topicmodels")
    values$dtmtxl <- removeSparseTerms(CONVERT,
                                       sparse= input$sparcexl)
    values$dtmFxl <-  chinese.misc::m3m(values$dtmtxl,
                                        to="dgCMatrix")
    Originalxl <- dim(odtm)
    Without_Sparsity <- dim(values$dtm)
    Finalxl  <- dim (values$dtmtxl)
    values$dimenxl <- rbind(Originalxl,
                            Finalxl)
    # colnames (values$dimen) <- c ("document", "term")
    values$tf_mat <- textmineR::TermDocFreq(dtm = values$dtmFxl)
    values$freq <- colSums(as.matrix(values$dtmFxl))  #
    values$wf <- data.frame(word=names(values$freq), freq=values$freq)
    beepr::beep(2)
    View(values$dimenxl)
  })

  output$Table_dimxl <- DT::renderDT({
    dat <- as.matrix(values$dimenxl)
    colnames (dat) <- c ("document", "term")
    DT::datatable(data = dat,
                  options = list(pageLength = 5,
                                 searching = FALSE,
                                 rownames = TRUE))
  })


  #tabla dimenciones de dtm
  output$Table_dim <- DT::renderDT({
    DT::datatable(data = as.matrix(values$dimen),
                  options = list(pageLength = 5,
                                 searching = FALSE,
                                 rownames = TRUE))
  })

  output$data_b <- DT::renderDT({
    DT::datatable(data = values$tf_mat, extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('pageLength',
                                'copy',
                                'csv',
                                'excel',
                                'pdf',
                                'print'),
                    pagelength = 5,
                    lengthMenu = list(c(5, 25, 100, -1),
                                      c('5', '25', '100','All'))
                    )
                  )
    })
  # grafico de barras
  output$plot_gg <-  highcharter::renderHighchart({
    export
    values$wf %>% top_n(input$b, freq) %>%
      hchart("column",
             hcaes(x = word, y = freq),
             color = input$colorbar,
             borderColor = "black") %>%
      hc_add_theme(hc_theme_ggplot2()) %>%
      hc_xAxis(title = list(text = "Term")) %>%
      hc_yAxis(title = list(text = "Frequency")) %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          text = "Export",
          theme = list(fill = "transparent"),
          menuItems = export)
        )
      )
  })
  # nube de palabras
  output$plot_gf<-  highcharter::renderHighchart({
    export
    values$wf %>% top_n(input$c, freq) %>%
      hchart( "wordcloud",
              hcaes(name = word, weight = freq)) %>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          text = "Export",
          theme = list(fill = "transparent"),
          menuItems = export)))

  })

  # co-ocurrencia
  output$plotcoc <- renderPlot({
    if(input$load == "import"){
      dtm <- values$dtmFxl }
    else if(input$load == "load"){
      dtm <- values$dtmF }
    dtm <- dtmremovetfidf(dtm, top = input$d)
    term_correlations <- dtmcorr(dtm)
    values$plotco <- textplot_correlation_glasso(term_correlations,
                                                 exclude_zero = TRUE)
    values$plotco
  })
  output$downplotco <- downloadHandler(
    filename =  function() {
      paste("coocurrenceplot",input$downplotcooc, sep=".")
    },
    # is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$downplotcooc == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$plotco) # draw the plot
      dev.off()  # turn the device off

    })


  #############################number topic###############
  observe({
    if (isTRUE(input$num1>=input$num2||input$num3>input$num2)) {
      shinyjs::disable("Run.model1")
    }
    else if(isTRUE(input$num5>=input$num4)) {
      shinyjs::disable("Run.model1")
    }
    else if(isTRUE(input$num1 < 2 )) {
      shinyjs::disable("Run.model1")
    }
    else if(is.na(as.numeric(input$num1))) {
      shinyjs::disable("Run.model1")
    }
    else if(is.na(as.numeric(input$num2))) {
      shinyjs::disable("Run.model1")
    }
    else if(is.na(as.numeric(input$num3))) {
      shinyjs::disable("Run.model1")
    }
    else if(is.na(as.numeric(input$num4))) {
      shinyjs::disable("Run.model1")
    }
    else if(is.na(as.numeric(input$num5))) {
      shinyjs::disable("Run.model1")
    }
    else if(is.na(as.numeric(input$num6))) {
      shinyjs::disable("Run.model1")
    }
    #else if(is.null(values$dtmF)) {
     # shinyjs::disable("Run.model1")
   # }
    else {shinyjs::enable("Run.model1")
    }
  })
  output$OthKcoh <- renderText({
    stpCohe <- unlist(strsplit(input$OtherKCoherence,","))
    stpCohe <- as.numeric(trimws(stpCohe))
    if (anyNA(stpCohe)) {
      "Invalid input"
    }
  })


  observeEvent(input$Run.model1,{

    ptm <- proc.time()
    stpCohe <- unlist(strsplit(input$OtherKCoherence,","))
    stpCohe <- as.numeric(trimws(stpCohe))
    seqk <- c(seq(from=input$num1,to=input$num2,by=input$num3),stpCohe)# Candidate number of topics k
    iterations <- input$num4     # Parameters control Gibbs sampling
    burnin <- input$num5  # Parameters control Gibbs sampling
    alpha <- input$num6 # Parameters control
    cores <- parallel::detectCores()
    #dtm <- values$dtmF
    if(input$load == "import"){
      dtm <- values$dtmFxl }
    else if(input$load == "load"){
      dtm <- values$dtmF }

    values$coherence_list <- textmineR::TmParallelApply(X = seqk ,
                                                        FUN = function(k){
      m <- textmineR::FitLdaModel(dtm= dtm ,
                                  k = k,
                                  iterations =iterations ,
                                  burnin = burnin,
                                  alpha = alpha,
                                  beta = colSums(dtm) / sum(dtm) * 100,
                                  optimize_alpha = TRUE,
                                  calc_likelihood = TRUE,
                                  calc_coherence = TRUE,
                                  calc_r2 = FALSE,
                                  cpus = cores)
      m$k <- k
      m
    },export= ls(), # c("nih_sample_dtm"), # export only needed for Windows machines
    cpus = cores)
    values$coherence_mat <- data.frame(k = sapply(values$coherence_list,
                                                  function(x) nrow(x$phi)),
                                       coherence = sapply(values$coherence_list,
                                                          function(x) mean(x$coherence)),
                                          stringsAsFactors = FALSE)
    beepr::beep(2)
    values$end_time <- proc.time() - ptm

  })

  output$timeCoherence <- renderPrint({
    req(values$end_time)
    print( values$end_time)
  })

  output$plot_gi <-  highcharter::renderHighchart({
    req(values$coherence_list)
    values$coherence_mat

    export
    values$coherence_mat %>%
      hchart("line", hcaes(x = k, y = coherence)) %>%
      hc_add_theme(hc_theme_ggplot2())%>%
    hc_credits(
      enabled = TRUE, text = "Source: LDAbiplots",
      style = list(fontSize = "12px")
    )%>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(contextButton = list(
          text = "Export",
          theme = list(fill = "transparent"),
          menuItems = export)))
  })

  ########LDA#####################
  observe({
    if (isTRUE(input$num27 >= input$num26|| input$num25 < 2)) {
      shinyjs::disable("Run.model5")
    }
    #else if(is.null(values$dtmF)) {
     # shinyjs::disable("Run.model5")
    #}

    else if(is.na(as.numeric(input$num25))) {
      shinyjs::disable("Run.model5")
    }
    else if(is.na(as.numeric(input$num26))) {
      shinyjs::disable("Run.model5")
    }
    else if(is.na(as.numeric(input$num27))) {
      shinyjs::disable("Run.model5")
    }
    else if(is.na(as.numeric(input$num28))) {
      shinyjs::disable("Run.model5")
    }
    else {
      shinyjs::enable("Run.model5")
    }
  })

  observeEvent(input$Run.model5,{
    set.seed(12345)
    shinybusy::show_modal_spinner(
      spin = "atom",
      color = "#428ded",
      text = "Please wait...Work in progress. You'll have to be
      patient, because the result may take a while"
    )

    k <- input$num25
    iter <- input$num26
    burnin <- input$num27
    alpha <- input$num28

    cpus <- parallel::detectCores()

    if(input$load == "import"){
      dtm <- values$dtmFxl
      }
    else if(input$load == "load"){
      dtm <- values$dtmF
      }
    values$model <- textmineR::FitLdaModel(dtm = dtm, # parameter
                                          k = k ,# Number of topics k
                                          iterations = iter, # parameter
                                          burnin = burnin, #parameter
                                          alpha = alpha,# parameter
                                          beta = colSums(dtm)/sum(dtm)*100,
                                          optimize_alpha = TRUE, # parameter
                                          calc_likelihood = TRUE,
                                          calc_coherence = TRUE,
                                          calc_r2 = FALSE,
                                          cpus = cpus)

        beepr::beep(2)
        remove_modal_spinner()
  })

  output$summLDA <- DT::renderDT({
    req(values$model)
    if(input$load == "import"){
      dtm <- values$dtmFxl
    }
    else if(input$load == "load"){
      dtm <- values$dtmF
    }
     model <- values$model
    top_terms <- textmineR::GetTopTerms(phi = model$phi, M = input$Topterm)

    top_term <- apply(top_terms, 2, function(x){
      paste(x, collapse = ", ")
    })

     prevalence <- colSums(model$theta) / sum(model$theta) * 100
    # textmineR has a naive topic labeling tool based on probable bigrams
    values$labels <- textmineR::LabelTopics(assignments = model$theta > input$assignments,
                          dtm = dtm,
                          M = input$Labels)
    summar <- data.frame(topic = rownames(model$phi),
                          label = values$labels,
                          coherence = round(model$coherence, 3),
                          prevalence = round(prevalence,3),
                          top_terms = top_term)


    DT::datatable(data = summar, extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 lengthMenu = list(c(5, 25, 100, -1),
                                                   c('5', '25', '100','All'))))%>%
      formatRound( columns= c("coherence","prevalence"),
                   digits=5)
  })

  output$theta <- DT::renderDT({
    req(values$model$theta)
    tidy_theta <- data.frame(ID = rownames(values$model$theta),
                                     round(values$model$theta,5),
                                     stringsAsFactors = FALSE) %>%
      tidyr::gather(topic, gamma, -ID)

    if(input$load == "import"){
      Newspaper <- values$datxl$Newspapers
      values$tidy_theta <- data.frame(Newspaper = Newspaper,tidy_theta)
      values$tidy_theta
      }
    else if(input$load == "load"){
      Newspaper <- values$diario
      values$tidy_theta <- data.frame(Newspaper = Newspaper,tidy_theta)
      values$tidy_theta
      }

    DT::datatable(data =  values$tidy_theta ,
                  extensions = 'Buttons',
                  filter = 'top',
                  colnames=c("Newspaper","ID news","Topic", "theta"),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 lengthMenu = list(c(5,100,1000,10000,-1),
                                                   c('5', '100', '1000','10000','All')
                                 )
                  )
    )%>% formatRound( columns= c("gamma"),digits=5)

  })

  output$phi <- DT::renderDT({
    req(values$model$phi)
    tidy_beta <- data.frame(topic = as.integer(str_replace_all(rownames(values$model$phi),                                                                "t_", "")
    ),
    round(values$model$phi,5),
    stringsAsFactors = FALSE)%>%
      tidyr::gather(term, beta, -topic)


  DT::datatable(data =  tidy_beta ,
                extensions = 'Buttons',
                filter = 'top',
                colnames=c("topic","term", "phi"),
                options = list(dom = 'Bfrtip',
                               buttons = c('pageLength',
                                           'copy',
                                           'csv',
                                           'excel',
                                           'pdf',
                                           'print'),
                               pagelength = 5,
                               lengthMenu = list(c(5,100,1000,10000, -1),
                                                 c('5', '100', '1000','10000','All')
                               )
                )
  )
  })




  output$plot_worcloud <-  highcharter::renderHighchart({

      req(values$model)
    tidy_beta <- data.frame(
      topic = as.integer(str_replace_all(rownames(values$model$phi),
                                         "t_", "")),
      round(values$model$phi,5),stringsAsFactors = FALSE)%>%
      tidyr::gather(term, beta, -topic)
    export
    tidy_beta %>% dplyr::filter(topic==input$num29)%>%
      dplyr::top_n(input$cloud, beta) %>%
      hchart(
        "wordcloud",
        hcaes(name = term,
              weight = beta)) %>%
      hc_credits(
        enabled = TRUE, text = "Source: LDAbiplots",
        style = list(fontSize = "12px")
      )%>%
      hc_exporting(
        enabled = TRUE,
        formAttributes = list(target = "_blank"),
        buttons = list(
          contextButton = list(
            text = "Export",
            theme = list(fill = "transparent"),
            menuItems = export)))
  })

  output$plot_heatmap <-  highcharter::renderHighchart({
    req(values$tidy_theta)

    colr <- list( list(0.005, '#9ecae1'),
                  list(0.01, '#4292c6'),
                  list(0.05,'#2171b5'),
                  list(0.10,'#08519c'),
                  list(0.30,'#fc8d59'),
                  list(0.45,'#ef6548'),
                  list(0.75,'#d7301f'),
                  list(0.95,'#b30000'),
                  list(1, '#7f0000'))

    if(input$load == "import"){
      heatm <- values$tidy_theta %>%
        group_by(topic,Newspaper) %>%
        summarise(proportion= mean(gamma))
      export
      heatm %>%
        hchart("heatmap", hcaes(x = Newspaper,
                                y = as.integer(str_replace_all(topic,"t_", " ")) ,
                                value =proportion)) %>%
        hc_colorAxis(stops= colr,
                     min=min(heatm$proportion),
                     max= max(heatm$proportion)) %>%
        hc_yAxis(title = list(text = "Topic"))%>%
        hc_credits(
          enabled = TRUE, text = "Source: LDABiplots",
          style = list(fontSize = "12px")
        )%>%
        hc_exporting(
          enabled = TRUE,
          formAttributes = list(target = "_blank"),
          buttons = list(contextButton = list(
            text = "Export",
            theme = list(fill = "transparent"),
            menuItems = export)))
    }

    else if(input$load == "load"){
      heatm <- values$tidy_theta %>%
        group_by(topic,diario) %>%
        summarise(proportion= mean(gamma))
      export
      heatm %>%
        hchart("heatmap", hcaes(x = diario,
                                y = as.integer(str_replace_all(topic,"t_", " ")) ,
                                value =proportion)) %>%
        hc_colorAxis(stops= colr,
                     min=min(heatm$proportion),
                     max= max(heatm$proportion)) %>%
        hc_yAxis(title = list(text = "Topic"))%>%
        hc_credits(
          enabled = TRUE, text = "Source: LDABiplots",
          style = list(fontSize = "12px")
        )%>%
        hc_exporting(
          enabled = TRUE,
          formAttributes = list(target = "_blank"),
          buttons = list(contextButton = list(
            text = "Export",
            theme = list(fill = "transparent"),
            menuItems = export)))
    }

  })

  #output$num_cluster <- renderPlot({
   # req(values$model$phi)
   # phi <- values$model$phi
    #p1 <- fviz_nbclust(phi, hcut, nstart = 2, method = "wss")+
     # labs(subtitle = "Elbow method")
    #p2 <-  fviz_nbclust(phi, hcut, method = "silhouette")+
     # labs(subtitle = "Silhouette method")
   # p3 <- fviz_nbclust(phi, hcut, nstart = 2, method = "gap_stat", nboot = 50, verbose = FALSE)+
      # labs(subtitle = "Gap statistic method")
   # figure <- ggpubr::ggarrange(p1,p2, ncol = 2, nrow = 2)

   # figure


  #})



 output$plot_cluster <- renderPlot({
 req(values$model$phi)

   phi <- values$model$phi
   label <- values$labels
   top_terms <- textmineR::GetTopTerms(phi = phi, M = input$numM)
   top_term <- apply(top_terms, 2, function(x){
     paste(x, collapse = ", ")
   })

   cluster_dist <- textmineR::CalcHellingerDist(phi)
   hc <- hclust(as.dist(cluster_dist),input$methodaglo)
   hc$labels <- paste(rownames(phi),top_term )

   values$p <-factoextra::fviz_dend(hc,
                          k = input$numberclu,
                          #k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
                          color_labels_by_k = TRUE, # color labels by groups
                          rect = TRUE ,
                          type = input$type)
   values$p
  })

 output$downcluster <- downloadHandler(
   filename =  function() {
     paste("cluster-",Sys.Date(),input$butdowncluster, sep=".")
   },
   # is a function with argument file. content writes the plot to the device
   content = function(file) {
     if(input$butdowncluster == "png")
       png(file) # open the png device
     else
       pdf(file) # open the pdf device
     print(values$p) # draw the plot
     dev.off()  # turn the device off

   }
 )

 output$cophenetic <- renderText({
   req(values$model$phi)
   phi <- values$model$phi
   cluster_dist <- textmineR::CalcHellingerDist(phi)
   hc <- hclust(as.dist(cluster_dist),input$methodaglo)
   cor(x = as.dist(cluster_dist), cophenetic(hc))
 })


  biptr <- reactive({
    biptr = input$biptransf
    biptr
  })


  observeEvent(input$runbiplot,{
  req(values$tidy_theta)
    if(input$load == "load"){
      bip <- values$tidy_theta %>%
        group_by(topic,diario) %>%
        summarise(proportion= mean(gamma))%>%
        tidyr::spread(diario, proportion)
      bip <- data.frame(bip)
      names<-bip$topic}
    else if(input$load == "import"){
      bip <- values$tidy_theta %>%
        group_by(topic,Newspaper) %>%
        summarise(proportion= mean(gamma))%>%
        tidyr::spread(Newspaper, proportion)
      bip <- data.frame(bip)
      names<-bip$topic}

 bip <- dplyr::select(bip,-"topic")
 row.names(bip)<-names
 if (input$selectypebiplot == 'HJ_Biplot') {
   values$Biplot <-  HJBiplot(bip,Transform.Data = biptr())
 }
 else if(input$selectypebiplot == 'JK_Biplot') {
   values$Biplot <-  JKBiplot(bip,Transform.Data = biptr())
 }
 else if(input$selectypebiplot == 'GH_Biplot') {
   values$Biplot <-  GHBiplot(bip,Transform.Data = biptr())
 }

})

  output$eigen <-  DT::renderDT({
    req(values$Biplot)
    dat <- values$Biplot
    dat <- data.frame(dat$eigenvalues)
    DT::datatable(data = dat,
                  colnames=c("value"),
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 #lengthMenu = list(c(5,10,20000,-1),
                                                   c('5', '10', 'All')
                                 )
                  )%>%
      formatRound( columns= c("dat.eigenvalues"),
                   digits=5)

    })

  output$variance <-  DT::renderDT({
    req(values$Biplot)
    dat <- values$Biplot
    dat <- data.frame(dat$explvar)
    DT::datatable(data = dat,
                 colnames=c("Percent %"),
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 #lengthMenu = list(c(5,10,20000,-1),
                                 c('5', '10', 'All')
                  )
    )%>%
      formatRound( columns= c("dat.explvar"),
                   digits= 2)

  })

  output$loading <-  DT::renderDT({
    req(values$Biplot)
    dat <- values$Biplot
    dat <- data.frame(dat$loadings)
    DT::datatable(data = dat%>% mutate_if(is.numeric, round, digits=5),
                  #colnames=c("Percent %"),
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 #lengthMenu = list(c(5,10,20000,-1),
                                 c('5', '10', 'All')
                  )
    )

  })
  output$indcoor <-  DT::renderDT({
    req(values$Biplot)
    dat <- values$Biplot
    dat <- data.frame(dat$coord_ind)
    DT::datatable(data = dat%>% mutate_if(is.numeric, round, digits=5),
                  #colnames=c("Percent %"),
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 #lengthMenu = list(c(5,10,20000,-1),
                                 c('5', '10', 'All')
                  )
    )

  })

  output$varcoor <-  DT::renderDT({
    req(values$Biplot)
    dat <- values$Biplot
    dat <- data.frame(dat$coord_var)
    DT::datatable(data = dat%>% mutate_if(is.numeric, round, digits=5),
                  #colnames=c("Percent %"),
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip',
                                 scrollX = TRUE,
                                 fixedColumns = TRUE,
                                 buttons = c('pageLength',
                                             'copy',
                                             'csv',
                                             'excel',
                                             'pdf',
                                             'print'),
                                 pagelength = 5,
                                 #lengthMenu = list(c(5,10,20000,-1),
                                 c('5', '10', 'All')
                  )
    )

  })

  output$biplot <- renderPlot({
    req(values$Biplot)
    dat <-values$Biplot
    values$plot <- Plot_Biplot (dat, axis = c(input$axisx,input$axisy),
                                hide = input$biphide,
                 labels = "auto", ind.shape = input$pch,
                 ind.color = input$colorind, ind.size = input$sizeind,
                 ind.label = input$indlabel, ind.label.size = input$labelsize,
                 var.color = input$colorvar, var.size = input$varsize,
                 var.label = input$varlabel, var.label.size = input$varlabelsize,
                 var.label.angle = input$angle)
   values$g <- values$plot + themesb[[input$themebi]] +
      theme(
        axis.title.x = element_text(size = input$sizeaxis),
        axis.text.x = element_text(size = input$axistest),
        axis.text.y = element_text(size = input$axistest),
        axis.title.y = element_text(size = input$sizeaxis))
   values$g

  })

  observe({
    if (isTRUE(is.null(values$plot))) {
      shinyjs::disable("down")
      shinyjs::disable("butdown")
    }
    else {shinyjs::enable("down")
      shinyjs::enable("butdown")
    }
  })
  output$down <- downloadHandler(
    filename =  function() {
      paste("Biplot-",Sys.Date(),input$butdown, sep=".")
    },
    # is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$butdown == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      print(values$g) # draw the plot
      dev.off()  # turn the device off

    }
  )


  ###_ DTM xl_ final#####

}) #FIN

