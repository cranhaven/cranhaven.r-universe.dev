require("textmineR")
require("magrittr")
require("highcharter")
require("dplyr")
require("parallel")
require("ldatuning")
require("purrr")
require("topicmodels")
require("stringr")
require("broom")
require("DT")

shinyServer(function(input,output,session) {
  options(shiny.maxRequestSize=50000000*1024^2)
  output$selectfile <- renderUI({
        if(is.null(input$file)) {return()}
        list(hr(),
             helpText("Select the files for which you need
                      to see data and summary stats"),
             selectInput("Select", "Select",
                         choices=input$file$name)
             )
        })

## Summary Stats code ##
# this reactive output contains the summary of the dataset and display the summary in table format
 output$summexample <- renderPrint({
        if(input$example == FALSE){return()}
          dataexample <- system.file("extdata", "scopusJOSS.csv",
                                     package = "LDAShiny")
          data_example <- read.csv(dataexample)
       summary(data_example)
       })


output$summ <- renderPrint({
       if(is.null(input$example)){return()}
       summary(read.table (input$file$datapath[input$file$name==input$Select],
                                   sep=input$sep,
                                   header = input$header,
                                   stringsAsFactors = input$stringAsFactors))
    })

observeEvent(input$example, {
        if(input$example == TRUE){
          shinyjs::disable("choice")
        } else {
          shinyjs::enable("choice")
        }
  })


     ## MainPanel tabset renderUI code ##
    # the following renderUI is used to dynamically g
    # enerate the tabsets when the file is loaded.
    # Until the file is loaded, app will not show the tabset.
    output$tb2 <- renderUI({
        if(input$example == FALSE){return()}
        else
            tabsetPanel(
                tabPanel("Statistical summary example",
                         verbatimTextOutput("summexample")
                         )
                )
      })

output$tb <- renderUI({
  if(is.null(input$file)){return()}
  else tabsetPanel(
       tabPanel("Statistical summary ",
                verbatimTextOutput("summ")
                )
       )
  })

info <- eventReactive(input$choice, {
        # Changes in read.table
        f <- read.table(file=input$file$datapath[input$file$name==input$Select],
                        sep=input$sep,
                        header = input$header,
                        stringsAsFactors = input$stringAsFactors)
        vars <- names(f)
        # Update select input immediately after clicking on the action button.
        updateSelectInput(session,
                          "column1",
                          "Select id document",
                          choices = vars)
        updateSelectInput(session,
                          "column2",
                          "Select document vector",
                          choices = vars)
        updateSelectInput(session,
                          "column3",
                          "Select publish year",
                          choices = vars)
        f
    })

     output$table_display1 <- renderTable({
        f <- info()
        f <- subset(f,
                    select = "input$column1",
                    drop = TRUE) #subsetting takes place here
    })

    output$table_display2 <- renderTable({
        f <- info()
        g <- subset(f,
                    select = "input$column2",
                    drop = TRUE) #subsetting takes place here
         })

observeEvent(input$checkStemming, {
      if(input$checkStemming == FALSE){
        shinyjs::disable("Stemm")}
  else {shinyjs::enable("Stemm")}
  })

observe({
  if (isTRUE(input$example == TRUE )) {
    shinyjs::disable("file")
  }
  else {shinyjs::enable("file")
  }
})

observe({
  if (!is.null(input$file)) {
    shinyjs::disable("example")
  }
  else {shinyjs::enable("example")
  }
})

observe({
  if (is.null(input$file)& input$example == FALSE) {
    shinyjs::disable("dtm.update")
  }
  else {shinyjs::enable("dtm.update")
  }
})


 z <- reactiveValues(odtm=NULL,
                      dtmt = NULL,
                      tf_mat = NULL,
                      dimen = NULL,
                      dtmF =NULL,
                      freq=NULL,
                      wf=NULL,
                      year=NULL,
                      endtime=NULL)


  observeEvent(input$dtm.update, {
    if( input$example == TRUE){
       dataexample <- system.file("extdata",
                                  "scopusJOSS.csv",
                                  package = "LDAShiny")
       data_example <- read.csv(dataexample)
       filtro <-  data.frame(doc_names = data_example$Title,
                             doc_vec = data_example$Abstract,
                             year = data_example$Year)
        print(dataexample)
        }
     else {filtro <- tibble::tibble(read.table(file=input$file$datapath[input$file$name==input$Select],
                                      sep=input$sep,
                                      header = input$header,
                                      stringsAsFactors = input$stringAsFactors))
         filtro <- dplyr::select(filtro,
                                 doc_names=input$column1,
                                 doc_vec=input$column2,
                                 year=input$column3)
         }

      z$year <- filtro$year
      stp <- unlist(strsplit(input$stopwords,","))
      stp <- trimws(stp)

      if(input$example == TRUE){
        cpus <- 2}
      else {
      cpus <- parallel::detectCores()
      }
      ngram <- as.integer(input$ngrams)
      Stemm <- trimws(input$Stemm)
       odtm <- textmineR::CreateDtm(doc_vec = filtro$doc_vec,
                       doc_names = filtro$doc_names,
                       ngram_window = c(1,ngram),
                       lower = FALSE,
                       remove_punctuation = FALSE,
                       remove_numbers = FALSE,
                       #stem_lemma_function = function(x) SnowballC::wordStem(x, Stemm), ## primero se debe decidir si se hace o no stemming y si se hace debe seleccionarse el idioma
                       cpus = cpus)

      if(input$checkStemming)
      {
      dtm <- textmineR::CreateDtm(doc_vec = filtro$doc_vec,
                       doc_names = filtro$doc_names,
                       ngram_window = c(1,ngram),
                       stopword_vec = c(stopwords::stopwords(input$Language),
                                        letters,stp),
                       lower = TRUE,
                       remove_punctuation = TRUE,
                       remove_numbers = input$removenumber,
                       stem_lemma_function = function(x) SnowballC::wordStem(x, Stemm),
                       cpus = cpus)
      } else
      {dtm <- textmineR::CreateDtm(doc_vec = filtro$doc_vec,
                        doc_names = filtro$doc_names,
                        lower = TRUE,
                        stopword_vec = c(stopwords::stopwords(input$Language),letters,stp),# Seleccionar el lenguaje
                        ngram_window = c(1,ngram),
                        remove_punctuation = TRUE,
                        remove_numbers = input$removenumber,
                        #stem_lemma_function = function(x) SnowballC::wordStem(x, Stemm), ## primero se debe decidir si se hace o no stemming y si se hace debe seleccionarse el idioma
                        cpus = cpus)    }

       z$dtm <- quanteda::as.dfm(dtm)
       CONVERT <- quanteda::convert(z$dtm,
                          to = "topicmodels")
       z$dtmt <- removeSparseTerms(CONVERT,
                                   sparse= input$sparce)
       z$dtmF <-  chinese.misc::m3m(z$dtmt,
                     to="dgCMatrix")
       Original <- dim(odtm)
       Without_Sparsity <- dim(z$dtm)
       Final  <- dim (z$dtmt)
       z$dimen <- rbind(Original,
                        Final)
       colnames (z$dimen) <- c ("document", "term")
       z$tf_mat <- textmineR::TermDocFreq(dtm = z$dtmF)
       z$freq <- colSums(as.matrix(z$dtmF))  #
       z$wf <- tibble::tibble(word=names(z$freq), freq=z$freq)
      beepr::beep(2)
    })


   output$Table_dim <- DT::renderDT({
    DT::datatable(data = as.matrix(z$dimen),
                 options = list(pageLength = 5,
                                searching = FALSE,
                                  rownames = TRUE))
   })


   output$data_b <- DT::renderDT({
     DT::datatable(data = z$tf_mat, extensions = 'Buttons',
                   options = list(dom = 'Bfrtip',
                                  buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),
                                  pagelength = 10,
                                  lengthMenu = list(c(10, 25, 100, -1),
                                                    c('10', '25', '100','All')
                                                    )
                                  )
                   )
     })

 output$plot_gg <-  highcharter::renderHighchart({
   export
     z$wf %>% top_n(input$b, freq) %>%
     hchart("column",
            hcaes(x = word, y = freq),
            color = "lightgray",
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

  output$plot_gf<-  highcharter::renderHighchart({
     export
     z$wf %>% top_n(input$c, freq) %>%
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
   else if(is.null(z$dtmF)) {
     shinyjs::disable("Run.model1")
      }
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

   alist <- reactiveValues(coherence_mat=NULL,
                           end_time=NULL)

   observeEvent(input$Run.model1,{
      set.seed(1234)
     ptm <- proc.time()
     stpCohe <- unlist(strsplit(input$OtherKCoherence,","))
     stpCohe <- as.numeric(trimws(stpCohe))
     seqk <- c(seq(from=input$num1,to=input$num2,by=input$num3),stpCohe)# Candidate number of topics k
     iterations <- input$num4     # Parameters control Gibbs sampling
     burnin <- input$num5  # Parameters control Gibbs sampling
     alpha <- input$num6 # Parameters control

     if(input$example == TRUE){
       cores <- 2}
     else {
       cores <- parallel::detectCores()
     }


     dtm <- z$dtmF
     coherence_list <- textmineR::TmParallelApply(X = seqk , FUN = function(k){
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

     alist$coherence_mat <- tibble::tibble(k = sapply(coherence_list, function(x) nrow(x$phi)),
                                 coherence = sapply(coherence_list, function(x) mean(x$coherence)),
                                 stringsAsFactors = FALSE)
     beepr::beep(2)
     alist$end_time <- proc.time() - ptm

})

output$timeCoherence <- renderPrint({
    print( alist$end_time)
   })

output$plot_gi <-  highcharter::renderHighchart({
     export
     alist$coherence_mat %>%
       hchart("line", hcaes(x = k, y = coherence)) %>%
       hc_add_theme(hc_theme_ggplot2())%>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))
   })

   observe({
      if (isTRUE(input$num7>=input$num8||input$num9>input$num8)) {
        shinyjs::disable("Run.model2")
      }

     else if(is.na(as.numeric(input$num7))) {
       shinyjs::disable("Run.model2")
     }
     else if(isTRUE(input$num7 < 2)) {
       shinyjs::disable("Run.model2")
     }
     else if(is.na(as.numeric(input$num8))) {
       shinyjs::disable("Run.model2")
     }
     else if(is.na(as.numeric(input$num9))) {
       shinyjs::disable("Run.model2")
     }
     else if(is.null(z$dtmt)) {
       shinyjs::disable("Run.model2")
      }
       else {
         shinyjs::enable("Run.model2")
      }
   })

   output$OthK4metric <- renderText({
     stpCohe <- unlist(strsplit(input$OtherK4metric,","))
     stpCohe <- as.numeric(trimws(stpCohe))
     if (anyNA(stpCohe)) {
       "Invalid input"
     }
   })

   blist <- reactiveValues(fourmetric_mat = NULL,
                           end_time2 = NULL)

    observeEvent(input$Run.model2, {

      set.seed(1234)
      ptm2 <- proc.time()
      #stp2 = unlist(strsplit(input$metric,","))
     #stp2 = trimws(stp2)
     method <- input$methods
     stpfourm <- unlist(strsplit(input$OtherK4metric,","))
     stpfourm <- as.numeric (trimws(stpfourm))
     seqk <- c(seq(from = input$num7,
                  to = input$num8,
                  by = input$num9),stpfourm)

     if(input$example == TRUE){
       cl <- makeCluster(2,
                                   setup_strategy = "sequential")}
     else {
       cl <- makeCluster(parallel::detectCores(),
                                   setup_strategy = "sequential")
     }

     fourmetric_mat <- ldatuning::FindTopicsNumber(
       z$dtmt,
       topics = seqk, # Select range number of topics
       metrics = c("Griffiths2004",
                   "CaoJuan2009",
                   "Arun2010",
                   "Deveaud2014"),
       method = method,
       control = list(seed = 77),
       mc.cores = cl )
     blist$fourmetric_mat <- g4metric(fourmetric_mat)
      beepr::beep(2)
     blist$end_time2 <- proc.time() - ptm2
     stopCluster(cl)
     })

   output$timefourmetric <- renderPrint({
     print(blist$end_time2)
   })
   output$plot_gj <-  highcharter::renderHighchart({
     export
     blist$fourmetric_mat %>%
       hchart("line", hcaes(x = topics, y = value, group =variable)) %>%
       hc_add_theme(hc_theme_ggplot2())%>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))
      })


   observe({
      if (isTRUE(input$num13>=input$num14 || input$num15>input$num14)) {
        shinyjs::disable("Run.model3")
      }
      else if(isTRUE(input$num17 > input$num16 ||input$num18 > input$num17)) {
        shinyjs::disable("Run.model3")
      }

     else if(isTRUE(input$num13 < 2 )) {
       shinyjs::disable("Run.model3")
     }
     else if(is.na(as.numeric(input$num13))) {
       shinyjs::disable("Run.model3")
     }
     else if(is.na(as.numeric(input$num14))) {
       shinyjs::disable("Run.model3")
     }
     else if(is.na(as.numeric(input$num15))) {
       shinyjs::disable("Run.model3")
     }
     else if(is.na(as.numeric(input$num16))) {
       shinyjs::disable("Run.model3")
     }
     else if(is.na(as.numeric(input$num17))) {
       shinyjs::disable("Run.model3")
     }

     else if(is.na(as.numeric(input$num17))) {
       shinyjs::disable("Run.model3")
     }
      else if(is.null(z$dtmt)) {
        shinyjs::disable("Run.model3")
      }
      else {
        shinyjs::enable("Run.model3")
      }
   })

   output$OthKLL <- renderText({
     stpCohe <- unlist(strsplit(input$OtherKLL,","))
     stpCohe <- as.numeric(trimws(stpCohe))
     if (anyNA(stpCohe)) {
       "Invalid input"
     }
   })

   clist <- reactiveValues(best.model = NULL,
                           end_time3 = NULL)

   observeEvent(input$Run.model3,{

       set.seed(12345)
     ptm3 <- proc.time()
     stpLL <- unlist(strsplit(input$OtherKLL,","))
    stpLL <- as.numeric (trimws(stpLL))
     seqk <- c(seq(from = input$num13,
                  to = input$num14,
                  by = input$num15),stpLL)
     iter <- input$num16
     burnin <- input$num17
     thin <- input$num18
    # best.model <- lapply(seqk, function(k){LDA(z$dtmt, k, method = "Gibbs",iter =iter,burnin=burnin,thin=thin)})
      #best.model<- tibble(as.matrix(lapply(best.model, logLik)))
     #clist$best.model <- tibble(topics=seqk, logL=as.numeric(as.matrix(best.model)))

     perplex <- seqk %>%
       purrr::map(topicmodels::LDA, x =z$dtmt ,
                   newdata = z$dtmt ,
                   estimate_theta=FALSE,
                   iter =iter,
                   burnin=burnin,
                   thin= thin)
     clist$best.model <- tibble::tibble(Topics = seqk,
                     Perplexity = map_dbl(perplex , perplexity))
     beepr::beep(2)
     clist$end_time3 <- proc.time() - ptm3
       })

   output$timeloglike <- renderPrint({
     print(clist$end_time3)
   })

    output$plot_gk <-  highcharter::renderHighchart({
     export
     Perplex <- clist$best.model
     Perplex %>%
       hchart("line", hcaes(x=Topics, y = Perplexity)) %>%
       hc_add_theme(hc_theme_ggplot2())%>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))
   })

 #############################
observe({
  if (isTRUE(input$num19>=input$num20 || input$num21>input$num20)) {
    shinyjs::disable("Run.model4")
       }
  else if(isTRUE(input$num23>input$num22 || input$num24>=input$num23)) {
    shinyjs::disable("Run.model4")
  }

  else if(isTRUE(input$num19 < 2 )) {
    shinyjs::disable("Run.model4")
  }
  else if(is.na(as.numeric(input$num19))) {
    shinyjs::disable("Run.model4")
  }
  else if(is.na(as.numeric(input$num20))) {
    shinyjs::disable("Run.model4")
  }
  else if(is.na(as.numeric(input$num21))) {
    shinyjs::disable("Run.model4")
  }
  else if(is.na(as.numeric(input$num22))) {
    shinyjs::disable("Run.model4")
  }
  else if(is.na(as.numeric(input$num23))) {
    shinyjs::disable("Run.model4")
  }
  else if(is.na(as.numeric(input$num24))) {
    shinyjs::disable("Run.model4")
  }
  else if(is.null(z$dtmt)) {
    shinyjs::disable("Run.model4")
       }
  else {
    shinyjs::enable("Run.model4")
       }
    })

    output$Okhm <- renderText({
      stpCohe <- unlist(strsplit(input$OtherKHM,","))
      stpCohe <- as.numeric(trimws(stpCohe))
      if (anyNA(stpCohe)) {
        "Invalid input"
      }
    })

dlist <- reactiveValues(hm_many = NULL)
   observeEvent(input$Run.model4,{
set.seed(12345)
     ptm4 <- proc.time()
     stpHM <- unlist(strsplit(input$OtherKHM,","))
     stpHM <- as.numeric (trimws(stpHM))
     seqk <- c(seq(from = input$num19,
                  to = input$num20,
                  by = input$num21),stpHM)
     iter <- input$num22
     burnin <- input$num23
     keep <- input$num24
     fitted_many <- lapply(seqk,
                           function(k)LDA(z$dtmt,
                                                              k = k,
                                                              method = "Gibbs",
                                                              control = list(burnin = burnin,
                                                                             iter = iter,
                                                                             keep = keep)
                                                              ))
     # extract logliks from each topic
     logLiks_many <- lapply(fitted_many, function(L)L@logLiks[-c(1:(burnin/keep))])
     # compute harmonic means
     hm_many <- tibble::tibble(as.matrix (sapply(logLiks_many,
                                                 function(h) harmonicMean(h)
                                                 )
                                          )
                               )
     # inspect
     dlist$hm_many <- tibble::tibble(topics=seqk,
                                     logL=as.numeric(as.matrix(hm_many)
                                                     )
                                     )
     beepr::beep(2)
     dlist$end_time4 <- proc.time() - ptm4
  })

   output$timeHmean<- renderPrint({
    print(dlist$end_time4)
   })

   output$plot_gl <-  highcharter::renderHighchart({
     export
     dlist$hm_many %>%
       hchart("line", hcaes(x=topics, y=logL)) %>%
       hc_add_theme(hc_theme_ggplot2())%>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))
   })

#############################
   observe({
      if (isTRUE(input$num27 >= input$num26|| input$num25 < 2)) {
        shinyjs::disable("Run.model5")
      }
      else if(is.null(z$dtmF)) {
        shinyjs::disable("Run.model5")
      }

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
   elist <- reactiveValues(summary= NULL,
                           tidy_thetha=NULL,
                           tidy_beta = NULL,
                           dfCoef=NULL,
                           model=NULL)

   observeEvent(input$Run.model5,{
     set.seed(12345)
      k <- input$num25
     iter <- input$num26
     burnin <- input$num27
     alpha <- input$num28

    if(input$example == TRUE){
      cpus <- 2L}
   else {
       cpus <- parallel::detectCores()
  }
     elist$model <- textmineR::FitLdaModel(z$dtmF, # parameter
                          k = k ,# Number of topics k
                          iterations = iter, # parameter
                          burnin = burnin, #parameter
                          alpha = alpha,# parameter
                          beta = colSums(z$dtmF)/sum(z$dtmF)*100,
                          optimize_alpha = TRUE, # parameter
                          calc_likelihood = TRUE,
                          calc_coherence = TRUE,
                          calc_r2 = FALSE,
                          cpus = cpus)

 top_terms <- GetTopTerms(phi = elist$model$phi,
                                     M = 10)
 prevalence <- colSums(elist$model$theta) / sum(elist$model$theta) * 100
  #textmineR has a naive topic labeling tool based on probable bigrams
  labels <- LabelTopics(assignments = elist$model$theta > 0.05,
                             dtm = z$dtmF,
                              M = input$Labels)

  elist$summary <- data.frame(topic = rownames(elist$model$phi),
                             label = labels,
                              coherence = round(elist$model$coherence, 3),
                              prevalence = round(prevalence,3),
                             top_terms = apply(top_terms, 2, function(x){
                               paste(x, collapse = ", ")
                              }),
                             stringsAsFactors = FALSE)

  elist$tidy_thetha <- data.frame(document = rownames(elist$model$theta),
                          round(elist$model$theta,5),
                          stringsAsFactors = FALSE) %>%
    tidyr::gather(topic, gamma, -document)
    elist$tidy_beta <- data.frame(topic = as.integer(str_replace_all(rownames(elist$model$phi),
                                                                              "t_", "")
                                                     ),
                                  round(elist$model$phi,5),
                                  stringsAsFactors = FALSE)%>%
      tidyr::gather(term, beta, -topic)

  elist$thetayear <- data.frame(elist$tidy_thetha,
                                year = rep(z$year))%>%
    group_by(topic,year) %>%
    summarise(proportion= mean(gamma))

  elist$dfreg  <- elist$thetayear %>% group_by(topic) %>%
   do(fitreg = lm(proportion ~ year, data = .))
  elist$thetayear <- data.frame(elist$thetayear)

  elist$dfCoef <- elist$thetayear %>%
   nest_by(topic) %>%
     #change do() to mutate(), then add list() before your model
     # make sure to change data = .  to data = data
    mutate(fitmodelreg = list(lm(proportion ~ year, data = data))) %>%
    summarise(tidy(fitmodelreg))

 classifications <- elist$tidy_thetha %>%
  dplyr::group_by(topic, document) %>%
   dplyr::top_n(1, gamma) %>%
   ungroup()
 beepr::beep(2)


    })

   output$sum <- DT::renderDT({
     DT::datatable(data = elist$summary, extensions = 'Buttons',
                   options = list(dom = 'Bfrtip',
                                  buttons = c('pageLength',
                                              'copy',
                                              'csv',
                                              'excel',
                                              'pdf',
                                              'print'),
                                  pagelength = 10,
                                  lengthMenu = list(c(10, 25, 100, -1),
                                                    c('10', '25', '100','All')
                                                    )
                                  )
                   )
     })

output$summLDA <- DT::renderDT({
    model <- elist$model
     top_terms <- textmineR::GetTopTerms(phi = model$phi, M = input$Topterm)
     prevalence <- colSums(model$theta) / sum(model$theta) * 100
     # textmineR has a naive topic labeling tool based on probable bigrams
     labels <- LabelTopics(assignments = model$theta > input$assignments,
                           dtm = z$dtmF,
                           M = input$Labels)
     summary <- data.frame(topic = rownames(model$phi),
                                 label = labels,
                                 coherence = round(model$coherence, 3),
                                 prevalence = round(prevalence,3),
                                 top_terms = apply(top_terms, 2, function(x){
                                   paste(x, collapse = ", ")
                                 }),
                                 stringsAsFactors = FALSE)


     DT::datatable(data = summary, extensions = 'Buttons',
                   options = list(dom = 'Bfrtip',
                                  buttons = c('pageLength',
                                              'copy',
                                              'csv',
                                              'excel',
                                              'pdf',
                                              'print'),
                                  pagelength = 10,
                                  lengthMenu = list(c(10, 25, 100, -1),
                                                    c('10', '25', '100','All'))))%>%
        formatRound( columns= c("coherence","prevalence"),
                         digits=5)
})


output$theta <- DT::renderDT({
      DT::datatable(data = elist$tidy_thetha ,
                    extensions = 'Buttons',
                    filter = 'top',
                    colnames=c("document","topic", "theta"),
                    options = list(dom = 'Bfrtip',
                                   buttons = c('pageLength',
                                               'copy',
                                               'csv',
                                               'excel',
                                               'pdf',
                                               'print'),
                                   pagelength = 10,
                                  lengthMenu = list(c(10,100,20000,-1),
                                                    c('10', '25', '10000','All')
                                                    )
                                  )
                    )%>% formatRound( columns= c("gamma"),digits=5)

   })

output$downloadData <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write.csv(elist$tidy_thetha, con)
    }
   )




   output$phi <- DT::renderDT({
     DT::datatable(data =elist$tidy_beta, extensions = 'Buttons',filter = 'top',
                   colnames=c("topic", "term", "phi"),
                   options = list(dom = 'Bfrtip',
                                  buttons = c('pageLength',
                                              'copy', 'csv',
                                              'excel', 'pdf',
                                              'print'),
                                  pagelength = 10,
                                  lengthMenu = list(c(10, 25, 100, -1),
                                                    c('10', '25', '100','All'))))%>%
      formatRound( columns= c("beta"),digits=5)

   })

output$Alloca <- DT::renderDT({
      classifications <- elist$tidy_thetha %>%
       dplyr::group_by(topic) %>%
       dplyr::top_n(input$topnumber, gamma) %>%
       ungroup()
     DT::datatable(data = classifications,
                   extensions = 'Buttons',
                   filter = 'top',
                   colnames=c("document","topic", "theta"),
                   options = list(dom = 'Bfrtip',
                                  buttons = c('pageLength',
                                              'copy',
                                              'csv',
                                              'excel',
                                              'pdf',
                                              'print'),
                                  pagelength = 10,
                                  lengthMenu = list(c(10, 25, 100, -1),
                                                    c('10', '25', '100','All'))))%>%
        formatRound( columns= c("gamma"),digits=5)
   })

 output$reg <- DT::renderDT({
     datareg <- elist$dfCoef
     DT::datatable(data = datareg,
                   extensions = 'Buttons',
                   options = list(dom = 'Bfrtip',
                                  buttons = c('pageLength',
                                              'copy',
                                              'csv',
                                              'excel',
                                              'pdf',
                                              'print'),
                                  pagelength = 10,
                                  lengthMenu = list(c(10, 25, 100, -1),
                                                    c('10',
                                                      '25', '100','All'))))%>%
         formatRound( columns= c("estimate",
                                     "std.error",
                                     "statistic",
                                     "p.value"),digits=5)
       })

   output$plot_trend <-  highcharter::renderHighchart({
     export
     elist$thetayear %>%
       hchart("line", hcaes(x = year,
                            y = proportion,
                            group = as.integer(str_replace_all(topic,"t_", " ")))) %>%
       hc_add_theme(hc_theme_ggplot2())%>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))
   })

   output$plot_worcloud <-  highcharter::renderHighchart({
     export
     elist$tidy_beta %>% dplyr::filter(topic==input$num29)%>%
       dplyr::top_n(input$cloud, beta) %>%
       hchart( "wordcloud", hcaes(name = term,
                                  weight = beta)) %>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))

   })

   output$plot_heatmap <-  highcharter::renderHighchart({
      colr <- list( list(0, '#2E86C1'),
                       list(1, '#FF5733'))
     export
     elist$thetayear %>%
       hchart("heatmap", hcaes(x = year,
                               y = as.integer(str_replace_all(topic,"t_", " ")) ,
                               value =proportion)) %>%
       hc_colorAxis(  stops= colr,
                      min=min(elist$thetayear$proportion),
                      max= max(elist$thetayear$proportion)) %>%
       hc_yAxis(title = list(text = "Topic"))%>%
       hc_exporting(
         enabled = TRUE,
         formAttributes = list(target = "_blank"),
         buttons = list(contextButton = list(
           text = "Export",
           theme = list(fill = "transparent"),
           menuItems = export)))
   })

#####################################end number topic
  #observe({
   #  if (input$Stop > 0) stopApp()  # stop shiny
   #})
   })

