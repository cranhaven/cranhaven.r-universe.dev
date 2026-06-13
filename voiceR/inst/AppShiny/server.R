server <- function(input, output) {
  volumes <- shinyFiles::getVolumes()()
  shinyFiles::shinyDirChoose(
    input,
    'dir',
    roots = c(Home = volumes),
    filetypes = c('', 'wav', 'mp3')
  )
  shinyjs::hide("AnalysisData")
  shinyjs::hide("downloadReport")
  shinyjs::hide("ResultTable")
  shinyjs::hide("splitN1")
  shinyjs::hide("splitN2")
  shinyjs::hide("splitN3")
  shinyjs::hide("splitN4")
  shinyjs::hide("split1")
  shinyjs::hide("split2")
  shinyjs::hide("split3")
  shinyjs::hide("split4")

  global <- shiny::reactiveValues(datapath = getwd())
  updatedText <- ""

  dir <- shiny::reactive(input$dir)

  output$dir <- shiny::renderText({
    global$datapath
  })

  shiny::observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath(volumes, winslash = "/")
                 home <- stringr::str_replace(home, "/", "\\")
                 paths <- file.path(home, paste(unlist(dir()$path[-1]), collapse = "/"))
                 paths <- paths[sapply(paths, dir.exists)]
                 global$datapath <-
                   paths
               })

  patternShiny <- shiny::reactive(input$patternRaw)
  files <- 0
  shiny::observeEvent(input$doAnalysis,
               {
                 shinyjs::html(id = "titleMain", html = "", add = FALSE)
                 shinyjs::html(id = "MainList", html = "", add = FALSE)
                 shinyjs::hide("downloadReport")
                 shinyjs::hide("AnalysisData")
                 shinyjs::hide("ErrorTable")
                 shinyjs::hide("ResultTable")
                 shinyjs::hide("splitN1")
                 shinyjs::hide("splitN2")
                 shinyjs::hide("splitN3")
                 shinyjs::hide("splitN4")
                 shinyjs::hide("split1")
                 shinyjs::hide("split2")
                 shinyjs::hide("split3")
                 shinyjs::hide("split4")
                 pattern <- trimws(strsplit(patternShiny(), ",")[[1]])
                 if(length(pattern) == 0){
                   pattern <- c()
                 } else {
                   pattern = paste(pattern,collapse="|")
                 }

                 files <- list.files(path = global$datapath, pattern = pattern, recursive = input$recursive)
                 fileType <- tolower(input$fileType)
                 files <- files[grep(paste0("\\.", fileType), files)]

                 if(length(files) > 0){

                   print(files)
                   processedNames <-
                      basename(stringr::str_remove_all(files, pattern = paste0("\\.", fileType)))

                 }


                 checkPattern <- stringr::str_split(input$fileNamePattern, input$sep, simplify = TRUE)
                 checkPattern <- tolower(checkPattern)
                 patternCorrectness <- checkPattern %in% c("id", "condition", "dimension", "null")



                 if(length(checkPattern[!patternCorrectness]) >= 1){
                   output$NumberFiles <- renderText(paste("Incorrect Components:", checkPattern[!patternCorrectness]))
                 }
                 else if(length(files) == 0){
                   output$NumberFiles <- renderText(paste("No Audio Files Found in", global$datapath))
                 }
                 else if(!"dimension" %in% checkPattern && input$includeDimensions){
                   output$NumberFiles <- renderText(paste("Attention: Include Dimensions defined but no Dimension in the pattern"))
                 }
                 else if(input$normalizeData && length(processedNames)){
                   output$NumberFiles <- renderText(paste("Attention: Not enough data to use the normalization feature."))
                 }
                 else{
                   componentsAndPositions <- getComponents(processedNames, input$fileNamePattern, input$sep)
                   ids <- unique(componentsAndPositions[["Components"]][,componentsAndPositions[["Positions"]][["ID"]]])

                   if(componentsAndPositions[["Positions"]][["Condition"]] == 99){
                     conditions <- ""
                     conditionPresence <- FALSE
                   }else{
                     conditions <- unique(componentsAndPositions[["Components"]][,"Condition"])
                     conditionPresence <- TRUE
                   }

                   if(componentsAndPositions[["Positions"]][["Dimension"]] == 99){
                     dimensions <- ""
                     dimensionPresence <- FALSE
                   }else{
                     dimensions <- unique(componentsAndPositions[["Components"]][,"Dimension"])
                     dimensionPresence <- TRUE
                   }

                   Description <- paste0(as.character(length(files)), " audio files have been analyzed\n",
                                         length(ids), " unique IDs, ", ifelse(length(conditions) == 1, ifelse(conditions == "", 0, length(conditions)), length(conditions)), " unique conditions and ",
                                         ifelse(length(dimensions) == 1, ifelse(dimensions == "", 0, length(dimensions)), length(dimensions)), " unique dimensions.")

                   output$NumberFiles <- renderText(Description)




                   shinyjs::html(id = "titleMain", html = "<h1> 1 - Reading Audio Files </h1>", add = FALSE)

                   audioList <- vector("list",length(files))
                   shiny::withProgress(message = 'Step 1: Reading Files', value = 0, {
                     i <- 1
                     for (file in files) {
                       if(input$fileType == "WAV"){

                           audioList[[i]] <- tuneR::readWave(paste(global$datapath, "/", file, sep = ""))


                       }
                       else{

                           audioList[[i]] <- tuneR::readMP3(paste(global$datapath, "/", file, sep = ""))


                       }
                       names(audioList)[i] <- basename(stringr::str_remove_all(file, pattern = paste0("\\.", fileType)))
                       i <- i + 1

                       textToShow <- paste0("<div class = 'soundFile'> <span class= 'audioIcon'><img src='https://img.icons8.com/cute-clipart/64/000000/audio-file.png'/></span><span class= 'audioName'>", basename(stringr::str_remove_all(file, pattern = paste0("\\.", fileType))), "</span> </div>" )
                       shinyjs::html(id = "MainList", html = textToShow, add = TRUE)
                       shiny::incProgress(1/length(files), detail = paste(i, "files read"))
                     }


                   })

                   Sys.sleep(2.5)
                   shinyjs::html(id = "titleMain", html = "", add = FALSE)
                   shinyjs::html(id = "MainList", html = "", add = FALSE)

                   if(input$preprocess){
                     shinyjs::html(id = "titleMain", html = "<h1> Preprocessing Audio Files </h1>", add = TRUE)


                     shiny::withProgress(message = 'Preprocessing Files',
                                         value = 0, {
                                           preprocess(audioList, verbose = FALSE, progress=TRUE)
                                         })


                   }

                   shinyjs::html(id = "titleMain", html = "", add = FALSE)
                   shinyjs::html(id = "MainList", html = "", add = FALSE)
                   Sys.sleep(2.5)

                   measures <- c("duration", "voice_breaks_percent", "RMS_env", "mean_loudness", "mean_F0", "sd_F0", "mean_entropy", "mean_HNR")
                   audioData <- as.data.frame(matrix(nrow = length(files), ncol = length(measures)))
                   errors <- vector(length = length(files))
                   errorNumber <- 0
                   row.names(audioData) <- basename(stringr::str_remove_all(files, pattern = paste0("\\.", fileType)))


                   colnames(audioData) <- measures
                   components <- getComponents(rownames(audioData), input$fileNamePattern, input$sep)

                   if(dimensionPresence){
                     audioData <- cbind("Dimension" = components[["Components"]][,"Dimension"], audioData)
                   }
                   if(conditionPresence){
                     audioData <- cbind("Condition" = components[["Components"]][,"Condition"], audioData)
                   }


                   audioData <- cbind("ID" = components[["Components"]][,"ID"], audioData)


                   shinyjs::html(id = "titleMain", html = "<h1> 2 - Analyzing Audio Files </h1>", add = TRUE)



                   shiny::withProgress(message = 'Step 2: Analyzing Files', value = 0, {

                     cl <- parallel::makeCluster(parallel::detectCores() - 2)
                     doParallel::registerDoParallel(cl)

                     for(i in 1:length(audioList)) {
                       tryCatch({
                         errorTest <- NULL
                         audioName <- names(audioList)[i]


                         componentsAndPositions <- getComponents(audioName, input$fileNamePattern, input$sep)
                         id <- componentsAndPositions[["Components"]][,componentsAndPositions[["Positions"]][["ID"]]]
                         sound_orig = as.numeric(scale(audioList[[audioName]]@left))
                         samplingRate = audioList[[audioName]]@samp.rate

                         if(conditionPresence & dimensionPresence & input$includeDimensions){
                           dimension <- componentsAndPositions[["Components"]][,"Dimension"]
                           Condition <- componentsAndPositions[["Components"]][,"Condition"]

                         }
                         else if(conditionPresence){
                           Condition <- componentsAndPositions[["Components"]][,"Condition"]

                         }
                         else if(dimensionPresence){
                           dimension <- componentsAndPositions[["Components"]][,"Dimension"]

                         }

                         rowsFilter <- which(rownames(audioData) %in% names(audioList)[i])


                         if(input$preprocess){
                           analyzeData <- soundgen::analyze(audioList[[audioName]]@left, samplingRate = audioList[[audioName]]@samp.rate, plot = FALSE, osc = FALSE, summaryFun = c("mean", "sd"))
                           analyzeData <- analyzeData$summary
                         }
                         else{
                           analyzeData <- soundgen::analyze(audioList[[audioName]]@left, samplingRate = audioList[[audioName]]@samp.rate, plot = FALSE, osc = FALSE, summaryFun = c("mean", "sd"))
                           analyzeData <- analyzeData$summary
                         }




                         audioData[rowsFilter, measures[1]] <- seewave::duration(audioList[[audioName]])
                         audioData[rowsFilter, measures[2]] <- 1 - analyzeData$voiced
                         audioData[rowsFilter, measures[3]] <- seewave::rms(seewave::env(seewave::zapsilw(audioList[[audioName]], plot = FALSE),f=audioList[[audioName]]@samp.rate, plot = FALSE))
                         audioData[rowsFilter, measures[4]] <- analyzeData$loudness_mean
                         audioData[rowsFilter, measures[5]] <- analyzeData$pitch_mean
                         audioData[rowsFilter, measures[6]] <- analyzeData$pitch_sd
                         audioData[rowsFilter, measures[7]] <- analyzeData$entropy_mean
                         audioData[rowsFilter, measures[8]] <- analyzeData$HNR_mean
                         errorTest <- audioData[rowsFilter, measures[1]]

                         textToShow <- paste0("<div class = 'soundFile'> <span class= 'audioIcon'><img src='https://img.icons8.com/cute-clipart/64/000000/audio-file.png'/></span><span class= 'audioName'>", audioName, "</span> <span style='margin-right: 30px; float: right;'><img src='https://img.icons8.com/fluent/48/000000/checked.png'/></div>" )

                         shinyjs::html(id = "MainList", html = textToShow, add = TRUE)
                         shiny::incProgress(1/length(files), detail = paste(i, "files analyzed"))
                       }, error = function(e) {
                         print(e)
                         textToShow <- paste0("<div class = 'soundFile'> <span class= 'audioIcon'><img src='https://img.icons8.com/cute-clipart/64/000000/audio-file.png'/></span><span class= 'audioName'>", audioName, "</span> <span style='margin-right: 30px; float: right;'><img src='https://img.icons8.com/emoji/48/000000/cross-mark-emoji.png'/></div>" )
                         shinyjs::html(id = "MainList", html = textToShow, add = TRUE)
                       }, finally ={if(is.null(errorTest)){
                         errorNumber <- errorNumber + 1
                         errors[errorNumber] <- audioName
                       }})
                     }
                     parallel::stopCluster(cl)
                     if(input$normalizeData){
                       audioData <- normalizeData(audioData = audioData, includeDimensions = input$includeDimensions, includeConditions = conditionPresence)
                       avoidNormalCheck <- audioData$avoidNormalCheck
                       audioData <- audioData$audioData
                     }
                     else{
                       avoidNormalCheck <- rep(FALSE, length(measures))
                     }
                     comparisons <- tryCatch({

                       if((conditionPresence & !dimensionPresence) | (conditionPresence & !input$includeDimensions)){
                         print("Doing Comparisons - Condition")

                         comparisons <- comparisonPlots(audioData, "Condition", avoidNormalCheck = input$normalizeData)
                       }
                       else if(conditionPresence & dimensionPresence & input$includeDimensions){
                         print("Doing Comparisons - Condition + Dimension")
                         comparisons <- comparisonPlots(audioData, c("Condition", "Dimension"), avoidNormalCheck = input$normalizeData)
                       }
                       else if(!conditionPresence & dimensionPresence){
                         print("Doing Comparisons - Dimension")
                         comparisons <- comparisonPlots(audioData, c("Dimension"), avoidNormalCheck = input$normalizeData)
                       }
                       else{
                         print("No comparisons")
                         comparisons <- comparisonPlots(audioData, avoidNormalCheck = input$normalizeData)
                       }

                    },
                    error=function(e) {
                         comparisons <- NULL

                    })

                     if(is.null(comparisons)){
                       output$NumberFiles <- renderText("Not enough data for comparisons")
                     }
                     else{
                       if(nrow(audioData) > 3){
                         normalPlots <- normalityPlots(audioData)

                       }
                       else{
                         normalPlots <- NULL
                       }
                       rownames(audioData) <- c()



                       Sys.sleep(2.5)
                       shinyjs::html(id = "titleMain", html = "", add = FALSE)
                       shinyjs::html(id = "MainList", html = "", add = FALSE)

                       shinyjs::show("AnalysisData", anim = TRUE, time = 1)
                       shinyjs::show("ResultTable", anim = TRUE, time = 1)
                       shinyjs::show("ErrorTable", anim = TRUE, time = 1)


                       output$ResultTable = DT::renderDataTable({
                         audioData
                       },
                       server = FALSE,
                       extensions = 'Buttons', options = list(
                         scrollX = TRUE,
                         dom = 'Bfrtip',
                         buttons = list('copy', 'print', list(
                           extend = 'collection',
                           buttons = list(
                             list(extend = 'csv', filename = "AudioData"),
                             list(extend = 'excel', filename = "AudioData"),
                             list(extend = 'pdf', filename = "AudioData")),
                           text = 'Download')
                         )

                       ))

                       errors <- data.frame(File = errors[errors != "FALSE"], Status = rep("Error", length(errors[errors != "FALSE"])))

                       output$ErrorTable = DT::renderDataTable({
                         errors
                       },
                       server = FALSE,
                       extensions = 'Buttons', options = list(
                         scrollX = TRUE,
                         dom = 'Bfrtip',
                         buttons = list('copy', 'print', list(
                           extend = 'collection',
                           buttons = list(
                             list(extend = 'csv', filename = "AudioData"),
                             list(extend = 'excel', filename = "AudioData"),
                             list(extend = 'pdf', filename = "AudioData")),
                           text = 'Download')
                         )

                       ))

                       shinyjs::show("splitN1", anim = TRUE, time = 1)
                       shinyjs::show("splitN2", anim = TRUE, time = 1)
                       shinyjs::show("splitN3", anim = TRUE, time = 1)
                       shinyjs::show("splitN4", anim = TRUE, time = 1)
                       shinyjs::show("split1", anim = TRUE, time = 1)
                       shinyjs::show("split2", anim = TRUE, time = 1)
                       shinyjs::show("split3", anim = TRUE, time = 1)
                       shinyjs::show("split4", anim = TRUE, time = 1)
                       if(!is.null(normalPlots$duration))
                       output$normalPlot1 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$duration + ggplot2::labs(title="Density Plot of Audio Duration \n by Condition")+ ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$voice_breaks_percent))
                       output$normalPlot2 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$voice_breaks_percent + ggplot2::labs(title="Density Plot of Voice Breaks Percentage \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$RMS_env))
                       output$normalPlot3 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$RMS_env + ggplot2::labs(title="Density Plot of RMS of the amplitude envelope \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$mean_loudness))
                       output$normalPlot4 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$mean_loudness + ggplot2::labs(title="Density Plot of Loudness \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$mean_F0))
                       output$normalPlot5 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$mean_F0 + ggplot2::labs(title="Density Plot of Average F0 \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$sd_F0))
                       output$normalPlot6 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$sd_F0 + ggplot2::labs(title="Density Plot of F0 Standard Deviation \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$mean_entropy))
                       output$normalPlot7 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$mean_entropy + ggplot2::labs(title="Density Plot of Average Entropy \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!is.null(normalPlots$mean_HNR))
                       output$normalPlot8 <- plotly::renderPlotly(plotly::ggplotly(normalPlots$mean_HNR + ggplot2::labs(title="Density Plot of Average Harmonics-To-Noise Ratio \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       if(!input$includeDimensions){
                         output$plot1 <- plotly::renderPlotly(plotly::ggplotly(comparisons$duration + ggplot2::ylim(min( audioData[, measures[1]]), max(audioData[, measures[1]])*1.1) + ggplot2::labs(title="Plot of Audio Duration \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot2 <- plotly::renderPlotly(plotly::ggplotly(comparisons$voice_breaks_percent + ggplot2::ylim(min( audioData[, measures[2]]), max(audioData[, measures[2]])*1.1) + ggplot2::labs(title="Plot of Voice Breaks Percentage \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot3 <- plotly::renderPlotly(plotly::ggplotly(comparisons$RMS_env + ggplot2::ylim(min( audioData[, measures[3]]), max(audioData[, measures[3]])*1.1) + ggplot2::labs(title="Plot of RMS of the amplitude envelope \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot4 <- plotly::renderPlotly(plotly::ggplotly(comparisons$mean_loudness + ggplot2::ylim(min( audioData[, measures[4]]), max(audioData[, measures[4]])*1.1) + ggplot2::labs(title="Plot of Loudness \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot5 <- plotly::renderPlotly(plotly::ggplotly(comparisons$mean_F0 + ggplot2::ylim(min( audioData[, measures[5]]), max(audioData[, measures[5]])*1.1) + ggplot2::labs(title="Plot of Average F0 \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot6 <- plotly::renderPlotly(plotly::ggplotly(comparisons$sd_F0 + ggplot2::ylim(min( audioData[, measures[6]]), max(audioData[, measures[6]])*1.1) + ggplot2::labs(title="Plot of F0 Standard Deviation \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot7 <- plotly::renderPlotly(plotly::ggplotly(comparisons$mean_entropy + ggplot2::ylim(min( audioData[, measures[7]]), max(audioData[, measures[7]])*1.1) + ggplot2::labs(title="Plot of Average Entropy \n by Condition") + ggthemes::theme_fivethirtyeight()))
                         output$plot8 <- plotly::renderPlotly(plotly::ggplotly(comparisons$mean_HNR + ggplot2::ylim(min( audioData[, measures[8]]), max(audioData[, measures[8]])*1.1) + ggplot2::labs(title="Plot of Average Harmonics-To-Noise Ratio \n by Condition") + ggthemes::theme_fivethirtyeight()))
                       }
                       else{

                         convertDimensionsAndConditionsPlot <- function(plot, measure, nameMeasure){
                           ggplotData <- ggplot2::ggplot_build(plot)
                           grobData <- capture.output(ggplotData$plot$layers[[2]])
                           grobData <- paste0(grobData, collapse = " ")
                           grobData <- stringr::str_split(grobData, ",")
                           grobData <- grobData[[1]]
                           grobData <- grobData[stringr::str_detect(grobData, "label = .*")]
                           grobData <- stringr::str_extract(grobData, '\".*\"')
                           grobData <- stringr::str_remove_all(grobData, '\"')
                           grobDataFrame <- data.frame(C1 = grobData[3:5] ,C2 = grobData[6:8])
                           colnames(grobDataFrame) <- grobData[1:2]



                           fig <- plotly::plot_ly(
                             type = 'table',

                             domain = list(x=c(0.3,0.7)),
                             header = list(
                               values = c( names(grobDataFrame)),
                               align = c("center", "center"),
                               line = list(width = 1, color = 'black'),
                               font = list(family = "Arial", size = 14, color = "black")

                             ),
                             cells = list(
                               values = rbind(

                                 t(as.matrix(unname(grobDataFrame)))
                               ),
                               align = c("center", "center"),

                               font = list(family = "Arial", size = 12, color = c("black"))
                             ))

                           fig2 <- plotly::layout(plotly::ggplotly(plot + ggplot2::ylim(min( audioData[, measure]), max(audioData[, measure])*1.1) + ggplot2::labs(title=paste0("Plot of Audio ", paste(toupper(substr(nameMeasure, 1, 1)), substr(nameMeasure, 2, nchar(measure)), sep=""), "\n by Condition")) + ggthemes::theme_fivethirtyeight()), boxmode = "group")


                           final <- plotly::subplot(fig, fig2, nrows = 2, which_layout = 2)

                           return(plotly::layout(final, yaxis = list(domain = c(0.8, 1)),
                                                 yaxis2 = list(domain = c(0, 0.8)), legend = list(x = 1.01, y = 0.41)))


                         }




                         if(!is.null(comparisons$duration))
                           output$plot1 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$duration, measures[[1]], "duration"))
                         if(!is.null(comparisons$voice_breaks_percent))
                           output$plot2 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$voice_breaks_percent, measures[[2]], "Voice Breaks Percentage"))
                         if(!is.null(comparisons$RMS_env))
                           output$plot3 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$RMS_env, measures[[3]], "RMS of the amplitude envelope"))
                         if(!is.null(comparisons$mean_loudness))
                           output$plot4 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$mean_loudness, measures[[4]], "Loudness"))
                         if(!is.null(comparisons$mean_F0))
                           output$plot5 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$mean_F0, measures[[5]], "Average F0"))
                         if(!is.null(comparisons$sd_F0))
                           output$plot6 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$sd_F0, measures[[6]], "F0 standard Deviation"))
                         if(!is.null(comparisons$mean_entropy))
                           output$plot7 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$mean_entropy, measures[[7]], "Average entropy"))
                         if(!is.null(comparisons$mean_HNR))
                           output$plot8 <- plotly::renderPlotly(convertDimensionsAndConditionsPlot(comparisons$mean_HNR, measures[[8]], "Average Harmonics-To-Noise Ratio"))
                       }

                       comparisons2 <- list()
                       normalPlots2 <- list()

                       for (i in 1:length(measures)) {
                         measure <- measures[i]
                         comparisons2[[i]] <- comparisons[[measure]]
                         normalPlots2[[i]] <- normalPlots[[measure]]

                       }



                       shinyjs::show("downloadReport")
                       params <<- list(audioData = audioData, comparisons = comparisons2, normalPlots = normalPlots2, avoidNormalCheck = avoidNormalCheck, includeDimensions = input$includeDimensions)

                     }






                   })





                 }


               })


  output$downloadReport <- shiny::downloadHandler(
    filename = "report.html",

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)


      out <- rmarkdown::render('report.Rmd', rmarkdown::html_document(), params = params)
      file.rename(out, file)
    }
  )



}

