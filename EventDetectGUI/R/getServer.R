#' Generate Server Part of GUI
#'
#' Generates the server part of the GUI.
#' This method is used internally in the starting process of the GUI.
#' Manual use of this function is not advised.
#'
#' @param input shiny UI-input
#' @param output shiny UI-output
#' @param session shiny UI-session
#' @export
#' @return None
getServer <- function(input, output, session) {

    #Reactive Vals:
    edsResults <- reactiveVal(NULL)

    #Reads the currently configured csv file and shows its head.
    #Initial Data loading
    output$outDataHead <- DT::renderDataTable({
        req(input$inputFile)
        path <- input$inputFile$datapath

        supportedFileTypes <- c(".csv",".rda",".rds", ".RData")

        if(!any(endsWith(path,supportedFileTypes))){
            showModal(modalDialog(title="Load Error",
                                  "The file you loaded has an unsupported file-type."))
            return()
        }

        if(endsWith(path, ".csv")){
            setEnvData("csvData",read.csv(input$inputFile$datapath,
                                          header = input$csvUseHeader,
                                          sep = input$csvSep,
                                          quote = input$csvQuote))
        }
        if(endsWith(path, ".rds")){
            setEnvData("csvData",readRDS(path))
        }
        if(endsWith(path, ".rda") || endsWith(path, ".RData")){
            loadedDataName <- load(path)
            setEnvData("csvData",get(loadedDataName))
        }

        return(getEnvData("csvData"))
    },options = list(scrollX = TRUE,
                     pageLength = 3,
                     lengthMenu = c( 5, 10, 15, 20)))

    #Update the data table whenever a checkbox is changed.
    observeEvent(input$csvColumnCheckBox,{
        req(input$inputFile)
        req(getEnvData("csvData"))
        cols <- getCSVCols(input)
        output$outDataHead <- DT::renderDataTable(getEnvData("csvData")[,cols],
                                                  options = list(scrollX = TRUE,
                                                                pageLength = length(input$outDataHead_rows_current),
                                                                lengthMenu = c(5, 10, 15, 20)))
    }, ignoreNULL = FALSE)

    output$outDataSelection <- renderUI({
        req(input$inputFile)
        req(getEnvData("csvData"))
        elementList <- list(
            wellPanel(
                checkboxInput("csvSelectColumns", "Dis-/Enable data columns", FALSE),
                # if the ceckbox "csvSelectColumns" is true, a new panel will pop up
                conditionalPanel(
                    condition = "input.csvSelectColumns == true",
                    helpText("Select which columns of your data file shall be used in
                             the event detection"),

                    # quick buttons for fast select -> actions are defined in server part
                    actionButton("csvSelectAll", "select ALL"),
                    actionButton("csvUnselectAll", "unselect All"),

                    # dynamic list for generating checkboxes
                    createCsvCheckBoxes()
                )
            ))
    })

    output$visuDataSlider <- renderUI({
        req(input$outDataHead_rows_all)
        sliderInput("amntPointsVisuPlots",label = "Show last x data points", value = 100,min = 10, max = length(input$outDataHead_rows_all))
    })

    output$visuDataInput <- renderUI({
        req(input$outDataHead_rows_all)
        numericInput("amntPointsVisuInputField", label = "",100)
    })

    output$uiOutPlotSelectColumnsVisu <- renderUI({
        req(input$outDataHead_rows_all)
        selectInput('plotSelectionVisu', 'Select Plot Columns', colnames(getEnvData("csvData")[,-1]), multiple=TRUE, selectize=TRUE)
    })

    #--------------------------------------------------------
    output$plotVisu <- plotly::renderPlotly({
        if(is.null(getEnvData("csvData"))){
            return(NULL)
        }
        plotCols <- which(colnames(getEnvData("csvData")) %in% input$plotSelectionVisu)
        if(length(plotCols) < 1){
            return(NULL)
        }

        csvdata_tail <- getEnvData("csvData")[(length(input$outDataHead_rows_all)
                                               -input$amntPointsVisuInputField
                                               +1):length(input$outDataHead_rows_all),]
        csvdata_tail[[1]] <- as.POSIXlt(csvdata_tail[,1]
                                       ,tryFormats = c("%m/%d/%Y %H:%M:%OS"))
        p <- plot_ly(
            x = ~(nrow(csvdata_tail):1),
            y = ~csvdata_tail[,plotCols[1]],
            type = 'scatter',
            name = colnames(csvdata_tail)[plotCols[1]],
            mode = "lines+markers"
        ) %>% layout(
            yaxis = list(title = colnames(csvdata_tail)[plotCols[1]]),
            xaxis = list(title="t minus nth data point", autorange="reversed")
        ) %>%
           add_trace(x = ~(nrow(csvdata_tail):1) , y= c(mean(csvdata_tail[,plotCols[1]]))
                     , mode = "lines", name = "mean") %>%
            add_trace(x = ~(nrow(csvdata_tail):1) , y= c(mean(csvdata_tail[,plotCols[1]])+sd(csvdata_tail[,plotCols[1]]))
                      , mode = "lines", name = "+sigma")%>%
            add_trace(x = ~(nrow(csvdata_tail):1) , y= c(mean(csvdata_tail[,plotCols[1]])-sd(csvdata_tail[,plotCols[1]]))
                      , mode = "lines", name = "-sigma")
        if(length(plotCols) == 2){
            p <- p %>%
                add_lines(x = ~(nrow(csvdata_tail):1) , y = csvdata_tail[,plotCols[2]],
                          mode = "lines+markers", name = colnames(csvdata_tail)[plotCols[2]]
                          ,yaxis = paste0("y",2)) %>%
                layout(yaxis2 = list(overlaying = "y", side = "right"
                                     , title = colnames(csvdata_tail)[plotCols[2]]))
        }
        if(length(plotCols) > 2){
            return(NULL)
        }
        p$elementId <- NULL
        p
    })

    observeEvent(input$amntPointsVisuInputField,{
        isolate(updateSliderInput(
            session = session,
            inputId = "amntPointsVisuPlots",
            value = input$amntPointsVisuInputField
        ))
    })

    observeEvent(input$amntPointsVisuPlots,{
        isolate(updateNumericInput(
            session = session,
            inputId = "amntPointsVisuInputField",
            value = input$amntPointsVisuPlots
        ))
    })

    observeEvent(input$csvSelectAll,{
        setAllCsvCheckBoxes(session)
    })

    observeEvent(input$csvUnselectAll,{
        resetAllCsvCheckBoxes(session)
    })


    output$preProcessSelector <- renderUI({
        getUiSelectorXML("preProcess",input)
    })

    output$algorithmSelector <- renderUI({
        getUiSelectorXML("algorithm",input)
    })

    output$postProcessSelector <- renderUI({
        getUiSelectorXML("postProcess",input)
    })

    output$preProcessUI <- renderUI({
        req(input$preProcessSelector)
        getUiXML("preProcess",input)
    })

    output$algorithmUI <- renderUI({
        req(input$algorithmSelector)
        getUiXML("algorithm",input)
    })

    output$postProcessUI <- renderUI({
        req(input$postProcessSelector)
        getUiXML("postProcess",input)
    })

    output$generalUI <- renderUI({
        getUiXML("general",input, selectedInput = "general"
                 , selectedElement = getSelectedElementList("general", "general", input))
    })

    observeEvent(input$runEDS,{
        if(is.null(input[["xml_ForecastETSShow"]])){
            showModal(modalDialog(title="Load Error",
                                  "Config was not fully loaded, please revisit Config tab"))
            return()
        }
        if(!checkInputCorrectness(input)){
            return()
        }

        tryCatch(expr = {
            windowSize <- input$xml_generalShowwindowSize
            nIterationsRefit <- input$xml_generalShownIterationsRefit
            verbosityLevel <- input$xml_generalShowverbosityLevel

            dataPreps <- input$preProcessSelector
            dataCtrl <- getControlList(input,"preProcess")

            algo <- input$algorithmSelector
            algoCtrl <- getControlList(input,"algorithm")

            dataPost <- input$postProcessSelector
            postCtrl <- getControlList(input,"postProcess")

            d <- getEnvData("csvData")
            d <- d[,-1] ## Remove the time column --- Later this should be done in one
            ## central point in code, not here! User should select column, its not always 1
            #browser()
            edsResults(EventDetectR::detectEvents(d, windowSize = windowSize,
                                                  nIterationsRefit = nIterationsRefit,
                                                  verbosityLevel = verbosityLevel,
                                                  dataPrepators = dataPreps,
                                                  dataPreparationControl = dataCtrl,
                                                  buildModelAlgo = algo,
                                                  buildForecastModelControl = algoCtrl,
                                                  postProcessors = dataPost,
                                                  postProcessorControl = postCtrl))
            return()
        }, error = function(cond) {
            showModal(modalDialog(title="Configuration Error",
                                  HTML(paste("There seems to be an error in your configuration.<br>
                                            detectEvents was not able to run.<br>
                                            Please check for typos/misconfigurations
                                            in the Config Tab<br><br>Original error was:<br>",cond))
                                  ,footer=NULL,easyClose=T))
            return()
        })
    })

    output$edsResultTable <- DT::renderDataTable({
        req(edsResults())
        edsResults()$classification
    },options = list(scrollX = TRUE,
                     pageLength = 3,
                     lengthMenu = c(5, 10, 15, 20)))

    output$uiOutPlotSelectColumnsResVisu <- renderUI({
        req(input$outDataHead_rows_all)
        selectInput('plotSelectionResVisu', 'Select Plot Columns', colnames(getEnvData("csvData")[,-1]), multiple=TRUE, selectize=TRUE)
    })
output$edsResult <- plotly::renderPlotly({
    req(edsResults())
y_label <- input$plotSelectionResVisu
    p <- which(colnames(edsResults()$classification) %in% input$plotSelectionResVisu)
    if(length(p) < 1){
        return(NULL)
    }

x <- NULL
y <- NULL

        ggplotly(
            ggplot2::ggplot(data = data.frame(x = 1:length(edsResults()$classification[[p]]),
                                                             y = edsResults()$classification[[p]])) +
            ggplot2::geom_point(ggplot2::aes(x = x, y = y,
                                             colour = edsResults()$classification$Event), show.legend = FALSE)+ggplot2::scale_x_continuous(name='Time-Index') + ggplot2::scale_y_continuous(name=y_label))

})

# Export csv of the result dataset ----
output$ExportResults <- downloadHandler(
filename = function() {
  paste("edsResult.csv")
},
    content = function(file) {
        write.csv(edsResults()$classification, file, row.names = FALSE)
    })


}
