shinyServer(function(input, output, session) {
  subject <- shiny::reactiveValues(index = 1)
  
  dataFromRds <- shiny::reactive({
    readData(
      databaseId = input$selectedDatabaseId,
      cohortId = input$selectedCohortId
    )
  })
  subjectIds <- shiny::reactive({
    dataFromRds()$cohort$personId %>% sort()
  })
  
  selectedSubjectId <- shiny::reactive({
    if (subject$index) {
      subjectIds()[subject$index]
    }
  })
  
  cohortAndObservationPeriod <- shiny::reactive({
    cohortFiltered <- dataFromRds()$cohort %>%
      dplyr::filter(personId == subjectIds()[subject$index]) %>%
      dplyr::mutate(
        domain = "Cohort",
        cdmTable = "Cohort",
        conceptName = "Cohort entry",
        typeConceptName = "Cohort entry"
      ) %>%
      dplyr::select(startDate,
                    endDate,
                    domain,
                    conceptName,
                    typeConceptName,
                    cdmTable)
    
    observationPeriodFiltered <- dataFromRds()$observationPeriod %>%
      dplyr::filter(personId == subjectIds()[subject$index]) %>%
      dplyr::select(startDate,
                    endDate,
                    typeConceptId) %>%
      dplyr::mutate(cdmTable = "Observation Period",
                    domain = "Observation Period",
                    conceptName = "Observation Period") %>%
      dplyr::left_join(
        dataFromRds()$conceptId %>%
          dplyr::rename("typeConceptName" = "conceptName") %>%
          dplyr::select(-domainId),
        by = c("typeConceptId" = "conceptId")
      )
    
    data <- dplyr::bind_rows(cohortFiltered,
                             observationPeriodFiltered) %>%
      dplyr::arrange(cdmTable, startDate)
    
    data$firstOccurrenceDate <- min(cohortFiltered$startDate)
    
    return(data)
  })
  
  
  queryResult <- shiny::reactive({
    filteredConceptIds <- dataFromRds()$conceptId
    if (is.null(input$cdmTables)) {
      return(cohortAndObservationPeriod())
    } else {
      if (input$filterRegex != "") {
        tryCatch(
          expr = {
            filteredConceptIds <- filteredConceptIds %>%
              dplyr::filter(stringr::str_detect(
                string = tolower(conceptName),
                pattern = tolower(input$filterRegex)
              ))
          },
          error = function(e) {
            showNotification("please check the regular expression for error", "", type = "error")
            return()
          }
        )
      }
      
      if (input$deleteRegex != "") {
        tryCatch(
          expr = {
            filteredConceptIds <- filteredConceptIds %>%
              dplyr::filter(stringr::str_detect(
                string = tolower(conceptName),
                pattern = tolower(input$deleteRegex),
                negate = TRUE
              ))
          },
          error = function(e) {
            showNotification("please check the regular expression for error", "", type = "error")
            return()
          }
        )
      }
      selectedCdmTables <-
        gsub(
          pattern = " ",
          replacement = "_",
          x = tolower(input$cdmTables)
        )
      
      data <- dplyr::tibble()
      
      for (i in (1:length(selectedCdmTables))) {
        domainTableData <-
          dataFromRds()[[snakeCaseToCamelCase(selectedCdmTables[[i]])]] %>%
          dplyr::filter(personId == subjectIds()[subject$index]) %>%
          dplyr::mutate(cdmTable = selectedCdmTables[[i]])
        
        if (selectedCdmTables[[i]] == "feature_cohort_data") {
          cohortDefinitionSet <-
            dataFromRds()$featureCohortDefinitionSet |>
            dplyr::select(cohortId,
                          cohortName) |>
            dplyr::rename(conceptId = cohortId,
                          conceptName = cohortName)
        }
        
        if (!'endDate' %in% colnames(domainTableData)) {
          domainTableData <- domainTableData %>%
            dplyr::mutate(endDate = startDate)
        }
        
        domainTableData <- domainTableData %>%
          dplyr::mutate(endDate = dplyr::if_else(
            condition = is.na(endDate),
            true = startDate,
            false = endDate
          ))
        
        data <- dplyr::bind_rows(data,
                                 domainTableData)
      }
      
      if (input$showSourceCode) {
        data <- data %>%
          dplyr::filter(sourceConceptId >= 0) %>% 
          dplyr::mutate(conceptId = sourceConceptId) %>%
          dplyr::select(-sourceConceptId) %>%
          dplyr::group_by(personId,
                          startDate,
                          endDate,
                          conceptId,
                          typeConceptId,
                          cdmTable) %>%
          dplyr::summarise(records = sum(records),
                           .groups = "keep") %>%
          dplyr::ungroup()
      } else {
        data <- data %>%
          dplyr::filter(conceptId >= 0) 
        if ("sourceConceptId" %in% colnames(data)) {
          data <- data %>% 
            dplyr::select(-sourceConceptId)
        }
        data <- data %>%
          dplyr::group_by(personId,
                          startDate,
                          endDate,
                          conceptId,
                          typeConceptId,
                          cdmTable) %>%
          dplyr::summarise(records = sum(records),
                           .groups = "keep") %>%
          dplyr::ungroup()
      }
      
      dataFiltered <- data |> 
        dplyr::filter(!cdmTable == "feature_cohort_data") %>%
        dplyr::inner_join(filteredConceptIds,
                          by = "conceptId")
      
      if (exists("cohortDefinitionSet")) {
        featureCohortData <- data |>
          dplyr::filter(cdmTable == "feature_cohort_data") |>
          dplyr::inner_join(cohortDefinitionSet,
                            by = "conceptId") |>
          dplyr::mutate(vocabularyId = "Cohort",
                        conceptCode = as.character(conceptId),
                        typeConceptId = 0,
                        records = 1)
        
        dataFiltered <- dplyr::bind_rows(dataFiltered,
                                         featureCohortData)
      }
      
      data <- dataFiltered
      
      if (isFALSE(input$showSourceCode)) {
        data <- data %>%
          dplyr::select(-conceptCode, -vocabularyId)
      }
      
      data <- data %>%
        dplyr::left_join(
          dataFromRds()$conceptId %>%
            dplyr::rename("typeConceptName" = "conceptName") %>%
            dplyr::select(-domainId, -vocabularyId, -conceptCode),
          by = c("typeConceptId" = "conceptId")
        ) %>%
        dplyr::select(-typeConceptId)
      
      data <- data %>%
        dplyr::rename(domain = domainId) %>%
        dplyr::select(-personId)
      firstOccurrenceDateValue <-
        cohortAndObservationPeriod()$firstOccurrenceDate %>% unique()
      
      cohortData <-
        cohortAndObservationPeriod() %>% dplyr::select(intersect(colnames(data),
                                                                 colnames(cohortAndObservationPeriod())))
      
      data <- dplyr::bind_rows(cohortData,
                               data) %>%
        dplyr::mutate(daysToFirst = firstOccurrenceDateValue - startDate)
      
      if (all(!is.null(input$daysFromCohortStart),
              input$daysFromCohortStart != '')) {
        data <- data %>%
          dplyr::filter(as.integer(daysToFirst) * -1 <= input$daysFromCohortStart)
      }
      
      if (all(!is.null(input$daysToCohortStart),
              input$daysFromCohortStart != '')) {
        data <- data %>%
          dplyr::filter(as.integer(daysToFirst) <= input$daysToCohortStart)
      }
      
      if (isTRUE(input$shiftDates)) {
        earliestDate <- cohortAndObservationPeriod() %>%
          dplyr::select(startDate) %>%
          dplyr::summarise(startDate = as.Date(min(startDate))) %>%
          dplyr::pull(startDate)
        
        data <- data %>%
          dplyr::mutate(startDate = addDays(x = as.Date(originDate),
                                            n = as.integer(
                                              difftime(
                                                time1 = startDate,
                                                time2 = earliestDate,
                                                units = "days"
                                              )
                                            ))) %>%
          dplyr::mutate(endDate = addDays(x = as.Date(originDate),
                                          n = as.integer(
                                            difftime(
                                              time1 = endDate,
                                              time2 = earliestDate,
                                              units = "days"
                                            )
                                          )))
      }
      return(data)
    }
  })
  
  filteredEvents <- shiny::reactive({
    events <- queryResult()
    if (nrow(events) != 0) {
      events <- events[order(events$conceptId), ]
      getY <- function(subset) {
        uniqueConceptIds <- unique(subset$conceptId)
        subset$y <- match(subset$conceptId, uniqueConceptIds)
        return(subset)
      }
      events <- lapply(split(events, events$cdmTable), getY)
      events <- do.call("rbind", events)
    }
    return(events)
  })
  
  
  colorScale <- shiny::reactive({
    selectedCdmTables <- input$cdmTables
    if (length(selectedCdmTables) > 0) {
      selectedCdmTables <-
        gsub(
          pattern = " ",
          replacement = "_",
          x = tolower(selectedCdmTables)
        )
    }
    
    tables <- c("Cohort", "Observation Period", selectedCdmTables)
    if (length(tables) == 2) {
      colors <- c("Red", "Orange")
    } else {
      temp <-
        RColorBrewer::brewer.pal(n = max(3, length(tables) - 2), name = "Set2")
      colors <- c("Red", "Orange", temp[1:(length(tables) - 2)])
    }
    names(colors) <- tables
    return(colors)
  })
  
  shiny::observeEvent(input$nextButton, {
    if (subject$index < length(subjectIds())) {
      subject$index <- subject$index + 1
    }
  })
  
  shiny::observeEvent(input$previousButton, {
    if (subject$index > 1) {
      subject$index <- subject$index - 1
    }
  })
  
  output$cohortName <- shiny::renderText({
    cohortName <- "Unknown"
    if (!is.null(dataFromRds()$cohortName)) {
      cohortName <- dataFromRds()$cohortName
    }
    return(cohortName)
  })
  
  output$subjectId <- shiny::renderText({
    return(subjectIds()[subject$index])
  })
  
  output$age <- shiny::renderText({
    selectedSubjectId <- subjectIds()[subject$index][1]
    age <- dataFromRds()$person %>%
      dplyr::filter(personId == selectedSubjectId) %>%
      dplyr::pull(age)
    return(age)
  })
  
  output$gender <- shiny::renderText({
    selectedSubjectId <- subjectIds()[subject$index][1]
    gender <- dataFromRds()$subjects %>%
      dplyr::filter(personId == selectedSubjectId) %>%
      dplyr::pull(gender)
    return(gender)
  })
  
  output$eventTable <- reactable::renderReactable(expr = {
    data <- filteredEvents() %>%
      dplyr::arrange(abs(daysToFirst)) %>%
      dplyr::select(-conceptId, -y) %>%
      dplyr::relocate(daysToFirst,
                      conceptName,
                      typeConceptName,
                      startDate,
                      endDate,
                      domain,
                      cdmTable)
    
    colnames(data) <-
      camelCaseToTitleCase(colnames(data))
    
    dataTable <- reactable::reactable(
      data = data,
      sortable = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      pagination = TRUE,
      showPagination = TRUE,
      showPageInfo = TRUE,
      highlight = TRUE,
      striped = TRUE,
      compact = TRUE,
      showSortIcon = TRUE,
      showSortable = TRUE,
      fullWidth = TRUE,
      borderless = TRUE,
      onClick = "select",
      wrap = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 50, 100, 1000),
      defaultPageSize = 1000
    )
    return(dataTable)
  })
  
  output$plotSmall <- plotly::renderPlotly(plot())
  output$plotBig <- plotly::renderPlotly(plot())
  
  plot <- shiny::reactive({
    events <- filteredEvents()
    if (nrow(events) == 0) {
      return(NULL)
    } else {
      colors <- colorScale()
      cdmTables <- events %>% 
        dplyr::group_by(cdmTable) %>% 
        dplyr::summarise(y = max(y), .groups = "keep") %>% 
        dplyr::ungroup()
      cdmTables <- cdmTables[order(cdmTables$cdmTable), ]
      cdmTables$offset <- cumsum(cdmTables$y) - cdmTables$y
      events <- merge(events, cdmTables[, c("cdmTable", "offset")])
      events$y <- events$y + events$offset
      yRange <- c(min(events$y) - 1, max(events$y) + 1)
      events$text <-
        sprintf(
          "%s - %s<br>%s<br>%s<br>%s<br>%s<br>%s",
          events$startDate,
          events$endDate,
          events$conceptName,
          events$conceptId,
          events$domain,
          events$cdmTable,
          events$typeConceptName
        )
      
      eventsPerY <- events %>%
        dplyr::group_by(y) %>%
        dplyr::summarise(cdmTable = length(y), .groups = "keep") %>% 
        dplyr::ungroup()
      
      yGrid <- eventsPerY$y[eventsPerY$cdmTable > 1]
      
      yAxis <- list(
        title = "",
        tickmode = "array",
        tickvals = yGrid,
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = TRUE,
        range = yRange,
        fixedrange = TRUE
      )
      xAxis <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = TRUE
      )
      plot <- plotly::plot_ly() %>%
        plotly::add_trace(
          data = events,
          x = ~ startDate,
          y = ~ y,
          color = ~ cdmTable,
          colors = colors,
          type = 'scatter',
          mode = 'markers',
          text = ~ text,
          hovertemplate = "%{text}"
        ) %>%
        plotly::add_segments(
          data = events,
          x = ~ startDate,
          y = ~ y,
          xend = ~ endDate,
          yend = ~ y,
          color = ~ cdmTable,
          showlegend = FALSE,
          hoverinfo = "skip"
        )
      
      shapes <- list()
      
      cohortData <- events %>%
        dplyr::filter(cdmTable == "Cohort") %>%
        dplyr::select(startDate,
                      endDate) %>%
        dplyr::distinct() %>%
        dplyr::arrange(startDate)
      
      first <- TRUE
      for (i in (1:nrow(cohortData))) {
        data <- data.frame(
          date = rep(cohortData$startDate[i], 2),
          y = rep(yRange, 2),
          text = sprintf("%s - %s",
                         cohortData$startDate[i],
                         cohortData$endDate[i])
        )
        plot <- plot %>% plotly::add_lines(
          x = ~ date,
          y = ~ y,
          data = data,
          mode = "lines",
          line = list(color = colors["Cohort"]),
          name = "Cohort",
          text = ~ text,
          hovertemplate = "%{text}",
          showlegend = first
        )
        first <- FALSE
        
        if (!is.na(cohortData$endDate[i])) {
          shapes[[length(shapes) + 1]] <- list(
            type = "rect",
            fillcolor = "red",
            line = list(color = colors["Cohort"]),
            opacity = 0.3,
            x0 = cohortData$startDate[i],
            x1 = cohortData$endDate[i],
            xref = "startDate",
            y0 = yRange[1],
            y1 = yRange[2],
            yref = "y"
          )
        }
      }
      
      plot <- plot %>% plotly::layout(
        yaxis = yAxis,
        xaxis = xAxis,
        shapes = shapes,
        legend = list(orientation = 'h'),
        margin =  list(
          l = 1,
          r = 1,
          b = 1,
          t = 25,
          pad = 1
        )
      )
      return(plot)
    }
  })
  
  shiny::observeEvent(input$filterInfo, {
    showModal(
      modalDialog(
        title = "Concept Name Filter",
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        HTML(
          "Filter the concept to include in the plot and table by concept name using a regular expression.
           For example, the regular expression 'celecox|diclof' finds concepts like 'Celecoxib 200mg Oral Tablet' and 'Diclofenac'.
           See <a href='https://en.wikipedia.org/wiki/Regular_expression'>Wikipedia</a> for more information on regular expressions."
        )
      )
    )
  })
})
