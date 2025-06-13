#' Interactive app for verifying auto cleanup.
#'
#' \code{verify_cleanup_app} plots the data points changed during the
#' previously completed auto cleanup and allows the user to verify the cleanup
#' for specific events.  The app saves the selection to the RDS file, which can
#' then be used to apply the changes to the data set.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @import shiny
#'
#' @param data A data table object.
#' @param LogFile A character string indicating the name (and location) of the
#' log file.
#' @examples
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex3")
#'
#' # Ensure the log file exists by running cleanup
#' # Writing log file to temporary folder for the example
#' dat <- clean_blink(Pupilex3, BlinkPadding = c(100, 100), Delta = 5,
#'                    MaxValueRun = 5, NAsAroundRun = c(2,2),
#'                    LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))
#'
#' # Read log file from temporary folder
#' verify_cleanup_app(dat, LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Cleanup", package="PupilPre")
#'
verify_cleanup_app <- function(data = data, LogFile = NULL){

  # Check for file and path
  if(is.null(LogFile)){
    stop("Please provide the name of the log file including file extension.
    Suggested names are 'BlinkCleanupLog.rds' and 'ArtifactCleanupLog.rds'.
    Please also provide the path if you have saved the file outside of your working directory.")
  }
  if(tolower(tools::file_ext(LogFile)) != "rds"){
    stop("The file extension you have provided is not valid.
    You MUST specify a .rds file.")
  }
  if(!dir.exists(dirname(LogFile))){
    stop("The file path you have provided does not exist.
    Please specify a valid path or remove path to read the file from your working directory.")
  }
  if(!file.exists(LogFile)) {
    stop(paste0("Cannot find ", LogFile))
  }


  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Cleanup"),
      shiny::fluidRow(
        shiny::splitLayout(cellWidths = c("100%"),
                           cellArgs = list(style = "padding: 6px"),
                           # shiny::plotOutput("Cleaned"),
                           shiny::plotOutput("AutoEvent"))
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::hr(),
        shiny::actionButton("prev", "Previous"),
        shiny::actionButton("nxt", "Next"),
        # shiny::actionButton("commitclean", label = "Commit Current Event Cleanup"),
        shiny::actionButton("revert", "Revert Event Cleanup"),
        shiny::hr(),
        # shiny::radioButtons("radio", label = "View",
        #              choices = list("All events" = 1, "Auto-cleaned events" = 2), selected = 1),
        shiny::selectInput("event",
                           "Event:", choices = unique(levels(droplevels(data$Event)))),
        shiny::hr(),
        shiny::actionButton("quit", "Exit and save")
      )),
    server = function(input,
                      output, session) {

      message(paste("Loading file", LogFile))
      blinks <- readRDS(file = LogFile)

      values <- shiny::reactiveValues(blnklist = blinks)

      indivdata <-
        shiny::reactive({
          data <- droplevels(data[data$Event == input$event, ])
          return(data)
        })

      # observeEvent(input$radio, {
      #   if(input$radio == 2) {
      #     updateSelectInput(session, "event", label = "Event:", choices = names(blinks[blinks==TRUE | blinks==F]))
      #   }
      # })
      lookup <- data.frame(choices = names(blinks[!is.na(blinks)]))
      shiny::updateSelectInput(session, "event", label = "Event:",
                               choices = unique(lookup$choices),
                               selected = lookup$choices[1])

      shiny::observeEvent(input$nxt, {
        currentevent <- input$event
        # if(input$radio == 2) {
        lookup <- data.frame(choices = names(blinks[!is.na(blinks)]))
        # } else{
        #   lookup <- data.frame(choices = unique(levels(data$Event)))
        # }
        ind <- as.numeric(as.character(rownames(lookup)[lookup$choices==currentevent]))
        ind <- ind+1
        nextevent <- lookup$choices[ind]
        shiny::updateSelectInput(session, "event", label = "Event:",
                                 choices = unique(lookup$choices),
                                 selected = nextevent)
        # message(sum(is.na(unique(lookup$choices))))
      })

      shiny::observeEvent(input$prev, {
        currentevent <- input$event
        # if(input$radio == 2) {
        lookup <- data.frame(choices = names(blinks[!is.na(blinks)]))
        # } else{
        #   lookup <- data.frame(choices = unique(levels(data$Event)))
        # }
        ind <- as.numeric(as.character(rownames(lookup)[lookup$choices==currentevent]))
        ind <- ind-1
        # message(lookup$choices)

        prevevent <- lookup$choices[ind]
        shiny::updateSelectInput(session, "event", label = "Event:",
                                 choices = unique(lookup$choices),
                                 selected = prevevent)
        # message(sum(is.na(unique(lookup$choices))))
      })

      shiny::observeEvent(input$revert, {
        values$blnklist[[input$event]] <- as.logical(FALSE)
        saveRDS(values$blnklist, file = LogFile, compress = TRUE)
      })

      shiny::observe({
        if(input$quit > 0){
          saveRDS(values$blnklist, file = LogFile, compress = TRUE)
          stopApp(NULL)
        }
      })

      indivdata <-
        shiny::reactive({
          data <- droplevels(data[data$Event == input$event, ])
          # data <- data %>% filter(Time >= input$range[1], Time <= input$range[2])

          data <- data %>% mutate(Compared = !(.compareNA(Pupil_Previous, Pupil)))

          data$Percent <- 100*(nrow(data[data$Compared==TRUE,])/nrow(data))

          data <- data %>% select(Pupil, Pupil_Previous, Time, Compared, Percent) %>%
            tidyr::gather(Column, PUPIL, -Time, -Compared, -Percent) %>% arrange(Column, Time)
          # data$Column <- relevel(as.factor(data$Column), ref = input$ref)
          data$Datapoint <- ifelse(data$Compared==FALSE, "Same", "Different")


          return(data)
        })
      output$AutoEvent <-
        shiny::renderPlot({
          # ylim <- c(min(indivdata()$PUPIL), max(indivdata()$PUPIL))

          if(values$blnklist[input$event]==TRUE){
            ggplot(indivdata(), aes(x = Time, y = PUPIL, colour = Datapoint)) +
              geom_point(na.rm = TRUE)+
              scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
              #facet_wrap(~ Event) +
              ylab("Pupil Dilation") +
              # scale_x_continuous(limits = input$range) +
              # scale_y_continuous(limits = c(ylim[1], ylim[2])) +
              theme_bw() + theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
              ) +
              labs(x = "Time", y = "Pupil") +
              ggtitle(paste0(unique(round(indivdata()$Percent, 2)), "% of data differs between Pupil and Pupil_Previous"))
          } else if(values$blnklist[input$event]==FALSE){
            indivdata() %>% filter(Column == "Pupil_Previous") %>%
              ggplot(., aes(x = Time, y = PUPIL)) +
              geom_point(na.rm = TRUE)+
              # scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
              #facet_wrap(~ Event) +
              ylab("Pupil Dilation") +
              # scale_x_continuous(limits = input$range) +
              # scale_y_continuous(limits = c(ylim[1], ylim[2])) +
              theme_bw() + theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
              ) +
              labs(x = "Time", y = "Pupil") +
              ggtitle("Reverting to Pupil_Previous")
          }
        })

    }
  )
}


#' Interactive app for manually cleaning pupil data.
#'
#' \code{user_cleanup_app} plots current pupil data and allows the user to
#' select data points which should be removed (changed to NA).  The app saves
#' a record of the to-be-executed changes in an RDS file.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @import shiny
#'
#' @param data A data table object.
#' @param LogFile A character string indicating the name (and location) of the
#' log file to be read/written.
#' @examples
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex4")
#'
#' # Writing log file to temporary folder for the example
#' user_cleanup_app(Pupilex4, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Cleanup", package="PupilPre")
#'
user_cleanup_app <- function(data = data, LogFile = NULL){

  # Check for file and path
  if(is.null(LogFile)){
    stop("Please provide the name of the log file including file extension.
    Suggested name is 'UserCleanupLog.rds'.
    Please also provide the path if you have saved the file outside of your working directory.")
  }
  if(tolower(tools::file_ext(LogFile)) != "rds"){
    stop("The file extension you have provided is not valid.
    You MUST specify a .rds file.")
  }
  if(!dir.exists(dirname(LogFile))){
    stop("The file path you have provided does not exist.
    Please specify a valid path or remove path to read the file from your working directory.")
  }
  if(!file.exists(LogFile)) {
    message(paste("Creating file", LogFile))
    blinks <- vector("list", length = length(unique(data$Event)))
    names(blinks) <- unique(data$Event)
    for(i in 1:length(blinks)) { blinks[[i]] <- NA }
    saveRDS(blinks, file = LogFile, compress = TRUE)
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Cleanup"),
      shiny::fluidRow(
        shiny::splitLayout(cellWidths = c("33.3%", "33.3%", "33.3%"),
                           cellArgs = list(style = "padding: 6px"),
                           shiny::plotOutput("Original"),
                           shiny::plotOutput("Preview"),
                           shiny::plotOutput("Committed"))
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::hr(),
        shiny::actionButton("prev", "Previous"),
        shiny::actionButton("nxt", "Next"),
        shiny::actionButton("commitclean", label = "Commit Current Event Cleanup"),
        shiny::actionButton("resetclean", "Reset Current Event Cleanup"),
        shiny::hr(),
        shiny::selectInput("event",
                           "Event:", choices = unique(levels(data$Event))),
        shiny::textInput("blinkpoints", "Time points:", value = NULL, width = '50%', placeholder = "Time values as individual numerals and/or sequences"),
        shiny::hr(),
        shiny::actionButton("quit", "Exit and save")
      )),
    server = function(input,
                      output, session) {

      message(paste("Loading file", LogFile))
      blinks <- readRDS(file = LogFile)


      values <- shiny::reactiveValues(bpslist = blinks)

      bps <- shiny::reactive({
        shiny::validate(
          shiny::need(!grepl("\\:$", input$blinkpoints), label = "Please complete the range of values")
        )
        as.numeric(eval(parse(text=paste("c(", input$blinkpoints, ")", sep=""))))
      })

      yl <- shiny::reactive({
        range(data[data$Event == input$event,]$Pupil, na.rm = TRUE)
      })

      observeEvent(input$nxt, {
        currentevent <- input$event
        lookup <- data.frame(choices = unique(levels(data$Event)))
        ind <- as.numeric(as.character(rownames(lookup)[lookup$choices==currentevent]))
        ind <- ind+1
        nextevent <- lookup$choices[ind]
        updateSelectInput(session, "event", label = "Event:", choices = unique(levels(data$Event)),
                          selected = nextevent)
        updateTextInput(session, "blinkpoints", label = "Blink points", value = "")
      })

      observeEvent(input$prev, {
        currentevent <- input$event
        lookup <- data.frame(choices = unique(levels(data$Event)))
        ind <- as.numeric(as.character(rownames(lookup)[lookup$choices==currentevent]))
        ind <- ind-1
        prevevent <- lookup$choices[ind]
        updateSelectInput(session, "event", label = "Event:", choices = unique(levels(data$Event)),
                          selected = prevevent)
        updateTextInput(session, "blinkpoints", label = "Blink points", value = "")
      })

      observeEvent(input$commitclean, {
        values$bpslist[[input$event]] <- bps()
        saveRDS(values$bpslist, file = LogFile, compress = TRUE)
      })

      observeEvent(input$resetclean, {
        currecteventbps <- NA
        values$bpslist[[input$event]] <- currecteventbps
        saveRDS(values$bpslist, file = LogFile, compress = TRUE)
      })

      shiny::observe({
        if(input$quit > 0){
          saveRDS(values$bpslist, file = LogFile, compress = TRUE)
          stopApp(NULL)
        }
      })

      output$Original <- shiny::renderPlot({
        if(input$blinkpoints=="") {
          data %>% filter(Event == input$event) %>% select(Pupil, Time) %>%
            ggplot(., aes(x = Time, y = Pupil)) +
            geom_point(na.rm = TRUE) +
            coord_cartesian(ylim = yl()) +
            ylab("Pupil") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            ggtitle("Original")
        } else {
          data %>% filter(Event == input$event) %>% select(Pupil, Time) %>%
            ggplot(., aes(x = Time, y = Pupil, color=ifelse(Time %in% bps(), "A", "B"))) +
            geom_point(na.rm = TRUE) +
            scale_color_manual(guide=FALSE, values=c("red", "black")) + #turn off the legend, define the colors
            coord_cartesian(ylim = yl()) +
            ylab("Pupil") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            ggtitle("Original")
        }
      })

      output$Preview <- shiny::renderPlot({
        if(input$blinkpoints=="") {
          data %>% filter(Event == input$event) %>% select(Pupil, Time) %>%
            ggplot(., aes(x = Time, y = Pupil)) +
            geom_point(na.rm = TRUE) +
            coord_cartesian(ylim = yl()) +
            ylab("Pupil") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            ggtitle("Preview")
        } else {
          tmp <- data %>% filter(Event == input$event) %>% select(Pupil, Time)
          yl <- range(tmp$Pupil, na.rm = TRUE)
          tmp$Preview <- ifelse(tmp$Time %in% bps(), NA, tmp$Pupil)
          ggplot(tmp, aes(x = Time, y = Preview)) +
            geom_point(na.rm = TRUE) +
            coord_cartesian(ylim = yl()) +
            ylab("Pupil") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            ggtitle("Preview")
        }
      })

      output$Committed <- shiny::renderPlot({
        if(any(is.na(values$bpslist[[input$event]]))) {
          data %>% filter(Event == input$event) %>% select(Pupil, Time) %>%
            ggplot(., aes(x = Time, y = Pupil)) +
            coord_cartesian(ylim = yl()) +
            ylab("Pupil") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            ggtitle("No committed cleanup")
        } else {
          bps_committed <- values$bpslist[[input$event]]
          data %>% filter(Event == input$event) %>% select(Pupil, Time) %>%
            mutate(Preview = ifelse(Time %in% bps_committed, NA, Pupil)) %>%
            ggplot(aes(x = Time, y = Preview)) +
            geom_point(na.rm = TRUE) +
            coord_cartesian(ylim = yl()) +
            ylab("Pupil") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            ggtitle("Committed cleanup")
        }
      })

    }
  )
}




#' Plots the effect of Butterworth filtering by event.
#'
#' \code{butter_filter_app} produces a plot of Butterworth filtered pupil data over the
#' original data to visually inspect the effect of different filter settings.
#'
#' @export
#' @importFrom signal butter
#' @importFrom signal filtfilt
#' @importFrom stats relevel
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import shiny
#'
#' @param data A data table object.
#' @examples
#'
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex5")
#'
#' butter_filter_app(Pupilex5)
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Interpolation_and_Filtering", package="PupilPre")
#'
butter_filter_app <- function (data)
{
  # Check if signal is available
  if("signal" %in% (.packages())){
  } else{
    if(nchar(system.file(package = "signal")) == 0){
      stop("Please install the package 'signal' for Butterworth filtering.")
    }
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Butterworth filter"),
      shiny::plotOutput("filt"),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          2,
          offset = 0,
          shiny::selectInput("event", "Event:", choices = unique(levels(data$Event)))
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::selectInput("type", "Filter type:", choices = c("low", "high", "stop", "pass"), selected = "low"),
          shiny::numericInput("b", "Order:", value = 1),
          shiny::textInput("w", "Critical Frequency(ies):", value = "0.1")
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::numericInput("buf1", "Exclude from beginning of timeseries:", value = 0),
          shiny::numericInput("buf2", "Explude from end of timeseries:", value = 0)
        ),
        shiny::column(
          2,
          offset = 0,
          shiny::actionButton("quit", "Quit")
        )
      )),
    server = function(input,
                      output) {

      shiny::observe({
        if(input$quit > 0){
          stopApp(NULL)
        }
      })

      indivdata <-
        shiny::reactive({
          data <- droplevels(data[data$Event == input$event, ])
          shiny::validate(
            shiny::need(!any(is.na(data$Pupil)), message = "This event contains NAs. Filtering cannot be performed.")
          )
          w <- as.numeric(eval(parse(text=paste("c(", input$w, ")", sep=""))))
          # message(w)
          shiny::validate(
            shiny::need((input$type %in% c("low", "high") && length(w)==1) ||
                         (input$type %in% c("stop", "pass") && length(w)==2),
                        message = "For types 'low' and 'high', provide only a single value between 0 and 1.\nFor types 'stop' and 'pass', provide two values between 0 and 1, separated by a comma.")
          )
          b1 <- signal::butter(n = input$b, W = w, type = input$type)
          data$Filtered <- signal::filtfilt(filt = b1, x = data$Pupil)
          data$Original <- data$Pupil
          data <- data %>%
            select(Original, Time, Filtered) %>%
            tidyr::gather(Data, PUPIL, -Time) %>% arrange(desc(Data), Time)
          data$Data <- factor(data$Data, levels = c("Original", "Filtered"))
          data$Data <- stats::relevel(data$Data, ref = "Original")
          return(data)
        })

      output$filt <-
        shiny::renderPlot({
          ylim <- c(min(indivdata()$PUPIL), max(indivdata()$PUPIL))

          plt <- ggplot(indivdata(), aes(x = Time, y = PUPIL, colour = Data)) +
            geom_point(na.rm = TRUE)+
            scale_color_manual(values=c("Filtered" = "red", "Original" = "black")) +
            ylab("Pupil Dilation") +
            scale_y_continuous(limits = c(ylim[1], ylim[2])) +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            labs(x = "Time", y = "Pupil")
          if(input$buf1 > 0 | input$buf1 > 0){
            plt +
              geom_vline(xintercept = min(indivdata()$Time)+input$buf1) +
              geom_vline(xintercept = max(indivdata()$Time)-input$buf2)
          } else{
            plt
          }
        })
    }
  )
}


#' Plots comparison of Pupil and Pupil_Previous by event.
#'
#' \code{plot_compare_app} produces a comparison plot of Pupil and
#' Pupil_Previous by event to visual changes.
#'
#' @export
#' @importFrom stats relevel
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import shiny
#'
#' @param data A data table object.
#' @examples
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex4")
#'
#' plot_compare_app(Pupilex4)
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
plot_compare_app <- function (data)
{
  if("Baseline" %in% colnames(data)){
    stop("The data appear to be baselined. Comparison is no longer meaningful.")
  }

  if(!("Pupil_Previous" %in% colnames(data))){
    stop("The data appear to be downsampled. Comparison is no longer meaningful.")
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Comparison"),
      shiny::plotOutput("comp"),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          2,
          offset = 0,
          shiny::actionButton("quit", "Quit")
        ),
        shiny::column(
          4,
          offset = 0,
          # shiny::selectInput("ref", "Reference:", choices = c("Pupil", "Pupil_Previous")),
          shiny::selectInput("event", "Event:", choices = unique(levels(data$Event)))
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::sliderInput("range", "Time:",
                             min = min(data$Time), max = max(data$Time),
                             value = c(min(data$Time), max(data$Time)))
        ),
        shiny::column(
          2,
          offset = 0
        )
      )),
    server = function(input,
                      output) {

      shiny::observe({
        if(input$quit > 0){
          stopApp(NULL)
        }
      })

      indivdata <-
        shiny::reactive({
          data <- droplevels(data[data$Event == input$event, ])
          data <- data %>% filter(Time >= input$range[1], Time <= input$range[2])

          data <- data %>% mutate(Compared = !(.compareNA(Pupil_Previous, Pupil)))

          data$Percent <- 100*(nrow(data[data$Compared==TRUE,])/nrow(data))

          data <- data %>% select(Pupil, Pupil_Previous, Time, Compared, Percent) %>%
            tidyr::gather(Column, PUPIL, -Time, -Compared, -Percent) %>% arrange(Column, Time)
          # data$Column <- relevel(as.factor(data$Column), ref = input$ref)
          data$Datapoint <- ifelse(data$Compared==FALSE, "Same", "Different")


          return(data)
        })
      output$comp <-
        shiny::renderPlot({
          ylim <- c(min(indivdata()$PUPIL), max(indivdata()$PUPIL))

          ggplot(indivdata(), aes(x = Time, y = PUPIL, colour = Datapoint)) +
            geom_point(na.rm = TRUE)+
            scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
            #facet_wrap(~ Event) +
            ylab("Pupil Dilation") +
            scale_x_continuous(limits = input$range) +
            scale_y_continuous(limits = c(ylim[1], ylim[2])) +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
            ) +
            labs(x = "Time", y = "Pupil") +
            ggtitle(paste0(unique(round(indivdata()$Percent, 2)), "% of data differs between Pupil and Pupil_Previous"))
        })
    }
  )
}



#' Plots summary of subject or item.
#'
#' \code{plot_summary_app} plots summary of a given subject or item.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @import shiny
#'
#' @param data A data table object.
#' @examples
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex4")
#'
#' plot_summary_app(Pupilex4)
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
plot_summary_app <- function (data)
{
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Summary"),
      shiny::plotOutput("Indiv"),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          4,
          offset = 0,
          shiny::selectInput(
            "column",
            "Column:",
            choices = c("Pupil", "Pupil_Previous")
          ),
          shiny::actionButton("quit", "Quit")
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::selectInput("type",
                             "Group:", choices = c("Subject", "Item")),
          shiny::conditionalPanel(
            condition = "input.type == 'Item'",
            shiny::selectInput("item", "Plot:", choices = unique(levels(data$Item)))
          ),
          shiny::conditionalPanel(
            condition = "input.type == 'Subject'",
            shiny::selectInput("subj", "Individual:", choices = unique(levels(data$Subject)))
          )
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::sliderInput(
            "range",
            "Time:",
            value = c(min(data$Time),max(data$Time)),
            min = min(data$Time),
            max = max(data$Time)
          )
        )
      )),
    server = function(input,
                      output) {

      shiny::observe({
        if(input$quit > 0){
          stopApp(NULL)
        }
      })

      indivdata <-
        shiny::reactive({
          if (input$type == "Subject") {
            data <- droplevels(data[data$Subject == input$subj, ])
            data <- data %>% select(input$column, Subject, Event, Time)
            colnames(data)[1] <- "Pupil"
            data <- data %>%
              group_by(Time) %>%
              summarise(PupilPlot = mean(Pupil, na.rm = TRUE), SD = stats::sd(Pupil, na.rm = TRUE), N = n()) %>%
              mutate(SE = SD/sqrt(N))
          }
          else if (input$type == "Item") {
            data <- droplevels(data[data$Item == input$item, ])
            data <- data %>% select(input$column, Item, Event, Time)
            colnames(data)[1] <- "Pupil"
            data <- data %>%
              group_by(Time) %>%
              summarise(PupilPlot = mean(Pupil, na.rm = TRUE), SD = stats::sd(Pupil, na.rm = TRUE), N = n()) %>%
              mutate(SE = SD/sqrt(N))
          }
          return(data)
        })
      output$Indiv <-
        shiny::renderPlot({
          if (is.null(input$column)) {
            message("Please select a column.")
          }
          else {
            #ylim <- c(min(indivdata()$Pupil), max(indivdata()$Pupil))
            ggplot(indivdata(), aes(x = Time, y = PupilPlot)) +
              geom_ribbon(aes(ymin = PupilPlot-SE, ymax = PupilPlot+SE), alpha = 0.5) +
              geom_line(na.rm = TRUE) +
              #facet_wrap(~ Event) +
              ylab("Pupil Dilation") +
              scale_x_continuous(limits = input$range) +
              #scale_y_continuous(limits = c(ylim[1], ylim[2])) +
              theme_bw() + theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
              )
          }
        })
    }
  )
}




