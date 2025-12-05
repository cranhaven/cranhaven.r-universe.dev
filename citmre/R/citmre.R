#' @title rmre_data (Representative Market Rate Exchange) dataset
#' @description
#' Download the colombian RMRE source: Portal de Datos Abiertos <www.datos.gov.co>
#'
#' @import dplyr
#' @import httr
#' @import jsonlite
#' @import lubridate
#' @import rvest
#' @import xml2
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom xts xts
#' @importFrom stats na.omit
#'
#' @param start_date An initial date in the "YYYY-MM-DD" type; by default, the series starts on the first date of the resource
#' @param end_date A final date in the "YYYY-MM-DD" form; by default, it shows the updated last date on the resource.
#' @param log_return Show the log return of the RMRE (Representative Market Rate Exchange) dataset; if it is TRUE, show the log return dataset; if it is FALSE, show the level dataset; in default, show the level dataset
#' @param plot_data Show a Plotly linear graph data set; by default, the argument is false, and the graph is built in the Viewer option. You can use the basic plot if the user does not use the plot_data option.
#' @param frequency Show frequencies for the data set in daily (365), month (12), quarter (4), and half-year (2); in default, the dataset is the daily frequency.
#' @param type It works only with 12,4,2 frequencies, showing the dataset using the last date ("last_date") or doing a mean ("mean") in the frequency series. By default, the type is "last_date".
#' @return dataset in xts and zoo type
#'
#' @export
#'
#' @examples
#' # Show full series dataset
#' rmre_serie <- rmre_data()
#'
#' # Show monthly dataset with Plotly Graph
#' rmre_splited <- rmre_data(frequency = 12, log_return = F, plot_data = T)
#'
#' # Show quaterly log_return dataset with Plotly Graph
#' rmre_splited <- rmre_data(frequency = 4, log_return = T, plot_data = T, type = "mean")
#'
#' # Show splited log return dataset
#' rmre_splited <- rmre_data("2000-01-01", "2023-12-31", log_return = TRUE)
rmre_data <- function(start_date = NULL, end_date = NULL, log_return = FALSE, plot_data = FALSE, frequency = 365, type = "last_date") {
  custom_error_handler <- function(e) {
    if (inherits(e, "simpleError")) {
      message(e$message)
    } else {
      message("An unexpected error occurred. Please check that you have written the argument in the correct order")
    }
  }

  tryCatch(
    {
      val_dat <- 0

      url <- "https://www.datos.gov.co/resource/ceyp-9c7c.json?$limit=1000000"

      response <- tryCatch(
        GET(url, timeout(10)),
        error = function(e) {
          val_dat <- 1
        }
      )

      if (val_dat == 0) {
        json_data <- content(response, "text", encoding = "UTF-8")
        df_data <- fromJSON(json_data)
        df_data$vigenciadesde <- as.POSIXct(df_data$vigenciadesde, format = "%Y-%m-%dT%H:%M:%OS")
        df_data$vigenciahasta <- as.POSIXct(df_data$vigenciahasta, format = "%Y-%m-%dT%H:%M:%OS")
      } else {
        url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQdWRVkiRnMafOBPyTo55Y7kGfogywsTagcs2uiOqSeeCWrplBcAtUezRwhRhxOeeIiszB7VE8Yu7FZ/pubhtml?gid=478587454&single=true"
        html <- read_html(url)
        tabla <- html %>% html_table(fill = TRUE)
        df_data <- data.frame(tabla[[1]], stringsAsFactors = FALSE)
        colnames(df_data) <- unlist(df_data[1, ])
        df_data <- df_data[-1, ]
        df_data <- df_data[, -1]
        df_data[, 1] <- as.numeric(df_data[, 1])
        df_data$vigenciadesde <- as.POSIXct(df_data$vigenciadesde, format = "%Y-%m-%dT%H:%M:%OS")
        df_data$vigenciahasta <- as.POSIXct(df_data$vigenciahasta, format = "%Y-%m-%dT%H:%M:%OS")
      }


      vigenciahasta <- NULL
      rmre <- NULL

      if (is.null(start_date) && is.null(end_date)) {
        start_date <- min(df_data$vigenciahasta)
        end_date <- max(df_data$vigenciahasta)
      } else if (is.null(start_date)) {
        start_date <- min(df_data$vigenciahasta)
      } else if (is.null(end_date)) {
        end_date <- max(df_data$vigenciahasta)
      }

      if (!is.null(start_date) && !grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date)) {
        stop("Error: 'start_date' should be in 'year-month-day' format")
      }
      if (!is.null(end_date) && !grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
        stop("Error: 'end_date' should be in 'year-month-day' format")
      }

      if (!is.null(start_date) && !is.null(end_date) && start_date > end_date) {
        stop("Error: 'start_date' is greater than 'end_date'")
      }

      if (!(type %in% c("mean", "last_date"))) {
        stop("Error: 'type' must be 'mean' or 'last_date'")
      }

      is_weekend <- function(date) {
        weekday <- weekdays(as.Date(date))
        weekday_clean <- tolower(gsub("[\u00e1]", "a", weekday))
        return(weekday_clean %in% c("sabado", "domingo"))
      }

      if (!is.null(start_date) && is_weekend(start_date)) {
        warning("start_date:The information will be obtained from the next business day, as the desired date is a holiday or weekend.")
      }
      if (!is.null(end_date) && is_weekend(end_date)) {
        warning("end_date: The information will be obtained from the next business day, as the desired date is a holiday or weekend.")
      }

      df_data <- subset(df_data, vigenciahasta >= start_date & vigenciahasta <= end_date)
      df_data <- df_data[, c(3, 1)]
      colnames(df_data) <- c("date", "rmre")
      df_data$rmre <- as.numeric(df_data$rmre)

      if (frequency == 365) {
        if (log_return == T) {
          df_data$rmre <- as.numeric(df_data$rmre)
          df_data$log_return <- log(df_data$rmre / lag(df_data$rmre))
          df_data <- df_data[, c(1, 3)]
          df_data <- na.omit(df_data)
        }

        if (plot_data == T) {
          if (log_return == T) {
            fig <- plot_ly(df_data, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~log_return, name = "log_return", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          } else {
            fig <- plot_ly(df_data, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~rmre, name = "rmre", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          }
        }

        if (log_return == T) {
          df_data$log_return <- as.numeric(df_data$log_return)
          df_data <- xts(df_data$log_return, order.by = df_data$date)
          colnames(df_data) <- "log_return"
        } else {
          df_data$rmre <- as.numeric(df_data$rmre)
          df_data <- xts(df_data$rmre, order.by = df_data$date)
          colnames(df_data) <- "rmre"
        }

        return(df_data)
      }

      if (frequency == 12) {
        result <- df_data %>%
          group_by(Month = ceiling_date(date, unit = "month") - days(1)) %>%
          summarise(
            Valor = ifelse(type == "mean", mean(rmre, na.rm = TRUE), last(rmre))
          )
        result_plot <- as.data.frame(result)
        colnames(result_plot) <- c("date", "rmre")

        result <- as.data.frame(result)
        result$Valor <- as.numeric(result$Valor)
        result <- xts(result$Valor, order.by = result$Month)
        colnames(result) <- "rmre"

        if (log_return == T) {
          result <- log(result / lag(result))
          result <- na.omit(result)
          colnames(result) <- "log_return"

          result_plot$rmre <- as.numeric(result_plot$rmre)
          result_plot$log_return <- log(result_plot$rmre / lag(result_plot$rmre))
          result_plot <- result_plot[, c(1, 3)]
          result_plot <- na.omit(result_plot)
        }

        if (plot_data == T) {
          if (log_return == T) {
            fig <- plot_ly(result_plot, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~log_return, name = "log_return", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          } else {
            fig <- plot_ly(result_plot, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~rmre, name = "rmre", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          }
        }
      } else if (frequency == 4) {
        result <- df_data %>%
          group_by(Quarter = ceiling_date(date, unit = "quarter") - days(1)) %>%
          summarise(
            Valor = ifelse(type == "mean", mean(rmre, na.rm = TRUE), last(rmre))
          )
        result_plot <- as.data.frame(result)
        colnames(result_plot) <- c("date", "rmre")

        result <- as.data.frame(result)
        result$Valor <- as.numeric(result$Valor)
        result <- xts(result$Valor, order.by = result$Quarter)
        colnames(result) <- "rmre"

        if (log_return == T) {
          result <- log(result / lag(result))
          result <- na.omit(result)
          colnames(result) <- "log_return"

          result_plot$rmre <- as.numeric(result_plot$rmre)
          result_plot$log_return <- log(result_plot$rmre / lag(result_plot$rmre))
          result_plot <- result_plot[, c(1, 3)]
          result_plot <- na.omit(result_plot)
        }

        if (plot_data == T) {
          if (log_return == T) {
            fig <- plot_ly(result_plot, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~log_return, name = "log_return", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          } else {
            fig <- plot_ly(result_plot, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~rmre, name = "rmre", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          }
        }
      } else if (frequency == 2) {
        result <- df_data %>%
          group_by(Semester = ceiling_date(date, unit = "6 months") - days(1)) %>%
          summarise(
            Valor = ifelse(type == "mean", mean(rmre, na.rm = TRUE), last(rmre))
          )
        result_plot <- as.data.frame(result)
        colnames(result_plot) <- c("date", "rmre")

        result <- as.data.frame(result)
        result$Valor <- as.numeric(result$Valor)
        result <- xts(result$Valor, order.by = result$Semester)
        colnames(result) <- "rmre"

        if (log_return == T) {
          result <- log(result / lag(result))
          result <- na.omit(result)
          colnames(result) <- "log_return"

          result_plot$rmre <- as.numeric(result_plot$rmre)
          result_plot$log_return <- log(result_plot$rmre / lag(result_plot$rmre))
          result_plot <- result_plot[, c(1, 3)]
          result_plot <- na.omit(result_plot)
        }

        if (plot_data == T) {
          if (log_return == T) {
            fig <- plot_ly(result_plot, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~log_return, name = "log_return", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          } else {
            fig <- plot_ly(result_plot, type = "scatter", mode = "lines") %>%
              add_trace(x = ~date, y = ~rmre, name = "rmre", line = list(color = "blue")) %>%
              layout(
                legend = list(orientation = "h", x = 0.5, y = 1.1),
                xaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                yaxis = list(zerolinecolor = "#ffff", zerolinewidth = 2, gridcolor = "ffff"),
                plot_bgcolor = "#e5ecf6",
                autosize = TRUE
              )
            print(fig)
          }
        }
      } else {
        stop("Error: Invalid 'frequency' argument. Should be one of 365 12, 4, or 2")
      }

      return(result)
    },
    error = custom_error_handler
  )
}
