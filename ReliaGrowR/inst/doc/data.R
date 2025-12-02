## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# data <- read.csv("path/to/your/data.csv")
# head(data)

## ----eval=FALSE---------------------------------------------------------------
# library(readxl)
# data <- read_excel("path/to/your/data.xlsx")
# head(data)

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(2, 3, 5, 7, 11)

## -----------------------------------------------------------------------------
data <- data.frame(col1 = times, col2 = failures)
head(data)

## -----------------------------------------------------------------------------
colnames(data) <- c("times", "failures")
head(data)

## -----------------------------------------------------------------------------
failures <- c(100, 200, 200, 400)
right_censored <- c(250, 350, 450)
interval_starts <- c(150, 300)
interval_ends <- c(180, 320)

## -----------------------------------------------------------------------------
library(ReliaGrowR)
result <- weibull_to_rga(failures, right_censored, interval_starts, interval_ends)
head(result)

## -----------------------------------------------------------------------------
data <- data.frame(
  times = c(100, 200, NA, 400, 500),
  failures = c(2, NA, 5, 7, 11)
)
cleaned_data <- na.omit(data)
head(cleaned_data)

## -----------------------------------------------------------------------------
data <- data.frame(
  times = c(100, 200, NA, 400, 500),
  failures = c(2, NA, 5, 7, 11)
)
data$times[is.na(data$times)] <-
  mean(
    data$times,
    na.rm = TRUE
  )
data$failures[is.na(data$failures)] <-
  ceiling(
    mean(
      data$failures,
      na.rm = TRUE
    )
  )
head(data)

## ----eval=FALSE---------------------------------------------------------------
# write.csv(data, "path/to/your/cleaned_data.csv", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# library(writexl)
# write_xlsx(data, "path/to/your/cleaned_data.xlsx")

