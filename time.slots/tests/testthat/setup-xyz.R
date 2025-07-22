Sys.setlocale("LC_TIME", "C")
Sys.setenv(TZ = "UTC")

mock_data_07 <- mtcars
mock_data_07$title <- row.names(mtcars)
mock_data_07$start_datetime <- seq.POSIXt(as.POSIXct("2022-03-07 00:00:00"), as.POSIXct("2022-03-13 23:30:00"), by = "30 min")[1:32]
mock_data_07$end_datetime <- seq.POSIXt(as.POSIXct("2022-03-07 00:30:00"), as.POSIXct("2022-03-13 23:30:00"), by = "30 min")[1:32]
mock_data_07 <- mock_data_07 |> tidyr::nest(body = c(mpg, hp, gear))

mock_data_06 <- mtcars
mock_data_06$title <- row.names(mtcars)
mock_data_06$start_datetime <- seq.POSIXt(as.POSIXct("2022-03-06 00:00:00"), as.POSIXct("2022-03-13 23:30:00"), by = "30 min")[1:32]
mock_data_06$end_datetime <- seq.POSIXt(as.POSIXct("2022-03-06 00:30:00"), as.POSIXct("2022-03-13 23:30:00"), by = "30 min")[1:32]
mock_data_06 <- mock_data_06 |> tidyr::nest(body = c(mpg, hp, gear))

mock_data <- dplyr::bind_rows(mock_data_06, mock_data_07)
