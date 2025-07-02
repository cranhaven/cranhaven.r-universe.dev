## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, message = FALSE----------------------------------------------
# library(flightsbr)
# library(data.table)
# library(ggplot2)
# 

## ----eval=FALSE---------------------------------------------------------------
# # in a given **month* of a given **year** (yyyymm)
# df_201506 <- read_flights(date = 201506)
# 
# 
# # from specific months
# df_various_months <- read_flights(date = c(202001, 202101, 202210))
# 
# 
# # in a given year (yyyy)
# df_2015 <- read_flights(date = 2015)
# 
# 
# # from specific years
# df_various_years <- read_flights(date = c(2019, 2021, 2022))
# 

## ----eval=FALSE---------------------------------------------------------------
# df_201506 <- read_flights(
#   date = 201506,
#   showProgress = FALSE,
#   select = c('id_empresa', 'nr_voo', 'dt_partida_real',
#              'sg_iata_origem' , 'sg_iata_destino')
#   )
# 
# head(df_201506)

## ----eval=FALSE---------------------------------------------------------------
# # download flights data
# df <- read_flights(
#   date = 2019:2022,
#   select = c('nr_passag_pagos', 'dt_partida_real'),
#   showProgress = TRUE
#   )
# 
# # count daily passengers
# count_df <- df[, .(total_pass = sum(nr_passag_pagos, na.rm=TRUE)),
#                by = dt_partida_real]
# 
# # reformat date
# count_df <- count_df[ between(dt_partida_real, as.Date('2019-01-01'), as.Date('2022-12-31')) ]
# 
# count_df[, date := as.IDate(dt_partida_real, format="%Y-%m-%d") ]
# count_df[, year := year(date) ]
# count_df[, date_plot := paste0("2030-", format(date, "%m-%d"))]
# count_df[, date_plot := as.Date(date_plot)]
# 
# # plot
# fig <- ggplot(data = count_df) +
#           geom_point(aes(x=date_plot, y=total_pass, color=factor(year)), alpha=.4, size=1) +
#           scale_y_log10(name="Number of Passengers",
#                         labels = scales::unit_format(unit = ""), limit=c(1000,NA)) +
#           scale_x_date(date_breaks = "1 months", date_labels = "%b", name = 'Month') +
#           labs(subtitle ='Daily number of air passengers in Brazil', color = "Legend") +
#           theme_minimal() +
#           theme(panel.grid.minor = element_blank(),
#                 axis.text = element_text(size = 7),
#                 axis.title=element_text(size=9),
#                 plot.background = element_rect(fill='white', colour='white'))
# 
# 
# fig
# 

## ----daily passengers, eval=TRUE, echo=FALSE, message=FALSE, out.width='100%'----
knitr::include_graphics("https://github.com/ipeaGIT/flightsbr/blob/main/inst/img/vig_output_flights.png?raw=true")

