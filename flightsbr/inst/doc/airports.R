## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, message=FALSE------------------------------------------------
# library(flightsbr)
# library(ggplot2)
# # library(geobr)
# 

## ----eval=FALSE, message=FALSE------------------------------------------------
# # private airports
# airports_prv <- flightsbr::read_airports(
#   type = 'private',
#   showProgress = FALSE
#   )
# 
# # public airports
# airports_pbl <- flightsbr::read_airports(
#   type = 'public',
#   showProgress = FALSE
#   )

## ----eval=FALSE, message=FALSE------------------------------------------------
# airports_all <- flightsbr::read_airports(
#   type = 'all',
#   showProgress = FALSE
#   )
# 
# # plot
# # brazil <- geobr::read_country()
# 
# ggplot() +
#    # geom_sf(data=brazil, color='gray') +
#   geom_point(data=airports_all, aes(x=longitude, y=latitude), size=.3 , alpha=.4) +
#   coord_equal()
# 

## ----map of airports, eval=TRUE, echo=FALSE, message=FALSE, out.width='100%'----
knitr::include_graphics("https://github.com/ipeaGIT/flightsbr/blob/main/inst/img/vig_output_airports.png?raw=true")

