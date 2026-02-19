## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(fig.width = 4,
                      fig.height = 6, 
                      fig.align = "center",
                      fig.pos = "!H",
                      warning = FALSE, 
                      message = FALSE,
                      echo = TRUE,
                      eval = TRUE)

## ----himalayas-aoi, fig.pos='h', out.width = "105%", out.height = "100%", fig.align = 'center', fig.cap = "Area of Interest", fig.alt="Display the area of interest in Himalayas", echo = F----

knitr::include_graphics("himalayas_aoi.png")

# mp1 = mapview::mapview(geoms_himal[2, ], legend = F)      # see next code snippet for the "geoms_himal" variable
# mp2 = mapview::mapview(geoms_himal[2, ], legend = F)
# 
# 
# leafsync::sync(mp1, mp2, ncol = 2)

## ----reference-plt-1, echo = F------------------------------------------------
# #......................................................... mapview visualization
# 
# # make the sf-objects valid
# nams = names(lst_out)
# lst_out = lapply(lst_out, function(x) sf::st_make_valid(x))
# names(lst_out) <- nams
# 
# sf_wkt = sf::st_make_valid(sf_wkt)
# 
# # Plot the two sf-objects
# RGTs = mapview::mapview(lst_out, legend = F)
# AOI_wkt = mapview::mapview(sf_wkt, legend = F)
# 
# lft = RGTs + AOI_wkt
# 
# require(magrittr)
# 
# lft = lft@map %>% leaflet::setView(lng = as.numeric(centr_wkt[, 'X']),
#                                    lat = as.numeric(centr_wkt[, 'Y']),
#                                    zoom = 7)
# lft
# #.......................................................


## ----himalayas-rgts, fig.pos='h', out.width = "80%", out.height = "80%", fig.align = 'center', fig.cap = "Intersected RGTs", fig.alt="Intersected Reference Ground Tracks", echo = F----

knitr::include_graphics("himalayas_rgts.png")


