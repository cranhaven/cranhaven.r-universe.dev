## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE, 
  warning = FALSE,
  fig.height = 7, 
  fig.width = 7,
  dpi = 75
)

## ----eval = FALSE-------------------------------------------------------------
# # install from CRAN
# install.packages("geofi")
# 
# # Install development version from GitHub
# remotes::install_github("ropengov/geofi")

## ----include = FALSE, eval = TRUE---------------------------------------------
# Let's first create a function that checks if the suggested 
# packages are available
check_namespaces <- function(pkgs){
  return(all(unlist(sapply(pkgs, requireNamespace,quietly = TRUE))))
}
apiacc <- geofi::check_api_access()
pkginst <- check_namespaces(c("sf","dplyr","patchwork","leaflet","ggplot2"))
apiacc_pkginst <- all(apiacc,pkginst)

## ----municipality_map, fig.width = 5, eval = apiacc---------------------------
library(geofi)
polygon <- get_municipalities(year = 2021, scale = 4500)
point <- geofi::municipality_central_localities
# municipality code into integer
point$municipality_code <- as.integer(point$kuntatunnus)

## ----base, fig.width = 5, eval = apiacc_pkginst-------------------------------
library(sf)
plot(st_geometry(polygon["municipality_code"]))
plot(polygon["municipality_code"], add = TRUE, border="white")
plot(st_geometry(point["municipality_code"]), add = TRUE, color = "black")

## ----gg, fig.width = 5, eval = apiacc_pkginst---------------------------------
library(ggplot2)
ggplot() + 
  geom_sf(data = polygon, aes(fill = municipality_code)) +
  geom_sf(data = point)

## ----uusimaa, fig.width=8, fig.height=4, eval = apiacc_pkginst----------------
library(dplyr)
polygon_uusimaa <- polygon %>% filter(maakunta_name_fi %in% "Uusimaa")
point_uusimaa <- point %>% filter(municipality_code %in% polygon_uusimaa$municipality_code)
ggplot() + 
  theme_light() +
  geom_sf(data = polygon_uusimaa, alpha = .3) + 
  geom_sf(data = point_uusimaa) + 
  geom_sf_text(data = point_uusimaa, aes(label = teksti))

## ----uusimaa_repel, fig.width=8, fig.height=4, eval = apiacc_pkginst----------
ggplot() + 
  theme_light() +
  geom_sf(data = polygon_uusimaa, alpha = .3) + 
  geom_sf(data = point_uusimaa) + 
  ggrepel::geom_text_repel(data = point_uusimaa %>%
                        sf::st_set_geometry(NULL) %>%
                        bind_cols(point_uusimaa %>% 
                                    sf::st_centroid() %>% 
                                    sf::st_coordinates() %>% as_tibble()),
                     aes(label = teksti, x = X, y = Y))

## ----create_popdata, eval = apiacc_pkginst------------------------------------
pop_data <- bind_rows(
  tibble(
    municipality_code = polygon$municipality_code
  ) %>% 
    mutate(population = rnorm(n = nrow(.), mean = 2000, sd = 250),
           time = 2020),
  tibble(
    municipality_code = polygon$municipality_code
  ) %>% 
    mutate(population = rnorm(n = nrow(.), mean = 2000, sd = 250),
           time = 2021)
  )
pop_data

## ----facet,  fig.height=7, eval = apiacc_pkginst------------------------------
pop_map <- right_join(polygon, pop_data)

ggplot(pop_map, 
       aes(fill = population)) +
  geom_sf() +
  facet_grid(~time)

## ----patchwork, fig.width = 8, fig.height=10, eval = apiacc_pkginst-----------
library(patchwork)
p_municipalities <- ggplot(polygon, aes(fill = municipality_code)) + 
  geom_sf() + 
  theme(legend.position = "top")
p_regions <- ggplot(polygon %>% count(maakunta_code), aes(fill = maakunta_code)) + 
  geom_sf() + 
  theme(legend.position = "top")
p_uusimaa <- ggplot(polygon_uusimaa, aes(fill = municipality_code)) + 
  geom_sf() + 
  theme(legend.position = "top")

(p_municipalities | p_regions) /
p_uusimaa + plot_layout(nrow = 2, heights = c(1,0.6)) +
  plot_annotation(title = "Combining multiple maps into a single (gg)plot")

## ----fig.height = 5, eval = apiacc_pkginst------------------------------------
ggplot(polygon_uusimaa, aes(fill = municipality_code)) +
  geom_sf(color = alpha("white", 1/3)) +
  scale_fill_fermenter(palette = "YlGnBu") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top"
        ) +
  labs(title = "Municipality code", 
       fill = NULL)

## ----leaflet, out.width="90%", eval = apiacc_pkginst--------------------------
polygon_wgs84 <- sf::st_transform(x = polygon, crs = "+proj=longlat +datum=WGS84")
point_wgs84 <- sf::st_transform(x = point, crs = "+proj=longlat +datum=WGS84")

library(leaflet)
# lets create a palette for polygon fill (municipality codes)
pal <- leaflet::colorNumeric(palette = "Blues", 
                            domain = polygon_wgs84$municipality_code)
# labels for localities
labels <- sprintf(
  "<strong>%s</strong> (%s)",
  point_wgs84$teksti, point_wgs84$kuntatunnus
) %>% lapply(htmltools::HTML)

# popup for polygons
popup <- sprintf(
  "<strong>%s</strong> (%s)",
  polygon_wgs84$municipality_name_fi, polygon_wgs84$municipality_code
) %>% lapply(htmltools::HTML)

EPSG3067 <- leaflet::leafletCRS(crsClass = "L.Proj.CRS",
                                code = "EPSG:3067", 
                                proj4def = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                                resolutions = 1.5^(25:15))

leaflet(polygon_wgs84, options = leafletOptions(worldCopyJump = F, crs = EPSG3067)) %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
   addPolygons(fillColor = ~pal(municipality_code),
              color = "black",
              weight = 1,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.4,
              popup = popup, 
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.4,
                bringToFront = TRUE)
  )  %>% 
  addMarkers(data = point_wgs84,
              label = labels,
              clusterOptions = markerClusterOptions(),
              labelOptions = labelOptions(opacity = .7,
                                          style = list("font-weight" = "normal",
                                                       padding = "2px 4px"),
                                          textsize = "12px",
                                          direction = "auto"))

