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
pkginst <- check_namespaces(c("sf","dplyr","patchwork","ggplot2"))
apiacc_pkginst <- all(apiacc,pkginst)

## ----eval = apiacc_pkginst----------------------------------------------------
library(geofi)
library(sf)
library(dplyr)
muni <- get_municipalities()
point <- municipality_central_localities
crs <- st_crs(muni)
crs$input

## ----crss, eval = apiacc_pkginst----------------------------------------------
muni_4326 <- st_transform(muni, "EPSG:4326")
crs <- st_crs(muni_4326)
crs$input

library(ggplot2)
  
p1 <- ggplot(muni %>% st_union()) + 
  geom_sf() + 
  labs(subtitle = "EPSG:3067")
p2 <- ggplot(muni_4326 %>% st_union()) + 
  geom_sf() +
  labs(subtitle = "EPSG:4326")
library(patchwork)
wrap_plots(list(p1,p2), nrow = 1) +
  plot_annotation(title = "Map of Finland in two different CRS")

## ----largest, fig.width=5, eval = apiacc_pkginst------------------------------
# compute area
muni$area <- sf::st_area(muni)
# largest
muni %>% 
  arrange(desc(area)) %>% 
  slice(1:10) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf_label(aes(label = name_fi)) +
  labs(title = "largest 10")
  # smallest
muni %>% 
  arrange(area) %>% 
  slice(1:10) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf_label(aes(label = name_fi)) +
  labs(title = "smallest 10")


## ----subsetting, eval = apiacc_pkginst----------------------------------------
greater_helsinki <- c('Espoo','Helsinki','Vantaa','Hyvinkää',
                      'Järvenpää','Kauniainen','Kerava','Kirkkonummi',
                      'Mäntsälä','Nurmijärvi','Pornainen','Sipoo','Tuusula','Vihti')
greater_helsinki_polygon <- muni %>% filter(municipality_name_fi %in% greater_helsinki)

ggplot(greater_helsinki_polygon) + 
  geom_sf() +
  geom_sf(data = point %>% 
            filter(teksti %in% toupper(greater_helsinki)))

## ----bb_poly, eval = apiacc_pkginst-------------------------------------------
bounding_box_polygon <- st_as_sfc(st_bbox(muni %>% filter(municipality_name_fi %in% greater_helsinki)))

ggplot(st_intersection(bounding_box_polygon, muni)) + 
  geom_sf() +
  geom_sf(data = point %>% filter(teksti %in% toupper(greater_helsinki)))

## ----bb_point, eval = apiacc_pkginst------------------------------------------
bounding_box_point <- st_as_sfc(st_bbox(point %>% filter(teksti %in% toupper(greater_helsinki))))

ggplot(st_intersection(bounding_box_point, muni)) + 
  geom_sf() +
  geom_sf(data = point %>% filter(teksti %in% toupper(greater_helsinki)))

## ----neighbours,  fig.height = 5, eval = apiacc_pkginst-----------------------
helsinki <- muni %>%  filter(municipality_name_fi == "Helsinki")
neigbour_codes <- st_intersection(muni,helsinki) %>% 
  pull(municipality_code)

ggplot(muni %>% filter(municipality_code %in% neigbour_codes)) +
  geom_sf() +
  geom_sf_label(aes(label = municipality_name_fi))

## ----unioin, eval = apiacc_pkginst--------------------------------------------
muni$area_class <- cut_number(x = as.numeric(muni$area), n = 3)

muni %>% 
  filter(area_class == levels(muni$area_class)[1]) %>% 
  st_union() %>% 
  ggplot() +
  geom_sf()

## ----union2, eval = apiacc_pkginst--------------------------------------------
muni %>% 
  group_by(area_class) %>%
  summarise() %>% 
  ggplot() +
  geom_sf(aes(fill = area_class))

## ----centroids, fig.width=5, eval = apiacc_pkginst----------------------------
muni_centroids <- st_centroid(muni)

ggplot() +
  geom_sf(data = muni) +
  geom_sf(data = muni_centroids, color = "blue") +
  # plot also the municipality_central_localities
  geom_sf(data = municipality_central_localities, color = "red")


## ----buffers, fig.width=5, eval = apiacc_pkginst------------------------------
muni_centroids_buffer <- muni_centroids %>%
    st_buffer(dist = 15000)

ggplot() +
  geom_sf(data = muni) +
  geom_sf(data = muni_centroids_buffer) +
  geom_sf(data = muni_centroids, shape = 3)

## ----muonio, fig.width=5, eval = apiacc_pkginst-------------------------------
muonio <- muni %>% filter(municipality_name_fi == "Muonio")

grid_sf <- st_make_grid(muonio, cellsize = c(2000,4000), what="polygons") %>%
    st_sf()

grid_clip <- st_intersection(grid_sf, muonio)
grid_clip$rank <- 1:nrow(grid_clip)

ggplot(grid_clip) +
  geom_sf(aes(fill = rank), color = alpha("white", 1/3), size = 3) +
  scale_fill_viridis_c() +
  theme_minimal()

## ----voronoi, fig.height = 9, eval = apiacc_pkginst---------------------------
library(geofi)
library(sf)

muni_voronoi <- municipality_central_localities %>% 
  st_union() %>%
  st_voronoi() %>% 
  st_cast() %>% 
  st_sf() %>% 
  st_intersection(st_union(muni)) %>% 
  mutate(rnk = 1:nrow(.))

ggplot(muni_voronoi) + 
  geom_sf(aes(fill = rnk)) +
  geom_sf(data = municipality_central_localities, shape = 4) +
  scale_fill_fermenter(palette = "YlGnBu") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

## ----kunnat, eval = apiacc_pkginst--------------------------------------------
kunta <- geofi::municipality_central_localities %>%  
  select(teksti,kuntatunnus) %>%
  mutate(kuntatunnus = as.integer(kuntatunnus)) %>%
  select(-teksti) %>%
  left_join(geofi::municipality_key_2022 %>% select(municipality_code, municipality_name_fi),
            by = c("kuntatunnus" = "municipality_code")) %>%
  rename(teksti = municipality_name_fi)
kunta

## ----dmatrix, eval = apiacc_pkginst-------------------------------------------
d_list <- list()
kuntadatan_teksti_ja_kuntatunnus <- sf::st_drop_geometry(kunta) %>%
  select(teksti,kuntatunnus)
for (i in 1:nrow(kunta)){
  dist_tmp <- sf::st_distance(x = kunta[i,], y =  kunta)
  tibble(origin_name = kunta[i,]$teksti,
         origin_code = kunta[i,]$kuntatunnus) %>%
    bind_cols(kuntadatan_teksti_ja_kuntatunnus %>% rename(destination_name = teksti,
                                                          destination_code = kuntatunnus)) %>%
    mutate(dist = dist_tmp[1,]) -> d_list[[i]]
}
kunta_dist <- do.call("bind_rows", d_list) %>%
  mutate(dist = as.numeric(dist))
head(kunta_dist)

## ----nearest, eval = apiacc_pkginst-------------------------------------------
ggplot(kunta_dist %>%
         filter(origin_name == "Helsinki") %>%
         arrange(dist) %>% slice(1:20),
       aes(x = dist, y = reorder(destination_name, dist), label = round(dist))) +
         geom_col() + geom_text(aes(x = 1000), color = "white", hjust = 0) +
  labs(title = "Nearest 20 municipality localities to Helsinki", x = "distance in meters")

## ----dist0, eval = apiacc_pkginst---------------------------------------------
# We firt need the country map as a single polygon
geofi::get_municipalities() %>% 
  sf::st_union() %>% 
  # then we need to compute the centroid of that polygon
  sf::st_centroid() -> fin_centroid

# The let's find the nearest neighbour with
distance <- st_distance(x = fin_centroid, y = kunta)
kuntadatan_teksti_ja_kuntatunnus %>% 
  mutate(dist = as.numeric(distance)) %>% 
  arrange(dist) -> closest_to_center
head(closest_to_center)

## ----dist1, eval = apiacc_pkginst---------------------------------------------
furthest20 <- kunta_dist %>%
         filter(origin_name == closest_to_center[1,]$teksti) %>%
         arrange(desc(dist)) %>% slice(1:20)
ggplot(furthest20,
       aes(x = dist, y = reorder(destination_name, dist), label = round(dist))) +
  geom_col() + geom_text(aes(x = 10000), color = "white", hjust = 0) +
  labs(title = paste("Furthest 20 municipality localities \nfrom the most central locality of ", closest_to_center[1,]$teksti), x = "distance in meters")

## ----dist2, eval = apiacc_pkginst---------------------------------------------
sf_lahto <- kunta %>% 
  filter(teksti %in% closest_to_center[1,]$teksti) %>%
  select(teksti)
sf_paate <- kunta %>% 
  filter(teksti %in% furthest20$destination_name) %>% 
  select(teksti)

triplst <- list()
for (i in 1:nrow(sf_paate)){
triplst[[i]] <- rbind(
  sf_lahto,
  sf_paate[i,]
) %>% 
  summarize(m = mean(row_number()),do_union=FALSE) %>% 
  st_cast("LINESTRING")
}
trips <- do.call("rbind", triplst)

ggplot() +
  geom_sf(data = muni %>% st_union(), alpha = .3) +
  geom_sf(data = trips, color = "dim grey") +
  geom_sf_label(data = sf_lahto, aes(label = teksti)) +
  geom_sf_text(data = sf_paate, aes(label = teksti))

