devtools::install_github("dkahle/ggmap") # fix loading png images

# Create lake sites figure
library(adklakedata)
library(ggplot2)
library(maps)
library(rgdal)
library(ggsn)
library(ggmap)
library("gridExtra")
library(grid)
library(magick)


# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html


ny = subset(map_data('state'), region=='new york')
lakesites = adk_lakes()
adkpark = readOGR(adk_shape())
adkpark = spTransform(adkpark, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
adk_df = fortify(adkpark)


theme_adkmap  <- function(base_size = 12, base_family = "Helvetica")
    {
      theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
          panel.border = element_rect(colour = "black", fill="transparent", size=0.7)
           , plot.background = element_rect(fill = "transparent",colour = NA)
           , panel.background = element_rect(fill = "transparent",colour = NA)
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , axis.line.x = element_blank()
          , axis.line.y = element_blank()
          , axis.text =element_text(size = 10)
          , axis.title.x = element_text(size=10, margin = margin(10,0,0,0))
          , axis.title.y = element_text(size=10, angle = 90, margin = margin(0,10,0,0))

        )
    }


## BASIC MAP with scale bar

tt = ggplot() +
          geom_polygon(data=ny, aes(long, lat, group=group), color='black', fill="cornsilk") +
          geom_polygon(data=adk_df, aes(long, lat), color='blue', fill= "lightblue1") +
          geom_point(data=lakesites, aes(x=long, y=lat), color="grey40", size=2, pch=16, alpha = 0.6) +
          labs(x = "Longitude", y = "Latitude")+
          theme_adkmap()+
          coord_map('lambert', lat0=20, lat1=50)#+
          #scalebar(ny, dist = 50, dd2km = TRUE, model ='WGS84', location = "bottomleft", st.size = 3)

# add in North Arrow
 north2(tt, x = 0.9, y = 0.9, symbol = 11)


# get background terrain images for park
sbbox <- make_bbox(lon = adk_df$long, lat = adk_df$lat, f = .1)
inbox_map <- get_map(location = sbbox, maptype = "terrain-background", source = "google")
# get background terrain images for all of NY STATE
nybox = make_bbox(lon = ny$long, lat = ny$lat, f = 0.01)
big_map = get_map(location = nybox, maptype = "terrain-background", source = "google")

# NY STATE MAP WITH PARK OUTLINE AND BOUNDING BOX
bigmap = ggmap(big_map)+
            geom_polygon(data=ny, aes(long, lat, group=group), color='black', fill=NA) +
            geom_polygon(data=adk_df, aes(long, lat), color='blue', fill= NA) +
            geom_rect(xmin = min(adk_df$long)-0.05 ,xmax = max(adk_df$long)+0.05
                     ,ymin = min(adk_df$lat)-0.05, ymax = max(adk_df$lat)+0.05 , color = "red", fill = NA)+
            #geom_point(data=lakesites, aes(x=long, y=lat), color="red", size=2, pch=16, alpha = 0.6) +
            labs(x = "Longitude", y = "Latitude")+
            #coord_map('lambert', lat0=20, lat1=50)+
            theme_adkmap()+
            scalebar(subset(ny,long >=-79.5 & lat >= 40.75) , dist = 50, dd2km = TRUE, model ='WGS84', st.size = 3, location = "bottomleft")
# add North Arrow
 # north2(bigmap, x = 0.9, y = 0.89, symbol = 11)

### PARK WITH LAKES
inbox = ggmap(inbox_map)+
          geom_polygon(data=adk_df, aes(long, lat), color='blue', fill= NA) +
          geom_point(data=lakesites, aes(x=long, y=lat), color="blue", size=2, pch=16, alpha = 0.6) +
          labs(x = "Longitude", y = "Latitude")+
          #coord_map('lambert', lat0=20, lat1=50)+
          theme_adkmap()+
          scalebar(adk_df, dist = 25, dd2km = TRUE, model ='WGS84', st.size = 3, location = "topleft")


# write figures

ppi = 300
      png(file =  "ADK_NYmap.png", width = 6*ppi, height = 5*ppi, res = ppi)
       north2(bigmap, x = 0.9, y = 0.89, symbol = 11)
      dev.off()

      png(file =  "ADK_parkmap.png", width = 6*ppi, height = 5*ppi, res = ppi)
       inbox
      dev.off()


### CREATE FANCY MAP
 theme_adkmap_simple  <- function(base_size = 12, base_family = "Helvetica")
    {
      theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(
          panel.border = element_rect(colour = "white", fill="transparent", size=0.7)
           , plot.background = element_rect(fill = "transparent",colour = NA)
           , panel.background = element_rect(fill = "transparent",colour = NA)
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , axis.line.x = element_blank()
          , axis.line.y = element_blank()
          , axis.ticks = element_blank()
          , axis.text =element_text(size = 8, color = "white")
          , axis.title.x = element_text(size=10, margin = margin(10,0,0,0))
          , axis.title.y = element_text(size=10, angle = 90, margin = margin(0,10,0,0))

        )
    }





  tt.simple = ggplot() +
          geom_polygon(data=ny, aes(long, lat, group=group), color='black', fill="yellowgreen", alpha = 0.2) +
          geom_polygon(data=adk_df, aes(long, lat), color='blue', fill= NA) +
          geom_rect(data = adk_df, aes(xmin = min(long)-0.05 ,xmax = max(long)+0.05
                     ,ymin = min(lat)-0.05, ymax = max(lat)+0.05) , color = "red", fill = NA)+
          #geom_point(data=lakesites, aes(x=long, y=lat), color="grey40", size=2, pch=16, alpha = 0.6) +
          labs(x = "", y = "")+
          theme_adkmap_simple()+
          coord_map('lambert', lat0=20, lat1=50)



dat.forbox = data.frame(long = c(-73.09102, -75.724), lat = c( 42.86986, 45.24300))
sbbox_fancy = make_bbox(lon = dat.forbox$long, lat = dat.forbox$lat, f= 0.05)
 map_fancy<- get_map(location = sbbox_fancy, maptype = "terrain-background", source = "google")


     bigmap =  ggmap(map_fancy)+
          geom_polygon(data=adk_df, aes(long, lat), color='blue', fill= NA) +
          geom_point(data=lakesites, aes(x=long, y=lat), color="blue", size=2, pch=16, alpha = 0.6) +
          labs(x = "Longitude", y = "Latitude")+
          #coord_map('lambert', lat0=20, lat1=50)+
          theme_adkmap()+
          scalebar(adk_df, dist = 25, dd2km = TRUE, model ='WGS84', st.size = 3, location = "bottomright")

     north2(bigmap, x = 0.8, y = 0.89, symbol = 11)

      png(file =  "ADK_parkmap_FANCY.png", width = 6*ppi, height = 5*ppi, res = ppi)
      north2(bigmap, x = 0.8, y = 0.89, symbol = 11)
      dev.off()

      png(file =  "NYS_simple.png", width = 3*ppi, height = 3*ppi, res = ppi)
      tt.simple
      dev.off()
adk_parkmap = image_read("ADK_parkmap_FANCY.png")
nys_map = image_read("NYS_simple.png")
nys_map = image_crop(nys_map, "870x666")
nys_map = image_crop(nys_map, "694x560+176+106")
nys_map = image_scale(nys_map, "50%x50%")
nys_map = image_border(nys_map, "black", "2x2")
composite = image_composite(adk_parkmap, nys_map, offset = "+500+50")
image_write(composite, "map_figure_composite.png", format = "png")
