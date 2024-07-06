##################################################
# flood3daily_Dessau.R
#
# author: arnd.weber@bafg.de
# date:   20.04.2023
#
# purpose: 
#   - compute daily flood extent for Dessau
#   - plot daily flood extents
#   - assemble them as movie for the presentation at the "Evaluierung 2023"
#
##################################################

write("floodExtents for Dessau will be computed", stdout())

# load packages
library(hyd1d)
library(hydflood)

# setwd
setwd(Sys.getenv("hydflood")) 
p <- "vignettes/movie/dessau/2013/"

# temporal sequence (last X days) Sys.Date() - 8
dates <- as.character(seq.Date(as.Date("2013-01-01"), as.Date("2013-12-31"),
                               by = "1 day"))

#####
# gauging_station_data
wgs84 <- st_crs("EPSG:4326")
crs <- st_crs("EPSG:25833")
df.gsd <- na.omit(df.gauging_station_data)
sf.gsd <- st_as_sf(df.gsd, coords = c("longitude", "latitude"), crs = wgs84)
sf.gsd_D <- sf.gsd[which(sf.gsd$gauging_station %in% c("ROSSLAU", "DESSAU")), ]
sf.gsd_D <- st_transform(sf.gsd_D, crs)

#####
# color function
dem_colfunc <- colorRampPalette(c("saddlebrown", "yellow", "darkgreen"))

#####
# DESSAU
ext_D <- ext(306050, 311870, 5747870, 5752220)
dem <- rast("data-raw/raster.dem_dessau.tif")
dem_plot <- dem
dem_plot[dem_plot > 62] <- 62
dem_plot[dem_plot < 50] <- 50
csa <- rast("data-raw/raster.csa_dessau.tif")
x <- hydSpatRaster(filename_dem = "data-raw/raster.dem_dessau.tif",
                   filename_csa = "data-raw/raster.csa_dessau.tif")

# loop over all dates
i <- 1
for (a_date in dates) {
    
    write(paste0("DESSAU: ", a_date), stdout())
    
    f_out <- paste0(p, "flood3_", sprintf("%03d", i), ".png")
    
    if (file.exists(f_out)) {
        write("  exists already", stdout())
    } else {
        write("  will be computed", stdout())
        
        # compute flood extent
        # flood_extent <- tryCatch({
            flood_extent <- flood3(x, as.Date(a_date))
            flood_extent[flood_extent == 0] <- NA
        # },
        # error = function(cond) {NA})
        
        # plotting with raster functions
        if (inherits(flood_extent, "SpatRaster")) {
            png(filename = f_out, width = 960, height = 640, units = "px")
            plot(raster(dem_plot), col = dem_colfunc((62 - 50)*2),
                 main = strftime(a_date, "%d.%m.%Y"),
                 bty = "n", box = FALSE, axis = FALSE, xaxt = "n", yaxt = "n",
                 legend = FALSE)
            # plot(dem_plot, col = dem_colfunc((62 - 50)*2), xlim = c(305000, 313000),
            #      bty = "n", xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2),
            #      plg = list(title = "elevation (m)", horiz = TRUE, width = 1))
            plot(raster(flood_extent), col = "blue", add = TRUE, legend = FALSE)
            plot(sf.gsd_D$geometry, pch = 21, bg = "white", add =TRUE)
            text(st_coordinates(sf.gsd_D[1, ]), pos = 3,
                 labels = sf.gsd_D$gauging_station[1])
            text(st_coordinates(sf.gsd_D[2, ]), pos = 1,
                 labels = sf.gsd_D$gauging_station[2])
            dev.off()
        }
    }
    i <- i + 1
}

#####
# assemble the daily images as movie
system(paste0("ffmpeg -y -framerate 4 -i ", p, "flood3_%03d.png -c:v libx264 ",
              "-r 30 -pix_fmt yuv420p ", p, "flood3-4fps.mp4"))
system(paste0("ffmpeg -y -framerate 8 -i ", p, "flood3_%03d.png -c:v libx264 ",
              "-r 30 -pix_fmt yuv420p ", p, "flood3-8fps.mp4"))

#
q("no")
