## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  install.packages('CSTools')
#  library(CSTools)

## -----------------------------------------------------------------------------
#  exp <- lonlat_prec_st

## -----------------------------------------------------------------------------
#  dim(exp$data)
#  #   dataset   var   member   sdate   ftime     lat    lon
#  #      1       1       6       3       31       4      4

## -----------------------------------------------------------------------------
#  ilon <- which(exp$coords$lon %in% 5:12)
#  ilat <- which(exp$coords$lat %in% 40:47)
#  exp$data <- exp$data[, , , , , ilat, ilon, drop = FALSE]
#  names(dim(exp$data)) <- names(dim(lonlat_prec_st$data))
#  exp$coords$lon <- exp$coords$lon[ilon]
#  exp$coords$lat <- exp$coords$lat[ilat]

## -----------------------------------------------------------------------------
#  downscaled <- RainFARM(exp$data, exp$coords$lon, exp$coords$lat,
#                         nf = 20, kmin = 1, nens = 3,
#                         time_dim = c("member", "ftime"))

## -----------------------------------------------------------------------------
#  a <- exp$data[1, 1, 1, 1, 17, , ] * 86400 * 1000
#  a[a > 60] <- 60
#  image(exp$coords$lon, rev(exp$coords$lat), t(apply(a, 2, rev)), xlab = "lon", ylab = "lat",
#        col = rev(terrain.colors(20)), zlim = c(0,60))
#  map("world", add = TRUE)
#  title(main = "pr 17/03/2010 original")
#  a <- exp_down$data[1, 1, 1, 1, 1, 17, , ] * 86400 * 1000
#  a[a > 60] <- 60
#  image(exp_down$coords$lon, rev(exp_down$coords$lat), t(apply(a, 2, rev)), xlab = "lon", ylab = "lat",
#        col = rev(terrain.colors(20)), zlim = c(0, 60))
#  map("world", add = TRUE)
#  title(main = "pr 17/03/2010 downscaled")

## -----------------------------------------------------------------------------
#  ww <- CST_RFWeights("./worldclim.nc", nf = 20, lon = exp$coords$lon, lat = exp$coords$lat)

## -----------------------------------------------------------------------------
#  exp_down_weights <- CST_RainFARM(exp, nf = 20, kmin = 1, nens = 3,
#                                   weights = ww, time_dim = c("member", "ftime"))

## -----------------------------------------------------------------------------
#  exp_down1 <- exp_down$data[, , , , , , , 1]
#  exp_down_weights1 <- exp_down_weights$data[, , , , , , , 1]
#  dim(exp_down1) <- c(member = 6 * 3 * 31, lat = 80, lon = 80)
#  dim(exp_down_weights1) <- c(member = 6 * 3 * 31, lat = 80, lon = 80)
#  ad <- apply(exp_down1, c(2, 3), mean)
#  adw <- apply(exp_down_weights1, c(2, 3), mean);
#  
#  png("Figures/RainFARM_fig2.png", width = 640, height = 243)
#  par(mfrow = c(1,3))
#  a <- exp_down_weights$data[1, 1, 1, 1, 17, , ,1] * 86400 * 1000
#  a[a > 60] <- 60
#  image(exp_down$coords$lon, rev(exp_down$coords$lat), t(apply(a, 2, rev)), xlab = "lon",
#        ylab = "lat", col = rev(terrain.colors(20)), zlim = c(0, 60))
#  map("world", add = TRUE)
#  title(main = "pr 17/03/2010 with weights")
#  a <- ad * 86400 * 1000
#  a[a > 5] <- 5
#  image(exp_down$coords$lon, rev(exp_down$coords$lat), t(apply(a, 2, rev)), xlab = "lon",
#        ylab = "lat", col = rev(terrain.colors(20)), zlim = c(0, 5))
#  map("world", add = TRUE)
#  title(main = "climatology no weights")
#  a <- adw * 86400 * 1000
#  a[a > 5] <- 5
#  image(exp_down$coords$lon, rev(exp_down$coords$lat), t(apply(a, 2, rev)), xlab = "lon",
#        ylab = "lat", col = rev(terrain.colors(20)), zlim = c(0, 5))
#  map("world", add = TRUE)
#  title(main = "climatology with weights")
#  dev.off()

## -----------------------------------------------------------------------------
#  slopes <- CST_RFSlope(exp, time_dim = c("member", "ftime"))
#  dim(slopes)
#  #    dataset   var   sdate
#  #       1       1      3
#  # slopes
#  # , , 1
#  
#  #         [,1]
#  # [1,] 1.09957
#  
#  # , , 2
#  
#  #          [,1]
#  # [1,] 1.768861
#  
#  # , , 3
#  
#  #          [,1]
#  # [1,] 1.190176

