# library(ggplot2)
# library(sf)
# library(hexSticker)
# sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))
# hexs <- st_make_grid(sfc, cellsize = .1, square = FALSE)
# hexs.sf <- st_sf(hexs)
# set.seed(12345)
# hexs.sf$H <- runif(149)
# q <- quantile(hexs.sf$H)
# hexs.sf$Quantile<- as.factor((hexs.sf$H > q[2]) + (hexs.sf$H > q[3]) +(hexs.sf$H >= q[4]) + 1)
# p <- ggplot(data = hexs.sf) +
#   geom_sf(size=.1, aes(fill=Quantile, color = H )) +
#   scale_fill_manual(values=c("#FFFEDE","#FFDFA2", "#FFA93F", "#D5610D")) +
# #  c("#2EB0BE", "#5680CA", "#E4A80D","#606060")
#   theme_bw()
# p
#
# st <- sticker(p, white_around_sticker = TRUE,
#               asp = 200,
#               s_width = 5,
#               p_x = 1,
#               p_y = 1,
#               p_family = "arial",
#               p_fontface = "bold",
#               p_color = "black",
#               s_height=3.5,
#               p_size= 11,
#               package="spQdep",
#               filename="hex.png",
#               h_fill="orange",
#               h_size = 2,
#               h_color = "orange",
#               url = "https://github.com/f8l5h9/spqdep",
#               spotlight=F)
# plot(st)
#
