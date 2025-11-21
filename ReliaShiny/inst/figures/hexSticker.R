library(hexSticker)
imgurl <- "Rplot.png"
my_sticker <- theme_sticker(size = 2)
my_sticker <- sticker(imgurl, package="ReliaShiny", p_size=18, p_color="red", h_fill="white",
        h_color="red", s_x=1, s_y=.75, s_width=.45, filename="hexSticker.png")
plot(my_sticker)
