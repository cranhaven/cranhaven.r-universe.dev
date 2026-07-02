library(hexSticker)
library(sysfonts)
library(magick)

img <- image_read('./inst/package_sticker/base3.png')

fonts_set <- font_files()

font_add("bookman", "BOOKOSI.TTF")

sticker(
  subplot = img,
  s_x = 1,
  s_y = 1,
  s_width = 1.2,
  s_height = 1.2,
  package = "circlesplot",
  p_family = "bookman",
  p_size = 15,
  p_color = "deeppink4",
  h_fill = "black",
  h_size = 0.8,
  h_color = "deeppink4",
  filename = "inst/figures/circlesplot_sticker.png",
) %>% print()
