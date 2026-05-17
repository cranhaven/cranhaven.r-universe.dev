library(hexSticker)
library(ggplot2)

hex <- sticker(
  "inst/gauge.svg",
  s_x       = 1,
  s_y       = 0.72,
  s_width   = 0.55,
  package   = "PRA",
  p_size    = 24,
  p_color   = "#fcbba1",
  p_y       = 1.42,
  h_fill    = "#67000d",
  h_color   = "#fb6a4a",
  h_size    = 2.0,
  url       = "paulgovan.github.io/PRA",
  u_size    = 5,
  u_color   = "#fc9272",
  filename  = "inst/PRA_hex_sticker.png"
)
plot(hex)
