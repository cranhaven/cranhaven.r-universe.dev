#Import necessary packages
library(hexSticker)
library(here)
library(showtext)

# Loading Google fonts
font_add_google("Atkinson Hyperlegible")
font_add_google("Roboto")
#Automatically use showtext to render text for future device
showtext_auto()

sticker(here("man/figures/icon.svg"), package = "tidyindex",
        p_size = 23, p_y = 1.4,
        s_x = 1, s_y = 0.8, s_width = 0.85, s_height = 0.85,
        h_fill = "#F49F1c",p_color = "#ffffff", p_family = "Roboto",
        spotlight = TRUE, l_x = 1.3, l_y = 1,  white_around_sticker = TRUE,
        filename = here("man/figures/logo.png"), h_color = "#65350f", dpi = 360)


