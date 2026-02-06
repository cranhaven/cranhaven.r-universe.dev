## ----setup, include=FALSE-----------------------------------------------------
library(unigd)
temp <- airquality$Temp

## -----------------------------------------------------------------------------
my_svg_1_0 <- ugd_render_inline({
  hist(temp, col="darkblue", main = "Zoom 1.0")
}, as="png-base64", width=300, height=300, zoom=1.0)

my_svg_1_5 <- ugd_render_inline({
  hist(temp, col="darkblue", main = "Zoom 1.5")
}, as="png-base64", width=300, height=300, zoom=1.5)

my_svg_0_5 <- ugd_render_inline({
  hist(temp, col="darkblue", main = "Zoom 0.5")
}, as="png-base64", width=300, height=300, zoom=0.5)

# (Output directly in this RMarkdown document)
knitr::raw_html(paste0(sprintf("<img src=\"%s\" />", c(my_svg_1_0, my_svg_1_5, my_svg_0_5))))

