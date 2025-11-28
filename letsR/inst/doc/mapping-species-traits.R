## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5, 
  fig.width = 6,
  fig.align = 'center')

## ---- message = FALSE, warning = FALSE, r, fig.width = 4----------------------
# Load the package
library(letsR)

# Load the data
data("PAM")

# Plot
plot(PAM)

## -----------------------------------------------------------------------------
data("Phyllomedusa")
rangesize <- lets.rangesize(Phyllomedusa,
                            coordinates = "geographic")
rangesize <- rangesize / 1000 # Transform in km2

## -----------------------------------------------------------------------------
resu <- lets.maplizer(PAM, rangesize, rownames(rangesize), ras = TRUE)
cols2 <- colorRampPalette(c('#e0ecf4','#9ebcda','#8856a7'))
plot(resu$Raster, col = cols2(10), main = "Mean Range Size")
data("wrld_simpl")
plot(sf::st_geometry(wrld_simpl), add = TRUE)

## -----------------------------------------------------------------------------
library(ggplot2)

## ---- warning=FALSE, message=FALSE--------------------------------------------
mpg <- as.data.frame(resu$Matrix)
f <- ggplot(mpg, aes(`Latitude(y)`, Variable_mean))
f + geom_smooth(model = lm) + 
  geom_point(col = rgb(0, 0, 0, .6)) + 
  labs(y = "Range Size") + 
  theme_bw()

