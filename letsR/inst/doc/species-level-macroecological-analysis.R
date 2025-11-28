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
centroids <- lets.midpoint(PAM)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
library(knitr)
library(dplyr)
library(kableExtra)

## ---- eval=FALSE--------------------------------------------------------------
#  centroids

## ---- echo = FALSE------------------------------------------------------------
kable(centroids, "html") %>%
  kable_styling() %>%
  scroll_box(width = "600px", height = "400px")

## -----------------------------------------------------------------------------
d <- data.frame(centroids[, 2:3], 
                "Species" = centroids[, 1], 
                "Range size" = rangesize)
sp <- terra::vect(x = d, geom  = c("x", "y"))
plot(sp)
plot(sf::st_geometry(wrld_simpl), add = TRUE)

## ---- message = FALSE, warning=FALSE------------------------------------------
library(ggplot2)

## ---- message=F---------------------------------------------------------------
data_plot <- data.frame(centroids[, 2:3], "Range size" = rangesize)
g <- ggplot(data_plot, aes(x, Range_size))
g + geom_smooth() + geom_point() + labs(x = "Latitude(x)", y = "Range size")

## -----------------------------------------------------------------------------
data(temp)
r <- terra::unwrap(temp)
PAM_env <- lets.addvar(PAM, r, fun = mean)

## -----------------------------------------------------------------------------
pos <- which(colnames(PAM_env) == "bio1_mean")
temp_mean <- lets.summarizer(PAM_env, pos)

## ---- eval=FALSE--------------------------------------------------------------
#  temp_mean

## ---- echo = FALSE------------------------------------------------------------
kable(temp_mean, "html") %>%
  kable_styling() %>%
  scroll_box(width = "400px", height = "400px")

## -----------------------------------------------------------------------------
data("IUCN")

## ---- eval=FALSE--------------------------------------------------------------
#  IUCN

## ---- echo = FALSE------------------------------------------------------------
kable(IUCN, "html") %>%
  kable_styling() %>%
  scroll_box(width = "800px", height = "400px")

## -----------------------------------------------------------------------------
level_order <- c("DD", "LC",  "EN", "CR") # ordering for the plot
data <- data.frame("Status" = factor(IUCN$Status, levels = level_order),
                   "Temperature" = temp_mean[, 2] / 10)
g <- ggplot(data, aes(Status, Temperature))
g + geom_boxplot() + coord_flip()

