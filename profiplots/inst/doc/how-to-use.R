## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  code_folding = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(profiplots)
library(ggplot2)

sample_df <- data.frame(x=1:8, y=1:8, category=as.factor(LETTERS[1:8]))

## ----eval=TRUE, echo=TRUE, results="hold"-------------------------------------
plot_gg <- ggplot(sample_df, aes(x=x, y=y, fill=x)) + 
  stat_identity(geom="bar") + 
  theme_profinit()
plot_gg + labs(title = "ggplot - Default")

# turn on the settings
profiplots::set_theme("blue-red")  
plot_gg + labs(title = "ggplot - Profinit")

# turn off the settings
profiplots::unset_theme()
plot_gg + labs(title = "ggplot - Default again")

## ----eval=TRUE, echo=TRUE, results="hold"-------------------------------------
barplot(sample_df$x, col=sample_df$category, main = "Base R - Default", border = NA)

# turn on the settings
profiplots::set_theme("blue-red", "blue-red")  
barplot(sample_df$x, col=sample_df$category, main = "Base R - Profinit", border = NA)

# turn off the settings
profiplots::unset_theme()
barplot(sample_df$x, col=sample_df$category, main = "Base R - Default again", border = NA)

## -----------------------------------------------------------------------------
plot_gg + 
  scale_fill_profinit_c(palette = "reds") +                             # HERE WE CHANGE THE PALETTE
  labs(title = "Example - monochromatic fill", fill = "Variable")

plot_gg + 
  scale_fill_profinit_c(palette = "blues-dark") +                       # HERE WE CHANGE THE PALETTE
  labs(title = "Example - monochromatic fill", fill = "Variable")

## -----------------------------------------------------------------------------
plot_gg + 
  scale_fill_profinit_c(palette = "blue-red") +                         # HERE WE CHANGE THE PALETTE
  labs(title = "Example - gradient fill", fill = "Variable")

plot_gg + 
  scale_fill_gradient(low = profinit_cols("red"), high = profinit_cols("yellow")) +  # HERE WE CHANGE THE PALETTE
  labs(title = "Example - gradient fill custom (NOT RECOMMEADED)", fill = "Variable")

## -----------------------------------------------------------------------------
plot_gg + 
  scale_fill_profinit_c(palette = "blue-white-red", reverse = TRUE) +                         # HERE WE CHANGE THE PALETTE
  labs(title = "Example - diverging fill", fill = "Variable")

plot_gg + 
  scale_fill_gradient2(low = profinit_cols("red"), mid = "white", high = profinit_cols("pink"), midpoint = 4)                         # HERE WE CHANGE THE PALETTE
  labs(title = "Example - diverging fill (NOT RECOMMANDED)", fill = "Variable")

## -----------------------------------------------------------------------------
plot_gg + 
  aes(fill = as.character(x)) + 
  scale_fill_profinit_d(palette = "discrete") +                                # HERE WE CHANGE THE PALETTE
  labs(title = "Example - discrete fill (exact)", fill = "Variable")

plot_gg + 
  aes(fill = as.character(x)) + 
  scale_fill_profinit(palette = "discrete", exact = FALSE) +                   # HERE WE CHANGE THE PALETTE
  labs(title = "Example - discrete fill (interpolated)", fill = "Variable")

plot_gg + 
  aes(fill = as.character(x)) + 
  scale_fill_profinit_d(palette = "discrete-full") +                   # HERE WE CHANGE THE PALETTE
  labs(title = "Example - discrete fill (full, exact)", fill = "Variable")


## -----------------------------------------------------------------------------
barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  las = "1",                                  # rotate y-axis labels
  lwd.ticks = 1,
  bty = "7",
  col = profinit_pal("blue-red")(8), 
  main = "Base R - monochromatic fill"
)

barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
   las = "1",                                  # rotate y-axis labels
  lwd.ticks = 1,
  bty = "]",
  col = profinit_pal("blues-dark")(8), 
  main = "Base R - monochromatic fill, another palette"
)

## -----------------------------------------------------------------------------
barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = profinit_pal("blue-red")(8),
  main = "Base R - gradient fill"
)

# This way you can define your own palette based on Profinit colors
red_yellow_pal <- grDevices::colorRampPalette(c(profinit_cols("red"), profinit_cols("yellow")))
barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = red_yellow_pal(8), 
  main = "Example - custom gradient fill (NOT RECOMMEADED)"
)

## -----------------------------------------------------------------------------
barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = profinit_pal("blue-white-red")(8),
  main = "Base R - diverging fill"
)


# Create your own diverging palette based on Profinit colors
red_white_pink_pal <- grDevices::colorRampPalette(c(profinit_cols("red"), "white", profinit_cols("pink")))
barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = red_white_pink_pal(8),
  main = "Base R - custom diverging fill (NOT RECOMMANDED)"
)

## -----------------------------------------------------------------------------
barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = profinit_pal("discrete")(8),
  main = "Base R - discrete fill (exact)"
)

barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = profinit_pal("discrete", exact = FALSE)(8),
  main = "Base R - discrete fill (interpolated, NOT RECOMMANDED)"
)


barplot(
  height = sample_df$x, 
  names.arg = sample_df$category, 
  border = NA, 
  col = profinit_pal("discrete-full")(8),
  main = "Base R - discrete fill (full, exact)"
)

## -----------------------------------------------------------------------------
sessionInfo()

