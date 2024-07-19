## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")
import::from(ramify, mat)

vo <- mat("0, 0; 600, 0; 630, 580; 1200, 650; 1200, 920; 900, 920; 900, 845; 0, 845")
vo

vi <- mat("0, 300; 300, 300; 300, 695; 0, 695")
vi

# The following will plot both the original and the final (transformed plot)
SP <- secprop(outer = vo, inner = vi, original_plot = 1, final_plot = 1)
SP

## ----echo = FALSE, out.width = '100%'-----------------------------------------
linguisticsdown::include_graphics2("secprop_gnu-octave_figure.png")

