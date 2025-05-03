#'Generate Climate Color Palettes
#'
#'Generates a colorblind friendly color palette with color ranges useful in 
#'climate temperature variable plotting.
#'
#'@param palette Which type of palette to generate: from blue through white 
#'  to red ('bluered'), from red through white to blue ('redblue'), from 
#'  yellow through orange to red ('yellowred'),  from red through orange to
#'  red ('redyellow'), from purple through white to orange ('purpleorange'),
#'  and from orange through white to purple ('orangepurple').
#'@param n Number of colors to generate.
#'
#'@examples
#'lims <- seq(-1, 1, length.out = 21)
#'
#'ColorBar(lims, color_fun = clim.palette('redyellow'))
#'
#'cols <- clim.colors(20)
#'ColorBar(lims, cols)
#'
#'@rdname clim.palette
#'@importFrom grDevices colorRampPalette
#'@export
clim.palette <- function(palette = "bluered") {
  if (palette == "bluered") {
    colorbar <- colorRampPalette(rev(c("#67001f", "#b2182b", "#d6604d",
                                       "#f4a582", "#fddbc7", "#f7f7f7", 
                                       "#d1e5f0", "#92c5de", "#4393c3", 
                                       "#2166ac", "#053061")))
    attr(colorbar, 'na_color') <- 'pink'
  } else if (palette == "redblue") {
    colorbar <- colorRampPalette(c("#67001f", "#b2182b", "#d6604d",
                                   "#f4a582", "#fddbc7", "#f7f7f7", 
                                   "#d1e5f0", "#92c5de", "#4393c3", 
                                   "#2166ac", "#053061"))
    attr(colorbar, 'na_color') <- 'pink'
  } else if (palette == "yellowred") {
    colorbar <- colorRampPalette(c("#ffffcc", "#ffeda0", "#fed976",
                                   "#feb24c", "#fd8d3c", "#fc4e2a",
                                   "#e31a1c", "#bd0026", "#800026"))
    attr(colorbar, 'na_color') <- 'pink'
  } else if (palette == "redyellow") {
    colorbar <- colorRampPalette(rev(c("#ffffcc", "#ffeda0", "#fed976",
                                       "#feb24c", "#fd8d3c", "#fc4e2a",
                                       "#e31a1c", "#bd0026", "#800026")))
    attr(colorbar, 'na_color') <- 'pink'
  } else if (palette == "purpleorange") {
    colorbar <- colorRampPalette(c("#2d004b", "#542789", "#8073ac",
				   "#b2abd2", "#d8daeb", "#f7f7f7",
				   "#fee0b6", "#fdb863", "#e08214", 
				   "#b35806", "#7f3b08"))
    attr(colorbar, 'na_color') <- 'pink'
  } else if (palette == "orangepurple") {
    colorbar <- colorRampPalette(rev(c("#2d004b", "#542789", "#8073ac",
				       "#b2abd2", "#d8daeb", "#f7f7f7",
				       "#fee0b6", "#fdb863", "#e08214",
				       "#b35806", "#7f3b08")))
    attr(colorbar, 'na_color') <- 'pink'
  } else {
    stop("Parameter 'palette' must be one of 'bluered', 'redblue', 'yellowred'",
	 "'redyellow', 'purpleorange' or 'orangepurple'.")
  }
  colorbar
}
#'@rdname clim.palette
#'@export
clim.colors <- function(n, palette = "bluered") {
  clim.palette(palette)(n)
}
