# Values used in CruzPlot shiny app

#------------------------------------------------------------------------------
# Countries to be removed for world2 map
# Reference: http://www.codedisqus.com/0yzeqXgekP/plot-map-of-pacific-with-filled-countries.html
remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali", "Burkina Faso", "Ghana", "Togo")
mapnames <- map("world2", fill = TRUE, plot = FALSE)$names
mapnames.hires <- map("world2Hires", fill = TRUE, plot = FALSE)$names
regions.rm <- mapnames[!(mapnames %in% remove)]
regions.rm.hires <- mapnames.hires[!(mapnames.hires %in% remove)]

bathy.col <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")


#MUST BE UPDATED IF TURTLE CODES IN SpCodes.dat ARE CHANGED
# NOTE: Assumed less likely to have new turtle codes added than mammal codes,
#   so turtle codes are hardcoded so that app can split up codes into mammal and turtle categories
turtle.codes <- c("CC", "CM", "DC", "EI", "HT", "LK", "LV", "ND", "UH", "UT")


#------------------------------------------------------------------------------
# Formatting options used in CruzPlot

# Ordered 1) by like color and 2) so that detailed eff default is as desired
cruz.palette.color <- list(
  "Black" = "black", "Purple" = "purple", "Light purple" = "darkorchid1",
  "Dark blue" = "darkblue", "Blue" = "blue",
  "Sky blue" = "dodgerblue2", "Light blue" = "lightblue",
  "Green" = "forestgreen", "Light green" = "green", "Aqua" = "aquamarine2", "Yellow" = "yellow",
  "Orange" = "orange",
  "Brown" = "tan4", "Light brown" = "wheat3", "Tan" = "bisque1",
  "Pink" = "hotpink", "Light red" = "indianred2", "Red" = "red", "Dark red" = "red4",
  "Gray" = "gray", "White" = "white"
)

cruz.palette.gray <- list(
  "Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
  "Gray" = 4, "Light Gray" = 5, "White" = 0
)

cruz.symbol.type <- list(
  "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3,
  "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7,
  "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
  "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13,
  "14: Square with Up Triangle" = 14, "15: Filled Square" = 15,
  "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
  "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
)

cruz.line.type <- list(
  "Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4,
  "Long dash" = 5, "Dot-long dash" = 6
)

cruz.beaufort <- list(
  "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4,
  "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9
)

font.family <- list("Sans" = 1, "Serif" = 2, "Mono" = 3)
font.family.vals <- tolower(names(font.family))

# DAS data-symbol property text inputs
symbol.col <- names(cruz.palette.color)
symbol.col.code <- unname(unlist(cruz.palette.color))
symbol.col.gray <- names(cruz.palette.gray)
symbol.col.code.gray <- unname(unlist(cruz.palette.gray))

# Effort by Beaufrot default
eff.bft.default <- c(
  "darkblue", "dodgerblue2", "forestgreen", "green", "orange", "wheat3",
  "hotpink", "indianred2", "red", "red4"
)


#------------------------------------------------------------------------------
