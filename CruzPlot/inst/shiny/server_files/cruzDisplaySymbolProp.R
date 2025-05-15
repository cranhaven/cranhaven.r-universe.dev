# cruzDisplaySymbolProp for CruzPlot; modified version of display.codes()
#   Creates a display of options for symbols, colors, line types, typefaces, and fonts

cruzDisplaySymbolProp <- reactive({
  input$display_redraw

  plot.max <- length(symbol.col) + length(symbol.col.gray) + 3
  plot.h1 <- plot.max - 1.5 + 1
  plot.h2 <- plot.max - 1.75 + 1
  plot.h3 <- plot.max - 1.85 + 1
  plot.h4 <- plot.max - 2 + 1
  plot.h5 <- plot.max - 3 + 1
  plot.h6 <- plot.max - 14 + 1

  plot.gs <- plot.h3 - 1.15 - length(symbol.col)

  oldpar <- par(mar = rep(1, 4), family = "sans")
  plot(c(0, 1), c(0, plot.max), type = "n", axes = FALSE, bty = "n")

  # Top labels
  text(c(0.05, 0.32, 0.7, 0.95), rep(plot.h1, 4), c("Symbols", "Colors", "Line Types", "Widths"), cex = 1.4)
  #text(c(0.05,0.3,0.7,0.95),rep(26.5,4),c("(pch= )","(col= )","(lty= )","(lwd= )"),cex=1.3)

  # Symbols
  for(i in 0:20) {
    text(0.02, plot.h5 - (i * 1.25), i, cex = 1)
    points(.08, plot.h5 - (i * 1.25), pch = i, cex = 1.8)
  }

  # Colors used in CruzPlot
  for(i in 1:length(symbol.col)) {
    text(0.33, plot.h3 - (i + .25), symbol.col[i], cex = 1, pos = 2)
    points(.35, plot.h2 - (i + .25), pch = 15, cex = 2.1, col = symbol.col.code[i])
  }
  points(0.35, (plot.h2 - (length(symbol.col) + .25)), pch = 0, cex = 2.0) #puts box around white
  text(0.24, plot.max-13, "Color", srt = 90, cex = 1.2, pos = 4)

  # Greyscale palette
  palette(gray((0:5) / 5))
  for(i in 1:6) {
    text(0.33, plot.gs - i, symbol.col.gray[i], cex = 1, pos = 2)
    points(0.35, plot.gs - i + 0.1, pch = 15, cex = 2.1, col = symbol.col.code.gray[i])
  }
  points(0.35, plot.gs - 6 + 0.11, pch = 0, cex = 2.0) #puts box around white
  text(0.24, 0.1, "Gray Scale", srt = 90, cex = 1.2, pos = 4)

  # Line types
  for(i in 1:6) {
    text(0.55, plot.h5 - i, i, cex = 1.1)
    lines(c(0.6, 0.7, 0.85), c(plot.h5 - i, plot.h4 - i, plot.h4 - i), lty = i)}
  # line widths
  for(i in 1:6) {
    lines(c(0.9, 1), c(plot.h4 - i, plot.h4 - i), lwd = i)
  }

  # Typefaces
  text(0.75, plot.h6, "Fonts", cex = 1.4)
  plot.h6b <- plot.h6 + 0.5
  for(i in 1:3) {
    i.m <- i * 1.5
    text(0.75, plot.h6b - 0.0 - (i.m * 2.3), names(font.family)[i], cex = 1.2, adj = 0.5, family = font.family.vals[i])
    text(0.75, plot.h6b - 0.9 - (i.m * 2.3), paste(LETTERS, collapse = ""), cex = 1, adj = 0.5, family = font.family.vals[i])
    text(0.75, plot.h6b - 1.8 - (i.m * 2.3), paste(c(letters, " ", 0:9), collapse = ""), cex = 1, adj = 0.5, family = font.family.vals[i])
  }

  palette("default")
  par(oldpar)
})
