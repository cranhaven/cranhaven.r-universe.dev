# Patch demo
require(colorpatch)
require(ggplot2)
require(grid)
require(gridExtra)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

data("GreenRedRGB")
data("OptimGreenRedLAB")

# if TRUE pdfs are created
doFile <- FALSE

try(dev.off(), silent=TRUE)

## Fig1 (random dataset)
if (doFile) {
  pdf("Fig1.pdf", height = 4)
}

set.seed(173)
dat <- CreateClusteredData(ncol.clusters = 3, nrow.clusters = 3, 
                           nrow = 25, ncol = 15, alpha = 50)
dat <- OrderData(dat)

thresh.ratio <- 0.5 * max(abs(dat$ratio))
thresh.conf <- 0.5 * max(dat$conf)
df <- ToDataFrame(dat)

p0 <- ggplot(df) + theme_colorpatch(plot.background = "white")
p0 <- p0 + stat_colorpatch(aes(ratio = ratio, conf = 1, x = x, y = y),
                           thresh.ratio = thresh.ratio,
                           color.fun = ColorPatchColorFun("GreenRedRGB"))
p0 <- p0 + coord_fixed(ratio = 1) + ggtitle("(a)")

p1 <- ggplot(df) + theme_colorpatch(plot.background = "white")
p1 <- p1 + stat_bicolor(aes(ratio = ratio, conf = conf, x = x, y = y),
                        thresh.ratio = thresh.ratio,
                        thresh.conf = thresh.conf)
p1 <- p1 + coord_fixed(ratio = 1) + ggtitle("(b)")

p2 <- ggplot(df) + theme_colorpatch(plot.background = "white")
p2 <- p2 + stat_colorpatch(aes(ratio = ratio, conf = conf, x = x, y = y),
                           thresh.ratio = thresh.ratio,
                           thresh.conf = thresh.conf)
p2 <- p2 + coord_fixed(ratio = 1) + ggtitle("(c)")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3),
                      gp = gpar(fill = "white", col = "white", lwd = 0)))
print(p0, vp = vplayout(1, 1))
print(p1, vp = vplayout(1, 2))
print(p2, vp = vplayout(1, 3))
popViewport()

if (doFile) {
  dev.off()
}

## Fig2
if (doFile) {
  pdf("Fig2.pdf", height = 3)
}

dat <- list(ratio = matrix(c(seq(1, 0.25, length.out = 4),
                             seq(-1, -0.25, length.out = 4)),
                           nrow = 2,
                           byrow = TRUE),
            conf = matrix(1, nrow = 2, ncol = 4))
df <- ToDataFrame(dat)
p1 <- ggplot(df) + theme_colorpatch("white") + coord_fixed(ratio = 1)
p1 <- p1 + stat_colorpatch(aes(ratio = ratio, conf = conf, x = x, y = y),
                           color.fun = ColorPatchColorFun("GreenRedRGB"))

p1 <- p1 + ggtitle("(a)")
p2 <- ggplot(df) + theme_colorpatch("white") + coord_fixed(ratio = 1)
p2 <- p2 + stat_colorpatch(aes(ratio = ratio, conf = conf, x = x, y = y),
                           color.fun = ColorPatchColorFun("OptimGreenRedLAB"))
p2 <- p2 + ggtitle("(b)")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2),
                      gp = gpar(fill = "white", col = "white", lwd = 0)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(1, 2))
popViewport()
if (doFile) {
  dev.off()
}

# Plot Uniformity
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1),
                       gp = gpar(fill = "black", col = "black", lwd = 0)))
p0 <- PlotUniformity(GreenRedRGB) + ggtitle("GreenRedRGB Uniformity")
p1 <- PlotUniformity(OptimGreenRedLAB) + ggtitle("OptimGreenRedLAB Uniformity")
print(p0, vp = vplayout(1, 1))
print(p1, vp = vplayout(2, 1))
popViewport()

# Plot Symmetry
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1),
                      gp = gpar(fill = "black", col = "black", lwd = 0)))
p0 <- PlotSymmetry(GreenRedRGB) + ggtitle("GreenRedRGB Symmetry")
p1 <- PlotSymmetry(OptimGreenRedLAB) + ggtitle("OptimGreenRedLAB Symmetry")
print(p0, vp = vplayout(1, 1))
print(p1, vp = vplayout(2, 1))
popViewport()

