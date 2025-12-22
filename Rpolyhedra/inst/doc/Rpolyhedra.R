## ----setup, include=FALSE-----------------------------------------------------
library(rgl)
library(Rpolyhedra)
library(dplyr)
setupKnitr()

## ----availablePolyhedra-------------------------------------------------------
#show only the first 10 polyhedra.
head(getAvailablePolyhedra(), n = 10)

## ----getPolyhedron------------------------------------------------------------
cube <- getPolyhedron(source = "netlib", polyhedron.name = "cube")

## ----demo, webgl=TRUE---------------------------------------------------------
# 1.  Obtain 5 regular solids
polyhedra.2.draw <- getAvailablePolyhedra(source = "netlib")
polyhedra.2.draw <- polyhedra.2.draw %>%
                        filter(scraped.name %in%
                            c("tetrahedron", "octahedron", "cube",
                               "icosahedron", "dodecahedron"))

# 2. Setup colors and scales
n <- nrow(polyhedra.2.draw)
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5

# 3. Open and setup RGL window
open3d()
par3d(FOV = 1)
rgl.bg( sphere =FALSE, fogtype = "none", color=c("black"))
rgl.viewpoint(theta = 0, phi=0, zoom=0.8, fov=1)

# 4. For each polyhedron, setup rotation, position and render
for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.2.draw[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)

  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)

  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}

