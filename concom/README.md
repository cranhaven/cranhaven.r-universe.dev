# concom

<!-- badges: start -->
[![R-CMD-check](https://github.com/stla/concom/workflows/R-CMD-check/badge.svg)](https://github.com/stla/concom/actions)
<!-- badges: end -->

Fast computation of the connected components of an undirected graph.

```r
f <- function(x, y, z, a, cosb, sinb){
    (sqrt((sqrt(x*x + (y*sinb + a*cosb)^2) - 2)^2) - 1)^2 +
      (sqrt((sqrt(z*z + (y*cosb - a*sinb)^2) - 2)^2) - 1)^2
}
a <- 0.6
b <- 0.785
cosb <- cos(b)
sinb <- sin(b)

x <- z <- seq(-3.5, 3.5, len = 150L)
y <- seq(-4.2, 4.2, len = 150L)
g <- expand.grid(X = x, Y = y, Z = z)
voxel <- array(
  with(g, f(X, Y, Z, a, cosb, sinb)),
  dim = c(150L, 150L, 150L)
)

library(rmarchingcubes)
contour_shape <- contour3d(
  griddata = voxel,
  level = 0.1,
  x = x,
  y = y,
  z = z
)

library(rgl)
tmesh <- tmesh3d(
  vertices = t(contour_shape[["vertices"]]),
  indices = t(contour_shape[["triangles"]]),
  normals = contour_shape[["normals"]],
  homogeneous = FALSE
)
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
shade3d(tmesh, color = "orangered")
```

![](https://raw.githubusercontent.com/stla/concom/master/inst/images/ICN5D_orange.gif)

```r
library(concom)
meshes <- concom3d(tmesh)

colors <- hcl.colors(length(meshes))
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
for(i in 1L:length(meshes)){
  shade3d(meshes[[i]], color = colors[i])
}
```

![](https://raw.githubusercontent.com/stla/concom/master/inst/images/ICN5D_viridis.gif)
