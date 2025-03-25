library(gyro)

pt <- function(x){
  c(
    sin(x) * cos(2 * x),
    sin(x) * sin(2 * x),
    cos(x)
  )
}
pts <- t(vapply(seq(0, pi, length.out = 50), pt, numeric(3L)))

open3d(windowRect = c(50, 50, 562, 562))
view3d(zoom = 0.9)
plotGyrohull3d(pts, s = 0.8, tubesRadius = 0.02, spheresRadius = 0.04)
