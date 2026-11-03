library(qrfactor)
## Tests for the `rotation` argument (varimax rotation of the retained factors).
## Data must be numeric.
data(UScereal, package = "MASS")
variables <- c("calories","protein","sodium","carbo","sugars","potassium")
d <- UScereal[variables]

## rotation = TRUE rotates (varimax); the default is the classical solution
mR <- qrfactor(d, rotation = TRUE)
mN <- qrfactor(d)

## 1. tags (default is unrotated)
stopifnot(identical(mR$rotation, "varimax"))
stopifnot(identical(mN$rotation, "none"))
stopifnot(identical(qrfactor(d, rotation = "varimax")$rotation, "varimax"))
stopifnot(identical(qrfactor(d, rotation = "none")$rotation, "none"))

## 2. rotation = FALSE reproduces the classical (unrotated) loadings exactly,
##    and the unrotated copy stored on the rotated object matches it
stopifnot(max(abs(mR$unrotated.r.loading - mN$r.loading)) < 1e-10)
stopifnot(max(abs(mR$unrotated.q.loading - mN$q.loading)) < 1e-10)

## 3. rotation does not touch the eigen-decomposition, correlation or PCA
stopifnot(max(abs(mR$correlation  - mN$correlation))  < 1e-12)
stopifnot(max(abs(mR$eigen.value  - mN$eigen.value))  < 1e-12)
stopifnot(max(abs(mR$eigen.vector - mN$eigen.vector)) < 1e-12)
stopifnot(max(abs(mR$pca          - mN$pca))          < 1e-12)

## 4. varimax is orthogonal: row communalities of the rotated block are preserved
comm_rot   <- rowSums(mR$r.loading[, 1:2]^2)
comm_unrot <- rowSums(mN$r.loading[, 1:2]^2)
stopifnot(max(abs(comm_rot - comm_unrot)) < 1e-8)

## 5. total variance of the rotated block is preserved; cumvariance still hits 100
stopifnot(abs(sum(mR$r.loading[, 1:2]^2) - sum(mN$r.loading[, 1:2]^2)) < 1e-8)
stopifnot(abs(utils::tail(mR$cumvariance, 1) - 100) < 1e-6)

## 6. rotation actually changed the loadings
stopifnot(max(abs(mR$r.loading[, 1:2] - mN$r.loading[, 1:2])) > 1e-6)

cat("rotation.R: all checks passed\n")
