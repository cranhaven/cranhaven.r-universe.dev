# kfa_constants — aboveR
# KyFromAbove S3 bucket paths, CRS, and phase metadata

#' @noRd
KFA_BUCKET <- "kyfromabove"
KFA_REGION <- "us-west-2"
KFA_BASE_URL <- "https://kyfromabove.s3.us-west-2.amazonaws.com"
KFA_CRS <- 3089L  # Kentucky Single Zone (FIPS:1600)

#' @noRd
KFA_PHASES <- list(
  "1" = list(dem_res_ft = 5, pointcloud_format = "laz", imagery_res = "6in"),
  "2" = list(dem_res_ft = 2, pointcloud_format = "copc", imagery_res = "6in"),
  "3" = list(dem_res_ft = 2, pointcloud_format = "copc", imagery_res = "3in")
)

#' @noRd
KFA_PRODUCTS <- list(
  dem = "elevation/DEM",
  pointcloud = "elevation/pointcloud",
  contours = "elevation/contours",
  spot_elevations = "elevation/spot_elevations",
  ortho = "imagery/orthos",
  oblique = "imagery/oblique"
)
