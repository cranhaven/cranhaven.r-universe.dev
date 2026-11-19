## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(upload.fun = identity)
eval_chunks <-
  CopernicusMarine:::has_blosc &&
  curl::has_internet() &&
  CopernicusMarine::cms_get_password() != "" &&
  any(sf::st_drivers("raster", "^HDF5$")$vsi)

## ----translate, eval=eval_chunks----------------------------------------------
library(CopernicusMarine)
## Example of command line code
## copied from website:
cli_code <-
"copernicusmarine subset
  --dataset-id cmems_mod_glo_phy_anfc_0.083deg_PT1H-m
  --variable uo
  --variable vo
  --start-datetime 2025-01-01T00:00:00
  --end-datetime 2025-01-01T23:00:00
  --minimum-longitude -2
  --maximum-longitude 8
  --minimum-latitude 52
  --maximum-latitude 59
  --minimum-depth 0.49402499198913574
  --maximum-depth 0.49402499198913574"

translated <- cms_translate(cli_code)
summary(translated)

## ----download-translation, eval=eval_chunks, echo=eval_chunks, message=FALSE, fig.width=4, fig.height=4, fig.alt="Data downloaded using translated query code"----
result <- do.call(cms_download_subset, translated)
plot(result, col = hcl.colors(100), axes = TRUE)

