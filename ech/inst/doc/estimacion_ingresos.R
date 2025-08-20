## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 400
)

options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
tail(ech::ipc_base2010)

## ----eval = FALSE-------------------------------------------------------------
#  library(ech)
#  df <- get_microdata(year = 2019, folder = tempdir(), toR = FALSE)
#  #df <- organize_names(df)

## ----eval = FALSE-------------------------------------------------------------
#  df <- income_constant_prices(data = df, base_month = 1, base_year = 2005, index = "IPC", level = "G")

## ----eval = FALSE-------------------------------------------------------------
#  get_estimation_mean(df, variable = "y_pc_d", level = "i", ids = "upm", estrato = "estrato")

## ----eval = FALSE-------------------------------------------------------------
#  # Estimación de ingresos promedio per cápita a pesos constantes de ene/05 según dpto
#  get_estimation_mean(df, variable = "y_pc_d", by.x = "nomdpto", level = "i", ids = "upm", estrato = "estrato")

