## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
eval_chunks <- curl::has_internet()

## ----listing-products, message=FALSE, eval=eval_chunks------------------------
library(CopernicusMarine)

cms_products_list() |> head(3)

cms_products_list2() |> head(3)

## ----searching-products, message=FALSE, eval=eval_chunks----------------------
cms_products_list(freeText = "wave",
                  facetValues = list(areas             = list("Europe"),
                                     specificVariables = list("Velocity")))

## ----product-details, eval=eval_chunks----------------------------------------
cms_product_details("GLOBAL_ANALYSISFORECAST_PHY_001_024") |> summary()

## ----product-metadata, eval=eval_chunks---------------------------------------
meta_info <- cms_product_metadata("GLOBAL_ANALYSISFORECAST_PHY_001_024")

## Get the dimension properties for the first layer in this product
meta_info$properties[[1]]$`cube:dimensions` |> summary()
## Get the variable properties for the first layer in this product
meta_info$properties[[1]]$`cube:variables` |> summary()

## ----product-proxy, warning=FALSE, eval=eval_chunks&&CopernicusMarine:::has_blosc----
library(stars) |> suppressMessages()

myproxy <- cms_zarr_proxy(
    product       = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
    layer         = "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m",
    asset         = "timeChunked")
st_dimensions(myproxy)

## ----services, eval=eval_chunks-----------------------------------------------
cms_product_services("GLOBAL_ANALYSISFORECAST_PHY_001_024")

