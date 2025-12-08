## ---- echo = FALSE, include = FALSE, message = FALSE--------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  dpi = 120,
  comment = "#>"
)


## ----setup, warning = FALSE---------------------------------------------------
library(amerifluxr)
library(data.table)
library(pander)

## ----echo=TRUE, results = "asis"----------------------------------------------
# get a full list of sites with general info
sites <- amf_site_info()
sites_dt <- data.table::as.data.table(sites)

pander::pandoc.table(sites_dt[c(1:3), ])


## ----results = "asis"---------------------------------------------------------
# total number of registered sites
pander::pandoc.table(sites_dt[, .N])

# total number of sites with available data
pander::pandoc.table(sites_dt[!is.na(DATA_START), .N])

# get number of sites with available data, grouped by data use policy
pander::pandoc.table(sites_dt[!is.na(DATA_START), .N, by = .(DATA_POLICY)])


## ----results = "asis"---------------------------------------------------------
# get a summary table of sites grouped by IGBP
pander::pandoc.table(sites_dt[, .N, by = "IGBP"])

# get a summary table of sites with available data, & grouped by IGBP
pander::pandoc.table(sites_dt[!is.na(DATA_START), .N, by = "IGBP"])

# get a summary table of sites with available data, 
#  & grouped by data use policy & IGBP
pander::pandoc.table(sites_dt[!is.na(DATA_START), .N, by = .(IGBP, DATA_POLICY)][order(IGBP)])


## ----results = "asis"---------------------------------------------------------

# get a list of cropland and grassland sites with available data,
#  shared under CC-BY-4.0 data policy,
#  located within 30-50 degree N in latitude,
# returned a site list with site ID, name, data starting/ending year
crop_ls <- sites_dt[IGBP %in% c("CRO", "GRA") &
                      !is.na(DATA_START) &
                      LOCATION_LAT > 30 &
                      LOCATION_LAT < 50 &
                      DATA_POLICY == "CCBY4.0",
                    .(SITE_ID, SITE_NAME, DATA_START, DATA_END)]
pander::pandoc.table(crop_ls[c(1:10),])


## ----results = "asis"---------------------------------------------------------
# get data availability for selected sites
metadata_aval <- data.table::as.data.table(amf_list_metadata())
pander::pandoc.table(metadata_aval[c(1:3), c(1:10)])

## ----results = "asis"---------------------------------------------------------
metadata_aval_sub <- as.data.table(amf_list_metadata(site_set = crop_ls$SITE_ID))

# down-select cropland & grassland sites by interested BADM group,
#  e.g., canopy height (GRP_HEIGHTC)
crop_ls2 <- metadata_aval_sub[GRP_HEIGHTC > 0, .(SITE_ID, GRP_HEIGHTC)][order(-GRP_HEIGHTC)]
pander::pandoc.table(crop_ls2[c(1:10), ])

## ----results = "asis"---------------------------------------------------------
# get data availability for selected sites
data_aval <- data.table::as.data.table(amf_list_data(site_set = crop_ls2$SITE_ID))
pander::pandoc.table(data_aval[c(1:10), ])

## ----results = "asis"---------------------------------------------------------
# down-select cropland & grassland sites based on the available wind speed (WS) and 
# friction velocity (USTAR) data in 2015-2018, regardless their qualifiers
data_aval_sub <- data_aval[data_aval$BASENAME %in% c("WS","USTAR"),
                           .(SITE_ID, BASENAME, Y2015, Y2016, Y2017, Y2018)]

# calculate mean availability of WS and USTAR in each site and each year
data_aval_sub <- data_aval_sub[, lapply(.SD, mean), 
                               by = .(SITE_ID),
                               .SDcols = c("Y2015", "Y2016", "Y2017", "Y2018")]

# sub-select sites that have WS and USTAR data for > 75%
#  during 2015-2018
crop_ls3 <- data_aval_sub[(Y2015 + Y2016 + Y2017 + Y2018) / 4 > 0.75]
pander::pandoc.table(crop_ls3)


## ----results = "asis"---------------------------------------------------------

# down-select cropland & grassland sites by available wind speed (WS) data,
#  mean availability of WS during 2015-2018
data_aval_sub2 <- data_aval[data_aval$BASENAME %in% c("WS"),
                            .(SITE_ID, VARIABLE, Y2015_2018 = (Y2015 + Y2016 + Y2017 + Y2018)/4)]

# calculate number of WS variables per site, for sites that 
#  have any WS data during 2015-2018
data_aval_sub2 <- data_aval_sub2[Y2015_2018 > 0, .(.N, Y2015_2018 = mean(Y2015_2018)), .(SITE_ID)]
pander::pandoc.table(crop_ls4 <- data_aval_sub2[N > 1, ])


## ----eval = FALSE-------------------------------------------------------------
#  #### not evaluated so to reduce vignette size
#  # plot data availability for WS & USTAR
#  #  for selected sites in 2015-2018
#  amf_plot_datayear(
#    site_set = crop_ls4$SITE_ID,
#    var_set = c("WS", "USTAR"),
#    nonfilled_only = TRUE,
#    year_set = c(2015:2018)
#  )
#  

## ----results = "asis"---------------------------------------------------------
## get data summary for selected sites & variables
data_sum <- amf_summarize_data(site_set = crop_ls3$SITE_ID,
                     var_set = c("WS", "USTAR"))
pander::pandoc.table(data_sum[c(1:10), ])


## ----eval = FALSE-------------------------------------------------------------
#  #### not evaluated so to reduce vignette size
#  ## plot data summary of USTAR for selected sites,
#  amf_plot_datasummary(
#    site_set = crop_ls3$SITE_ID,
#    var_set = c("USTAR")
#  )
#  

## ----eval = FALSE-------------------------------------------------------------
#  #### not evaluated so to reduce vignette size
#  ## plot data summary of WS for selected sites,
#  #  including clustering information
#  amf_plot_datasummary(
#    site_set = crop_ls3$SITE_ID,
#    var_set = c("WS"),
#    show_cluster = TRUE
#  )
#  

