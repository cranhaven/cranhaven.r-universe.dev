## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE---------------------------------------------------
library(amerifluxr)
library(pander)

## ----eval = FALSE-------------------------------------------------------------
#  ## When running, replace user_id and user_email with a real AmeriFlux account
#  floc2 <- amf_download_base(
#    user_id = "my_user",
#    user_email = "my_email@mail.com",
#    site_id = "US-CRT",
#    data_product = "BASE-BADM",
#    data_policy = "CCBY4.0",
#    agree_policy = TRUE,
#    intended_use = "other",
#    intended_use_text = "amerifluxr package demonstration",
#    verbose = TRUE,
#    out_dir = tempdir()
#  )
#  

## ----eval = FALSE-------------------------------------------------------------
#  ## When running, replace user_id and user_email with a real AmeriFlux account
#  floc1 <- amf_download_bif(
#    user_id = "my_user",
#    user_email = "my_email@mail.com",
#    data_policy = "CCBY4.0",
#    agree_policy = TRUE,
#    intended_use = "other",
#    intended_use_text = "amerifluxr package demonstration",
#    out_dir = tempdir(),
#    verbose = TRUE,
#    site_w_data = TRUE
#  )
#  

## ----results = 'hide'---------------------------------------------------------
# An example of BASE zipped files downloaded for US-CRT site
floc2 <- system.file("extdata", "AMF_US-CRT_BASE-BADM_2-5.zip", package = "amerifluxr")

# An example of unzipped BASE files from the above zipped file
floc3 <- system.file("extdata", "AMF_US-CRT_BASE_HH_2-5.csv", package = "amerifluxr")

# An example of all sites' BADM data
floc1 <- system.file("extdata", "AMF_AA-Flx_BIF_CCBY4_20201218.xlsx", package = "amerifluxr")


## ----results = "asis"---------------------------------------------------------
# read the BASE from a zip file, without additional parsed time-keeping columns
base1 <- amf_read_base(
  file = floc2,
  unzip = TRUE,
  parse_timestamp = FALSE
)
pander::pandoc.table(base1[c(1:3),])

# read the BASE from a csv file, with additional parsed time-keeping columns
base2 <- amf_read_base(
  file = floc3,
  unzip = FALSE,
  parse_timestamp = TRUE
)
pander::pandoc.table(base2[c(1:3), c(1:10)])

## ----results = "asis"---------------------------------------------------------
# get a list of latest base names and units. 
FP_ls <- amf_variables()
pander::pandoc.table(FP_ls[c(11:20), ])

## ----results = "asis"---------------------------------------------------------
# parse the variable name
basename_decode <- amf_parse_basename(var_name = colnames(base1))
pander::pandoc.table(basename_decode[c(1, 2, 3, 4, 6, 11, 12),])

## -----------------------------------------------------------------------------
# filter data, using default physical range +/- 5% buffer
base_f <- amf_filter_base(data_in = base1)


## ----results = "asis"---------------------------------------------------------
# obtain the latest measurement height information
var_info <- amf_var_info()

# subset the variable by target Site ID
var_info <- var_info[var_info$Site_ID == "US-CRT", ]
pander::pandoc.table(var_info[c(1:10), ])

## ----results = "asis"---------------------------------------------------------
# read the BADM BIF file, using an example data file
bif <- amf_read_bif(file = floc1)

# subset by target Site ID
bif <- bif[bif$SITE_ID == "US-CRT", ]
pander::pandoc.table(bif[c(1:15), ])

# get a list of all BADM variable groups and variables
unique(bif$VARIABLE_GROUP)
length(unique(bif$VARIABLE))


## ----results = "asis"---------------------------------------------------------
# extract the FLUX_MEASUREMENTS group
bif_flux <- amf_extract_badm(bif_data = bif, select_group = "GRP_FLUX_MEASUREMENTS")
pander::pandoc.table(bif_flux)

# extract the HEIGHTC (canopy height) group
bif_hc <- amf_extract_badm(bif_data = bif, select_group = "GRP_HEIGHTC")
pander::pandoc.table(bif_hc)

## ----fig.width = 7------------------------------------------------------------
# convert HEIGHTC_DATE to POSIXlt
bif_hc$TIMESTAMP <- strptime(bif_hc$HEIGHTC_DATE, format = "%Y%m%d", tz = "GMT")

# convert HEIGHTC column to numeric
bif_hc$HEIGHTC <- as.numeric(bif_hc$HEIGHTC)

# plot time series of canopy height
plot(bif_hc$TIMESTAMP, bif_hc$HEIGHTC, xlab = "TIMESTAMP", ylab = "canopy height (m)")


## ----results = "asis"---------------------------------------------------------
# get a list of contacts
bif_contact <- amf_extract_badm(bif_data = bif, select_group = "GRP_TEAM_MEMBER")
pander::pandoc.table(bif_contact)

# get data DOI
bif_doi <- amf_extract_badm(bif_data = bif, select_group = "GRP_DOI")
pander::pandoc.table(bif_doi)

