## ----include = FALSE----------------------------------------------------------
can_decrypt <- httr2::secret_has_key('KHIS_KEY')

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE,
  purl = can_decrypt,
  eval = can_decrypt
)
options(tibble.print_min = 5L, tibble.print_max = 5L)

## ----eval = !can_decrypt, echo = FALSE, comment = NA--------------------------
#  message("No token available. Code chunks will not be evaluated.")

## ----eval = can_decrypt, include=FALSE----------------------------------------
khisr:::khis_cred_docs()

## ----include=FALSE------------------------------------------------------------
library(khisr)

## ----eval = FALSE-------------------------------------------------------------
#  # Set the credentials using username and password
#  khis_cred(username = 'your-dhis2-username', password = 'your-dhis2-password', base_url = 'https://<your dhis2 instance>')
#  
#  # Set credentials using configuration path
#  khis_cred(config_path = 'path/to/secret.json')

## ----eval = can_decrypt-------------------------------------------------------
# Retrieve organisation units by county (level 2)
county <- get_organisation_units(level %.eq% '2')
county

# Retrieve county by name (Mombasa)
county <- get_organisation_units(level %.eq% '2',
                                 name %.like% 'mombasa')
county

data_element_id <- c('cXe64Yk0QMY', 'XEX93uLsAm2')

# Retrieve data elements by ID using operator in
data_elements <- get_data_elements(id %.in% data_element_id)
data_elements

# Retrieve data elements by filtering using dataElementGroups
data_elements <- get_data_elements(dataElementGroups.name %.like% 'moh 705')
data_elements

## ----eval = can_decrypt-------------------------------------------------------
# To include a list dimensions for data elements id, dataset ids
dx %.d% c('dimension-id-1', 'dimension-id-2')

pe %.d% 'LAST_YEAR'

ou %.d% 'USER_ORGUNIT'

# showing in the analytics
get_analytics(
    dx %.d% c('siOyOiOJpI8', 'Lt0FqtnHraW', 'OoakJhWiyZp'),
    pe %.d% 'LAST_YEAR',
    ou %.d% c('qKzosKQPl6G')
)

# Using the startDate and endDate with organisation unit keyword 'USER_ORGUNIT'
get_analytics(
    dx %.d% c('siOyOiOJpI8', 'Lt0FqtnHraW', 'OoakJhWiyZp'),
    ou %.d% 'USER_ORGUNIT',
    pe %.d% 'all',
    startDate = '2023-07-01',
    endDate = '2023-12-31'
)

## ----eval = can_decrypt-------------------------------------------------------
# Filter by period
pe %.f% 'LAST_YEAR'

# Filter by organisation unit
ou %.f% 'USER_ORGUNIT'

# showing in the analytics. filter by organisation unit with id 'qKzosKQPl6G'
# and period 'LAST_YEAR'
get_analytics(
    dx %.d% c('siOyOiOJpI8', 'Lt0FqtnHraW', 'OoakJhWiyZp'),
    pe %.f% 'LAST_YEAR',
    ou %.f% 'qKzosKQPl6G'
)

