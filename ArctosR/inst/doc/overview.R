## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ArctosR)

## ----eval = FALSE-------------------------------------------------------------
#  # run the function and store results in an object
#  query_params <- get_query_parameters()
#  
#  # checking the dataframe obtained (showing only 6 rows and 3 columns)
#  query_params[1:6,1:3]
#  #>             display                obj_name category
#  #> 1     Verbatim Date           verbatim_date    event
#  #> 2 Collecting Method       collecting_method    event
#  #> 3 Collecting Source       collecting_source    event
#  #> 4        Ended Date              ended_date    event
#  #> 5  Event Attributes evtAttributeSearchTable    event
#  #> 6           Habitat                 habitat    event

## ----eval = FALSE-------------------------------------------------------------
#  # checking row 37 in the dataframe
#  query_params[37,1:5]
#  #>       display    obj_name   category subcategory
#  #> 37 Collection guid_prefix identifier       basic
#  #>                                                                description
#  #> 37 Collection responsible for the record. Turning this off will break most
#  #>    forms.

## ----eval = FALSE-------------------------------------------------------------
#  query_params[28,1:5]
#  #>    display obj_name       category subcategory
#  #> 28   Genus    genus identification  curatorial
#  #>                                               description
#  #> 28 Genus as provided in collection's preferred Source(s).
#  
#  query_params[23,1:5]
#  #>    display obj_name       category subcategory
#  #> 23 Species  species identification  curatorial
#  #>                                                            description
#  #> 23 Species (binomial) as provided in collection's preferred Source(s).

## ----eval = FALSE-------------------------------------------------------------
#  query <- get_records(guid_prefix = "MSB:Mamm", genus = "Canis",
#                       species = "lupus")

## ----eval = FALSE-------------------------------------------------------------
#  result_params <- get_result_parameters()
#  result_params[result_params$category == 'core',1:2]
#  #>                      display                      obj_name
#  #> 1  GUID (DarwinCore Triplet)                          guid
#  #> 7              Identified As               scientific_name
#  #> 43          Asserted Country                       country
#  #> 44   Asserted State/Province                    state_prov
#  #> 51         Specific Locality                 spec_locality
#  #> 57             Verbatim Date                 verbatim_date
#  #> 69          Decimal Latitude                       dec_lat
#  #> 70         Decimal Longitude                      dec_long
#  #> 71      Coordinate Error (m) coordinateuncertaintyinmeters

## ----eval = FALSE-------------------------------------------------------------
#  # making a list of additional columns to get (see get_query_parameters())
#  add_cols <- list("guid", "scientific_name", "relatedcatalogeditems", "collectors",
#                   "state_prov", "spec_locality", "dec_lat", "dec_long",
#                   "verbatim_date", "examined_for", "detected", "not_detected")
#  
#  # getting records with additional columns
#  query <- get_records(guid_prefix = "MSB:Mamm", genus = "Canis",
#                       species = "lupus", columns = add_cols)

## ----eval = FALSE-------------------------------------------------------------
#  # defining the columns to be obtained
#  some_cols <- list("guid", "parts", "partdetail")
#  
#  # performing the query
#  query <- get_records(guid_prefix = "MSB:Mamm", genus = "Canis",
#                       species = "lupus", columns = some_cols)

## ----eval = FALSE-------------------------------------------------------------
#  get_record_count(guid_prefix = "MSB:Mamm", genus = "Canis", species = "lupus")
#  #> [1] 1694
#  
#  query <- get_records(guid_prefix = "MSB:Mamm", genus = "Canis",
#                       species = "lupus", all_records = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  # a list of columns to download with the query
#  my_cols <- list("guid", "scientific_name", "parts", "collectors", "state_prov",
#                  "spec_locality", "dec_lat", "dec_long", "verbatim_date",
#                  "partdetail")
#  
#  # download records
#  query <- get_records(guid_prefix = "MSB:Mamm", genus = "Canis",
#                       species = "lupus", columns = my_cols)
#  
#  # getting only the dataframe of data
#  msb_wolves <- response_data(query)

## ----eval = FALSE-------------------------------------------------------------
#  # process the information in partdetail into sub-dataframes
#  expand_column(query, "partdetail")

## ----eval = FALSE-------------------------------------------------------------
#  save_response_csv(query, "msb_wolves.csv")

## ----eval = FALSE-------------------------------------------------------------
#  save_response_csv(query, "msb_wolves.csv", expanded = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  save_response_rds(query, "msb_wolves.rds")

## ----eval = FALSE-------------------------------------------------------------
#  save_response_csv(query, "msb_wolves2.csv", with_metadata = TRUE)

