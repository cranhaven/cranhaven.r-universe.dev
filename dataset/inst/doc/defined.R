## ----setupdefinedvignette, include = FALSE------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)
data("gdp")

## ----gdp1---------------------------------------------------------------------
gdp_1 <- defined(
  gdp$gdp,
  label = "Gross Domestic Product",
  unit = "CP_MEUR",
  concept = "http://data.europa.eu/83i/aa/GDP"
)

## ----seeattributes------------------------------------------------------------
attributes(gdp_1)

## ----convenience--------------------------------------------------------------
cat("Get the label only: ", var_label(gdp_1), "\n")
cat("Get the unit only: ", var_unit(gdp_1), "\n")
cat("Get the concept definition only: ", var_concept(gdp_1), "\n")
cat("All attributes:\n")

## ----printdefined-------------------------------------------------------------
print(gdp_1)

## ----summarydefined-----------------------------------------------------------
summary(gdp_1)

## ----ambiguous----------------------------------------------------------------
gdp_2 <- defined(
  c(2523.6, 2725.8, 3013.2),
  label = "Gross Domestic Product"
)

## ----notevaluatedc, eval=FALSE------------------------------------------------
# c(gdp_1, gdp_2)

## ----gpd2---------------------------------------------------------------------
var_unit(gdp_2) <- "CP_MEUR"

## ----vardef2------------------------------------------------------------------
var_concept(gdp_2) <- "http://data.europa.eu/83i/aa/GDP"

## ----c------------------------------------------------------------------------
new_gdp <- c(gdp_1, gdp_2)
summary(new_gdp)

## ----country------------------------------------------------------------------
country <- defined(
  c("AD", "LI", "SM"),
  label = "Country name",
  concept = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
  namespace = "https://www.geonames.org/countries/$1/"
)

## ----shownamespace------------------------------------------------------------
var_namespace(country)

## ----characters---------------------------------------------------------------
countries <- defined(
  c("AD", "LI"),
  label = "Country code",
  namespace = "https://www.geonames.org/countries/$1/"
)

countries
as_character(countries)

## ----subsettingmethods--------------------------------------------------------
gdp_1[1:2]
gdp_1[gdp_1 > 5000]

## ----coerctionmethods---------------------------------------------------------
as.vector(gdp_1)
as.list(gdp_1)

## ----coerce-char--------------------------------------------------------------
as_character(country)
as_character(c(gdp_1, gdp_2))

## ----coerce-factor------------------------------------------------------------
as_factor(country)

## ----coerce-num---------------------------------------------------------------
as_numeric(c(gdp_1, gdp_2))

## ----coerce-logical-----------------------------------------------------------
flag <- defined(c(TRUE, FALSE, TRUE), label = "Example flag")
as_logical(flag)

## ----coerce-date--------------------------------------------------------------
dates <- defined(
  as.Date(c("2020-01-01", "2020-01-02")),
  label = "Reference date"
)
as.Date(dates)

## ----coerce-posix-------------------------------------------------------------
times <- defined(
  as.POSIXct(c("2020-01-01 12:00:00", "2020-01-01 18:00:00")),
  label = "Timestamp"
)

times 

