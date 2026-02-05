#' dataset1: Documentation of data file - ecodata.rda
#'
#' @description This data file is consider as raw data file after merging and removing duplicate records of all data sources. e.g. this file is an output of occurrence records of mollusc species "Mexacanthina lugubris" with all modern records extracted from GBIF, OBIS, IDIGBIO and InvertEBase
#'
#' @format A data frame with 1115 rows and 19 variables:
#' \describe{
#'   \item{X}{index}
#'   \item{basisOfRecord}{Type of record (e.g., preserved specimen, fossil)}
#'   \item{occurrenceStatus}{Presence or absence of the organism}
#'   \item{institutionCode}{Code of the institution that holds the record}
#'   \item{verbatimEventDate}{Original recorded date of the event}
#'   \item{scientificName}{Full scientific name of the organism}
#'   \item{individualCount}{Number of individuals observed}
#'   \item{organismQuantity}{Reported quantity of the organism}
#'   \item{abundance}{Calculated or standardized abundance value}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{coordinateUncertaintyInMeters}{Uncertainty in coordinates (meters)}
#'   \item{locality}{Named place where the occurrence was recorded}
#'   \item{verbatimLocality}{Original text for locality description}
#'   \item{municipality}{Municipality or town of the occurrence}
#'   \item{county}{County where the record was observed}
#'   \item{stateProvince}{State or province name}
#'   \item{country}{Country name}
#'   \item{cleaned_catalog}{Standardized catalog number for de-duplication}
#' }
#' @source - used rgbif for GBIF, ridigbio for iDigBio, robis for OBIS and rsymbiota for InvertEBase
"ecodata"


#' dataset2: Documentation of data file - ecodata_corrected.rda
#'
#' @description This data file created by using GEOLocate tool and we only kept 4 columns. These georeference information will be merge back with the main data file ecodata
#'
#' @format A data frame with 433 rows and 4 variables:
#' \describe{
#'   \item{cleaned_catalog}{catalog number}
#'   \item{corrected_latitude}{latitude values assigned by GEOLocate}
#'   \item{corrected_longitude}{longitude values assigned by GEOLocate}
#'   \item{corrected_uncertainty}{uncertainty values assigned by GEOLocate}
#' }
#' @source - this file was created manually after extracting the csv file from GEOLocate online software to assign coordiante and uncertainty values for the records has locality information
"ecodata_corrected"


#' dataset3: Documentation of data file - ecodata_with_outliers.rda
#'
#' @description This data file created after running ec_flag_outlier function. It has records with outlier probability
#'
#' @format A data frame with 713 rows and 35 variables:
#' \describe{
#'   \item{X}{index}
#'   \item{basisOfRecord}{Type of occurrence record (e.g., preserved specimen, fossil)}
#'   \item{occurrenceStatus}{Indicates presence or absence of the species}
#'   \item{institutionCode}{Code of the institution that provided the record}
#'   \item{verbatimEventDate}{Original text for the event or collection date}
#'   \item{scientificName}{Scientific name of the organism}
#'   \item{individualCount}{Number of individuals recorded}
#'   \item{organismQuantity}{Reported quantity (unit may vary)}
#'   \item{abundance}{Standardized or calculated abundance value}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{coordinateUncertaintyInMeters}{Spatial uncertainty of coordinates in meters}
#'   \item{locality}{Named location where the record was collected}
#'   \item{verbatimLocality}{Original locality text as provided by the source}
#'   \item{municipality}{Municipality or town of occurrence}
#'   \item{county}{County of occurrence}
#'   \item{stateProvince}{State or province of occurrence}
#'   \item{country}{Country of occurrence}
#'   \item{cleaned_catalog}{Standardized catalog number used for de-duplication}
#'   \item{lat_precision}{Number of decimal places in the latitude coordinate}
#'   \item{lon_precision}{Number of decimal places in the longitude coordinate}
#'   \item{flag_cordinate_precision}{Flag for low coordinate precision}
#'   \item{flag_cc_val}{Flag for invalid or impossible coordinates}
#'   \item{flag_cc_equal}{Flag for identical latitude and longitude (likely erroneous)}
#'   \item{flag_cc_zero}{Flag for coordinates at (0,0)}
#'   \item{flag_cc_cent}{Flag for coordinates placed at a country or region centroid}
#'   \item{flag_cc_gbif}{Flag for coordinates matching GBIF headquarters (artifact)}
#'   \item{flag_cc_inst}{Flag for coordinates matching institution location}
#'   \item{flag_non_region}{Flag for coordinates outside the study region}
#'   \item{outliers}{Flag for outliers based clustering of spatial and env variables}
#'   \item{BO_sstmean}{Mean sea surface temperature from Bio-ORACLE}
#'   \item{BO_sstmax}{Maximum sea surface temperature from Bio-ORACLE}
#'   \item{BO_sstmin}{Minimum sea surface temperature from Bio-ORACLE}
#'   \item{BO_chloro}{Chlorophyll concentration from Bio-ORACLE}
#'   \item{BO_dissox}{Dissolved oxygen level from Bio-ORACLE}
#' }
#' @source - this file was created manually after extracting the csv file from GEOLocate online software to assign coordiante and uncertainty values for the records has locality information
"ecodata_with_outliers"


#' dataset4: Documentation of data file - ecodata_cleaned.rda
#'
#' @description This data shows the final cleaned occurrence records
#'
#' @format A data frame with 698 rows and 35 variables:
#' \describe{
#'   \item{X}{Index}
#'   \item{basisOfRecord}{Type of occurrence record (e.g., preserved specimen, fossil)}
#'   \item{occurrenceStatus}{Indicates presence or absence of the species}
#'   \item{institutionCode}{Code of the institution that provided the record}
#'   \item{verbatimEventDate}{Original text for the event or collection date}
#'   \item{scientificName}{Scientific name of the organism}
#'   \item{individualCount}{Number of individuals recorded}
#'   \item{organismQuantity}{Reported quantity (unit may vary)}
#'   \item{abundance}{Standardized or calculated abundance value}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{coordinateUncertaintyInMeters}{Spatial uncertainty of coordinates in meters}
#'   \item{locality}{Named location where the record was collected}
#'   \item{verbatimLocality}{Original locality text as provided by the source}
#'   \item{municipality}{Municipality or town of occurrence}
#'   \item{county}{County of occurrence}
#'   \item{stateProvince}{State or province of occurrence}
#'   \item{country}{Country of occurrence}
#'   \item{cleaned_catalog}{Standardized catalog number used for de-duplication}
#'   \item{lat_precision}{Number of decimal places in the latitude coordinate}
#'   \item{lon_precision}{Number of decimal places in the longitude coordinate}
#'   \item{flag_cordinate_precision}{Flag for low coordinate precision}
#'   \item{flag_cc_val}{Flag for invalid or impossible coordinates}
#'   \item{flag_cc_equal}{Flag for identical latitude and longitude (likely erroneous)}
#'   \item{flag_cc_zero}{Flag for coordinates at (0,0)}
#'   \item{flag_cc_cent}{Flag for coordinates placed at a country or region centroid}
#'   \item{flag_cc_gbif}{Flag for coordinates matching GBIF headquarters (artifact)}
#'   \item{flag_cc_inst}{Flag for coordinates matching institution location}
#'   \item{flag_non_region}{Flag for coordinates outside the study region}
#'   \item{outliers}{Flag for outliers based on clustering of spatial and environmental variables}
#'   \item{BO_sstmean}{Mean sea surface temperature from Bio-ORACLE}
#'   \item{BO_sstmax}{Maximum sea surface temperature from Bio-ORACLE}
#'   \item{BO_sstmin}{Minimum sea surface temperature from Bio-ORACLE}
#'   \item{BO_chloro}{Chlorophyll concentration from Bio-ORACLE}
#'   \item{BO_dissox}{Dissolved oxygen level from Bio-ORACLE}
#' }
#'
#' @source Generated after filtering outlier data points
"ecodata_cleaned"


#' dataset5: Documentation of data file - ecodata_x.rda
#'
#' @description This data was created to get unique combination of coordinate values to extract env variables from bio-oracle and merge back in main data table - ecodata
#'
#' @format A data frame with 705 rows and 6 variables:
#' \describe{
#'   \item{species}{species name}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{temperature_mean_BO}{Mean sea surface temperature from Bio-ORACLE}
#'   \item{temperature_max_BO}{Maximum sea surface temperature from Bio-ORACLE}
#'   \item{temperature_min_BO}{Minimum sea surface temperature from Bio-ORACLE}
#' }
#' @source - this file has unique coordinate information with unique values of enviornemnt variables
"ecodata_x"

#' dataset6: Documentation of data file - example_sp_invertebase.rda
#'
#' @description This is a data dump downloaded from invertEbase, as the R package link with InverEbase is currently archive and not maintained, we are providing an example file.
#'
#' @format A data frame with 710 rows and 20 variables:
#' \describe{
#'   \item{source}{invertEbase}
#'   \item{catalogNumber}{CatalogNumber}
#'   \item{basisOfRecord}{type of observations}
#'   \item{occurrenceStatus}{presence or absent}
#'   \item{institutionCode}{Institution code}
#'   \item{verbatimEventDate}{when was this occurrence created}
#'   \item{scientificName}{species name}
#'   \item{individualCount}{abundance}
#'   \item{organismQuantity}{abundance}
#'   \item{abundance}{abundance}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{coordinateUncertaintyInMeters}{uncertainty of coordiantes}
#'   \item{locality}{location information}
#'   \item{verbatimLocality}{verbatim location information}
#'
#'   \item{municipality}{municipality}
#'   \item{country}{country}
#'   \item{stateProvince}{State or Provinces}
#'   \item{county}{county}
#'   \item{countryCode}{country code}
#' }
#' @source - this file is downloaded file from invertEBase for species - "Mexacanthina lugubris" and modified field names based on TDWG standard.
"example_sp_invertebase"
