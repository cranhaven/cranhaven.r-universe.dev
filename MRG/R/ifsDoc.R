#' Test data sets for the MRG package
#' 
#' 
#' Synthetic data set of Danish farming data, similar to the  
#' structure of the real Farm Structure Survey (FSS) data. It contains more than 
#' 37,000 synthetic records - generated in a way that should
#' replicate the structure and the distribution of real data,
#' but where the individual data are different from the real data.
#' 
#' The variables are as follows:
#' @format A data frame with 37,088 rows and 14 variables
#' \itemize{
#'   \item COUNTRY The name of the country
#'   \item YEAR The year of the survey data
#'   \item ID_SYNTH Unique ID of the record
#'   \item FARMTYPE Farm typology. Farms are classified into different types according to their dominant activity and standard output value (proxy for farm income). For further information see https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Farm_typology
#'   \item HLD_FEF Not used. Farm is included in frame extension (HLD_FEF=1) or main frame (HLD_FEF=0)
#'   \item REGIONS NUTS2 region
#'   \item GEO_LCT The geolocation in typical FSS-format, including both country, CRS and xy coordinates
#'   \item EXT_CORE The extrapolation weights for core data (1 in this data set)
#'   \item STRA_ID_CORE Which stratum the record belongs to - only used for the reliability checking
#'   \item UAA The utilized agricultural area of the farm
#'   \item UAAXK0000_ORG The organic utilized agricultural area, excluding kitchen gardens of the farm. UAAXK0000_ORG includes fully certified area and area under conversion 
#'   \item Sample Whether the record should be included as a weighted subsample
#'   \item EXT_MODULE The extrapolation weights for the sample data 
#' }
#' @docType data
#' @keywords datasets
#' @name ifs_dk
#' @usage data(ifs_dk)
#' @format A data frame with 37088 rows and 14 variables
#' 
#' @details
#' For practical purposes, we have derived a synthetic data set from the 
#' original 2020 agricultural census micro data. 
#' Although synthetic data sets are a feasible way to provide public access 
#' to the data by mitigating any confidentiality concerns, there have only been 
#' a few attempts made to create synthetic public files of micro data collected 
#' by official statistical institutes.
#' 
#' The attached data set has been produced by application of a hot-deck procedure - 
#' originally developed to impute missing information - 
#' to substitute a data entry from the original data (i.e., the recipient) 
#' by using a value from a similar record (i.e., the donor) within the same 
#' classification group (Andridge and Little, 2010; Ford, 1983; Joenssen and Bankhofer, 2012).
#' 
#' A single hot deck imputed data set is computed for each country individually. 
#' First, records are partitioned into homogeneous groups so that the donors 
#' follow the same distribution as the recipients. Data points from the recipients 
#' are substituted sequentially based on a value from a varying pool of donors. 
#' Furthermore, the nearest neighbour matching technique using distance metrics 
#' is applied to select the most appropriate donor from the pool of donors. 
#' For a few of the discrete variables, such as $FARMTYPE$, $SO_EUR$, $HLD_FEF$ and $NUTS2$, 
#' a donor was chosen randomly by preserving the original empirical distribution 
#' or they were simply randomly decoded (i.e., renamed). The variable containing 
#' information about the geographical location ($GEO_LCT$) of the agricultural 
#' holding was imputed by restricting the donor to the same country. 
#' To assess the quality rating system (i.e., the reliability), 
#' we created an artificial sample ($SAMPLE$) with the respective extrapolation 
#' factors ($EXT_MODULE$) based on stratification. The sample size consists of 
#' approximately one third of the synthetic 2020 census for Denmark. 
#' 
#' The empirical distribution of the two main variables of interest of the 
#' synthetic data, $UAA$ and $UAAXK0000_ORG$ are widely preserved within the 
#' different economic size classes.
#' 
#' @references
#' Andridge RR, Little RJ (2010). A review of hot deck imputation for survey non-response.
#'     International statistical review, 78(1), 40–64.
#'     
#' Ford BL (1983). An overview of hot-deck procedures.” Incomplete data in sample surveys,
#' 2(Part IV), 185–207.
#' 
#' Joenssen DW, Bankhofer U (2012). Hot deck methods for imputing missing data. In
#' P Perner (ed.), Machine Learning and Data Mining in Pattern Recognition, Lecture Notes
#' in Computer Science, pp. 63–75. Springer, Berlin, Heidelberg. ISBN 978-3-642-31537-4.
#' doi:10.1007/978-3-642-31537-4_6
#' 
#' 
#' 
NULL
