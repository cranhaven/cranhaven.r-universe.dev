#' Retrieve data from DEQ VAHydro database and format data for ELF generation. Contact Virginia DEQ Office of Water Supply to request access to the VAHydro database.
#' @description Given a set of VAHydro input parameters, outputs a dataframe of
#' flow metric and richness metric data for hydrologic unit supplied
#' @param watershed.code Hydrologic unit code, either HUC6, HUC8, HUC10, or HUC12 (e.g. HUC10 code '0208020101').
#' @param watershed.bundle dH bundle of hydrologic unit
#' @param watershed.ftype dH ftype of hydrologic unit
#' @param x.metric x-metric, i.e. streamflow or drainage area
#' @param y.metric y-metric, most commonly species richness
#' @param y.sampres Sample resolution of y.metric (e.g. 'species')
#' @param datasite VAHydro database URL
#' @param EDAS.localpath Local file path for storing downloaded EDAS data. Defaults to a temp directory.
#' @return A dataframe of sites containing species richness data (NT Total values) and mean annual flow (MAF) data.
#' @export elfdata_vahydro
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' # Retrieve dataset of interest
#' watershed.df <- elfdata_vahydro(
#'    'nhd_huc8_02080201',
#'    'watershed',
#'    'nhd_huc8',
#'    'nhdp_drainage_sqmi',
#'    'aqbio_nt_total',
#'    'species'
#'    )
#' elfdata_vahydro(watershed.df)
#' }
elfdata_vahydro <- function (watershed.code,watershed.bundle,watershed.ftype,x.metric,y.metric,y.sampres,datasite,EDAS.localpath = tempdir()) {

  if (missing(datasite)) {
    return("Contact package maintainer to request access to DEQ database")
  } else {

  EDAS_item <- paste(
    datasite,"elfgen_data_export",x.metric,y.metric,
    watershed.bundle,watershed.ftype,y.sampres,watershed.code,sep="/"
  )

  EDAS_filename <- paste("EDAS_data_",watershed.code,"_",x.metric,"_",y.metric,"_",y.sampres,".csv",sep="")

  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  if (file.exists(paste(EDAS.localpath, EDAS_filename, sep = '/')) == FALSE) {
    message(paste("Downloading EDAS dataset from DEQ VAHydro",sep = ''))

    destfile <- paste(EDAS.localpath,EDAS_filename,sep="\\")
    download.file(EDAS_item, destfile = destfile, method = "libcurl")

  } else {
    message(paste("EDAS dataset previously downloaded",sep = ''))
  }
  message(paste("Dataset download location: ",EDAS.localpath,sep = ''))

  #read csv from local directory
  EDAS.dataframe <- read.csv(file=paste(EDAS.localpath,EDAS_filename,sep="\\"), header=TRUE, sep=",")

  if (length(EDAS.dataframe[,1]) == 0) {
    stop("No VAHydro EDAS Data for Hydrologic Unit")
  }

  #reformat dataframe to conform to elfgen format
  watershed.df <- data.frame("x.metric" = EDAS.dataframe$x_value,
                             "NT.TOTAL.UNIQUE" = EDAS.dataframe$y_value,
                             "watershed.code" = watershed.code,
                             "hydrocode" = EDAS.dataframe$hydrocode,
                             "DA_SQMI" = EDAS.dataframe$drainage_area_sqmi,
                             "MAF" = EDAS.dataframe$qmean_annual

  )

  return(watershed.df)
  } #close missing datasite parameter
} #close function
