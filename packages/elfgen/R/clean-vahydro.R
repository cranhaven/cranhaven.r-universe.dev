#' Clean dataset of ecological and hydrologic data
#' @description Given a dataframe of flow metric and richness metric data
#' (Typically retrieved from the DEQ VAHydro database), removes all sites where the
#' ratio of Drainage Area:Mean Annual Flow is greater than 1000, also aggregates to the
#' maximum richness value at each x-metric value
#' @param watershed.df A dataframe of sites with ecological and hydrologic data
#' @return A cleaned dataframe of sites with ecological and hydrologic data
#' @import sqldf
#' @export clean_vahydro
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' # Retrieve dataset of interest
#' watershed.df <- data.frame(
#'     MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 400, 900, 1000, 100, 100),
#'     NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 50, 10, 10,99999,87),
#'     watershed.code = "test_testcode",
#'     hydrocode = c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12","t13"),
#'     DA_SQMI = c(110, 220000, 280, 360, 530, 604, 712, 698, 40000, 905, 1087, 98, 87),
#'     x.metric = c(100, 200, 300, 400, 526, 600, 700, 800, 400, 900, 1000, 100, 100)
#'     )
#' # Clean the dataset
#' clean_vahydro(watershed.df)
#' }
clean_vahydro <- function (watershed.df) {
  message(paste("Length of input dataset:  ",length(watershed.df[,1]),sep = ''))

  #ADD COLUMN OF RATIO OF DRAINAGE AREA TO MEAN FLOW
  watershed.df["ratio"] <- (watershed.df$DA_SQMI)/(watershed.df$MAF)
  #REMOVE ALL STATIONS WHERE THE RATIO OF DA:Q IS GREATER THAN 1000
  watershed.df <-watershed.df[!(watershed.df$ratio > 1000),]

  #USE ONLY MAX NT VALUE FOR EACH STATION
  watershed.df.query <- paste('SELECT "x.metric",
                                      MAX("NT.TOTAL.UNIQUE") as "NT.TOTAL.UNIQUE",
                                      "watershed.code",
                                      hydrocode,
                                      DA_SQMI,
                                      MAF
                                 FROM "watershed.df" a
                                 GROUP BY "x.metric"'
                              ,sep='')
  watershed.df <- sqldf(watershed.df.query)
  message(paste("Length of output dataset: ",length(watershed.df[,1]),sep = ''))

  return(watershed.df)
} #close function
