#' Stablecoins History Data
#'
#' Retrieves stablecoin data, including details like name, symbol, price, and market cap.
#' The data is returned as a sorted table.
#' @param stablecoin_id An integer representing the ID of the stablecoin. This ID can be obtained
#' from the result of the \code{\link{get_defillama_stablecoin_data}} function.
#'
#' @return A `data.table` object containing historical data of the specified stablecoin.
#' The table is sorted by date.
#' @export
#' @examples
#' \donttest{
#' stablecoin_hist_data = get_defillama_stablecoin_hist_data( 5 )
#' head( stablecoin_hist_data )
#' }

get_defillama_stablecoin_hist_data = function( stablecoin_id ) {

  # due to NSE notes in R CMD check
  symbol = name = circulating = NULL

  # Validate the input
  if (!is.numeric(stablecoin_id) || stablecoin_id <= 0) {
    stop("Invalid stablecoin ID. Please provide a positive numeric ID.")
  }

  # Construct the API URL with the stablecoin ID
  url = paste0("https://stablecoins.llama.fi/stablecoin/", stablecoin_id)

  # Send a GET request to the API
  res <- httr::GET(url)
  content <- httr::content(res, type = 'application/json')

  # Convert the list to a data.table
  stablecoin_hist_data = rbindlist(content$tokens, fill = TRUE)

  # set name and symbol
  stablecoin_hist_data[ , symbol := content$symbol  ]
  stablecoin_hist_data[ , name   := content$name  ]
  setcolorder( stablecoin_hist_data, c( 'symbol', 'name'  ) )

  # Convert 'date' column to POSIXct datetime
  stablecoin_hist_data[, date := as.Date( as.POSIXct(date,
                                                     format = "%Y-%m-%dT%H:%M:%S",
                                                     tz = "UTC") )]
  stablecoin_hist_data[ , circulating := as.numeric( circulating ) ]

  stablecoin_hist_data[]
}
