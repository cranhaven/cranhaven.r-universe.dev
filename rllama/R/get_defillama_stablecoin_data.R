#' Stablecoins Data
#'
#' Retrieves stablecoin data, including details like name, symbol, price, and market cap.
#' The data is returned as a sorted table by market cap.
#'
#' @return A `data.table` with stablecoin information, sorted by market capitalization.
#' @export
#' @examples
#' stablecoin_data = get_defillama_stablecoin_data()
#' head( stablecoin_data )

get_defillama_stablecoin_data = function() {

  # due to NSE notes in R CMD check
  circulating = market_cap = price = id = NULL


  url = "https://stablecoins.llama.fi/stablecoins?includePrices=true"

  res = httr::GET( url )
  content = httr::content( res, type = 'application/json' )


  stablecoin_dt = lapply(content$peggedAssets, function(dt) {

    # Define the columns of interest
    cols_of_interest = c("id", "name", "symbol", "price", "gecko_id",
                         "pegType", "pegMechanism", "circulating")

    # Initialize an empty data.table
    selected_data <- data.table(matrix(NA, ncol = length(cols_of_interest), nrow = 1))
    setnames(selected_data, cols_of_interest)

    # Fill the data.table with data if it exists
    for (col in cols_of_interest) {
      if (col %in% names(dt)) {
        selected_data[[col]] = dt[[col]]
      }
    }

    return( selected_data )
  })

  # Convert the list to a data.table,
  stablecoin_dt  = rbindlist( stablecoin_dt, fill = TRUE )

  # Market Cap calculation
  stablecoin_dt[ , circulating := as.numeric( circulating ) ]
  stablecoin_dt[ , id := as.numeric( id ) ]
  stablecoin_dt[ , market_cap := circulating * price ]

  setorder( stablecoin_dt, -"market_cap", na.last = TRUE )

  stablecoin_dt[]

}
