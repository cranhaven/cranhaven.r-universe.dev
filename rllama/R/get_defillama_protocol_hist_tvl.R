#' Protocol History Data
#'
#' Fetches historical TVL data for a specified DeFi protocol,
#' including TVL in USD and token amount. The data is returned as a table sorted by date.
#' @param slug A string representing the unique name (slug) of the protocol.
#' This slug can be obtained from the result of the \code{\link{get_defillama_protocol_tvl}} function.
#' @param by_token A boolean indicating whether to include token-specific data. Defaults to FALSE.
#' @return A `data.table` object containing historical TVL data of the specified protocol.
#' The table includes columns for date, TVL in USD. It is sorted by date.
#' @export
#' @examples
#' # total tvl by date
#' protocol_tvl = get_defillama_protocol_hist_tvl( "lido" )
#' tail( protocol_tvl )
#'
#'# tvl by token
#' protocol_tvl = get_defillama_protocol_hist_tvl( "lido", by_token = TRUE )
#' tail( protocol_tvl )

get_defillama_protocol_hist_tvl = function( slug, by_token = FALSE ) {

  request = paste0(  'https://api.llama.fi/protocol/', slug )
  res = httr::GET( request )

  tryCatch({

    content = httr::content( res, type = 'application/json' )

    # Check if the response has a 'tvl' field
    if( 'tvl' %in% names( content ) ){

      # get total tvl data
      tvl_hist = rbindlist( content$tvl )

      # get tvl by tokens
      if ( by_token ) {

        # function for token value processing
        process_tokens = function(token_data, value_field) {

          lapply(token_data, function(item) {

            date   = item$date
            tokens = names ( item$tokens )
            values = unlist( item$tokens )

            dt = data.table(date = date, token = tokens, value = values)
            setnames(dt, "value", value_field)
            dt

          })

        }

        tvl_by_token_amount = rbindlist( process_tokens( content$tokens     , "token_amount"), fill = TRUE )
        tvl_by_token_usd    = rbindlist( process_tokens( content$tokensInUsd, "tokens_usd"  ), fill = TRUE )

        tvl_hist = merge( tvl_hist, tvl_by_token_amount, by = "date", all.x = TRUE )
        tvl_hist = merge( tvl_hist, tvl_by_token_usd, by = c("date", "token"), all.x = TRUE )
      }

    } else {
      stop("The response does not have a 'tvl' field")
    }
  }, error = function(e){
    cat( " error! /n" )
    return( NA )

  })


  if( nrow( tvl_hist ) < 2 )
    return( NA )

  setnames( tvl_hist, "totalLiquidityUSD", "tvl" )
  tvl_hist[ , date := as.Date( as.POSIXct( as.numeric(date), origin = '1970-01-01', tz = 'UTC' ) ) ]

  tvl_hist[]
}
