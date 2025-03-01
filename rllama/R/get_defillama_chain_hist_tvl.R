#' Chain History Data
#'
#' Fetches historical TVL data for a specified Blockchain in USD.
#' The data is returned as a table sorted by date.
#' @param name A string representing the unique name (name) of the protocol.
#' This name can be obtained from the result of the \code{\link{get_defillama_chain_tvl}} function.
#' @return A `data.table` object containing historical TVL data of the specified chain
#' The table includes columns for date, TVL in USD. It is sorted by date.
#' @export
#' @examples
#' # total tvl by date
#' chain_tvl = get_defillama_chain_hist_tvl( "Ethereum" )
#' tail( chain_tvl )

get_defillama_chain_hist_tvl = function( name ) {

  # due to NSE notes in R CMD check
  name_for_adj = tvl = NULL

  # name adj for tvl hist data
  name_adj_dt = fread( 'name_for_adj | name_new
                        Binance      | BSC' )

  if( nrow( name_adj_dt[ name_for_adj == name ] ) > 0 )
    name = name_adj_dt[ name_for_adj == name ]$name_new[1]

  # Replace spaces with '%20' in the string for URL encoding
  name = gsub(" ", "%20", name)

  request = paste0(  'https://api.llama.fi/charts/', name )
  res = httr::GET( request )
  content = httr::content( res, type = 'application/json' )

  if( length( content ) > 1 )
    tvl_hist  = rbindlist( content )
  else{
    message( "There is no chain with that name" )
    return( NA )
  }

  setnames( tvl_hist, "totalLiquidityUSD", "tvl" )
  tvl_hist
}
