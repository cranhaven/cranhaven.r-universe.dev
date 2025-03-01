#' Chain TVL Data
#'
#' This function retrieves TVL data for various chains
#' from a specified API and formats it into a data.table for analysis.
#'
#' @return A `data.table` containing the TVL data with columns: name, symbol, tvl, gecko_id, cmcId, and chainId.
#' The data is sorted by TVL in descending order.
#' @export
#' @examples
#' chain_tvl = get_defillama_chain_tvl()
#' head( chain_tvl )

get_defillama_chain_tvl = function() {

  # due to NSE notes in R CMD check
  tvl = NULL

  # API request
  request = paste0(  'https://api.llama.fi/chains' )
  res = httr::GET( request )
  content = httr::content( res, type = 'application/json' )

  tvl_chains = lapply(content, function(x) {
    data.table(name     = x$name,
               symbol   = x$tokenSymbol,
               tvl      = x$tvl,
               gecko_id = x$gecko_id,
               cmcId    = x$cmcId
               )
  })


  # Convert the list to a data.table,
  tvl_chains  = rbindlist( tvl_chains, fill = T )[ order( -tvl ) ]

  tvl_chains[]

}
