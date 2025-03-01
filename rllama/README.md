# rllama

`rllama` is an R package designed to access and analyze data from the
DeFiLlama API. This package simplifies the process of fetching and
manipulating DeFiLlama data, making it easier for users to perform
analyses and create visualizations.

[![](https://www.r-pkg.org/badges/version/rllama)](https://cran.r-project.org/package=rllama)
[![](https://cranlogs.r-pkg.org/badges/rllama)](https://cran.r-project.org/package=rllama)
[![](https://cranlogs.r-pkg.org/badges/grand-total/rllama)](https://cran.r-project.org/package=rllama)

## Installation

You can install the development version of `rllama` from GitHub with:

    # install.packages("devtools")
    devtools::install_github("AlexTwoR/rllama")

    ## Skipping install of 'rllama' from a github remote, the SHA1 (b231ef9f) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Usage

Here is a basic example of how to use rllama to retrieve TVL data:

    library( rllama )

    # Example usage
    # This will fetch and display TVL data
    chain_tvl = get_defillama_chain_tvl()
    head( chain_tvl )

    ##        name symbol         tvl      gecko_id cmcId
    ## 1: Ethereum    ETH 59430242034      ethereum  1027
    ## 2:     Tron   TRON  7502492328          tron  1958
    ## 3:  Binance    BNB  3673133628   binancecoin  1839
    ## 4: Arbitrum    ARB  2502882968      arbitrum 11841
    ## 5:   Solana    SOL  1520704963        solana  5426
    ## 6:  Polygon  MATIC   956230584 matic-network  3890
