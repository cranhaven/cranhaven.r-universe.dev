.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "====================================================\n",
    "brfinance - Democratizing access to Brazilian economic data\n",
    "====================================================\n\n",

    "DOWNLOAD ECONOMIC DATA:\n",
    "  * get_selic_rate()     - SELIC interest rate (annual)\n",
    "  * get_inflation_rate() - IPCA inflation with YTD & 12-month rates\n",
    "  * get_unemployment()   - Quarterly unemployment rate (IBGE PNAD)\n",
    "  * get_exchange_rate()  - US Dollar exchange rate (commercial)\n",
    "  * get_cdi_rate()       - CDI interbank rate\n",
    "  * get_gdp_growth()     - Quarterly GDP growth rate\n\n",

    "VISUALIZE DATA:\n",
    "  * plot_selic_rate()    - Plot SELIC evolution\n",
    "  * plot_unemployment()  - Plot unemployment trends\n",
    "  * More plots coming soon!\n\n",

    "KEY FEATURES:\n",
    "  * Set language = 'pt' for Portuguese column names\n",
    "  * Flexible date formats: YYYY, YYYY-MM, or YYYY-MM-DD\n",
    "  * Automatic variable labels with labelled package\n",
    "  * Built-in data validation and error handling\n\n",

    "Learn more: browseVignettes('brfinance')\n",
    "Report issues: https://github.com/efram2/brfinance/issues\n\n",

    "Developed by Joao Paulo dos Santos P. Barbosa (efram2)\n",
    "Contribute: https://github.com/efram2/brfinance"
  )
}
