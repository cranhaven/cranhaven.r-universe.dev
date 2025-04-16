## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  # development version, via devtools
#  devtools::install_github('danielkovtun/rpredictit')

## ---- eval=FALSE--------------------------------------------------------------
#  library(rpredictit)
#  rpredictit::runExample('demo')

## -----------------------------------------------------------------------------
rpredictit::all_markets()

## -----------------------------------------------------------------------------
data <- rpredictit::all_markets()
rpredictit::markets_table(data)

## -----------------------------------------------------------------------------
filename <- "What_will_be_the_balance_of_power_in_Congress_after_the_2020_election.csv"
csv_path <- system.file("extdata", filename, package = "rpredictit")
contract_data <- rpredictit::parse_historical_csv(csv_path)
rpredictit::historical_plot(contract_data)

## ---- eval=FALSE--------------------------------------------------------------
#  markets <- rpredictit::all_markets()
#  id <- markets$id[1]
#  rpredictit::single_market(id)

