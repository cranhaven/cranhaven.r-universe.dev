library(dplyr)
load('./tests/testthat/fixtures/UrbanRandomSubset.rda')

GeneSelectR::configure_environment()
GeneSelectR::set_reticulate_python()

X <- UrbanRandomSubset %>% dplyr::select(-treatment)
y <- UrbanRandomSubset %>% dplyr::select(treatment)
y <- as.factor(y$treatment)
y <- as.integer(y)

res <- GeneSelectR(X,y)

