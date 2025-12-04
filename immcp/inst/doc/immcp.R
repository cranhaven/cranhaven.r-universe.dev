## ----eval=FALSE---------------------------------------------------------------
#  library(immcp)
#  data(drugdeom)
#  names(drugdemo)

## ----eval=FALSE---------------------------------------------------------------
#  drug_herb <- PrepareData(drugdemo$drug_herb, from = "drug", to="herb")
#  herb_compound <- PrepareData(drugdemo$herb_compound, from = "herb", to="compound")
#  compound_target <- PrepareData(drugdemo$compound_target, from = "compound", to="target")
#  disease <- PrepareData(drugdemo$disease, diseaseID = "disease",from = "target", to="target")
#  BasicData <- CreateBasicData(drug_herb, herb_compound, compound_target, diseasenet = disease)

## ----eval=FALSE---------------------------------------------------------------
#  DisDrugNet <- CreateDisDrugNet(BasicData, drug = "Drug1",     disease = "disease")
#  plot_graph(DisDrugNet, size = 20)

