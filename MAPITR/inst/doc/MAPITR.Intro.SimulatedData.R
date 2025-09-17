## -----------------------------------------------------------------------------
library("MAPITR")

data(MAPITR_SimData_Phenotype, MAPITR_SimData_Genotypes, MAPITR_SimData_Pathways, MAPITR_SimData_PCs)

## -----------------------------------------------------------------------------
MAPITR_SimData_Genotypes[1:10,1:10]

## -----------------------------------------------------------------------------
head(MAPITR_SimData_Phenotype)

## -----------------------------------------------------------------------------
head(MAPITR_SimData_Pathways)

## -----------------------------------------------------------------------------
MAPITR_Output <- MAPITR(MAPITR_SimData_Genotypes, MAPITR_SimData_Phenotype, MAPITR_SimData_Pathways)

## -----------------------------------------------------------------------------
head(MAPITR_Output$Results)

## -----------------------------------------------------------------------------
MAPITR_Output$Results[MAPITR_Output$Results[,2] < .01,]

## -----------------------------------------------------------------------------
MAPITR_SimData_PCs[1:5,1:10]
MAPITR_Output2 <- MAPITR(MAPITR_SimData_Genotypes, MAPITR_SimData_Phenotype, MAPITR_SimData_Pathways, Covariates=MAPITR_SimData_PCs)
MAPITR_Output2$Results[MAPITR_Output2$Results[,2] < .01,]

