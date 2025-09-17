context("Tests for MAPITR.R") 

data(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, MAPITR_TestData_Pathways, MAPITR_TestData_PCs)

#assign("MAPITR_TestData_Genotypes", MAPITR_TestData_Genotypes, envir = .GlobalEnv)
assign("MAPITR_TestData_Genotypes", MAPITR_TestData_Genotypes)
assign("MAPITR_TestData_Phenotype", MAPITR_TestData_Phenotype)
assign("MAPITR_TestData_Pathways", MAPITR_TestData_Pathways)
assign("MAPITR_TestData_PCs", MAPITR_TestData_PCs)
assign("MAPITR_Output1", MAPITR(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, MAPITR_TestData_Pathways, OpenMP=TRUE, cores=1))
assign("MAPITR_Output3", MAPITR(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, MAPITR_TestData_Pathways, OpenMP=FALSE))
assign("MAPITR_Output2", MAPITR(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, MAPITR_TestData_Pathways, Covariates=MAPITR_TestData_PCs, OpenMP=TRUE, cores=1))
assign("MAPITR_Output4", MAPITR(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, MAPITR_TestData_Pathways, Covariates=MAPITR_TestData_PCs, OpenMP=FALSE))

assign("MAPITR_Output1_pValue1", 0.511314)
assign("MAPITR_Output1_Est1", 1.09663702)
assign("MAPITR_Output1_PVE1", 1.32243728)
assign("MAPITR_Output1_Eigen1", -0.02855374)
assign("MAPITR_Output1_Eigen2", -0.03918452)
assign("MAPITR_Output2_pValue1", 0.2208826)
assign("MAPITR_Output2_Est1", 1.2121906)
assign("MAPITR_Output2_PVE1", 1.47945478)
assign("MAPITR_Output2_Eigen1", -0.02899182)
assign("MAPITR_Output2_Eigen2", -0.03836659)

test_that("MAPITR runs the main MAPITR function, with OpenMP", {
	expect_equal(unname(MAPITR_Output1$Results[1,2]*100), MAPITR_Output1_pValue1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output1$Results[1,3]), MAPITR_Output1_Est1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output1$Results[1,4]), MAPITR_Output1_PVE1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output1$Eigenvalues[1,1]), MAPITR_Output1_Eigen1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output1$Eigenvalues[1,2]), MAPITR_Output1_Eigen2, tolerance=1e-4)
})

test_that("MAPITR runs the main MAPITR function with covariate, with OpenMP", {
	expect_equal(unname(MAPITR_Output2$Results[1,2]*100), MAPITR_Output2_pValue1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output2$Results[1,3]), MAPITR_Output2_Est1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output2$Results[1,4]), MAPITR_Output2_PVE1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output2$Eigenvalues[1,1]), MAPITR_Output2_Eigen1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output2$Eigenvalues[1,2]), MAPITR_Output2_Eigen2, tolerance=1e-4)
})

test_that("MAPITR runs the main MAPITR function, without OpenMP", {
	expect_equal(unname(MAPITR_Output3$Results[1,2]*100), MAPITR_Output1_pValue1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output3$Results[1,3]), MAPITR_Output1_Est1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output3$Results[1,4]), MAPITR_Output1_PVE1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output3$Eigenvalues[1,1]), MAPITR_Output1_Eigen1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output3$Eigenvalues[1,2]), MAPITR_Output1_Eigen2, tolerance=1e-4)
})

test_that("MAPITR runs the main MAPITR function with covariate, without OpenMP", {
	expect_equal(unname(MAPITR_Output4$Results[1,2]*100), MAPITR_Output2_pValue1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output4$Results[1,3]), MAPITR_Output2_Est1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output4$Results[1,4]), MAPITR_Output2_PVE1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output4$Eigenvalues[1,1]), MAPITR_Output2_Eigen1, tolerance=1e-4)
	expect_equal(unname(MAPITR_Output4$Eigenvalues[1,2]), MAPITR_Output2_Eigen2, tolerance=1e-4)
})

#rm(MAPITR_TestData_Genotypes, MAPITR_TestData_Phenotype, MAPITR_TestData_Pathways, MAPITR_TestData_PCs, MAPITR_Output1, MAPITR_Output1_pValue1, MAPITR_Output1_Est1, MAPITR_Output1_PVE1, MAPITR_Output1_Eigen1, MAPITR_Output1_Eigen2, MAPITR_Output2, MAPITR_Output2_pValue1, MAPITR_Output2_Est1, MAPITR_Output2_PVE1, MAPITR_Output2_Eigen1, MAPITR_Output2_Eigen2, envir = .GlobalEnv)
