context("Tests for DataPreprocessing.R")

#assign("Pathways_Example", c(1,2,3,4), envir = .GlobalEnv)
assign("Pathways_Example", c(1,2,3,4))
assign("CenterStandardize_Orig", matrix(c(1,2,0,1,0,0,2,1,1,1,2,1), ncol=3, byrow=FALSE))
assign("CenterStandardize_Mean", c(1.00,0.75,1.25))
assign("CenterStandardize_SD", c(0.8164966,0.9574271,0.5000000))
assign("CenterStandardize_Final", matrix(c(0.0000000,1.2247449,-1.2247449,0.0000000,-0.7833495,-0.7833495,1.3055824,0.2611165,-0.5000000,-0.5000000,1.5000000,-0.5000000), ncol=3, byrow=FALSE))
assign("OrigPhenos", c(0.2999148,-2.1918209,-1.3970979,-0.4458862,0.8048022,-1.4659387,1.6504161,-0.9612443,-0.7646495,-0.6219103))
assign("OrigGenos", c(2,2,0,1,2,1,0,2,1,2))
assign("AdjPhenos", c(5.8193989,5.2890087,-1.3970979,2.4034901,7.8825863,-1.0977948,1.6504161,2.0164610,0.8981305,4.8637056))
assign("Residuals", c(1.02205278,0.49166253,-0.58154207,0.41259495,3.08524020,-3.08868997,2.46597191,-2.78088513,-1.09276466,0.06635945))

test_that("Checking behavior of pathway format splitting and setup", {
	 expect_equal(lapply(strsplit(as.character("1,2,3,4"), ","), as.numeric)[[1]], Pathways_Example)
})

test_that("Checking behavior of centering and standardizing genotypes procedure", {
	expect_equal(apply(CenterStandardize_Orig, 2, mean), CenterStandardize_Mean)
	expect_equal(apply(CenterStandardize_Orig, 2, sd), CenterStandardize_SD, tolerance=1e-6)
	expect_equal(t((t(CenterStandardize_Orig) - CenterStandardize_Mean)/CenterStandardize_SD), CenterStandardize_Final, tolerance=1e-6)
})

test_that("Checking behavior of linear regression residuals extraction", {
	expect_equal(unname(residuals(lm(AdjPhenos ~ OrigGenos))), Residuals, tolerance=1e-6)
})

#rm(Pathways_Example, CenterStandardize_Orig, CenterStandardize_Mean, CenterStandardize_SD, CenterStandardize_Final, OrigPhenos, OrigGenos, AdjPhenos, Residuals, envir = .GlobalEnv)
