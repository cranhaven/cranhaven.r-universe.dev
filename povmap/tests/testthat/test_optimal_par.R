# Test of function optimal parameter gives benchmark lambda for
# Box-Cox transformation

# The data that is used for testing is the data from the sae package.
load("EBP/incomedata.RData")
load("EBP/incomedata_woTeruel.RData")
load("EBP/Xoutsamp_AuxVar.RData")


test_that("Does function optimal_parameter() return the benchmark lambda?", {

  # Fixed optimal parameter and shift (benchmark values)
  ebp_optpar_bc <- read.csv2("EBP/ebp_optpar_bc.csv", sep=",",
                             stringsAsFactors = TRUE)

  lambda <- as.numeric(as.character(ebp_optpar_bc[,"Optpar"]))

  # Single elements needed in optimal_parameter()
  framework <- framework_ebp(income~educ1,
                        Xoutsamp_AuxVar,
                        "provlab",
                        incomedata,
                        "provlab",
                        4282.081,
                        custom_indicator = NULL,
                        na.rm = TRUE,
                        pop_weights = NULL,
                        weights = NULL,
                        weights_type = "Guadarrama",
                        benchmark_level = NULL,
                        benchmark_weights = NULL,
                        rescale_weights = FALSE,
                        nlme_maxiter = NULL,
                        nlme_tolerance = NULL,
                        nlme_opt="nlminb",
                        nlme_optimmethod = "BFGS",
                        nlme_method = "REML", 
                        nlme_msmaxiter = NULL, 
                        nlme_mstol = NULL,
                        nlme_returnobject = F)



  optimal_lambda <- optimal_parameter(generic_opt    = generic_opt,
                                      fixed          = income~educ1,
                                      smp_data       = framework$smp_data,
                                      smp_domains    = framework$smp_domains,
                                      transformation = "box.cox",
                                      interval       = c(-1,2),
                                      framework      = framework
  )

  expect_equal(optimal_lambda,
               lambda)
})
