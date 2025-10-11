test_calES <- function() {
    library(GSEMA)
    data("simulatedData")
    checkEqualsNumeric(calculateESpath(objectMApathSim, measure = "limma", 
        missAllow = 0.3)$ES[1,2],
        0.06154452, tolerance=1.0e-6)}

