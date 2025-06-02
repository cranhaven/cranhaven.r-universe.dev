# BELOW THE T.TEST INPUT FOR BAIN IS TESTED

# ===============================================================================================

# THE ONE SAMPLE T-TEST WITH A T.TEST OBJECT



x<-sesamesim$postnumb
ttest <- t_test(x)
set.seed(100)
z_gor <- gorica(ttest, "x=30; x>30; x<30", iterations = 1000)

test_that("t_test one sample bain gor equivalent", {
  expect_true(order(z_gor$fit$gorica_weights)[4] == order(c(0.804179489440423, 0.0325376792911945, 0.0980093277485244, 0.065273503519858))[4])
})

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = FALSE)
set.seed(100)
z_gor <- gorica(ttest, "x=y; x>y; x<y", iterations = 1000)

test_that("t_test independent samples bain gor equivalent", {
  expect_true(all(order(z_gor$fit$gorica_weights)[3:4] == order(c(0.794317856691633, 0.106425526453867, 0.0306959024183789, 0.0685607144361216))[3:4]))
})

# THE INDEPENDENT GROUPS T-TEST WITH A T.TEST OBJECT

x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]
ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
z_gor <- gorica(ttest, "x=y; x>y; x<y", iterations = 1000)

test_that("t_test independent samples equal variances bain gor equivalent", {
  expect_true(all(order(z_gor$fit$gorica_weights)[3:4] == order(c(0.79427857891428, 0.106538356391755, 0.0306092576653946, 0.0685738070285696))[3:4]))
})


# THE PAIRED SAMPLES T-TEST WITH A T.TEST OBJECT



x<-sesamesim$prenumb
y<-sesamesim$postnumb

ttest <- t_test(x,y,paired = TRUE)
set.seed(100)
z_gor <- gorica(ttest, "difference=0; difference>0; difference<0", iterations = 1000)

test_that("paired t_test bain and gorica similar", {
  expect_equivalent(c(1.84163019716944e-43, 6.67246246541227e-46, 0.666666666666667,
                      0.333333333333333), z_gor$fit$gorica_weights, tolerance = .1)
})

#==================================================================================================

# THE EQUIVALENCE TEST WITH A T.TEST OBJECT



x<-sesamesim$postnumb[which(sesamesim$sex==1)]
y<-sesamesim$postnumb[which(sesamesim$sex==2)]

ttest <- t_test(x,y,paired = FALSE, var.equal = TRUE)
set.seed(100)
z_gor <- gorica(ttest, "x - y > -1 & x - y < 1", iterations = 1000)

test_that("equal variance t_test gorica bain similar", {
  expect_true(0.89 > .11 & z_gor$fit$gorica_weights[1] > z_gor$fit$gorica_weights[2])
})
