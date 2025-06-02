# ANCOVA VIA LM OBJECT
data(sesamesim)
sesamesim$site <- as.factor(sesamesim$site)
ancov <- lm(postnumb ~ site + prenumb + peabody -1, data = sesamesim)


y_fit_pmpb <- c(0.00243472436771211, 0.952122011002619, 0.0454432646296689)
set.seed(100)
y_gor <- gorica(ancov, "site1=site2=site3=site4=site5;site2 > site5 > site3 > site1 >site4;", iterations = 100)

test_that("ancova gorica bain similar", {
  expect_equivalent(y_fit_pmpb, y_gor$fit$gorica_weights, tolerance = .1)
})


# TESTING ANCOVA WITH RESTRICTIONS ON THE COVARIATES

sesamesim$sex <- as.factor(sesamesim$sex)
ancov <- lm(postnumb ~ sex + prenumb + peabody -1, data = sesamesim)
coef(ancov)

z<-bain(ancov, " sex1 = sex2 & pre > 0 &  pea > 0")
set.seed(100)
z_gor<-gorica(ancov, " sex1 = sex2 & pre > 0 &  pea > 0", iterations = 100)
z_fit_PMPb <- c(0.981369626075153, 0.0186303739248469)
test_that("ancova gorica bain similar", {
  expect_equivalent(z_fit_PMPb, z_gor$fit$gorica_weights, tolerance = .1)
})


# TESTING ANCOVA WITH LM OBJECT WITH INTERCEPTS UNEQUAL TO ZERO AND TWO COVARIATES

df <- sesamesim
df$site <- as.factor(df$site)
model <- lm(postnumb~site+prenumb+peabody-1, df)

y_fit_PMPb <- c(1.04874764393017e-13, 0.806266965406935, 0.19373303459296)
set.seed(100)
y_gor <- gorica(model, "site1 = 19.35 & site2 = 29.33;site1>19.35&site2>29.33", iterations = 100)

test_that("ancova gorica bain similar", {
  expect_equivalent(y_fit_PMPb, y_gor$fit$gorica_weights, tolerance = 1)
})


# TESTING ANCOVA WITH LM OBJECT WITH INTERCEPTS UNEQUAL TO ZERO AND ONE COVARIATE

sesamesim$site <- as.factor(sesamesim$site)
ancov <- lm(postnumb ~ site + prenumb -1, data = sesamesim)
#hyp<-"v.1=19.35 & v.2 = 29.33; v.1>19.35 & v.2 > 29.33;"

y_fit_PMPb <- c(8.82945906865172e-14, 0.796205398711517, 0.203794601288395)
set.seed(100)
y_gor <- gorica(ancov, "site1=19.35 & site2 = 29.33; site1>19.35 & site2 > 29.33", iterations = 100)

test_that("ancova gorica bain similar", {
  expect_equivalent(y_fit_PMPb, y_gor$fit$gorica_weights, tolerance = 1)
})

# testing that the order of input of group and covariates does not matter


sesamesim$site <- as.factor(sesamesim$site)
ancov <- lm(postnumb ~ site + prenumb + peabody -1, data = sesamesim)
set.seed(100)
y<-gorica(ancov, "site1=site2=site3=site4=site5;site2 > site5 > site3 > site1 >site4;", iterations = 100)

ancov2 <- lm(postnumb ~ prenumb + peabody +site -1, data = sesamesim)
set.seed(100)
y2<-gorica(ancov2, "site1=site2=site3=site4=site5;site2 > site5 > site3 > site1 >site4;", iterations = 100)

ancov3 <- lm(postnumb ~ prenumb + site + peabody -1, data = sesamesim)
set.seed(100)
y3<-gorica(ancov3, "site1=site2=site3=site4=site5;site2 > site5 > site3 > site1 >site4;", iterations = 100)
test_that("Bain mutual", {expect_equal(y$fit$gorica_weights , y2$fit$gorica_weights, tolerance = .001)})
test_that("Bain mutual", {expect_equal(y$fit$gorica_weights , y3$fit$gorica_weights, tolerance = .001)})
