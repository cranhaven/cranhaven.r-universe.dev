library(lavaan)
data(sesamesim)
sesameCFA <- sesamesim
names(sesameCFA)[6] <- "pea"
df <- sesameCFA
model3 <- '
    A  =~ Ab + Al + Af + An + Ar + Ac
    B =~ Bb + Bl + Bf + Bn + Br + Bc

    A ~ B + age + pea
'

df$sex <- factor(df$sex, labels = c("boy", "girl"))
# fit a multiple group latent regression model
fit3 <- sem(model3, data = df, std.lv = TRUE, group = "sex")

# HERE FOLLOWS THE CALL TO THE BAIN S3 FUNCTION:

hypotheses31 <-
  "A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl
"
set.seed(100)
y1 <- gorica(fit3, hypotheses31, standardize = TRUE, iterations = 1000)

test_that("gorica and bain give similar results for multigroup", {
  expect_equivalent(y1$fit$gorica_weights, c(0.998362681570012, 0.00163731842998828), tolerance = .1)
})

hypotheses32 <-
  "A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy < A~age.girl  &
A~pea.boy < A~pea.girl  &
A~B.boy < A~B.girl;
A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy = A~age.girl  &
A~pea.boy < A~pea.girl  &
A~B.boy < A~B.girl;
A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy = A~age.girl  &
A~pea.boy = A~pea.girl  &
A~B.boy < A~B.girl;
A=~Ab.boy = A=~Ab.girl &
A=~Al.boy = A=~Al.girl &
A=~Af.boy = A=~Af.girl &
A=~An.boy = A=~An.girl &
A=~Ar.boy = A=~Ar.girl &
A=~Ac.boy = A=~Ac.girl &
B=~Bb.boy = B=~Bb.girl &
B=~Bl.boy = B=~Bl.girl &
B=~Bf.boy = B=~Bf.girl &
B=~Bn.boy = B=~Bn.girl &
B=~Br.boy = B=~Br.girl &
B=~Bc.boy = B=~Bc.girl &
Ab~1.boy = Ab~1.girl &
Al~1.boy = Al~1.girl &
Af~1.boy = Af~1.girl &
An~1.boy = An~1.girl &
Ar~1.boy = Ar~1.girl &
Ac~1.boy = Ac~1.girl &
Bb~1.boy = Bb~1.girl &
Bl~1.boy = Bl~1.girl &
Bf~1.boy = Bf~1.girl &
Bn~1.boy = Bn~1.girl &
Br~1.boy = Br~1.girl &
Bc~1.boy = Bc~1.girl &
A~age.boy = A~age.girl  &
A~pea.boy = A~pea.girl  &
A~B.boy = A~B.girl"

set.seed(100)
y2_gor <- gorica(fit3, hypotheses32, standardize = TRUE, iterations = 1000)

test_that("gorica and bain give similar results for multigroup", {
  expect_equivalent(y2_gor$fit$gorica_weights, c(0.152985385659435, 0.287451220066925, 0.547375070410637, 0.0118969066339179, 0.000291417229085222), tolerance = .15)
})
