library(lavaan)
model1 <- '
    A =~ Ab + Al + Af + An + Ar + Ac
    B =~ Bb + Bl + Bf + Bn + Br + Bc
'
fit1 <- sem(model1, data = sesamesim, std.lv = TRUE)
hypotheses1 <- "
A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac > .6 &
B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc > .6
"
set.seed(100)
out1_c <- gorica(fit1, hypotheses1, comparison = "complement", standardize=FALSE) # TO DO
expect_equivalent(out1_c$fit[,2][1], 9.14, tolerance = .01)
expect_equivalent(out1_c$fit[,2][2], 11.89, tolerance = .5)
