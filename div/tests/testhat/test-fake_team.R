# test_div

library(div)
d <- div_fake_team(seed=2003, N = 550)
p <- div_paygap(d)

t <- tibble(Grade = p$grade, JobID = p$jobID, PayGap = round(p$paygap, 2), Confidence = p$confidence)
knitr::kable(t)
