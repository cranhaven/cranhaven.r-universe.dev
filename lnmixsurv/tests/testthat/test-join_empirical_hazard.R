km <- survival::survfit(survival::Surv(time, status == 2) ~ sex, data = survival::lung) |> 
  broom::tidy()

test_that("empirical hazards are joined as expected", {
  km_haz_joined <- join_empirical_hazard(km)
  expect_snapshot(km_haz_joined)
})
