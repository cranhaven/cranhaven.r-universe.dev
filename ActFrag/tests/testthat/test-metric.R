testthat::context("Checking fragmentation code")
data(example_activity_data)
count1 = c(t(example_activity_data$count[1,-c(1,2)]))
wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))

testthat::test_that("Running all metrics", {

  frag = fragmentation(x = count1, w = wear1, thresh = 100,
                       bout.length = 1, metrics = "mean_bout")
  testthat::expect_named(frag, c("mean_r", "mean_a"))
  # no w
  testthat::expect_error({
    fragmentation(x = count1, thresh = 100,
                  bout.length = 1, metrics = "mean_bout")
  })

  # no w
  testthat::expect_error({
    fragmentation( w = wear1, thresh = 100,
                   bout.length = 1, metrics = "mean_bout")
  })
  testthat::expect_equal(
    frag,
    list(mean_r = 6.6144578313253,
         mean_a = 2.03571428571429))
  frag = fragmentation(x = count1, w = wear1, thresh = 100,
                       bout.length = 1, metrics = "all")

  testthat::expect_equal(
    frag,
    list(mean_r = 6.6144578313253,
         mean_a = 2.03571428571429,
         SATP = 0.151183970856102,
         ASTP = 0.491228070175439,
         Gini_r = 0.649295837220667,
         Gini_a = 0.356443317128162,
         alpha_r = 1.55403223604806,
         alpha_a = 1.83585270210506,
         h_r = 0.22655568748512,
         h_a = 0.531574675324675) )
})


count = example_activity_data$count
wear = example_activity_data$wear


testthat::test_that("LONG: Running all metrics", {


  frag_by_day = fragmentation_long(
    count.data = count,
    weartime = wear,thresh = 100,bout.length = 1,
    metrics = "all", by = "day")
  testthat::expect_equal(dim(frag_by_day), c(275L, 12L))

  cn = c("mean_r", "mean_a", "SATP", "ASTP", "Gini_r",
         "Gini_a", "alpha_r", "alpha_a", "h_r", "h_a")
  testthat::expect_named(
    frag_by_day,
    c("ID", "Day", "mean_r", "mean_a",
      "SATP", "ASTP", "Gini_r",
      "Gini_a", "alpha_r", "alpha_a", "h_r", "h_a"))

  mns = colMeans(frag_by_day[, cn])

  testthat::expect_equal(
    mns,
    c(mean_r = 7.51030640100523,
      mean_a = 3.37636573560949,
      SATP = 0.164897923735134,
      ASTP = 0.325778313620286,
      Gini_r = 0.612809231248764,
      Gini_a = 0.46593315347615,
      alpha_r = 1.54774162696022,
      alpha_a = 1.687593281176,
      h_r = 0.249041535002562,
      h_a = 0.393625413291449))

  tp_by_subject = fragmentation_long(
    count.data = count,
    weartime = wear,thresh = 100,bout.length = 1,
    metrics = "TP", by = "subject")
  testthat::expect_equal(dim(tp_by_subject), c(50L, 3L))
  testthat::expect_named(tp_by_subject, c("ID", "SATP", "ASTP"))

  cn = c("SATP", "ASTP")
  mns = colMeans(tp_by_subject[, cn])

  testthat::expect_equal(
    mns,
    c(SATP = 0.16807853343359, ASTP = 0.317880153886575))
})

