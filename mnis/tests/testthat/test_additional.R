library(mnis)
context("additional")


test_that("mnis_additional returns expected format", {
  skip_on_cran()

  xbdnu <- mnis_basic_details(172)
  expect_type(xbdnu, "list")

  xbdnudods <- mnis_basic_details(25790, ref_dods = TRUE)
  expect_type(xbdnudods, "list")

  xbdncom <- mnis_committees(172)
  expect_type(xbdncom, "list")

  xstaff <- mnis_staff(172)
  expect_type(xstaff, "list")

  xfbs <- mnis_full_biog(7)
  expect_type(xfbs, "list")

  xbioe <- mnis_biography_entries(7)
  expect_type(xbioe, "list")

  xadd <- mnis_addresses(452)
  expect_type(xadd, "list")

  xcon <- mnis_constituencies(143)
  expect_type(xcon, "list")

  xec <- mnis_elections_contested(7)
  expect_type(xec, "list")

  xexp <- mnis_experiences(3898)
  expect_type(xexp, "list")

  xgp <- mnis_government_posts(15)
  expect_type(xgp, "list")

  xhon <- mnis_honours(288)
  expect_type(xhon, "list")

  xhm <- mnis_house_memberships(500)
  expect_type(xhm, "list")

  xstatus <- mnis_statuses(1467)
  expect_type(xstatus, "list")

  xi <- mnis_interests(500)
  expect_type(xi, "list")

  xka <- mnis_known_as(500)
  expect_type(xka, "list")

  xms <- mnis_maiden_speeches(7)
  expect_type(xms, "list")

  xop <- mnis_opposition_posts(172)
  expect_type(xop, "list")

  xoparls <- mnis_other_parliaments(577)
  expect_type(xoparls, "list")

  xpp <- mnis_parliamentary_posts(17)
  expect_type(xpp, "list")

  xparties <- mnis_parties(1527)
  expect_type(xparties, "list")

  xpn <- mnis_preferred_names(288)
  expect_type(xpn, "list")


})
