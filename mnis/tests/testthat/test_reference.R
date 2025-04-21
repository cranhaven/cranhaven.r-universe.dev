
library(mnis)
context("reference")
test_that("mnis_reference returns expected format", {
  skip_on_cran()


  refallreferences <- mnis_all_reference()
  expect_type(refallreferences, "list")
  expect_true(is.list(refallreferences))

  refa <- ref_address_types()
  expect_type(refa, "list")
  expect_true(tibble::is_tibble(refa))

  refb <- ref_answering_bodies(tidy_style = "camelCase")

  expect_type(refb, "list")
  expect_true(tibble::is_tibble(refb))

  refc <- ref_areas(tidy_style = "period.case")

  expect_type(refc, "list")
  expect_true(tibble::is_tibble(refc))

  refd <- ref_area_types()

  expect_type(refd, "list")
  expect_true(tibble::is_tibble(refd))

  refe <- ref_biography_categories()

  expect_type(refe, "list")
  expect_true(tibble::is_tibble(refe))

  reff <- ref_cabinets()

  expect_type(reff, "list")
  expect_true(tibble::is_tibble(reff))

  refg <- ref_committees()

  expect_type(refg, "list")
  expect_true(tibble::is_tibble(refg))

  refh <- ref_committee_types()

  expect_type(refh, "list")
  expect_true(tibble::is_tibble(refh))

  refi <- ref_constituencies()

  expect_type(refi, "list")
  expect_true(tibble::is_tibble(refi))

  refj <- ref_constituency_areas()

  expect_type(refj, "list")
  expect_true(tibble::is_tibble(refj))

  refk <- ref_constituency_types()

  expect_type(refk, "list")
  expect_true(tibble::is_tibble(refk))

  refl <- ref_countries()

  expect_type(refl, "list")
  expect_true(tibble::is_tibble(refl))

  refm <- ref_departments()

  expect_type(refm, "list")
  expect_true(tibble::is_tibble(refm))

  refn <- ref_disqualification_types()

  expect_type(refn, "list")
  expect_true(tibble::is_tibble(refn))

  refo <- ref_elections()

  expect_type(refo, "list")
  expect_true(tibble::is_tibble(refo))

  refp <- ref_election_types()

  expect_type(refp, "list")
  expect_true(tibble::is_tibble(refp))

  refq <- ref_end_reasons()

  expect_type(refq, "list")
  expect_true(tibble::is_tibble(refq))

  refr <- ref_experience_types()

  expect_type(refr, "list")
  expect_true(tibble::is_tibble(refr))

  refs <- ref_government_post_departments()

  expect_type(refs, "list")
  expect_true(tibble::is_tibble(refs))

  reft <- ref_government_posts()

  expect_type(reft, "list")
  expect_true(tibble::is_tibble(reft))

  refu <- ref_government_ranks()

  expect_type(refu, "list")
  expect_true(tibble::is_tibble(refu))

  refv <- ref_honourary_prefixes()

  expect_type(refv, "list")
  expect_true(tibble::is_tibble(refv))

  refw <- ref_honour_lists()

  expect_type(refw, "list")
  expect_true(tibble::is_tibble(refw))

  refx <- ref_honours()

  expect_type(refx, "list")
  expect_true(tibble::is_tibble(refx))

  refy <- ref_interest_categories()

  expect_type(refy, "list")
  expect_true(tibble::is_tibble(refy))

  refz <- ref_lords_membership_types()

  expect_type(refz, "list")
  expect_true(tibble::is_tibble(refz))

  refaa <- ref_lords_ranks()

  expect_type(refaa, "list")
  expect_true(tibble::is_tibble(refaa))

  refbb <- ref_opposition_post_departments()

  expect_type(refbb, "list")
  expect_true(tibble::is_tibble(refbb))

  refcc <- ref_opposition_posts()

  expect_type(refcc, "list")
  expect_true(tibble::is_tibble(refcc))

  refdd <- ref_opposition_ranks()

  expect_type(refdd, "list")
  expect_true(tibble::is_tibble(refdd))

  refee <- ref_other_parliaments()

  expect_type(refee, "list")
  expect_true(tibble::is_tibble(refee))

  refff <- ref_parliamentary_posts()

  expect_type(refff, "list")
  expect_true(tibble::is_tibble(refff))

  refgg <- ref_parliamentary_ranks()

  expect_type(refgg, "list")
  expect_true(tibble::is_tibble(refgg))

  refhh <- ref_parliament_types()

  expect_type(refhh, "list")
  expect_true(tibble::is_tibble(refhh))

  refii <- ref_parties()

  expect_type(refii, "list")
  expect_true(tibble::is_tibble(refii))

  refjj <- ref_party_sub_types()

  expect_type(refjj, "list")
  expect_true(tibble::is_tibble(refjj))

  refkk <- ref_photo_outputs()

  expect_type(refkk, "list")
  expect_true(tibble::is_tibble(refkk))

  refll <- ref_statuses()

  expect_type(refll, "list")
  expect_true(tibble::is_tibble(refll))

  refmm <- ref_titles()

  expect_type(refmm, "list")
  expect_true(tibble::is_tibble(refmm))
})
