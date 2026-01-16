# address_body ####
test_that("clean_address_body works for various addresses", {
  expect_equal(
    clean_address_body(c("18, 20 Londun st.", "136 Dixon rd")),
    c("18, 20 London Street.", "136 Dixon Road.")
  )
})

# address_number ####
test_that("clean_address_number works for various addresses", {
  expect_equal(
    clean_address_number(c("1S20 London Street.", "13<J Dixon Road.")),
    c("18, 20 London Street", "136 Dixon Road")
  )
})

# title ####
test_that("clean_title works for various titles", {
  expect_equal(
    clean_title(c("  Capt William Abbott", "?-Rev Alexander Abercromby")),
    c("Captain William Abbott", "Reverend Alexander Abercromby")
  )
})

# forename ####
test_that("clean_forename works for various forenames", {
  expect_equal(clean_forename(c("McWm.", "Jas.")), c("Mac William", "James"))
})

# surname ####
test_that("clean_surname works for various surnames", {
  expect_equal(clean_surname(c("ABOT", "ABRCROMBIE")), c("Abbott", "Abercromby"))
})

# specials ####
test_that("clean_specials works for various special characters", {
  expect_equal(
    clean_specials(c("Wine and spirit \u00bbmerchant", "Mac\u00bb William")),
    c("Wine and spirit merchant", "Mac William")
  )
})

# parentheses ####
test_that("clean_parentheses works in general", {
  expect_equal(clean_parentheses("Miller Street. (Agents)."), c("Miller Street."))
})

# mac ####
test_that("clean_mac works in general", {
  expect_equal(clean_mac("McWilliam"), c("Mac William"))
})

# address_pre_clean ####
test_that("clean_address_pre_clean works for various addresses", {
  expect_equal(
    clean_address_pre_clean(c(": 1S20 Lond»n st.   -", "13<J st enoch sq,;")),
    c("1S20 Londn st.", "13<J Saint Enoch sq.")
  )
})

# address_post_clean ####
test_that("clean_address_post_clean works for various addresses", {
  expect_equal(
    clean_address_post_clean(c(": 18, 20 London Street.   -", "136 Saint Enoch Square,;")),
    c("18, 20 London Street.", "136 Saint Enoch Square.")
  )
})

# address_mac ####
test_that("clean_address_mac works for various addresses", {
  expect_equal(
    clean_address_mac(c("McWilliam", "M'Donald", "M c Fyfe")),
    c("Mac William", "Mac Donald", "Mac Fyfe")
  )
})

# address_saints ####
test_that("clean_address_saints works for various addresses", {
  expect_equal(
    clean_address_saints(c("St Georges st.", "St Enoch sq")),
    c("Saint George st.", "Saint Enoch sq")
  )
})

# address_possessives ####
test_that("clean_address_possessives works for various address bodies", {
  expect_equal(
    clean_address_possessives(c("Adam's Court", "Aird's Lane")),
    c("Adams Court", "Airds Lane")
  )
})

# address_ends ####
test_that("clean_address_ends works for various addresses", {
  expect_equal(
    clean_address_ends(c(" -; 18, 20 London st.", ",,12 Dixon st.$  ")),
    c("18, 20 London st.", "12 Dixon st.")
  )
})

# address_attached_words ####
test_that("clean_address_attached_words works for various addresses", {
  expect_equal(
    clean_address_attached_words(c("18, 20 LondonSt.", "12 Dixon.St.")),
    c("18, 20 London St.", "12 Dixon St.")
  )
})

# address_places ####
test_that("clean_address_places works for various address bodies", {
  expect_equal(
    clean_address_places(c("Bothwell Cir.", "Railway arch.")),
    c("Bothwell Circus.", "Railway Arches.")
  )
})

# address_worksites ####
test_that("clean_address_worksites works in general", {
  expect_equal(clean_address_worksites("Woodyd"), "Woodyard")
})

# address_suffixes ####
test_that("clean_address_suffixes works in general", {
  expect_equal(
    clean_address_suffixes(c("36 Rose street so")), "36 Rose street South"
  )
})

# address_names ####
test_that("clean_address_names works for various addresses", {
  expect_equal(
    clean_address_names(c("18, 20 Londun Street", "136 Dixn Road")),
    c("18, 20 London Street", "136 Dixon Road")
  )
})

# address_others ####
test_that("clean_address_others works for various addresses", {
  expect_equal(
    clean_address_others(c("18, 20 London. Street, Agents", "136 Dixon Road d")),
    c("18, 20 London Street", "136 Dixon Road")
  )
})

# name_ends ####
test_that("clean_name_ends works for various names", {
  expect_equal(
    clean_name_ends(c("William-", "$Alexander", "John Hugh   ")),
    c("William", "Alexander", "John Hugh")
  )
})

# forename_separate_words ####
test_that("clean_forename_separate_words works for various forenames", {
  expect_equal(
    clean_forename_separate_words(c("William", "Alexander", "JohnHugh")),
    c("William", "Alexander", "John Hugh")
  )
})

# forename_punctuation ####
test_that("clean_forename_punctuation works for various forenames", {
  expect_equal(
    clean_forename_punctuation(c("William;", "Miss", "John,Hugh")),
    c("William.", "Miss.", "John Hugh.")
  )
})

# forename_spelling ####
test_that("clean_forename_spelling works for various forenames", {
  expect_equal(
    clean_forename_spelling(c("Wm", "A.", "Jn Huh")),
    c("William", "Alexander", "John Hugh")
  )
})

# surname_punctuation ####
test_that("clean_surname_punctuation works for various surnames", {
  expect_equal(
    clean_surname_punctuation(c("Abbott*", "*Abercromby", "Blair")),
    c("Abbott", "Abercromby", "Blair")
  )
})

# surname_spelling ####
test_that("clean_surname_spelling works for various surnames", {
  expect_equal(
    clean_surname_spelling(c("ABOT junior", "ABRCROMBIE", "BLAI senior")),
    c("Abbott", "Abercromby", "Blair")
  )
})

# string_ends ####
test_that("clean_string_ends works for various strings", {
  expect_equal(
    clean_string_ends(c(" -; 18, 20 Mary hill.*", ",,12 Dixon Street^")),
    c("18, 20 Mary hill", "12 Dixon Street")
  )
})

