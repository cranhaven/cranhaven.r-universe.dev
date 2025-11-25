# file path utilities ----------------------------
test_that("match chamber", {
  # all
  expect_equal(match_chamber("all"), "HS")
  expect_equal(match_chamber("ALL"), "HS")
  expect_equal(match_chamber("congress"), "HS")
  expect_equal(match_chamber("coNGrEsS"), "HS")
  expect_equal(match_chamber("hs"), "HS")
  expect_equal(match_chamber("HS"), "HS")

  # house
  expect_equal(match_chamber("h"), "H")
  expect_equal(match_chamber("H"), "H")
  expect_equal(match_chamber("hr"), "H")
  expect_equal(match_chamber("HR"), "H")
  expect_equal(match_chamber("hR"), "H")
  expect_equal(match_chamber("house"), "H")
  expect_equal(match_chamber("House"), "H")
  expect_equal(match_chamber("HOUSE"), "H")

  # senate
  expect_equal(match_chamber("s"), "S")
  expect_equal(match_chamber("S"), "S")
  expect_equal(match_chamber("sen"), "S")
  expect_equal(match_chamber("Sen"), "S")
  expect_equal(match_chamber("senate"), "S")
  expect_equal(match_chamber("Senate"), "S")
  expect_equal(match_chamber("SENATE"), "S")
  expect_equal(match_chamber("sENaTE"), "S")

  # all by default
  expect_warning(abc_match <- match_chamber("abc"))
  expect_equal(abc_match, "HS")
  expect_warning(spaces_match <- match_chamber(" senate "))
  expect_equal(spaces_match, "HS")
})

test_that("match congress number", {
  # valid congress numbers
  expect_equal(match_congress(118), "118")
  expect_equal(match_congress(110), "110")
  expect_equal(match_congress(23), "023")
  expect_equal(match_congress(1), "001")

  # invalid congress numbers
  expect_error(match_congress(0), "Invalid `congress` argument")
  expect_error(match_congress(-2), "Invalid `congress` argument")
  expect_error(match_congress("five"), "Invalid `congress` argument")
  expect_error(match_congress("all"), "Invalid `congress` argument")
})

test_that("invalid data sources for `build_url()`", {
  expect_error(build_url(), "argument \"data_source\" is missing, with no default")
  expect_error(build_url(data_source = ""), "Invalid data source name: ")
  expect_error(build_url(data_source = "not a source"), "Invalid data source name: `not a source`")
})

# Voteview ---------------------------------------
test_that("`build_voteview_url()`: online paths", {
  # sheet_type
  expect_equal(build_voteview_url(sheet_type = "members"),
               "https://voteview.com/static/data/out/members/HSall_members.csv")
  expect_equal(build_voteview_url(sheet_type = "votes"),
               "https://voteview.com/static/data/out/votes/HSall_votes.csv")
  expect_equal(build_voteview_url(sheet_type = "parties"),
               "https://voteview.com/static/data/out/parties/HSall_parties.csv")
  expect_equal(build_voteview_url(sheet_type = "rollcalls"),
               "https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.csv")

  # chamber_code
  expect_equal(build_voteview_url(sheet_type = "votes", chamber_code = "HS"),
               "https://voteview.com/static/data/out/votes/HSall_votes.csv")
  expect_equal(build_voteview_url(sheet_type = "votes", chamber_code = "S"),
               "https://voteview.com/static/data/out/votes/Sall_votes.csv")
  expect_equal(build_voteview_url(sheet_type = "votes", chamber_code = "H"),
               "https://voteview.com/static/data/out/votes/Hall_votes.csv")

  # congress_code
  expect_equal(build_voteview_url(sheet_type = "parties", congress_code = "001"),
               "https://voteview.com/static/data/out/parties/HS001_parties.csv")
  expect_equal(build_voteview_url(sheet_type = "parties", congress_code = "090"),
               "https://voteview.com/static/data/out/parties/HS090_parties.csv")
  expect_equal(build_voteview_url(sheet_type = "parties", congress_code = "118"),
               "https://voteview.com/static/data/out/parties/HS118_parties.csv")
  expect_equal(build_voteview_url(sheet_type = "parties", congress_code = "all"),
               "https://voteview.com/static/data/out/parties/HSall_parties.csv")
})

test_that("`build_url()` for Voteview", {
  # expected usage
  expect_equal(build_url(data_source = "voteview", sheet_type = "parties"),
               "https://voteview.com/static/data/out/parties/HSall_parties.csv")
  expect_equal(build_url(data_source = "voteview", sheet_type = "rollcalls"),
               "https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.csv")

  # specify chamber, congress
  expect_equal(build_url(data_source = "voteview", sheet_type = "members", congress = 117),
               "https://voteview.com/static/data/out/members/HS117_members.csv")
  expect_equal(build_url(data_source = "voteview", sheet_type = "rollcalls", chamber = "senate"),
               "https://voteview.com/static/data/out/rollcalls/Sall_rollcalls.csv")
  expect_equal(build_url(data_source = "voteview", sheet_type = "votes", chamber = "hr", congress = 90),
               "https://voteview.com/static/data/out/votes/H090_votes.csv")
})

# Harbridge-Yong, Volden, and Wiseman ------------
test_that("`build_hvw_url()`: online paths", {
  expect_equal(build_hvw_url(chamber_code = "H"),
               "https://dataverse.harvard.edu/api/access/datafile/6299608")
  expect_equal(build_hvw_url(chamber_code = "S"),
               "https://dataverse.harvard.edu/api/access/datafile/6299605")
})

test_that("`build_hvw_url()`: invalid `chamber_code` errors", {
  # any `chamber_code` beside "H" or "S" is an error
  expect_error(build_hvw_url(chamber_code = "HS"),
               "Invalid `chamber` argument \\(`HS`\\) provided for `get_hvw_data\\(\\)`.")
  expect_error(build_hvw_url(chamber_code = "HS"),
               "Invalid `chamber` argument")
  expect_error(build_hvw_url(chamber_code = "something else"),
               "Invalid `chamber` argument \\(`something else`\\) provided for `get_hvw_data\\(\\)`.")
})

test_that("`build_url()` for HVW", {
  # hvw and lhy are equivalent
  expect_equal(build_url(data_source = "hvw", chamber = "hr"),
               "https://dataverse.harvard.edu/api/access/datafile/6299608")
  expect_equal(build_url(data_source = "lhy", chamber = "hr"),
               "https://dataverse.harvard.edu/api/access/datafile/6299608")

  expect_equal(build_url(data_source = "hvw", chamber = "s"),
               "https://dataverse.harvard.edu/api/access/datafile/6299605")
  expect_equal(build_url(data_source = "lhy", chamber = "s"),
               "https://dataverse.harvard.edu/api/access/datafile/6299605")

  # need to specify a chamber
  expect_error(build_url(data_source = "hvw"),
               "Invalid `chamber` argument \\(`HS`\\) provided for `get_hvw_data\\(\\)`.")
})

# LES --------------------------------------------
test_that("`build_les_url()`: online paths", {
  # classic LES
  expect_equal(build_les_url(chamber_code = "H"),
               "https://thelawmakers.org/wp-content/uploads/2025/03/CELHouse93to118Reduced.dta")
  expect_equal(build_les_url(chamber_code = "S"),
               "https://thelawmakers.org/wp-content/uploads/2025/03/CELSenate93to118Reduced.dta")
})

test_that("`build_les_url()`: invalid `chamber_code` errors", {
  # any `chamber_code` beside "H" or "S" is an error
  expect_error(build_les_url(chamber_code = "HS"),
               "Invalid `chamber` argument \\(`HS`\\) provided for `get_les\\(\\)`.")
  expect_error(build_les_url(chamber_code = "foo"),
               "Invalid `chamber` argument \\(`foo`\\) provided for `get_les\\(\\)`.")
})

test_that("`build_url()` for LES", {
  # basics
  # NOTE: `sheet_type` is not used for LES
  expect_equal(build_url(data_source = "les", chamber = "sen", sheet_type = FALSE),
               "https://thelawmakers.org/wp-content/uploads/2025/03/CELSenate93to118Reduced.dta")
  expect_equal(build_url(data_source = "les", chamber = "house", sheet_type = TRUE),
               "https://thelawmakers.org/wp-content/uploads/2025/03/CELHouse93to118Reduced.dta")

  # need to specify a chamber
  expect_error(build_url(data_source = "les"),
               "Invalid `chamber` argument \\(`HS`\\) provided for `get_les\\(\\)`.")
})
