# file path utilities ----------------------------
test_that("match chamber", {
  # all
  expect_equal(match_chamber("all"), "HS")
  expect_equal(match_chamber("ALL"), "HS")
  expect_equal(match_chamber("congress"), "HS")
  expect_equal(match_chamber("coNGrEsS"), "HS")

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
  expect_equal(match_congress(0), "all")
  expect_equal(match_congress(-2), "all")
  expect_equal(match_congress("five"), "all")
  expect_equal(match_congress("all"), "all")
})

test_that("invalid data sources for `build_file_path()`", {
  expect_error(build_file_path(), "argument \"data_source\" is missing, with no default")
  expect_error(build_file_path(data_source = ""), "Invalid data source name: ")
  expect_error(build_file_path(data_source = "not a source"), "Invalid data source name: not a source")
})

# Voteview ---------------------------------------
test_that("`build_voteview_file_path()`: online paths", {
  # sheet_type
  expect_equal(build_voteview_file_path(sheet_type = "members", local = FALSE),
               "https://voteview.com/static/data/out/members/HSall_members.csv")
  expect_equal(build_voteview_file_path(sheet_type = "votes", local = FALSE),
               "https://voteview.com/static/data/out/votes/HSall_votes.csv")
  expect_equal(build_voteview_file_path(sheet_type = "parties", local = FALSE),
               "https://voteview.com/static/data/out/parties/HSall_parties.csv")
  expect_equal(build_voteview_file_path(sheet_type = "rollcalls", local = FALSE),
               "https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.csv")

  # chamber_code
  expect_equal(build_voteview_file_path(sheet_type = "votes", chamber_code = "HS", local = FALSE),
               "https://voteview.com/static/data/out/votes/HSall_votes.csv")
  expect_equal(build_voteview_file_path(sheet_type = "votes", chamber_code = "S", local = FALSE),
               "https://voteview.com/static/data/out/votes/Sall_votes.csv")
  expect_equal(build_voteview_file_path(sheet_type = "votes", chamber_code = "H", local = FALSE),
               "https://voteview.com/static/data/out/votes/Hall_votes.csv")

  # congress_code
  expect_equal(build_voteview_file_path(sheet_type = "parties", congress_code = "001", local = FALSE),
               "https://voteview.com/static/data/out/parties/HS001_parties.csv")
  expect_equal(build_voteview_file_path(sheet_type = "parties", congress_code = "090", local = FALSE),
               "https://voteview.com/static/data/out/parties/HS090_parties.csv")
  expect_equal(build_voteview_file_path(sheet_type = "parties", congress_code = "118", local = FALSE),
               "https://voteview.com/static/data/out/parties/HS118_parties.csv")
  expect_equal(build_voteview_file_path(sheet_type = "parties", congress_code = "all", local = FALSE),
               "https://voteview.com/static/data/out/parties/HSall_parties.csv")
})

test_that("`build_voteview_file_path()`: local paths", {
  # basics
  expect_equal(build_voteview_file_path(sheet_type = "members", local = TRUE),
               "./HSall_members.csv")
  expect_equal(build_voteview_file_path(sheet_type = "parties", congress_code = "105", local = TRUE),
               "./HS105_parties.csv")
  expect_equal(build_voteview_file_path(sheet_type = "votes", chamber_code = "S", local = TRUE),
               "./Sall_votes.csv")

  # local_dir
  expect_equal(build_voteview_file_path(sheet_type = "rollcalls", local = TRUE, local_dir = "dir"),
               "dir/HSall_rollcalls.csv")
  expect_equal(build_voteview_file_path(sheet_type = "votes", chamber_code = "H", congress_code = "042",
                                        local = TRUE, local_dir = "../other_dir/subdir"),
               "../other_dir/subdir/H042_votes.csv")
})

test_that("`build_file_path()` for Voteview", {
  # expected usage
  expect_equal(build_file_path(data_source = "voteview", sheet_type = "parties"),
               "https://voteview.com/static/data/out/parties/HSall_parties.csv")
  expect_equal(build_file_path(data_source = "voteview", sheet_type = "rollcalls"),
               "https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.csv")

  # specify chamber, congress
  expect_equal(build_file_path(data_source = "voteview", sheet_type = "members", congress = 117),
               "https://voteview.com/static/data/out/members/HS117_members.csv")
  expect_equal(build_file_path(data_source = "voteview", sheet_type = "rollcalls", chamber = "senate"),
               "https://voteview.com/static/data/out/rollcalls/Sall_rollcalls.csv")
  expect_equal(build_file_path(data_source = "voteview", sheet_type = "votes", chamber = "hr", congress = 90),
               "https://voteview.com/static/data/out/votes/H090_votes.csv")

  # TODO: specify local path
})

# Harbridge-Yong, Volden, and Wiseman ------------
test_that("`build_hvw_file_path()`: online paths", {
  expect_equal(build_hvw_file_path(chamber_code = "H", local = FALSE),
               "https://dataverse.harvard.edu/api/access/datafile/6299608")
  expect_equal(build_hvw_file_path(chamber_code = "S", local = FALSE),
               "https://dataverse.harvard.edu/api/access/datafile/6299605")
})

test_that("`build_hvw_file_path()`: local paths", {
  expect_equal(build_hvw_file_path(chamber_code = "H", local = TRUE),
               "./HarbridgeYong_Volden_Wiseman_House_Replication.tab")
  expect_equal(build_hvw_file_path(chamber_code = "S", local = TRUE),
               "./HarbridgeYong_Volden_Wiseman_Senate_Replication.tab")

  # local_dir
  expect_equal(build_hvw_file_path(chamber_code = "H", local = TRUE, local_dir = "test_dir"),
               "test_dir/HarbridgeYong_Volden_Wiseman_House_Replication.tab")
  expect_equal(build_hvw_file_path(chamber_code = "S", local = TRUE, local_dir = "~/a/b/c"),
               "~/a/b/c/HarbridgeYong_Volden_Wiseman_Senate_Replication.tab")
})

test_that("`build_hvw_file_path()`: invalid `chamber_code` errors", {
  # any `chamber_code` beside "H" or "S" is an error
  expect_error(build_hvw_file_path(chamber_code = "HS"),
               paste("Invalid `chamber` argument \\(\"HS\"\\) provided for `get_hvw_data\\(\\)`.",
                     "`chamber` must be either House or Senate, not both.", sep = "\\n"))
  expect_error(build_hvw_file_path(chamber_code = "something else"),
               paste("Invalid `chamber` argument \\(\"something else\"\\) provided for `get_hvw_data\\(\\)`.",
                     "`chamber` must be either House or Senate, not both.", sep = "\\n"))
})

test_that("`build_file_path()` for HVW", {
  # hvw and lhy are equivalent
  expect_equal(build_file_path(data_source = "hvw", chamber = "hr"),
               "https://dataverse.harvard.edu/api/access/datafile/6299608")
  expect_equal(build_file_path(data_source = "lhy", chamber = "hr"),
               "https://dataverse.harvard.edu/api/access/datafile/6299608")

  expect_equal(build_file_path(data_source = "hvw", chamber = "s"),
               "https://dataverse.harvard.edu/api/access/datafile/6299605")
  expect_equal(build_file_path(data_source = "lhy", chamber = "s"),
               "https://dataverse.harvard.edu/api/access/datafile/6299605")

  # need to specify a chamber
  expect_error(build_file_path(data_source = "hvw"),
               paste("Invalid `chamber` argument \\(\"HS\"\\) provided for `get_hvw_data\\(\\)`.",
                     "`chamber` must be either House or Senate, not both.", sep = "\\n"))

  # TODO: how to check that it successfully reads local files?
  # withr::with_file(test_path("./HarbridgeYong_Volden_Wiseman_House_Replication.tab"),
  #                  expect_equal(build_file_path(data_source = "hvw", chamber = "hr"),
  #                               "./HarbridgeYong_Volden_Wiseman_House_Replication.tab"))
})

# LES --------------------------------------------
test_that("`build_les_file_path()`: online paths", {
  # classic LES
  expect_equal(build_les_file_path(chamber_code = "H", les_2 = F, local = F),
               "https://thelawmakers.org/wp-content/uploads/2023/04/CELHouse93to117ReducedClassic.dta")
  expect_equal(build_les_file_path(chamber_code = "S", les_2 = F, local = F),
               "https://thelawmakers.org/wp-content/uploads/2023/04/CELSenate93to117ReducedClassic.dta")

    # LES 2
  expect_equal(build_les_file_path(chamber_code = "H", les_2 = T, local = F),
               "https://thelawmakers.org/wp-content/uploads/2023/04/CELHouse117ReducedLES2.dta")
  expect_equal(build_les_file_path(chamber_code = "S", les_2 = T, local = F),
               "https://thelawmakers.org/wp-content/uploads/2023/04/CELSenate117ReducedLES2.dta")
})

test_that("`build_les_file_path()`: local paths", {
  # basics
  expect_equal(build_les_file_path(chamber_code = "H", les_2 = F, local = T),
               "./CELHouse93to117ReducedClassic.dta")
  expect_equal(build_les_file_path(chamber_code = "S", les_2 = T, local = T),
               "./CELSenate117ReducedLES2.dta")

  # local_dir
  expect_equal(build_les_file_path(chamber_code = "S", les_2 = F,
                                   local = T, local_dir = "~/Downloads"),
               "~/Downloads/CELSenate93to117ReducedClassic.dta")
  expect_equal(build_les_file_path(chamber_code = "H", les_2 = T,
                                   local = T, local_dir = "./subdir/subsubdir"),
               "./subdir/subsubdir/CELHouse117ReducedLES2.dta")
})

test_that("`build_les_file_path()`: invalid `chamber_code` errors", {
  # any `chamber_code` beside "H" or "S" is an error
  expect_error(build_les_file_path(chamber_code = "HS"),
               paste("Invalid `chamber` argument \\(\"HS\"\\) provided for `get_les\\(\\)`.",
                     "`chamber` must be either House or Senate, not both.", sep = "\\n"))
  expect_error(build_les_file_path(chamber_code = "foo"),
               paste("Invalid `chamber` argument \\(\"foo\"\\) provided for `get_les\\(\\)`.",
                     "`chamber` must be either House or Senate, not both.", sep = "\\n"))
})

test_that("`build_file_path()` for LES", {
  # basics
  # NOTE: `sheet_type` in `build_file_path()` refers to `les_2` for LES data
  expect_equal(build_file_path(data_source = "les", chamber = "sen", sheet_type = FALSE),
               "https://thelawmakers.org/wp-content/uploads/2023/04/CELSenate93to117ReducedClassic.dta")
  expect_equal(build_file_path(data_source = "les", chamber = "house", sheet_type = TRUE),
               "https://thelawmakers.org/wp-content/uploads/2023/04/CELHouse117ReducedLES2.dta")

  # need to specify a chamber
  expect_error(build_file_path(data_source = "les"),
               paste("Invalid `chamber` argument \\(\"HS\"\\) provided for `get_les\\(\\)`.",
                     "`chamber` must be either House or Senate, not both.", sep = "\\n"))
})
