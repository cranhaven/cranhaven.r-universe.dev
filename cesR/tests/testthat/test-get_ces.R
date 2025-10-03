context("get_ces")

test_that("get_ces produces a data object from the requested dataset",{

  skip_if_offline()
  skip_on_cran()

  # User requests the 2019 CES online survey
  cesR::get_ces("ces2019_web")
  expect_true(exists("ces2019_web"))

  # User requests the 2019 CES phone survey
  cesR::get_ces("ces2019_phone")
  expect_true(exists("ces2019_phone"))

  # User requests the 2015 CES online survey
  cesR::get_ces("ces2015_web")
  expect_true(exists("ces2015_web"))

  # User requests the 2015 CES phone survey
  cesR::get_ces("ces2015_phone")
  expect_true(exists("ces2015_phone"))

  # User requests the 2015 CES combined survey
  cesR::get_ces("ces2015_combo")
  expect_true(exists("ces2015_combo"))

  # User requests the 2011 CES survey
  cesR::get_ces("ces2011")
  expect_true(exists("ces2011"))

  # User requests the 2008 CES survey
  cesR::get_ces("ces2008")
  expect_true(exists("ces2008"))

  # User requests the 2004 CES survey
  cesR::get_ces("ces2004")
  expect_true(exists("ces2004"))

  # User requests the 2004-2011 CES surveys
  cesR::get_ces("ces0411")
  expect_true(exists("ces0411"))

  # User requests the 2004-2006 CES surveys
  cesR::get_ces("ces0406")
  expect_true(exists("ces0406"))

  # User requests the 2000 CES survey
  cesR::get_ces("ces2000")
  expect_true(exists("ces2000"))

  # User requests the 1997 CES survey
  cesR::get_ces("ces1997")
  expect_true(exists("ces1997"))

  # User requests the 1993 CES survey
  cesR::get_ces("ces1993")
  expect_true(exists("ces1993"))

  # User requests the 1988 CES survey
  cesR::get_ces("ces1988")
  expect_true(exists("ces1988"))

  # User requests the 1984 CES survey
  cesR::get_ces("ces1984")
  expect_true(exists("ces1984"))

  # User requests the 1974 CES survey
  cesR::get_ces("ces1974")
  expect_true(exists("ces1974"))

  # User requests the 1974-1980 CES surveys
  cesR::get_ces("ces7480")
  expect_true(exists("ces7480"))

  # User requests the 1972 June-July CES surveys
  cesR::get_ces("ces72_jnjl")
  expect_true(exists("ces72_jnjl"))

  # User requests 1972 September CES surveys
  cesR::get_ces("ces72_sep")
  expect_true(exists("ces72_sep"))

  # User requests the 1972 November CES survey
  cesR::get_ces("ces72_nov")
  expect_true(exists("ces72_nov"))

  # User requests the 1968 CES survey
  cesR::get_ces("ces1968")
  expect_true(exists("ces1968"))

  # User requests the 1965 CES survey
  cesR::get_ces("ces1965")
  expect_true(exists("ces1965"))
})

test_that("get_ces removes the downloaded data file",{

  skip_if_offline()
  skip_on_cran()

  # User requests the 2019 CES online survey
  cesR::get_ces("ces2019_web")
  expect_true(!file.exists(tempfile(fileext = ".dta")))

  # User requests the 2019 CES phone survey
  cesR::get_ces("ces2019_phone")
  expect_true(!file.exists(tempfile(fileext = ".tab")))

  # User requests the 2015 CES online survey
  cesR::get_ces("ces2015_web")
  expect_true(!file.exists(tempfile(fileext = ".zip")))

  # User requests the 2015 CES phone survey
  cesR::get_ces("ces2015_phone")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces2015_phone")))

  # User requests the 2015 CES combined survey
  cesR::get_ces("ces2015_combo")
  expect_true(!file.exists(tempfile(fileext = ".zip")))

  # User requests the 2011 CES survey
  cesR::get_ces("ces2011")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces2011")))

  # User requests the 2008 CES survey
  cesR::get_ces("ces2008")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces2008")))

  # User requests the 2004 CES survey
  cesR::get_ces("ces2004")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces2004")))

  # User requests the 2004-2011 CES surveys
  cesR::get_ces("ces0411")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces0411")))

  # User requests the 2004-2006 CES surveys
  cesR::get_ces("ces0406")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces0406")))

  # User requests the 2000 CES survey
  cesR::get_ces("ces2000")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces2000")))

  # User requests the 1997 CES survey
  cesR::get_ces("ces1997")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1997")))

  # User requests the 1993 CES survey
  cesR::get_ces("ces1993")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1993")))

  # User requests the 1988 CES survey
  cesR::get_ces("ces1988")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1988")))

  # User requests the 1984 CES survey
  cesR::get_ces("ces1984")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1984")))

  # User requests the 1974 CES survey
  cesR::get_ces("ces1974")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1974")))

  # User requests the 1974-1980 CES surveys
  cesR::get_ces("ces7480")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces7480")))

  # User requests the 1972 June-July CES surveys
  cesR::get_ces("ces72_jnjl")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces72jnjl")))

  # User requests 1972 September CES surveys
  cesR::get_ces("ces72_sep")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces72sep")))

  # User requests the 1972 November CES survey
  cesR::get_ces("ces72_nov")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces72nov")))

  # User requests the 1968 CES survey
  cesR::get_ces("ces1968")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1968")))

  # User requests the 1965 CES survey
  cesR::get_ces("ces1965")
  expect_true(!file.exists(tempfile(fileext = ".zip")))
  expect_true(!file.exists(paste0(tempdir(), "\\ces1965")))

})

test_that("get_ces produces and error message when provided with an incorrect request code",{

  expect_error(cesR::get_ces("ces2019web"))
  expect_error(cesR::get_ces("cesphone2019"))
  expect_error(cesR::get_ces("2011ces"))
})
