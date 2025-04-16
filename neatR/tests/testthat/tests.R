test_that('ndate tests', {
  expect_equal(ndate(as.Date('2023-01-01')), 'Jan 01, 2023 (Sun)')
  expect_equal(ndate(as.POSIXct('2023-01-01 07:00:00 PST')),
               'Jan 01, 2023 (Sun)')
  expect_error(ndate(1))
  expect_error(ndate(1.2))
  expect_error(ndate('a'))
  expect_error(ndate('2023-01-01'))
  expect_error(ndate(data.frame(x = '2023-01-01')))
})

test_that('nday tests', {
  expect_equal(nday(as.Date('2022-01-01'), reference.alias = TRUE), 'Sat')
  expect_equal(nday(as.POSIXct('2022-01-01 07:00:00 PST')), 'Sat')
  expect_error(nday(1))
  expect_error(nday(1.2))
  expect_error(nday('a'))
  expect_error(nday('2023-01-01'))
  expect_error(nday(data.frame(x = '2023-01-01')))
})

test_that('nnumber tests', {
  expect_equal(nnumber(1e3), '1.0 K')
  expect_equal(nnumber(123456.123456), '123.5 K')
  expect_equal(nnumber(12e6, unit = 'custom'), "12.0 Mn")
  expect_equal(nnumber(12e6, unit = 'custom', digits = 0), "12 Mn")
  expect_equal(nnumber(12e6, unit = 'Mn'), "12 Mn")
  expect_equal(nnumber(12e6, unit = 'M',
                       unit.labels = list(million = 'M')), "12 M")
  expect_equal(nnumber(12e6, unit = 'Mn', prefix = "$"), "$12 Mn")
  expect_equal(nnumber(12e6, unit = 'Mn', prefix = "$",
                       suffix = " CAD"), "$12 Mn CAD")
  expect_equal(nnumber(12e6, unit = '', prefix = "$",
                       thousand.separator = ","), "$12,000,000")
  expect_equal(nnumber(12e6, unit = '', prefix = "$",
                       thousand.separator = "."), "$12.000.000")
  expect_error(nnumber(as.Date('2022-01-01')))
  expect_error(nnumber(as.POSIXct('2022-01-01 07:00:00 PST')))
  expect_error(nnumber('a'))
  expect_error(nnumber('2023-01-01'))
  expect_error(nnumber(data.frame(x = '2023-01-01')))
})

test_that('npercent tests', {
  expect_equal(npercent(0.22), '+22.0%')
  expect_equal(npercent(22, is.decimal = FALSE), '+22.0%')
  expect_equal(npercent(22.345, is.decimal = FALSE, digits = 1), '+22.3%')
  expect_equal(npercent(22.345, is.decimal = FALSE, digits = 1,
    plus.sign = FALSE), '22.3%')
  expect_equal(npercent(223.345, is.decimal = FALSE, digits = 1,
    plus.sign = FALSE, factor.out = TRUE),
               '223.3% (2.2x growth)')
  expect_error(npercent('a'))
  expect_error(npercent(as.Date('2022-01-01')))
  expect_error(npercent(as.POSIXct('2022-01-01 07:00:00 PST')))
  expect_error(npercent('2023-01-01'))
  expect_error(npercent(data.frame(x = '2023-01-01')))
})


test_that('nstring tests', {
  expect_equal(nstring('  abcDEF!!    '), 'abcDEF!!')
  expect_equal(nstring('  abcDEF!!    ', case = NULL), 'abcDEF!!')
  expect_equal(nstring('  abcDEF!!    ', case = 'lower'), 'abcdef!!')
  expect_equal(nstring('  abcDEF!!    ', case = 'upper'), 'ABCDEF!!')
  expect_equal(nstring('  abcDEF are alphabets!!    ', case = 'title'),
    'Abcdef are Alphabets!!')
  expect_equal(nstring('  abcDEF are alphabets!!    ',
    case = 'start', remove.specials = TRUE), 'Abcdef Are Alphabets')
  expect_equal(nstring('all is well âÂ', case = 'start', en.only = TRUE),
               'All Is Well')
  expect_error(nstring(1))
  expect_error(nstring(0.22))
  expect_error(nstring(as.Date('2022-01-01')))
  expect_error(nstring(as.POSIXct('2022-01-01 07:00:00 PST')))
  expect_error(nstring(data.frame(x = '2023-01-01')))
})

test_that('ntimestamp tests', {
  expect_equal(ntimestamp(as.POSIXct('2022-01-01 07:15:43 PST'),
    include.timezone = FALSE), 'Jan 01, 2022 07H 15M 43S AM (Sat)')
  expect_equal(ntimestamp(as.POSIXct('2022-01-01 07:15:43 PST'),
    include.date = FALSE, include.timezone = FALSE), '07H 15M 43S AM (Sat)')
  expect_equal(ntimestamp(as.POSIXct('2022-01-01 07:15:43 PST'),
    include.seconds = FALSE, include.timezone = FALSE),
    'Jan 01, 2022 07H 15M AM (Sat)')
  expect_error(ntimestamp(1))
  expect_error(ntimestamp(0.22))
  expect_error(ntimestamp(as.Date('2022-01-01')))
  expect_error(ntimestamp('a'))
  expect_error(ntimestamp(data.frame(x = '2023-01-01')))
})
