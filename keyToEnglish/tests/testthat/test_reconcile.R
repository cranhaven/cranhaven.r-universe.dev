test_that(
  'reconcile_leporine',
  expect_true(reconcile_misspellings('leoprine LEOPRINE Leoprine') == 'leporine LEPORINE Leporine')
)
