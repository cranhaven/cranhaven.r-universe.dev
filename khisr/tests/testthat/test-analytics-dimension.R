test_that("Analytics dimensions helper works", {
    expect_error(analytics_dimension('filter', 1, c(1, 2)))
    expect_error(analytics_dimension('dimension', c('P1', 'P2'), c(1, 2)))
    expect_error(analytics_dimension('dim', 'dx', c(1, 2)))

    expect_identical(analytics_dimension('filter', 'dx', c('dim-1', 'dim-2')), splice(list2(filter = 'dx:dim-1;dim-2')))
    expect_identical('dx' %.f% c('dim-1', 'dim-2'), splice(list2(filter = 'dx:dim-1;dim-2')))
    expect_identical('dx' %.d% c('dim-1', 'dim-2'), splice(list2(dimension = 'dx:dim-1;dim-2')))
    expect_identical('pe' %.d% 'all', splice(list2(dimension = 'pe')))
})
