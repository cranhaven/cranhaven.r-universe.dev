test_that("Bhattacharyya Distance works", {
   expect_equal(dissbhattacharyya(matrix(c('c2', 'c3', 'c1',
                                       'c3', 'c3', 'c2',
                                       'c3', 'c1', 'c3',
                                       'c3', 'c3', 'c2',
                                       'c2', 'c2', 'c3'),
                                     nrow = 3)),
                distancematrix(matrix(c('c2', 'c3', 'c1',
                                        'c3', 'c3', 'c2',
                                        'c3', 'c1', 'c3',
                                        'c3', 'c3', 'c2',
                                        'c2', 'c2', 'c3'),
                                      nrow = 3), d='bhattacharyya'))
})

test_that("Chi-square Distance works", {
   expect_equal(disschisquare(matrix(c('c2', 'c3', 'c1',
                                       'c3', 'c3', 'c2',
                                       'c3', 'c1', 'c3',
                                       'c3', 'c3', 'c2',
                                       'c2', 'c2', 'c3'),
                                     nrow = 3)),
                distancematrix(matrix(c('c2', 'c3', 'c1',
                                        'c3', 'c3', 'c2',
                                        'c3', 'c1', 'c3',
                                        'c3', 'c3', 'c2',
                                        'c2', 'c2', 'c3'),
                                      nrow = 3), d='chisquare'))
})

test_that("Cramer's V Distance works", {
  expect_equal(disscramerv(matrix(c('c2', 'c3', 'c1',
                                      'c3', 'c3', 'c2',
                                      'c3', 'c1', 'c3',
                                      'c3', 'c3', 'c2',
                                      'c2', 'c2', 'c3'),
                                    nrow = 3)),
               distancematrix(matrix(c('c2', 'c3', 'c1',
                                       'c3', 'c3', 'c2',
                                       'c3', 'c1', 'c3',
                                       'c3', 'c3', 'c2',
                                       'c2', 'c2', 'c3'),
                                     nrow = 3), d='cramerV'))
})

test_that("Hamming Distance works", {
   expect_equal(disshamming(matrix(c('c2', 'c3', 'c1',
                                       'c3', 'c3', 'c2',
                                       'c3', 'c1', 'c3',
                                       'c3', 'c3', 'c2',
                                       'c2', 'c2', 'c3'),
                                     nrow = 3)),
                distancematrix(matrix(c('c2', 'c3', 'c1',
                                        'c3', 'c3', 'c2',
                                        'c3', 'c1', 'c3',
                                        'c3', 'c3', 'c2',
                                        'c2', 'c2', 'c3'),
                                      nrow = 3), d='hamming'))
})

test_that("Hellinger Distance works", {
   expect_equal(disshellinger(matrix(c('c2', 'c3', 'c1',
                                     'c3', 'c3', 'c2',
                                     'c3', 'c1', 'c3',
                                     'c3', 'c3', 'c2',
                                     'c2', 'c2', 'c3'),
                                   nrow = 3)),
                distancematrix(matrix(c('c2', 'c3', 'c1',
                                        'c3', 'c3', 'c2',
                                        'c3', 'c1', 'c3',
                                        'c3', 'c3', 'c2',
                                        'c2', 'c2', 'c3'),
                                      nrow = 3), d='hellinger'))
})
