context('Adding new individuals')

test_that('age',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 = as.Date(
                              c('1994-01-01', '1990-01-01', '1991-01-01', '1992-01-01')
                            ))
  new.order <- DynaRankR:::add_new_ids_age(new.ids = 'a', working.ranks =  letters[2:4], 
                              contestants = contestants, period = 1)
  
  expect_identical(new.order[4], 'a')
})

test_that('tenure',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 = as.Date(
                              c('1994-01-01', '1990-01-01', '1991-01-01', '1992-01-01')
                            ))
  new.order <- DynaRankR:::add_new_ids_tenure(new.ids = 'a', working.ranks =  letters[2:4], 
                                           contestants = contestants, period = 1)
  
  expect_identical(new.order[4], 'a')
})

test_that('mri',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 =  c('b', 'o', 'f', 'g'))
  new.order <- DynaRankR:::add_new_ids_mri(new.ids = 'a', working.ranks =  letters[2:4], 
                                              contestants = contestants, period = 1)
  
  expect_identical(new.order[2], 'a')
})

test_that('phys.attr',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 = c(45, 55, 34, 23))
  new.order <- DynaRankR:::add_new_ids_phys_attr(new.ids = 'a', working.ranks =  letters[2:4], 
                                              contestants = contestants, period = 1)
  
  expect_identical(new.order[2], 'a')
})


##Elo
test_that('age elo',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 = as.Date(
                              c('1994-01-01', '1990-01-01', '1991-01-01', '1992-01-01')
                            ))
  
  current.scores <- data.frame(id = letters[2:4],
                               score = c(100, 80, 60))
  
  
  id.score <- DynaRankR:::add_new_ids_age_elo(new.ids = 'a', 
                                           contestants = contestants, period = 1,
                                           current.scores = current.scores)
  
  expect_identical(id.score[1,2], 60)
})

test_that('tenure elo',{
  
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 = as.Date(
                              c('1994-01-01', '1990-01-01', '1991-01-01', '1992-01-01')
                            ))
  
  current.scores <- data.frame(id = letters[2:4],
                               score = c(100, 80, 60))
  
  
  id.scores <- DynaRankR:::add_new_ids_tenure_elo(new.ids = 'a', 
                                              contestants = contestants, period = 1,
                                              current.scores = current.scores)
  
  expect_equal(id.scores[1,2], as.numeric(quantile(c(100,80,60), 0)))
})

test_that('mri elo',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 =  c('b', 'o', 'f', 'g'),stringsAsFactors = FALSE)
  
  current.scores <- data.frame(id = letters[2:4],
                               score = c(100, 80, 60))
  
  ids.score <- DynaRankR:::add_new_ids_mri_elo(new.ids = 'a', current.scores = current.scores, 
                                           contestants = contestants, period = 1)
  
  expect_equal(ids.score[1,2], 99.9)
})

test_that('phys.attr elo',{
  
  contestants <- data.frame(id = letters[1:4],
                            period = 1,
                            convention1 = c(45, 55, 34, 23))
  
  current.scores <- data.frame(id = letters[2:4],
                               score = c(100, 80, 60))
  
  id.scores <- DynaRankR:::add_new_ids_phys_attr_elo(new.ids = 'a', current.scores = current.scores, 
                                                 contestants = contestants, period = 1)
  
  expect_equal(id.scores[1,2], as.numeric(quantile(c(100,80,60), 0.66666666667)))
})

