# Archive ####
test_that("Archive works",
          {expect_equal(Archive(3, 5),
                        list("nsols" = 0,
                             "dim" = 3,
                             "scores" = matrix(0, 5, 3),
                             "solutions" = vector("list", 5)
                        )
          )
          }
)

test_that("Archive works(2)",
          {expect_equal(Archive(5, 3),
                        list("nsols" = 0,
                             "dim" = 5,
                             "scores" = matrix(0, 3, 5),
                             "solutions" = vector("list", 3)
                        )
          )
          }
)

# Resize ####
ar <- Archive(3,5)

test_that("Resize works", {
  expect_equal(Resize(ar),
               list("nsols" = 0,
                    "dim" = 3,
                    "scores" = matrix(0, 10, 3),
                    "solutions" = vector("list", 10)
               )
  )
})

# Add ####
ar <- Resize(ar)
sol <- "sol 1"
score <- c(324, 55, 6)

test_that("Add works",
          {expect_equal(Add(ar, sol, score),
                        list("nsols" = 1,
                             "dim" = 3,
                             "scores" = matrix(c(324, 55, 6, rep(0, 27)), 10, 3,
                                               byrow = T),
                             "solutions" = c(sol, vector("list", 9))
                        )
          )
          }
)

# FixRepmat ####
data <- matrix(c(3, 3, 2, 2), ncol = 2, byrow = T)

test_that("FixRepmat works(1)",
          {expect_equal(FixRepmat(data, 2, 3), matrix(c(3, 3, 3, 3, 3, 3,
                                                        2, 2, 2, 2, 2, 2,
                                                        3, 3, 3, 3, 3, 3,
                                                        2, 2, 2, 2, 2, 2),
                                                      ncol = 6, byrow = T
                                                      )
                        )
          }
         )

test_that("FixRepmat works(2)",
          {expect_equal(FixRepmat(c(1, 2, 3), 5, 1), matrix(rep(c(1, 2, 3), 5),
                                                            ncol = 3, byrow = T
                                                            )
                        )
          }
         )

test_that("FixRepmat works",
          {expect_equal(FixRepmat(8, 1, 3), matrix(c(8,8,8), ncol = 3))}
         )

# RemoveDominated ####

ar = Add(ar, "sol 1", c(4, 5, 6))
ar = Add(ar, "sol 2", c(1, 2, 3))
ar = Add(ar, "sol 3", c(3, 3, 6))
ar = Add(ar, "sol 4", c(0.5, 3, 4))
ar = Add(ar, "sol 5", c(3, 2, 6))
ar = Add(ar, "sol 6", c(0.5, 3, 3))
ar = Add(ar, "sol 7", c(3, 3, 6))
ar = Add(ar, "sol 8", c(2, 1, 1))
ar = Add(ar, "sol 9", c(2, 3, 3))
ar = Add(ar, "sol 10", c(2, 3, 3))

test_that("RemoveDominated works", {
  expect_equal(RemoveDominated(ar),
               list("nsols" = 6,
                    "dim" = 3,
                    "scores" = matrix(c( 1.0, 2, 3,
                                         0.5, 3, 4,
                                         0.5, 3, 3,
                                         2.0, 1, 1,
                                         2.0, 3, 3,
                                         2.0, 3, 3),
                                      ncol = 3,
                                      byrow = T
                                      ),
                    "solutions" = list("sol 2",
                                       "sol 4", "sol 6",
                                       "sol 8", "sol 9",
                                       "sol 10")
                    )
               )
            }
         )

# RemoveDuplicates ####
ar = RemoveDominated(ar)

test_that("RemoveDuplicates works", {
  expect_equal(RemoveDuplicates(ar),
               list("nsols" = 5,
                    "dim" = 3,
                    "scores" = matrix(c( 1.0, 2, 3,
                                         0.5, 3, 4,
                                         0.5, 3, 3,
                                         2.0, 1, 1,
                                         2.0, 3, 3),
                                      ncol = 3, byrow = T),
                    "solutions" = list("sol 2", "sol 4",
                                       "sol 6", "sol 8",
                                       "sol 9"))
  )
})

# rowleq ####
a <- c(4, 5, 7, 6, 8)
b <- c(4, 5, 6, 7, 8)

test_that("rowleq works", {expect_equal(rowleq(a, b), FALSE)})
test_that("rowleq works", {expect_equal(rowleq(b, a), TRUE)})




