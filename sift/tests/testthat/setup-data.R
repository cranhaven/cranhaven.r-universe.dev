# sift
set.seed(1)

df <- tibble(a = runif(100, 0, 100),
             b = sample(LETTERS[1:3], size = 100, prob = c(0.49, 0.49, 0.02), replace = TRUE))
df[sample(1:100, size = 1), "a"] <- NA_real_

result <- sift(df, a, 4, b == "C")
result2 <- sift(dplyr::group_by(df, b), a, 0.1, b == "C")

# break_join
set.seed(2)

# evil break_join data
a <- tibble::tibble(.x = 1:10, by = runif(10, 3.5, 6), .bj1 = 1, .bj2 = 2)
b <- tibble::tibble(by = c(4, 5), .brk = c("A", "B"), .bj1 = 1, .bj2 = 2)

# grouped df
e <- dplyr::group_by(tibble::tibble(p = c(rep("A", 10), rep("B", 10)), q = runif(20, 0, 10)), p)
f <- dplyr::group_by(tibble::tibble(p = c("A", "B"), q = c(5, 9), r = c("a", "b")), p)

# duplicate names not included in "by"
g <- tibble::tibble(a = 1:10, b = TRUE, c = runif(10, 0, 10))
h <- tibble::tibble(c = c(5, 5), b = c(TRUE, FALSE))
