
 library(fuzzyRankTests)

 options(digits=5) # avoid rounding differences

 x <- c(-3, -2, -2, 0, 0, 0, 0, 1, 2, 3, 4, 4, 4, 5, 6, 7)

 fuzzy.signrank.test(x)

 wilcox.test(x, exact = FALSE)

 fuzzy.signrank.test(x, alt = "less")

 wilcox.test(x, alt = "less", exact = FALSE)

 fuzzy.signrank.test(x, alt = "great")

 wilcox.test(x, alt = "great", exact = FALSE)

 fuzzy.signrank.ci(x)

 wilcox.test(x, conf.int = TRUE, exact = FALSE)

 fuzzy.signrank.ci(x, alt = "less")

 wilcox.test(x, conf.int = TRUE, alt = "less", exact = FALSE)

 fuzzy.signrank.ci(x, alt = "great")

 wilcox.test(x, conf.int = TRUE, alt = "great", exact = FALSE)

