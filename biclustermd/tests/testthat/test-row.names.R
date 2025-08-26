context("row names")

test_that(
  "row.names.biclustermd() returns all rows and row names", {

    sbc <- biclustermd(synthetic)
    expect_equal(nrow(row.names(sbc)), nrow(synthetic))
    expect_equal(ncol(row.names(sbc)), 2)
    expect_equal(all(row.names(synthetic) %in% row.names(sbc)$row_name), TRUE)

  }
)

test_that(
  "row.names() is a subset of gather()", {

    sbc <- biclustermd(synthetic)
    library(dplyr)
    expect_equal(
      row.names(sbc),
      gather(sbc) %>% distinct(row_cluster, row_name) %>% select(row_cluster, row_name)
    )

  }
)
