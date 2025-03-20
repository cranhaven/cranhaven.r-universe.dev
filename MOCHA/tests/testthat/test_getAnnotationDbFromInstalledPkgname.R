if (
  require("TxDb.Hsapiens.UCSC.hg38.refGene", quietly = TRUE) &&
    require("org.Hs.eg.db", quietly = TRUE) &&
    require("BSgenome.Hsapiens.UCSC.hg19", quietly = TRUE)
) {
  test_that("getAnnotationDbFromInstalledPkgname works in valid cases", {
    db1 <- MOCHA:::getAnnotationDbFromInstalledPkgname(
      "TxDb.Hsapiens.UCSC.hg38.refGene", "TxDb"
    )
    expect_s4_class(db1, "TxDb")

    db2 <- MOCHA:::getAnnotationDbFromInstalledPkgname(
      "org.Hs.eg.db", "OrgDb"
    )
    expect_s4_class(db2, "OrgDb")
  })

  test_that("getAnnotationDbFromInstalledPkgname errors with swapped type", {
    expect_error(db1 <- MOCHA:::getAnnotationDbFromInstalledPkgname(
      "TxDb.Hsapiens.UCSC.hg38.refGene",
      type = "OrgDb"
    ))

    expect_error(db2 <- MOCHA:::getAnnotationDbFromInstalledPkgname(
      "org.Hs.eg.db",
      type = "TxDb"
    ))
  })

  test_that("getAnnotationDbFromInstalledPkgname errors with non-character input", {
    expect_error(db1 <- getAnnotationDbFromInstalledPkgname(
      "foo.bar.db",
      type = "OrgDb"
    ))
  })
}

test_that("getAnnotationDbFromInstalledPkgname errors with invalid type", {
  expect_error(MOCHA:::getAnnotationDbFromInstalledPkgname(
    TxDb.Hsapiens.UCSC.hg38.refGene,
    type = "TxDb"
  ))
})

test_that("getAnnotationDbFromInstalledPkgname errors with invalid package", {
  expect_error(db1 <- MOCHA:::getAnnotationDbFromInstalledPkgname(
    "foo.bar.db",
    type = "OrgDb"
  ))
})
