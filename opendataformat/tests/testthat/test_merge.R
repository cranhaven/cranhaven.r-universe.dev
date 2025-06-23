


#test merging
test_that("merge.odf", {
  #' - get data
  df1 <- read_odf("testdata/data.zip", select = c(1:4))
  df2 <- read_odf("testdata/data.zip", select = c(4:7))
  df3 <- read_odf("testdata/data.zip", select = c(1:2))
  df4 <- read_odf("testdata/data.zip", select = c(3:5))
  df5 <- read_odf("testdata/data.zip", select = c(6:7))
  df6 <- read_odf("testdata/data.zip")
  df7 <- read_odf("testdata/data.zip")



  df1$id <- 1:20
  df2$id <- 1:20
  df3$id <- c(1:10, 1:10)
  df4$id <- c(1:10, 1:10)
  df5$id <- c(1:15, 21:25)
  df6$id <- c(6:20, 16:20)
  df1$id2 <- c(1:5, 21:35)
  df2$id2 <- 16:35
  df3$id2 <- c(11:20, 11:20)
  df4$id2 <- c(1:10, 11:20)
  df5$id2 <- c(1:5, 16:20, 16:25)
  df7$id <- 1:20


  df_out <- merge(x = df1, y = df2)
  expect_equal(dim(df_out), c(15, 9))
  expect_equal(colnames(df_out), c("bap9002", "id", "id2", "bap87", "bap9201",
                                   "bap9001", "bap9003", "bap96", "name"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", ".internal.selfref", "sorted", "name",
                                            "languages", "lang", "class"))
  expect_equal(attributes(df_out)[["name"]],
               "dataset merged from other datasets bap bap")
  expect_equal(attributes(df_out)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out)[["lang"]], "en")

  expect_equal(attributes(df_out$bap9002)[["label_en"]],
               "Run-down, Melancholy Last 4 Weeks")
  expect_equal(attributes(df_out$bap87)[["label_de"]],
               "Gesundheitszustand gegenwärtig")
  expect_equal(attributes(df_out$bap9201)[["description_en"]],
               "Sleep hours per weekday")
  expect_equal(attributes(df_out$bap9001)[["description_de"]],
               "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen")
  expect_equal(attributes(df_out$bap9003)[["type"]], "numeric")
  expect_equal(attributes(df_out$bap96)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap96")

  labels_en <- -2:-1
  names(labels_en) <- c("Does not apply", "No Answer")
  expect_equal(attributes(df_out$name)[["labels_en"]], labels_en)

  labels_de <- c(-2, -1, 1:5)
  names(labels_de) <- c("trifft nicht zu", "keine Angabe", "Immer", "Oft",
                        "Manchmal", "Fast nie", "Nie")
  expect_equal(attributes(df_out$bap9002)[["labels_de"]], labels_de)

  expect_equal(attributes(df_out$bap87)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$bap9201)[["lang"]], "en")
  expect_equal(attributes(df_out$bap9003)[["label"]],
               "Well-balanced Last 4 Weeks")
  expect_equal(attributes(df_out$bap9003)[["label_en"]],
               "Well-balanced Last 4 Weeks")
  expect_equal(attributes(df_out$bap96)[["label_de"]], "Körpergröße")
  expect_equal(attributes(df_out$name)[["description_en"]], "Firstname")


  df_out <- merge(x = df3, y = df4)
  df_out2 <- merge(x = df_out, y = df5)
  expect_equal(dim(df_out2), c(10, 9))
  expect_equal(colnames(df_out2), c("id", "id2", "bap87", "bap9201", "bap9001",
                                   "bap9002", "bap9003", "bap96", "name"))
  expect_equal(names(attributes(df_out2)), c("row.names", "names", ".internal.selfref", "sorted", "name",
                                             "languages", "lang", "class"))
  expect_equal(attributes(df_out2)[["name"]],
               "dataset merged from other datasets bap bap bap")
  expect_equal(attributes(df_out2)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out2)[["lang"]], "en")

  expect_equal(attributes(df_out2$bap9002)[["label_en"]],
               "Run-down, Melancholy Last 4 Weeks")
  expect_equal(attributes(df_out2$bap9001)[["label_de"]],
               "Eile, Zeitdruck letzten 4 Wochen")
  expect_equal(attributes(df_out2$bap9201)[["description_en"]],
               "Sleep hours per weekday")
  expect_equal(attributes(df_out2$bap9001)[["description_de"]],
               "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen")
  expect_equal(attributes(df_out2$bap9003)[["type"]], "numeric")
  expect_equal(attributes(df_out2$bap96)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap96")

  labels_en <- -2:-1
  names(labels_en) <- c("Does not apply", "No Answer")
  expect_equal(attributes(df_out2$name)[["labels_en"]], labels_en)

  labels_de <- c(-2, -1, 1:5)
  names(labels_de) <- c("trifft nicht zu", "keine Angabe", "Immer", "Oft",
                        "Manchmal", "Fast nie", "Nie")
  expect_equal(attributes(df_out2$bap9002)[["labels_de"]], labels_de)

  expect_equal(attributes(df_out2$bap96)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out2$bap9201)[["lang"]], "en")
  expect_equal(attributes(df_out2$bap9003)[["label"]],
               "Well-balanced Last 4 Weeks")
  expect_equal(attributes(df_out2$bap9003)[["label_en"]],
               "Well-balanced Last 4 Weeks")
  expect_equal(attributes(df_out2$bap96)[["label_de"]],
               "Körpergröße")
  expect_equal(attributes(df_out2$name)[["description_en"]],
               "Firstname")

  df_out <- merge(x = df1, y = df4)
  df_out2 <- merge(x = df_out, y = df5, by = c("id", "id2"))
  expect_equal(dim(df_out2), c(5, 9))
  expect_equal(colnames(df_out2), c("id", "id2", "bap9001", "bap9002", "bap87",
                                   "bap9201",  "bap9003", "bap96", "name"))
  expect_equal(names(attributes(df_out2)),
               c("row.names", "names", ".internal.selfref", "sorted",  "name",
                 "languages", "lang", "class"))
  expect_equal(attributes(df_out2)[["name"]],
               "dataset merged from other datasets bap bap bap")
  expect_equal(attributes(df_out2)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out2)[["lang"]], "en")

  expect_equal(attributes(df_out2$bap9002)[["label_en"]],
               "Run-down, Melancholy Last 4 Weeks")
  expect_equal(attributes(df_out2$bap9001)[["label_de"]],
               "Eile, Zeitdruck letzten 4 Wochen")
  expect_equal(attributes(df_out2$bap9201)[["description_en"]],
               "Sleep hours per weekday")
  expect_equal(
    attributes(df_out2$bap87)[["description_de"]],
    "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?")
  expect_equal(attributes(df_out2$bap9003)[["type"]],
               "numeric")
  expect_equal(attributes(df_out2$bap96)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap96")

  labels_en <- -2:-1
  names(labels_en) <- c("Does not apply", "No Answer")
  expect_equal(attributes(df_out2$name)[["labels_en"]], labels_en)

  labels_de <- c(-2, -1, 1:5)
  names(labels_de) <- c("trifft nicht zu", "keine Angabe", "Immer", "Oft",
                        "Manchmal", "Fast nie", "Nie")
  expect_equal(attributes(df_out2$bap9002)[["labels_de"]], labels_de)

  expect_equal(attributes(df_out2$bap96)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out2$bap9201)[["lang"]], "en")
  expect_equal(attributes(df_out2$bap87)[["label"]], "Current Health")
  expect_equal(attributes(df_out2$bap9003)[["label_en"]],
               "Well-balanced Last 4 Weeks")
  expect_equal(attributes(df_out2$bap96)[["label_de"]], "Körpergröße")
  expect_equal(attributes(df_out2$name)[["description_en"]], "Firstname")

  df_out <- merge(x = df1, y = df6)
  df_out2 <- merge(x = df2, y = df_out)

  expect_equal(dim(df_out2), c(5, 9))
  expect_equal(colnames(df_out2), c("bap9002", "bap9003", "bap96", "name", "id",
                                    "id2", "bap87", "bap9201",
                                    "bap9001"))
  expect_equal(names(attributes(df_out2)),
               c("row.names", "names", ".internal.selfref", "sorted", "name",
                 "languages", "lang", "class"))
  expect_equal(attributes(df_out2)[["name"]],
               "dataset merged from other datasets bap bap bap")
  expect_equal(attributes(df_out2)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out2)[["lang"]], "en")

  expect_equal(attributes(df_out2$bap9002)[["label_en"]],
               "Run-down, Melancholy Last 4 Weeks")
  expect_equal(attributes(df_out2$bap9001)[["label_de"]],
               "Eile, Zeitdruck letzten 4 Wochen")
  expect_equal(attributes(df_out2$bap9201)[["description_en"]],
               "Sleep hours per weekday")
  expect_equal(
    attributes(df_out2$bap87)[["description_de"]],
    "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?")
  expect_equal(attributes(df_out2$bap9003)[["type"]], "numeric")
  expect_equal(attributes(df_out2$bap96)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap96")

  labels_en <- -2:-1
  names(labels_en) <- c("Does not apply", "No Answer")
  expect_equal(attributes(df_out2$name)[["labels_en"]], labels_en)

  labels_de <- c(-2, -1, 1:5)
  names(labels_de) <- c("trifft nicht zu", "keine Angabe", "Immer", "Oft",
                        "Manchmal", "Fast nie", "Nie")
  expect_equal(attributes(df_out2$bap9002)[["labels_de"]], labels_de)

  expect_equal(attributes(df_out2$bap96)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out2$bap9201)[["lang"]], "en")
  expect_equal(attributes(df_out2$bap87)[["label"]], "Current Health")
  expect_equal(attributes(df_out2$bap9003)[["label_en"]],
               "Well-balanced Last 4 Weeks")
  expect_equal(attributes(df_out2$bap96)[["label_de"]], "Körpergröße")
  expect_equal(attributes(df_out2$name)[["description_en"]], "Firstname")
})



#test merging with all arguments
test_that("merge.odf", {
  #' - get data
  df1 <- read_odf("testdata/data.zip", select = c(1:4))
  df2 <- read_odf("testdata/data.zip", select = c(4:7))


  df1$id <- 1:20
  df2$id <- c(1:15, 1:5)
  df_out <- merge(x = df1, y = df2, all = FALSE)
  expect_equal(dim(df_out), c(15, 8))

  df_out <- merge(x = df1, y = df2, all = TRUE)
  expect_equal(dim(df_out), c(25, 8))

  df_out <- merge(x = df1, y = df2, all.x = FALSE, all.y = TRUE)
  expect_equal(dim(df_out), c(20, 8))

  df_out <- merge(x = df1, y = df2, all.x = TRUE, all.y = FALSE)
  expect_equal(dim(df_out), c(20, 8))

})
