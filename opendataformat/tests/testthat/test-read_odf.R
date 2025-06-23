#' read_odf: all languages
test_that("read_odf_all", {
  df <- opendataformat::read_odf(
    file = "testdata/data.odf.zip",
    languages = "all"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9001", "bap9002",
                                       "bap9003", "bap96", "name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})

test_that("read_odf_all", {
  df <- opendataformat::read_odf(
    file = "testdata/data.zip",
    languages = "all"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9001", "bap9002",
                                       "bap9003", "bap96", "name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})


#' read_odf: with default language
test_that("read_odf_variables", {
  df <- read_odf(
    file = "testdata/data_with_default.odf.zip",
    languages = "all"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_NA",
    "description_en",
    "description_de",
    "label_NA",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_NA",
    "label_en",
    "label_de",
    "description_NA",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_NA",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9001",
                                       "bap9002", "bap9003", "bap96", "name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels_NA), c(-2, -1))
})

#' read_odf: "de" languages
test_that("read_odf_de", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    languages = "de"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_de",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_de",
    "description_de",
    "type",
    "url",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9001",
                                       "bap9002", "bap9003", "bap96", "name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels),
               c("trifft nicht zu", "keine Angabe"))
  expect_equal(names(attributes(df$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})

#' read_odf: nrows=10
test_that("read_odf_specific_rows", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    nrows = 10
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9001", "bap9002",
                                       "bap9003", "bap96", "name"))
  expect_equal(nrow(df), 10)
  expect_equal(ncol(df), 7)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url,
               "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})

#' read_odf: nrows = 13, skip = 3
test_that("read_odf_specific_rows2", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    nrows = 13,
    skip = 3
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9001",
                                       "bap9002", "bap9003", "bap96", "name"))
  expect_equal(nrow(df), 13)
  expect_equal(ncol(df), 7)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url,
               "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})


#' read_odf: select =c(1,2,4,5)
test_that("read_odf_specific_variables", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    select = c(1, 2, 4, 5)
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9002",
                                       "bap9003"))
  expect_equal(nrow(df), 20)
  expect_equal(ncol(df), 4)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url,
               "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
})


#' read_odf: select = c("bap87", "bap96", "bap9002", "bap9003")
test_that("read_odf_specific_variables", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    select = c("bap87", "bap96", "bap9002",
               "bap9003")
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap96", "bap9002",
                                       "bap9003"))
  expect_equal(nrow(df), 20)
  expect_equal(ncol(df), 4)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url,
               "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})



#' read_odf: rows and cols
test_that("read_odf_specific_rows_and_cols", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    nrows = 13,
    skip = 3,
    select = c(1, 2, 4, 5)
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9002",
                                       "bap9003"))
  expect_equal(nrow(df), 13)
  expect_equal(ncol(df), 4)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url,
               "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap9201)$name, "bap9201")
  expect_equal(attributes(df$bap9201)$label,
               "hours of sleep, normal workday")
  expect_equal(attributes(df$bap9201)$label_de,
               "Stunden Schlaf, normaler Werktag")
  expect_equal(attributes(df$bap9201)$label_en,
               "hours of sleep, normal workday")
  expect_equal(attributes(df$bap9201)$description_NA, NULL)
  expect_equal(attributes(df$bap9201)$description_de,
               "Schlafstunden pro Wochentag")
  expect_equal(attributes(df$bap9201)$description_en,
               "Sleep hours per weekday")
  expect_equal(attributes(df$bap9201)$url,
               "https://paneldata.org/soep-core/data/bap/bap9201")
  expect_equal(unname(attributes(df$bap9201)$labels), NULL)
  expect_equal(names(attributes(df$bap9201)$labels), NULL)
  expect_equal(unname(attributes(df$bap9201)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap9201)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap9201)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap9201)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})


#' read_odf: rows and cols2
test_that("read_odf_specific_rows_and_cols2", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    nrows = 13,
    skip = 3,
    select = c("bap87", "bap9201", "bap9002",
               "bap9003")
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_en",
    "description_de",
    "label_en",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9002",
                                       "bap9003"))
  expect_equal(nrow(df), 13)
  expect_equal(ncol(df), 4)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url,
               "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap9201)$name, "bap9201")
  expect_equal(attributes(df$bap9201)$label,
               "hours of sleep, normal workday")
  expect_equal(attributes(df$bap9201)$label_de,
               "Stunden Schlaf, normaler Werktag")
  expect_equal(attributes(df$bap9201)$label_en,
               "hours of sleep, normal workday")
  expect_equal(attributes(df$bap9201)$description_NA, NULL)
  expect_equal(attributes(df$bap9201)$description_de,
               "Schlafstunden pro Wochentag")
  expect_equal(attributes(df$bap9201)$description_en,
               "Sleep hours per weekday")
  expect_equal(attributes(df$bap9201)$url,
               "https://paneldata.org/soep-core/data/bap/bap9201")
  expect_equal(unname(attributes(df$bap9201)$labels), NULL)
  expect_equal(names(attributes(df$bap9201)$labels), NULL)
  expect_equal(unname(attributes(df$bap9201)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df$bap9201)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap9201)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap9201)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
})


#' read_odf: language rows and cols
test_that("read_odf_specific_language_rows_and_cols", {
  df <- read_odf(
    file = "testdata/data.odf.zip",
    nrows = 13,
    skip = 3,
    select = c("bap87", "bap9201", "bap9002",
               "bap9003"),
    languages = "de"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)), c(
    "row.names",
    "names",
    ".internal.selfref",
    "study",
    "name",
    "description_de",
    "label_de",
    "url",
    "languages",
    "lang",
    "label",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_de",
    "description_de",
    "type",
    "url",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87", "bap9201", "bap9002",
                                       "bap9003"))
  expect_equal(nrow(df), 13)
  expect_equal(ncol(df), 4)
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, NULL)
  expect_equal(attributes(df)$description,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_en, NULL)
  expect_equal(attributes(df)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df$bap9201)$name, "bap9201")
  expect_equal(attributes(df$bap9201)$label,
               "Stunden Schlaf, normaler Werktag")
  expect_equal(attributes(df$bap9201)$label_de,
               "Stunden Schlaf, normaler Werktag")
  expect_equal(attributes(df$bap9201)$label_en, NULL)
  expect_equal(attributes(df$bap9201)$description_NA, NULL)
  expect_equal(attributes(df$bap9201)$description_de,
               "Schlafstunden pro Wochentag")
  expect_equal(attributes(df$bap9201)$description_en, NULL)
  expect_equal(attributes(df$bap9201)$url,
               "https://paneldata.org/soep-core/data/bap/bap9201")
  expect_equal(unname(attributes(df$bap9201)$labels), c(-2, -1))
  expect_equal(names(attributes(df$bap9201)$labels),
               c("trifft nicht zu", "keine Angabe"))
  expect_equal(unname(attributes(df$bap9201)$labels_en), NULL)
  expect_equal(names(attributes(df$bap9201)$labels_en), NULL)
  expect_equal(unname(attributes(df$bap9201)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df$bap9201)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
    unlink(paste0(tempdir(), "/*"), recursive = TRUE)
})
