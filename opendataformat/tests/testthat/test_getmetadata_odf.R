
#test for data frame input
test_that("docu_odf", {
  #' - get data
  df <- get(load("testdata/data_odf.RData"))

  expect_equal(as.character(getmetadata_odf(df, type = "label")),
               c("Current Health", "hours of sleep, normal workday",
                 "Pressed For Time Last 4 Weeks",
                 "Run-down, Melancholy Last 4 Weeks",
                 "Well-balanced Last 4 Weeks",
                 "Height", "Firstname"))
  expect_equal(names(getmetadata_odf(df, type = "label")),
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))

  expect_equal(as.character(
    getmetadata_odf(df, type = "label", language = "en")),
    c("Current Health", "hours of sleep, normal workday",
      "Pressed For Time Last 4 Weeks",
      "Run-down, Melancholy Last 4 Weeks", "Well-balanced Last 4 Weeks",
      "Height", "Firstname"))
  expect_equal(names(getmetadata_odf(df, type = "label", language = "en")),
               c("bap87", "bap9201", "bap9001", "bap9002", "bap9003",
                 "bap96", "name"))

  expect_equal(
    as.character(getmetadata_odf(df, type = "label", language = "de")),
    c("Gesundheitszustand gegenwärtig", "Stunden Schlaf, normaler Werktag",
      "Eile, Zeitdruck letzten 4 Wochen", "Niedergeschlagen letzten 4 Wochen",
      "Ausgeglichen letzten 4 Wochen", "Körpergröße", "Vorname"))
  expect_equal(names(getmetadata_odf(df, type = "label", language = "de")),
               c("bap87", "bap9201", "bap9001", "bap9002", "bap9003",
                 "bap96", "name"))

  expect_equal(
    as.character(getmetadata_odf(df, type = "description", language = "de")),
    c("Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?",
      "Schlafstunden pro Wochentag",
      "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen",
      "Häufigkeit der Niedergeschlagenheit",
      "Häufigkeit der Ausgeglichenheit",
      "Körpergröße",
      "Vorname"))
  expect_equal(names(getmetadata_odf(df, type = "url", language = "de")),
               c("bap87", "bap9201", "bap9001", "bap9002", "bap9003",
                 "bap96", "name"))

  #set type to "description"
  expect_equal(
    as.character(getmetadata_odf(df, type = "description", language = "de")),
    c("Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?",
      "Schlafstunden pro Wochentag",
      "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen",
      "Häufigkeit der Niedergeschlagenheit",
      "Häufigkeit der Ausgeglichenheit",
      "Körpergröße",
      "Vorname"))
  expect_equal(
    names(getmetadata_odf(df, type = "description", language = "de")),
    c("bap87", "bap9201", "bap9001", "bap9002", "bap9003",
      "bap96", "name")
  )

  #set type to "url"
  expect_equal(
    as.character(getmetadata_odf(df, type = "url", language = "de")),
    c("https://paneldata.org/soep-core/data/bap/bap87",
      "https://paneldata.org/soep-core/data/bap/bap9201",
      "https://paneldata.org/soep-core/data/bap/bap9001",
      "https://paneldata.org/soep-core/data/bap/bap9002",
      "https://paneldata.org/soep-core/data/bap/bap9003",
      "https://paneldata.org/soep-core/data/bap/bap96",
      ""))
  expect_equal(names(getmetadata_odf(df, type = "url", language = "de")),
               c("bap87", "bap9201", "bap9001", "bap9002", "bap9003",
                 "bap96", "name"))

  #set type to "type"
  expect_equal(
    as.character(getmetadata_odf(df, type = "type", language = "de")),
    c("numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "character"))
  expect_equal(names(getmetadata_odf(df, type = "type", language = "de")),
               c("bap87", "bap9201", "bap9001", "bap9002", "bap9003",
                 "bap96", "name"))
})

#test for variable input
test_that("docu_odf", {
  #' - get data
  df <- get(load("testdata/data_odf.RData"))

  expect_equal(as.character(getmetadata_odf(df$bap87, type = "label")),
               "Current Health")
  expect_equal(names(getmetadata_odf(df$bap87, type = "label")), "bap87")
  #change language
  expect_equal(
    as.character(getmetadata_odf(df$bap87, language = "en", type = "label")),
    "Current Health")
  expect_equal(
    names(getmetadata_odf(df$bap87, language = "en", type = "label")),
    "bap87")

  expect_equal(
    as.character(getmetadata_odf(df$bap9001, language = "de", type = "label")),
    "Eile, Zeitdruck letzten 4 Wochen")
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "de", type = "label")),
    "bap9001")

  #set type = label
  expect_equal(as.character(getmetadata_odf(df$bap9001, type = "label")),
               "Pressed For Time Last 4 Weeks")
  expect_equal(names(getmetadata_odf(df$bap9001, type = "label")), "bap9001")


  #set language = "en"
  expect_equal(
    as.character(getmetadata_odf(df$bap9201, type = "label", language = "en")),
    "hours of sleep, normal workday")
  expect_equal(
    names(getmetadata_odf(df$bap9201, type = "label", language = "en")),
    "bap9201")
  #set language = "de"
  expect_equal(
    as.character(getmetadata_odf(df$bap9201, type = "label", language = "de")),
    "Stunden Schlaf, normaler Werktag")
  expect_equal(
    names(getmetadata_odf(df$bap9201, type = "label", language = "de")),
    "bap9201")

  #set , type = "label"
  expect_equal(
    as.numeric(getmetadata_odf(
      df$bap9001, language = "de", type = "valuelabels")),
    c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "de", type = "valuelabels")),
    c("trifft nicht zu", "keine Angabe", "Immer", "Oft", "Manchmal",
      "Fast nie", "Nie"))

  expect_equal(
    as.numeric(getmetadata_odf(
      df$bap9001, type = "valuelabels", language = "en")),
    c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(
    names(getmetadata_odf(df$bap9001, type = "valuelabels", language = "en")),
    c("Does not apply", "No Answer", "Always", "Often", "Sometimes",
      "Almost Never", "Never"))

  #set , type = "label"
  expect_equal(
    as.numeric(getmetadata_odf(
      df$bap9001, language = "de", type = "valuelabels")),
    c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "de", type = "valuelabels")),
    c("trifft nicht zu", "keine Angabe", "Immer", "Oft", "Manchmal",
      "Fast nie", "Nie"))

  expect_equal(
    as.numeric(
      getmetadata_odf(df$bap9001, language = "en", type = "valuelabels")),
    c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "en", type = "valuelabels")),
    c("Does not apply", "No Answer", "Always", "Often", "Sometimes",
      "Almost Never", "Never"))

  #set , type = "label"
  expect_equal(
    as.numeric(
      getmetadata_odf(df$bap9001, language = "de", type = "valuelabels")),
    c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(
    names(
      getmetadata_odf(df$bap9001, language = "de", type = "valuelabels")),
    c("trifft nicht zu", "keine Angabe", "Immer", "Oft", "Manchmal",
      "Fast nie", "Nie"))

  expect_equal(
    as.numeric(getmetadata_odf(
      df$bap9001, language = "en", type = "valuelabels")),
    c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "en", type = "valuelabels")),
    c("Does not apply", "No Answer", "Always", "Often", "Sometimes",
      "Almost Never", "Never"))

  #set type to "url"
  expect_equal(
    as.character(getmetadata_odf(df$bap9001, language = "de",  type = "url")),
    "https://paneldata.org/soep-core/data/bap/bap9001")
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "de", type = "url")),
    "bap9001")

  #set type to "description"
  expect_equal(
    as.character(
      getmetadata_odf(df$bap9001, language = "en", type = "description")),
    "Frequency of feeling time pressure in the past 4 weeks")
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "en", type = "description")),
    "bap9001")

  #set type to "type"
  expect_equal(
    as.character(getmetadata_odf(df$bap9001, language = "en", type = "type")),
    "numeric")
  expect_equal(
    names(getmetadata_odf(df$bap9001, language = "en", type = "type")),
    "bap9001")
})
