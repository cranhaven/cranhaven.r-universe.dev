


#test as_odf_table
test_that("as_odf_tbl", {
  # create dataframe
  df <- data.frame(id = 1:5, name = c("Klaus", "Anna", "Rebecca", "Kevin", "Janina"), age = c(55, 40, 19, 25, 60), diagnosis = c(1,3,3,2,1))
  # Add metadata for dataset
  attr(df, "name") <- "patientdata"
  attr(df, "label_en") <- "Patient Data"
  attr(df, "description_en") <- "Patient database of the practice Dr. Sommer"
  attr(df, "url") <- "www.example.url.en"
  
  # Add metadata for diagnosis variable with label, description and value labels.
  attr(df$id, "name") <- "id"
  attr(df$id, "label_en") <- "Patient ID"
  attr(df$id, "description_en") <- "Practice Patient ID"
  attr(df$diagnosis, "name") <- "diagnosis"
  attr(df$diagnosis, "label_en") <- "Diagnosis"
  attr(df$diagnosis, "description_en") <- "Diagnosis patient last visit"
  valuelabels_diagnosis <- 1:4
  names(valuelabels_diagnosis) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  attr(df$diagnosis, "labels_en") <- valuelabels_diagnosis
  df_out <- as_odf_tbl(df)
  
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label_en", "description_en", "url",
                                            "languages", "lang", "label", 
                                            "class"))
  expect_equal(attributes(df_out)[["name"]],
               "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("en"))
  expect_equal(attributes(df_out)[["lang"]], "en")
  expect_equal(attributes(df_out)[["label_en"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_en"]], c("Patient database of the practice Dr. Sommer"))
  
  expect_equal(attributes(df_out$id)[["label_en"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_en"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], "en")
  expect_equal(attributes(df_out$id)[["lang"]], "en")
  expect_equal(attributes(df_out$name)[["languages"]], "en")
  expect_equal(attributes(df_out$age)[["lang"]], "en")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_en"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["description_en"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_en"]], valuelabels)
  expect_equal(attributes(df_out$name)[["labels"]], NULL)
  
  
  # Test with testdatasets
  df <- opendataformat::read_odf(
    file = "testdata/data.zip",
    languages = "all"
  )
  
  df_out <- as_odf_tbl(df, active_language = "en")
  # - dataset attributes
  expect_equal(names(attributes(df_out)), c(
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
  expect_equal(names(attributes(df_out$bap87)), c(
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
  expect_equal(attributes(df_out)$names, c("bap87", "bap9201", "bap9001", "bap9002",
                                       "bap9003", "bap96", "name"))
  expect_equal(attributes(df_out)$name, "bap")
  expect_equal(attributes(df_out)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df_out)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df_out)$label_en,
               "Data from individual questionnaires 2010")
  expect_equal(attributes(df_out)$description, NULL)
  expect_equal(attributes(df_out)$description_en,
               "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df_out)$description_de,
               "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df_out)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df_out)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df_out$bap96)$name, "bap96")
  expect_equal(attributes(df_out$bap96)$label, "Height")
  expect_equal(attributes(df_out$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df_out$bap96)$label_en, "Height")
  expect_equal(attributes(df_out$bap96)$description, NULL)
  expect_equal(attributes(df_out$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df_out$bap96)$description_en, "Body size")
  expect_equal(attributes(df_out$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df_out$bap96)$labels), NULL)
  expect_equal(names(attributes(df_out$bap96)$labels), NULL)
  expect_equal(unname(attributes(df_out$bap96)$labels_en), c(-2, -1))
  expect_equal(names(attributes(df_out$bap96)$labels_en),
               c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df_out$bap96)$labels_de), c(-2, -1))
  expect_equal(names(attributes(df_out$bap96)$labels_de),
               c("trifft nicht zu", "keine Angabe"))
  
  #With default language
  df <- read_odf(
    file = "testdata/data_with_default.odf.zip",
    languages = "all"
  )
  
  df_out <- as_odf_tbl(df, active_language = "en")
  
  # - dataset attributes
  expect_equal(names(attributes(df_out)), c(
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
  expect_equal(names(attributes(df_out$bap87)), c(
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
  expect_equal(attributes(df_out)$names, c("bap87", "bap9201", "bap9001",
                                       "bap9002", "bap9003", "bap96", "name"))
  expect_equal(attributes(df_out)$name, "bap")
  expect_equal(attributes(df_out)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df_out)$class,  c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  # - variables content
  expect_equal(attributes(df_out$bap96)$name, "bap96")
  expect_equal(attributes(df_out$bap96)$url,
               "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df_out$bap96)$labels_NA), c(-2, -1))
  
  
  library(ISLR)
  data(Auto)
  # make xml and data
  df_out<-as_odf_tbl(x = Auto)
  expect_equal(dim(df_out), c(392, 9))
  expect_equal(colnames(df_out), c("mpg", "cylinders", "displacement",
                                   "horsepower", "weight", "acceleration",
                                   "year", "origin", "name"))
  expect_equal(names(attributes(df_out)), c("row.names", "names",
                                            "languages", "lang", 
                                            "class"))
  expect_equal(attributes(df_out)[["name"]],
               NULL)
  expect_equal(attributes(df_out)[["languages"]], c("NA"))
  expect_equal(attributes(df_out)[["lang"]], "NA")
  expect_equal(attributes(df_out)[["label"]], NULL)

  expect_equal(attributes(df_out$mpg)[["label"]], NULL)
  expect_equal(attributes(df_out$mpg)[["name"]], "mpg")
  expect_equal(attributes(df_out$mpg)[["languages"]], "NA")
  expect_equal(attributes(df_out$mpg)[["lang"]], "NA")
})



#test ps_odf_tbl with metadata without language tag
test_that("as_odf_tibble with metadata without a tag", {
  # create dataframe
  df <- data.frame(id = 1:5, name = c("Klaus", "Anna", "Rebecca", "Kevin", "Janina"), age = c(55, 40, 19, 25, 60), diagnosis = c(1,3,3,2,1))
  # Add metadata for dataset
  attr(df, "name") <- "patientdata"
  attr(df, "label") <- "Patient Data"
  attr(df, "description") <- "Patient database of the practice Dr. Sommer"
  attr(df, "url") <- "www.example.url.en"
  
  # Add metadata for diagnosis variable with label, description and value labels.
  attr(df$id, "name") <- "id"
  attr(df$id, "label") <- "Patient ID"
  attr(df$id, "description") <- "Practice Patient ID"
  attr(df$diagnosis, "name") <- "diagnosis"
  attr(df$diagnosis, "label") <- "Diagnosis"
  attr(df$diagnosis, "description") <- "Diagnosis patient last visit"
  valuelabels_diagnosis <- 1:4
  names(valuelabels_diagnosis) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  attr(df$diagnosis, "labels") <- valuelabels_diagnosis
  
  df_out <- as_odf_tbl(df)
  
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label", "url", "label_NA",
                                            "description_NA", "languages",
                                            "lang", "class"))
  expect_equal(attributes(df_out)[["name"]], "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("NA"))
  expect_equal(attributes(df_out)[["lang"]], "NA")
  expect_equal(attributes(df_out)[["label_NA"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_NA"]], c("Patient database of the practice Dr. Sommer"))
  
  expect_equal(attributes(df_out$id)[["label_NA"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_NA"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], "NA")
  expect_equal(attributes(df_out$id)[["lang"]], "NA")
  expect_equal(attributes(df_out$name)[["languages"]], "NA")
  expect_equal(attributes(df_out$age)[["lang"]], "NA")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_NA"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["description_NA"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_NA"]], valuelabels)
  expect_equal(attributes(df_out$name)[["labels"]], NULL)
  
  
  # Indicate language of metadata without languagetag
  df_out <- as_odf_tbl(df, language_of_metadata = "en")
  
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label", "url",
                                            "label_en", "description_en",
                                            "languages", "lang",
                                            "class"))
  expect_equal(attributes(df_out)[["name"]],
               "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("en"))
  expect_equal(attributes(df_out)[["lang"]], "en")
  expect_equal(attributes(df_out)[["label_en"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_en"]], c("Patient database of the practice Dr. Sommer"))
  
  expect_equal(attributes(df_out$id)[["label_en"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_en"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], "en")
  expect_equal(attributes(df_out$id)[["lang"]], "en")
  expect_equal(attributes(df_out$name)[["languages"]], "en")
  expect_equal(attributes(df_out$age)[["lang"]], "en")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_en"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["description_en"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_en"]], valuelabels)
  expect_equal(attributes(df_out$name)[["labels"]], NULL)
  
})



test_that("as_odf_tibble active language argument and metadata without a tag", {
  # create dataframe
  df <- data.frame(id = 1:5, name = c("Klaus", "Anna", "Rebecca", "Kevin", "Janina"), age = c(55, 40, 19, 25, 60), diagnosis = c(1,3,3,2,1))
  # Add metadata for dataset
  attr(df, "name") <- "patientdata"
  attr(df, "label_en") <- "Patient Data"
  attr(df, "label") <- "Patientendaten"
  attr(df, "description_en") <- "Patient database of the practice Dr. Sommer"
  attr(df, "description") <- "Patientendaten Praxis Dr. Sommer"
  attr(df, "url") <- "www.example.url.en"
  
  # Add metadata for diagnosis variable with label, description and value labels.
  attr(df$id, "name") <- "id"
  attr(df$id, "label_en") <- "Patient ID"
  attr(df$id, "label") <- "Patienten-ID"
  attr(df$id, "description_en") <- "Practice Patient ID"
  attr(df$id, "description") <- "Praxis Dr. Sommer Patienten-ID"
  attr(df$diagnosis, "name") <- "diagnosis"
  attr(df$diagnosis, "label_en") <- "Diagnosis"
  attr(df$diagnosis, "label") <- "Diagnose"
  attr(df$diagnosis, "description_en") <- "Diagnosis patient last visit"
  attr(df$diagnosis, "description") <- "Diagnose Patient letzter Besuch"
  valuelabels_diagnosis <- 1:4
  names(valuelabels_diagnosis) <- c("Covid", "Influenza", 
                                    "Common cold", "Tonsillitis")
  attr(df$diagnosis, "labels_en") <- valuelabels_diagnosis
  names(valuelabels_diagnosis) <- c("Corona", "Grippe", 
                                    "Grippaler Infekt", "Mandelentzündung")
  attr(df$diagnosis, "labels") <- valuelabels_diagnosis
  
  df_out <- as_odf_tbl(df)
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label_en", "label", 
                                            "description_en", "url", "label_NA",
                                            "description_NA",
                                            "languages", "lang", "class"))
  expect_equal(attributes(df_out)[["name"]], "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("en", "NA"))
  expect_equal(attributes(df_out)[["lang"]], "en")
  expect_equal(attributes(df_out)[["label_NA"]], c("Patientendaten"))
  expect_equal(attributes(df_out)[["label_en"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_en"]],
               c("Patient database of the practice Dr. Sommer"))
  expect_equal(attributes(df_out)[["description_NA"]],
               c("Patientendaten Praxis Dr. Sommer"))
  expect_equal(attributes(df_out)[["description"]],
               NULL)
  
  expect_equal(attributes(df_out$id)[["label"]], "Patient ID")
  expect_equal(attributes(df_out$id)[["label_NA"]], "Patienten-ID")
  expect_equal(attributes(df_out$id)[["label_en"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_NA"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], c("en", "NA"))
  expect_equal(attributes(df_out$id)[["lang"]], "en")
  expect_equal(attributes(df_out$name)[["languages"]], c("en", "NA"))
  expect_equal(attributes(df_out$age)[["lang"]], "en")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_en"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_NA"]], "Diagnose")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["description_en"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description_NA"]], "Diagnose Patient letzter Besuch")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_en"]], valuelabels)
  names(valuelabels) <- c("Corona", "Grippe", 
                          "Grippaler Infekt", "Mandelentzündung")
  expect_equal(attributes(df_out$diagnosis)[["labels_NA"]], valuelabels)
  expect_equal(attributes(df_out$diagnosis)[["labels"]], NULL)
  
  
  # Indicate language of metadata without language tag
  df_out <- as_odf_tbl(df, language_of_metadata = "de")
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label_en", "label", 
                                            "description_en", "url", "label_de",
                                            "description_de",
                                            "languages", "lang", "class"))
  expect_equal(attributes(df_out)[["name"]], "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out)[["lang"]], "en")
  expect_equal(attributes(df_out)[["label_de"]], c("Patientendaten"))
  expect_equal(attributes(df_out)[["label_en"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_en"]],
               c("Patient database of the practice Dr. Sommer"))
  expect_equal(attributes(df_out)[["description_de"]],
               c("Patientendaten Praxis Dr. Sommer"))
  expect_equal(attributes(df_out)[["description"]],
               NULL)
  
  expect_equal(attributes(df_out$id)[["label"]], "Patient ID")
  expect_equal(attributes(df_out$id)[["label_de"]], "Patienten-ID")
  expect_equal(attributes(df_out$id)[["label_en"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_de"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$id)[["lang"]], "en")
  expect_equal(attributes(df_out$name)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$age)[["lang"]], "en")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_en"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_de"]], "Diagnose")
  expect_equal(attributes(df_out$diagnosis)[["description_en"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description_de"]], "Diagnose Patient letzter Besuch")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_en"]], valuelabels)
  names(valuelabels) <- c("Corona", "Grippe", 
                          "Grippaler Infekt", "Mandelentzündung")
  expect_equal(attributes(df_out$diagnosis)[["labels_de"]], valuelabels)
  expect_equal(attributes(df_out$diagnosis)[["labels"]], NULL)
  
  
  ### Indicate active language
  df_out <- as_odf_tbl(df, active_language = "de", language_of_metadata = "de")
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label_en", "label", 
                                            "description_en", "url", "label_de",
                                            "description_de",
                                            "languages", "lang", "class"))
  expect_equal(attributes(df_out)[["name"]], "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out)[["lang"]], "de")
  expect_equal(attributes(df_out)[["label_de"]], c("Patientendaten"))
  expect_equal(attributes(df_out)[["label_en"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_en"]],
               c("Patient database of the practice Dr. Sommer"))
  expect_equal(attributes(df_out)[["description_de"]],
               c("Patientendaten Praxis Dr. Sommer"))
  expect_equal(attributes(df_out)[["description"]],
               NULL)
  
  expect_equal(attributes(df_out$id)[["label"]], "Patienten-ID")
  expect_equal(attributes(df_out$id)[["label_de"]], "Patienten-ID")
  expect_equal(attributes(df_out$id)[["label_en"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_de"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$id)[["lang"]], "de")
  expect_equal(attributes(df_out$name)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$age)[["lang"]], "de")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnose")
  expect_equal(attributes(df_out$diagnosis)[["label_en"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_de"]], "Diagnose")
    expect_equal(attributes(df_out$diagnosis)[["description_en"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description_de"]], "Diagnose Patient letzter Besuch")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_en"]], valuelabels)
  names(valuelabels) <- c("Corona", "Grippe", 
                          "Grippaler Infekt", "Mandelentzündung")
  expect_equal(attributes(df_out$diagnosis)[["labels_de"]], valuelabels)
  expect_equal(attributes(df_out$diagnosis)[["labels"]], NULL)
  
  ### Indicate active language with wrong active language
  df_out <- as_odf_tbl(df, active_language = "fr", language_of_metadata = "de")
  expect_equal(class(df_out), c("odf_tbl", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(df_out), c(5, 4))
  expect_equal(colnames(df_out), c("id", "name", "age", "diagnosis"))
  expect_equal(names(attributes(df_out)), c("row.names", "names", "name",
                                            "label_en", "label", 
                                            "description_en", "url", "label_de",
                                            "description_de",
                                            "languages", "lang", "class"))
  expect_equal(attributes(df_out)[["name"]], "patientdata")
  expect_equal(attributes(df_out)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out)[["lang"]], "en")
  expect_equal(attributes(df_out)[["label_de"]], c("Patientendaten"))
  expect_equal(attributes(df_out)[["label_en"]], c("Patient Data"))
  expect_equal(attributes(df_out)[["description_en"]],
               c("Patient database of the practice Dr. Sommer"))
  expect_equal(attributes(df_out)[["description_de"]],
               c("Patientendaten Praxis Dr. Sommer"))
  expect_equal(attributes(df_out)[["description"]],
               NULL)
  
  expect_equal(attributes(df_out$id)[["label"]], "Patient ID")
  expect_equal(attributes(df_out$id)[["label_de"]], "Patienten-ID")
  expect_equal(attributes(df_out$id)[["label_en"]], "Patient ID")
  expect_equal(attributes(df_out$name)[["label_de"]], NULL)
  expect_equal(attributes(df_out$id)[["name"]], "id")
  expect_equal(attributes(df_out$id)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$id)[["lang"]], "en")
  expect_equal(attributes(df_out$name)[["languages"]], c("en", "de"))
  expect_equal(attributes(df_out$age)[["lang"]], "en")
  expect_equal(attributes(df_out$age)[["name"]], "age")
  
  expect_equal(attributes(df_out$diagnosis)[["name"]], "diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_en"]], "Diagnosis")
  expect_equal(attributes(df_out$diagnosis)[["label_de"]], "Diagnose")
  expect_equal(attributes(df_out$diagnosis)[["description_en"]], "Diagnosis patient last visit")
  expect_equal(attributes(df_out$diagnosis)[["description_de"]], "Diagnose Patient letzter Besuch")
  expect_equal(attributes(df_out$diagnosis)[["description"]], NULL)
  
  valuelabels<-1:4
  names(valuelabels) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
  expect_equal(attributes(df_out$diagnosis)[["labels_en"]], valuelabels)
  names(valuelabels) <- c("Corona", "Grippe", 
                          "Grippaler Infekt", "Mandelentzündung")
  expect_equal(attributes(df_out$diagnosis)[["labels_de"]], valuelabels)
  expect_equal(attributes(df_out$diagnosis)[["labels"]], NULL)
})
