## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  
#  # At this point you can download and install the the latest version of the
#  # opendataformat package from CRAN:
#  install.packages("opendataformat")
#  
#  

## ----read_odf-----------------------------------------------------------------
library(opendataformat)

path <- system.file("extdata", "data.odf.zip", package = "opendataformat")
df <- read_odf(file = path)

## ----view df, comment = ""----------------------------------------------------
df

## ----view df haven, eval=FALSE------------------------------------------------
#  library(haven)
#  View(df)

## ----read_odf language, eval=FALSE--------------------------------------------
#  df_en <- read_odf(file = path, languages = "en")

## ----read_odf language all, eval=FALSE----------------------------------------
#  df_en <- read_odf(file = path, languages = "all")

## ----read_odf language list, eval=FALSE---------------------------------------
#  df_en <- read_odf(file = path, languages = c("en", "de"))

## ----read_odf all inputs, eval = FALSE----------------------------------------
#  df <- read_odf(file, languages = "all", nrows = Inf, skip = 0, select = NULL)

## ----docu_odf, eval = FALSE---------------------------------------------------
#  docu_odf(df)

## ----docu_odf print-----------------------------------------------------------
docu_odf(df, style = "print")

## ----docu_odf variables, comment=""-------------------------------------------
docu_odf(df, variables = "yes", style = "print")

## ----docu_odf selected variable default---------------------------------------
docu_odf(df$bap9001, style = "print")

## ----docu_odf languages all, eval = FALSE-------------------------------------
#  docu_odf(df$bap9001, style = "print", variables = "yes", languages = "all")

## ----docu_odf languages code print--------------------------------------------
docu_odf(df$bap9001, style = "print", languages = "de")

## ----docu_odf languages code all data, eval = FALSE---------------------------
#  docu_odf(df, style = "print", variables = "yes", languages = "de")

## ----docu own style, eval = TRUE, comment = ""--------------------------------
for (i in names(df)) {
  cat(
    paste0(attributes(df[[i]])$name, ": ", attributes(df[[i]])$label_de, "\n")
  )
}

## ----getmetadata_odf1, comment = ""-------------------------------------------
getmetadata_odf(df, type = "label")

## ----getmetadata_odf2, comment = ""-------------------------------------------
getmetadata_odf(df$bap87, type = "valuelabels")

## ----docu_odf setLanguage2, eval = FALSE--------------------------------------
#  df <- setlanguage_odf(df, language = "de")
#  
#  docu_odf(df$bap9001, style = "print")

## ----display languages--------------------------------------------------------
attributes(df)$languages
attr(df, "languages")

## ----dataset attributes, comment = ""-----------------------------------------
attributes(df)

## ----variable attributes, comment = ""----------------------------------------
attributes(df$bap87)

## ----variable label attributes, comment = ""----------------------------------
attributes(df$bap87)$label_de

## ----variable label attributes2, eval = FALSE, comment = ""-------------------
#  attr(df$bap87, "label_de")

## ----attributes deletion, comment = ""----------------------------------------
attributes(df$bap87)$description_de <- NULL
attributes(df$bap87)$description_de

## ----getmetadata_odf3, comment = ""-------------------------------------------
getmetadata_odf(df, type = "labels")

## ----getmetadata_odf4, comment = ""-------------------------------------------
getmetadata_odf(df$bap96, type = "labels")

## ----getmetadata_odf5, eval = FALSE, comment = ""-----------------------------
#  getmetadata_odf(df, type = "labels", language = "en")

## ----getmetadata_odf6, eval = FALSE, comment = ""-----------------------------
#  df <- setlanguage_odf(df, language = "en")
#  getmetadata_odf(df, type = "labels")

## ----getmetadata_odf valuelabels, comment = ""--------------------------------
getmetadata_odf(df$bap9001, type = "valuelabels")


## ----getmetadata_odf valuelabels names, comment = ""--------------------------
names(getmetadata_odf(df$bap9001, type = "valuelabels"))

## ----getmetadata_odf descriptions, comment = ""-------------------------------
getmetadata_odf(df, type = "description")

## ----getmetadata_odf url, eval = FALSE, comment = ""--------------------------
#  getmetadata_odf(df, type = "url")

## ----getmetadata_odf type, eval = FALSE, comment = ""-------------------------
#  getmetadata_odf(df, type = "type")

## ----write_odf, comment = "", eval = FALSE------------------------------------
#  write_odf(
#    x = df[, 1:4],
#    file = "../df_1_4.odf.zip"
#  )
#  
#  #or :
#  df_14 <- df[, 1:4]
#  write_odf(
#    x = df[, 1:4],
#    file = "df_1_4.odf.zip"
#  )

## ----write_odf metadata, comment = "", eval = FALSE---------------------------
#  write_odf(
#    x = df,
#    file = "../df_metadata.odf.zip",
#    export_data = FALSE
#  )

## ----write_odf english, comment = "", eval = FALSE----------------------------
#  write_odf(
#    x = df,
#    file = "../df_en.odf.odf.zip",
#    languages = "en"
#  )

## ----write_odf english german, comment = "", eval = FALSE---------------------
#  write_odf(
#    x = df,
#    file = "../df_en_de.odf.odf.zip",
#    languages = c("en", "de")
#  )

## ----as_odf_tbl, comment = "", eval = FALSE-----------------------------------
#  #Create a data frame with four variables ind 5 rows
#  exampledata <- data.frame(id = 1:5,
#                            name = c("Klaus", "Anna", "Rebecca",
#                                     "Kevin", "Janina"),
#                            age = c(55, 40, 19, 25, 60),
#                            diagnosis = c(1,3,3,2,1))
#  # Add metadata for dataset according to ODF tibble framework.
#  attr(exampledata, "name") <- "patientdata"
#  attr(exampledata, "label_en") <- "Patient Data"
#  attr(exampledata, "description_en") <- "Patient database of the practice Dr. Sommer"
#  attr(exampledata, "url") <- "www.example.url.en"
#  
#  # Add metadata for diagnosis variable with label, description and value labels.
#  attr(exampledata$id, "name") <- "id"
#  attr(exampledata$id, "label_en") <- "Patiend ID"
#  attr(exampledata$id, "description_en") <- "Practice Patiend ID"
#  attr(exampledata$diagnosis, "name") <- "diagnose"
#  attr(exampledata$diagnosis, "label_en") <- "Diagnosis"
#  attr(exampledata$diagnosis, "description_en") <- "Diagnosis patient last visit"
#  valuelabels_diagnosis <- 1:4
#  names(valuelabels_diagnosis) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
#  attr(exampledata$diagnosis, "labels_en") <- valuelabels_diagnosis
#  # use as_odf_bl to transform dataframe to an ODF tibble ('odf_tbl'-class object)
#  example_odf  <-  as_odf_tbl(exampledata)
#  
#  # Display metadata of diagnosis Variable
#  docu_odf(example_odf$diagnosis, style = "print")

## ----table, comment = ""------------------------------------------------------
table(df$bap87, useNA = "ifany")

## ----table attributes labels, comment = ""------------------------------------
attributes(df$bap87)$labels_en

## ----table attributes labels_de, comment = ""---------------------------------
attributes(df$bap87)$labels_de

## ----table factor, comment = ""-----------------------------------------------
table(factor(df$bap87, labels = names(attributes(df$bap87)$labels_en)))

## ----table factor getmetadata_odf, eval=F, comment = ""-----------------------
#  table(factor(df$bap87,
#               labels = names(getmetadata_odf(df$bap87, type = "valuelabels"))))

## ----table factor german, comment = ""----------------------------------------
table(
  factor(
    df$bap87,
      labels = names(attributes(df$bap87)$labels_de)
    )
  )

## ----table factor german getmetadata_odf, comment = ""------------------------
table(
  factor(
    df$bap87,
      labels = names(getmetadata_odf(df$bap87, type = "valuelabels",
                                     language = "de"))
    )
  )

## ----merge with join, eval = FALSE--------------------------------------------
#  library(dplyr)
#  #similar to merge(df[,c(1:3,6)], df[,c(4:6)], by="name", all.x=T, all.y=F)
#  merged_df <- left_join(df[, c(1:3, 6)], df[, c(4:6)], by = "name")
#  #or
#  merged_df <- left_join(df[, c(1:3, 6)], df[, c(4:6)])

## ----copy data, comment = ""--------------------------------------------------
bap87_rec <- df$bap87

## ----check metadata, comment = ""---------------------------------------------
attributes(bap87_rec)

## ----NA, comment = ""---------------------------------------------------------
for (row in seq(1, length(bap87_rec))) {
  if (!is.na(bap87_rec[row]) && bap87_rec[row] <= -1) {
    bap87_rec[row] <- NA
  }
}

table(bap87_rec, useNA = "ifany")

## ----check metadata again, comment = ""---------------------------------------
attributes(bap87_rec)$labels_en

## ----copy metadata to recoded var, comment = ""-------------------------------
attributes(bap87_rec)$labels_en <-
  unname(attributes(df$bap87)$labels_en)[3:7] # values
names(attributes(bap87_rec)$labels_en) <-
  names(attributes(df$bap87)$labels_en)[3:7] # labels

attributes(bap87_rec)$labels_en

## ----remove language versions of labels, comment = ""-------------------------
attributes(bap87_rec)$labels_de <-
  unname(attributes(df$bap87)$labels_de)[3:7] # values
names(attributes(bap87_rec)$labels_de) <-
  names(attributes(df$bap87)$labels_de)[3:7] # labels



## ----change variable name of recoded var, comment = ""------------------------
attributes(bap87_rec)$name <- "bap87_rec"
attributes(bap87_rec)$name

## ----frequency table of recoded var, comment = ""-----------------------------
table(
  factor(
    bap87_rec,
      labels = names(attributes(bap87_rec)$labels_en)
    )
  )

## ----barplot, comment = "", fig.height = 3, fig.width = 5, fig.align = "center"----
barplot(
  table(
  factor(
    bap87_rec,
      labels = names(attributes(bap87_rec)$labels_en)
    )
  ),
  main = attributes(bap87_rec)$description_en, # title
  xlab = paste0(
    attributes(bap87_rec)$name, ": ", attributes(bap87_rec)$label), # label
  sub = attributes(bap87_rec)$url, # subtitle
  cex.main = 0.9, cex.names = 0.7, cex.sub = 0.8, cex.axis = 0.6,
  cex.lab = 0.7 # font sizes
)

## ----barplot german, comment = "", fig.height = 3, fig.width = 5, fig.align = "center"----
barplot(
  table(
  factor(
    bap87_rec,
      labels = names(attributes(bap87_rec)$labels_de)
    )
  ),
  main = attributes(bap87_rec)$description_de, # title
  xlab = paste0(
    attributes(bap87_rec)$name, ": ", attributes(bap87_rec)$label_de), # label
  sub = attributes(bap87_rec)$url, # subtitle
  cex.main = 0.7, cex.names = 0.5, cex.sub = 0.8, cex.axis = 0.7,
  cex.lab = 0.7 # font sizes
)

