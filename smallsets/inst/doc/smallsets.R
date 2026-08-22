## -----------------------------------------------------------------------------
library(smallsets)

head(s_data)

## ----timeline1, fig.width=8, fig.height=3, fig.retina=2, fig.align="center"----
library(smallsets)

set.seed(145)

Smallset_Timeline(data = s_data, 
                  code = system.file("s_data_preprocess.R", package = "smallsets"))

## ---- eval=FALSE--------------------------------------------------------------
#  Smallset_Timeline(data = s_data, code = "s_data_preprocess.R")

## ---- code = readLines(system.file("s_data_preprocess.R", package = "smallsets")), eval=FALSE, attr.source='.numberLines'----
#  # smallsets snap s_data caption[Remove rows where C2 is FALSE.]caption
#  s_data <- s_data[s_data$C2 == TRUE, ]
#  
#  # smallsets snap +2 s_data caption[Replace missing values in C6 and C8 with
#  # column means. Drop C7 because there are too many missing values.]caption
#  s_data$C6[is.na(s_data$C6)] <- mean(s_data$C6, na.rm = TRUE)
#  s_data$C8[is.na(s_data$C8)] <- mean(s_data$C8, na.rm = TRUE)
#  s_data$C7 <- NULL
#  
#  # smallsets snap +1 s_data caption[Create a new column, C9, by summing C3 and
#  # C4.]caption
#  s_data$C9 <- s_data$C3 + s_data$C4

## ---- code = readLines(system.file("s_data_preprocess_block.R", package = "smallsets")), eval=FALSE, attr.source='.numberLines'----
#  # smallsets snap 7 s_data caption[Remove rows where C2 is FALSE.]caption
#  # smallsets snap 12 s_data caption[Replace missing values in C6 and C8 with
#  # column means. Drop C7 because there are too many missing values.]caption
#  # smallsets snap 16 s_data caption[Create a new column, C9, by summing C3 and
#  # C4.]caption
#  
#  # remove rows where C2 is false
#  s_data <- s_data[s_data$C2 == TRUE,]
#  
#  # deal with missing data
#  s_data$C6[is.na(s_data$C6)] <- mean(s_data$C6, na.rm = TRUE)
#  s_data$C8[is.na(s_data$C8)] <- mean(s_data$C8, na.rm = TRUE)
#  s_data$C7 <- NULL
#  
#  # create a new variable
#  s_data$C9 <- s_data$C3 + s_data$C4

## ---- eval=FALSE--------------------------------------------------------------
#  Smallset_Timeline(data = s_data,
#                    code = system.file("s_data_preprocess.Rmd", package = "smallsets"))

## ---- eval=FALSE--------------------------------------------------------------
#  rmarkdown::render(system.file("s_data_preprocess.Rmd", package = "smallsets"),
#                    output_dir = getwd())

## ---- eval=FALSE--------------------------------------------------------------
#  Smallset_Timeline(data = s_data,
#                    code = system.file("s_data_preprocess.py", package = "smallsets"))

## ---- fig.keep="none"---------------------------------------------------------
Smallset_Timeline(data = s_data, 
                  code = system.file("s_data_preprocess.R", package = "smallsets"), 
                  rowCount = 7, rowSelect = NULL)

## ---- eval=FALSE, fig.keep="none"---------------------------------------------
#  Smallset_Timeline(data = s_data,
#                    code = system.file("s_data_preprocess.R", package = "smallsets"),
#                    rowCount = 5, rowSelect = 1, rowReturn = TRUE)

## ---- echo=FALSE, fig.keep="none"---------------------------------------------
Smallset_Timeline(data = s_data, 
                  code = system.file("s_data_preprocess.R", package = "smallsets"),
                  rowIDs = c("27", "42", "95", "96", "99"),
                  rowReturn = T)

## ----timeline3, fig.width=8, fig.height = 3, fig.retina = 2, fig.align='center'----
Smallset_Timeline(data = s_data, 
                  code = system.file("s_data_preprocess.R", package = "smallsets"), 
                  rowCount = 5, rowIDs = c("27", "42", "95", "96", "99"))

## ---- eval=FALSE--------------------------------------------------------------
#  Smallset_Timeline(data = s_data,
#                    code = system.file("s_data_preprocess.R", package = "smallsets"),
#                    rowSelect = 2, rowReturn = T)

## ----timeline4, echo=FALSE, fig.width=8, fig.height = 3, fig.retina = 2, fig.align='center'----
Smallset_Timeline(data = s_data,
                  code = system.file("s_data_preprocess.R", package = "smallsets"),
                  rowIDs = c("3", "32", "80", "97", "99"),
                  rowReturn = T)

## ----timeline5, fig.width=8, fig.height = 4, fig.retina = 2, fig.align='center'----
set.seed(145)

Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  colours = list(added = "#FFC500",
                 deleted = "#FF4040",
                 edited = "#5BA2A6",
                 unchanged = "#E6E3DF"),
  printedData = TRUE,
  truncateData = 4,
  missingDataTints = TRUE,
  ghostData = FALSE,
  font = "Georgia",
  sizing = sets_sizing(data = 2, captions = 3.5, columns = 3.5),
  labelling = sets_labelling(labelCol = "darker", labelColDif = 1),
  spacing = sets_spacing(captions = 3)
)

## ----timeline6, fig.width=5, fig.height = 7, fig.retina = 2, fig.align='center'----
set.seed(145)

Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess.R", package = "smallsets"),
  align = "vertical",
  colours = 2,
  spacing = sets_spacing(captions = 8, header = 2.5),
  labelling = sets_labelling(labelColDif = 1),
  sizing = sets_sizing(tiles = .4, captions = 3, columns = 3, legend = 12)
)

## ----timeline7, fig.width = 5, fig.height = 6, fig.retina = 2, fig.align='center'----
set.seed(145)

Smallset_Timeline(
  data = s_data,
  code = system.file("s_data_preprocess_4.R", package = "smallsets"),
  rowCount = 8,
  colours = 3,
  ghostData = TRUE,
  missingDataTints = TRUE,
  font = "serif",
  spacing = sets_spacing(
    captions = 3,
    rows = 2,
    degree = 60,
    header = 1.5
  ),
  sizing = sets_sizing(
    legend = 12,
    captions = 4,
    columns = 4
    )
)

## ---- fig.keep="none", comment=''---------------------------------------------
set.seed(145)

Smallset_Timeline(data = s_data, 
                  code = system.file("s_data_preprocess.R", package = "smallsets"), 
                  altText = TRUE)

## ---- code = readLines(system.file("s_data_preprocess_resume.R", package = "smallsets")), eval=FALSE----
#  # smallsets snap s_data caption[Removed rows where C2
#  # is FALSE.]caption
#  s_data <- s_data[s_data$C2 == TRUE,]
#  
#  # smallsets snap 9 s_data caption[Replaced missing values in C6 and
#  # C8 with column means. Dropped C7 because there are too many
#  # missing values.]caption
#  s_data$C6[is.na(s_data$C6)] <- mean(s_data$C6, na.rm = TRUE)
#  s_data$C8[is.na(s_data$C8)] <- mean(s_data$C8, na.rm = TRUE)
#  s_data$C7 <- NULL
#  
#  # smallsets snap +1 s_data caption[Created a new column,
#  # C9, by summing C3 and C4.]caption
#  s_data$C9 <- s_data$C3 + s_data$C4
#  
#  # smallsets resume caption[Ran the model but then
#  # decided to make another change to the dataset.]caption
#  
#  # smallsets snap 27 s_data caption[Created a new categorical
#  # column, C10, based on C9 terciles.]caption
#  t <- quantile(s_data$C9, c(0:3 / 3))
#  s_data$C10 = with(s_data, cut(
#    C9,
#    t,
#    include.lowest = T,
#    labels = c("Low", "Med", "High")
#  ))
#  

## ----timeline8, fig.width = 7, fig.height = 2, fig.align='center', fig.retina = 2----
set.seed(145)

Smallset_Timeline(data = s_data, 
                  code = system.file("s_data_preprocess_resume.R", package = "smallsets"), 
                  sizing = sets_sizing(
                    columns = 1.8,
                    captions = 1.8,
                    legend = 7,
                    icons = .8
                    ),
                  spacing = sets_spacing(
                    captions = 3,
                    degree = 45,
                    header = 3.5,
                    right = 2
                    )
                  )

