## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(out.width = "100%")
knitr::opts_chunk$set(fig.align = 'center')
library(knitr)
library(dataSDA)
library(RSDA)

## ----prompt=TRUE--------------------------------------------------------------
library(dataSDA)
data(Abalone.iGAP)
head(Abalone.iGAP)
class(Abalone.iGAP)
data(Abalone)
head(Abalone)
class(Abalone)

## ----prompt=TRUE--------------------------------------------------------------
data(mushroom)
head(mushroom) 

## ----prompt = TRUE------------------------------------------------------------
mushroom_set <- set_variable_format(data = mushroom, location = 8, 
                                       var = "Species")
head(mushroom_set, 3)

## ----prompt = TRUE------------------------------------------------------------
mushroom_tmp <- RSDA_format(data = mushroom_set, 
                               sym_type1 = c("I", "I", "I", "S"),
                               location = c(25, 27, 29, 31), 
                               sym_type2 = c("S"),
                               var = c("Species"))
head(mushroom_tmp, 3)

## ----prompt = TRUE------------------------------------------------------------
mushroom_clean <- clean_colnames(data = mushroom_tmp)
head(mushroom_clean, 3)

## ----prompt = TRUE------------------------------------------------------------
write_csv_table(data = mushroom_clean, file = "mushroom_interval.csv")
mushroom_int <- read.sym.table(file = 'mushroom_interval.csv', 
                                  header = T, sep = ';', dec = '.', 
                                  row.names = 1)
head(mushroom_int, 3)
class(mushroom_int)

## ----prompt = TRUE, eval = FALSE----------------------------------------------
# library(dataSDA)
# data(BLOOD)
# BLOOD[1:3, 1:2]

## ----prompt = TRUE------------------------------------------------------------
library(HistDAWass)
BLOOD[1:3, 1:2]

## ----prompt = TRUE------------------------------------------------------------
A1 <- c(50, 60, 70, 80, 90, 100, 110, 120)
B1 <- c(0.00, 0.02, 0.08, 0.32, 0.62, 0.86, 0.92, 1.00)
A2 <- c(50, 60, 70, 80, 90, 100, 110, 120)
B2 <- c(0.00, 0.05, 0.12, 0.42, 0.68, 0.88, 0.94, 1.00)
A3 <- c(50, 60, 70, 80, 90, 100, 110, 120)
B3 <- c(0.00, 0.03, 0.24, 0.36, 0.75, 0.85, 0.98, 1.00)
List <- list(A1, B1, A2, B2, A3, B3)
List

ListOfWeight <- vector("list", 3)
x <- 0
for (i in 1:length(ListOfWeight)){
  ListOfWeight[[i]] <- distributionH(List[[i + x]], List[[i + x + 1]])
  x <- x + 1
}
Weight <- methods::new("MatH",
                    nrows = 3, ncols = 1, ListOfDist = ListOfWeight,
                    names.rows = c("20s", "30s", "40s"),
                    names.cols = c("weight"), by.row = FALSE)
Weight

## ----prompt = TRUE------------------------------------------------------------
data(Face.iGAP)
class(Face.iGAP)
head(Face.iGAP)
Face <- iGAP_to_MM(data = Face.iGAP, location = 1:6)
head(Face)

## ----prompt = TRUE------------------------------------------------------------
Face.tmp <- RSDA_format(data = Face, 
                        sym_type1 = c("I", "I", "I", "I", "I", "I"), 
                        location = c(1, 3, 5, 7, 9, 11))
head(Face.tmp)

## ----prompt = TRUE------------------------------------------------------------
Face.clean <- clean_colnames(data = Face.tmp)
head(Face.clean)

## ----prompt = TRUE------------------------------------------------------------
write_csv_table(data = Face.clean, file = 'Face_interval.csv')

## ----prompt = TRUE------------------------------------------------------------
Face.interval <- read.sym.table(file = 'Face_interval.csv', header = T, sep = ';', dec = '.', row.names = 1)
head(Face.interval)

## ----prompt = TRUE------------------------------------------------------------
Face.MM <- RSDA_to_MM(Face.interval, RSDA = TRUE)
head(Face.MM)

## ----prompt = TRUE------------------------------------------------------------
Face.iGAP_trans <- MM_to_iGAP(Face.MM)
head(Face.iGAP_trans)

## ----prompt = TRUE------------------------------------------------------------
data(mushroom.int)
int_mean(mushroom.int, var_name = "Pileus.Cap.Width")
int_mean(mushroom.int, var_name = 2:3)

var_name <- c("Stipe.Length", "Stipe.Thickness")
method <- c("CM", "FV", "EJD")
int_mean(mushroom.int, var_name, method)
int_var(mushroom.int, var_name, method)

var_name1 <- "Pileus.Cap.Width"
var_name2 <- c("Stipe.Length", "Stipe.Thickness")
method <- c("CM", "VM", "QM", "SE", "FV", "EJD", "GQ", "SPT") 
int_cov(mushroom.int, var_name1, var_name2, method)
int_cor(mushroom.int, var_name1, var_name2, method)

## ----prompt = TRUE------------------------------------------------------------
data(BLOOD)
hist_mean(BLOOD, "Cholesterol")
hist_var(BLOOD, "Cholesterol")
hist_cov(BLOOD, 'Cholesterol', 'Hemoglobin', method = "B")
hist_cor(BLOOD, 'Cholesterol', 'Hemoglobin', method = "L2W") 

