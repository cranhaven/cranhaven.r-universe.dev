## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bioassays)

## ----rawdata------------------------------------------------------------------
data(rawdata96)
head(rawdata96)

## ----metadata-----------------------------------------------------------------
data(metafile96)
head(metafile96)

## ----extract_fname eg1--------------------------------------------------------
extract_filename("L HEPG2 P3 72HRS.csv")

## ----extract_fname eg2--------------------------------------------------------
extract_filename("L HEPG2 P3 72HRS.csv", split=" ",end=".csv",remove="L",sep="")

## ----rmodd_summary eg1--------------------------------------------------------
x<- c(1.01,0.98,0.6,0.54,0.6,0.6,0.4,3)
rmodd_summary(x, rm = "FALSE", strict= "FALSE", cutoff=80,n=3)

## ----ermodd_summary eg2-------------------------------------------------------
rmodd_summary(x, rm = "TRUE", strict= "FALSE", cutoff=80,n=3)

## ----rmodd_summary eg3--------------------------------------------------------
rmodd_summary(x, rm = "TRUE", strict= "TRUE", cutoff=20,n=5)

## ----data2plateformat eg2-----------------------------------------------------
rawdata<-data2plateformat(rawdata96,platetype = 96)
head(rawdata)

## ----plate2df eg1-------------------------------------------------------------
OD_df <- plate2df(rawdata)
head(OD_df)

## ----matrix96 eg2-------------------------------------------------------------
matrix96(OD_df,"value")

## ----matrix96 eg3-------------------------------------------------------------
matrix96(OD_df,"position")

## ----plate_metadata eg 2------------------------------------------------------
head(metafile96)

## ----plate_metadata eg 3, echo=TRUE-------------------------------------------
plate_details <- list("compound" = "Taxol",
                "concentration" = c(0.00,0.01,0.02,0.05,0.10,1.00,5.00,10.00),
                "type" = c("S1","S2","S3","S4","S5","S6","S7","S8"),
                "dilution" = 1)

## ----plate_metadata eg 5, message=FALSE, warning=FALSE------------------------
plate_meta<-plate_metadata(plate_details,metafile96,mergeby="type")
head(plate_meta)

## ----plate_metadata eg 6, message=FALSE, warning=FALSE------------------------
data_DF<- dplyr::inner_join(OD_df,plate_meta,by=c("row","col","position"))

head(data_DF)

## ----heatplate eg:1, warning= FALSE-------------------------------------------
 datamatrix<-matrix96(metafile96,"id")
datamatrix

## ----heatplate eg:2, warning= FALSE, fig.width=4, fig.height=3.5--------------
heatplate(datamatrix,"Plate 1", size=5)

## ----heatplate eg:3, warning=FALSE--------------------------------------------
rawdata<-data2plateformat(rawdata96,platetype = 96)
OD_df<- plate2df(rawdata)
data<-matrix96(OD_df,"value")
data

## ----heatplate eg:4, warning=FALSE, fig.width=4, fig.height=3.5---------------
heatplate(data,"Plate 1", size=5)

## ----reduceblank eg1, warning=FALSE-------------------------------------------
data_DF<-reduceblank(data_DF, x_vector =c("All"),blank_vector = c("Blank"), "value")
head(data_DF)


## ----standards----------------------------------------------------------------
std<- dplyr::filter(data_DF, data_DF$id=="STD")  
std<- aggregate(std$blankminus ~ std$concentration, FUN = mean )
colnames (std) <-c("con", "OD")
head(std)

## ----fitmodels----------------------------------------------------------------
fit2<-stats::lm(formula = con ~ OD,data = std)# linear model
fit1<-nplr::nplr(std$con,std$OD,npars=3,useLog = FALSE)#  nplr, 3 parameter model

## ----nplr estimating nplr, message=FALSE, warning=FALSE-----------------------
estimated<-estimate(data_DF,colname="blankminus",fitformula=fit2,method="linear")
head(estimated)

## ----nplr estimating linear, message=FALSE, warning=FALSE---------------------
estimated2<-estimate(data_DF,colname="blankminus",fitformula=fit1,method="nplr")
head(estimated2)

## ----summary 3, message=FALSE, warning=FALSE, out.width=40--------------------
result<-dfsummary(estimated,"estimated",c("id","type"),
        c("STD","Blank"),"plate1", rm="FALSE",
        param=c(strict="FALSE",cutoff=40,n=12))

result

## ----pvalue eg1, warning=FALSE, message = FALSE-------------------------------
pval<-pvalue(result, control="S8", sigval=0.05)
head(pval)

