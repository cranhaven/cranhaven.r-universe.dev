## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bioassays)

## ----libraries, message=FALSE, warning=FALSE, echo=TRUE, eval = FALSE---------
#  library(tcltk)# for selecting the folder for analysis
#  library(dplyr)
#  library(ggplot2)# for plotting graphs
#  library(reshape2)
#  library(nplr)# for the standard curve fitting

## ----directory,echo=TRUE, eval = FALSE----------------------------------------
#  path1<-tk_choose.dir(getwd(), "Choose the folder for Analysis")
#  # A window will popup and ask you to select the folder containg the data
#  setwd(path1)

## ----files--------------------------------------------------------------------
filelist <-list("L_HEPG2_P3_72HRS.csv","L_HEPG2_P3_24HRS.csv") # list of files
fno<-1 # file number (in fileslist) that is going to be analyzed
result <- data.frame(stringsAsFactors= FALSE) ## An empty dataframe to dump result
zzz <- data.frame(stringsAsFactors= FALSE) ## An empty dataframe to dump result

## ----readcsv------------------------------------------------------------------
filename<-extract_filename(filelist[fno])[1]
filename
nickname<-extract_filename(filelist[fno], split="_",end=".csv",remove="",sep="")[2]
nickname

## ----readcsv2, eval=FALSE-----------------------------------------------------
#  rawdata<-read.csv(filename,stringsAsFactors = FALSE, strip.white = TRUE,
#                    na.strings = c("NA",""),header = TRUE,skip=1)
#  head(rawdata)

## ----readcsv3, echo=FALSE-----------------------------------------------------
data(rawdata96)
rawdata<-rawdata96
head(rawdata)

## ----metafile, eval=FALSE-----------------------------------------------------
#  metadata<-read.csv("metafile.csv",stringsAsFactors = FALSE,strip.white = TRUE,
#                    na.strings = c("NA",""),header = TRUE)
#  head(metadata)

## ----metafile2, echo=FALSE----------------------------------------------------
data(metafile96)
metadata<-metafile96
head(metadata)

## ----rearrange-plate-reading--------------------------------------------------
rawdata<-data2plateformat(rawdata,platetype = 96)
head(rawdata)

## ----plate2df OD--------------------------------------------------------------
OD_df<- plate2df(rawdata)
head(OD_df)

## ----heatmap, normalization, echo = TRUE, eval=TRUE, fig.width=4, fig.height=3.5----
data<-matrix96(OD_df,"value",rm="TRUE")
heatplate(data,"Plate 1", size=5)

## ----joining metatadata-------------------------------------------------------
plate_info<-function(file,i){
  
  file<-file[1]
  plate<- extract_filename(file,split = "_",end = ".csv", remove = " ", sep=" ")[5]

if(plate == "P2"){
  compound<-"CyclosporinA"   # Concentration of cyclosporinA used for experiment
  concentration<-c(0,1,5,10,15,20,25,50) # Concentration of cyclosporinA used for experiment
  type<-c("S1","S2","S3","S4","S5","S6","S7","S8") # sample names of corresponding concentration
  dilution<-5
  plate_meta<-list(compound=compound,concentration=round(concentration,2),type=type,
                   dilution=dilution)
}


if(plate == "P3"){
  compound<-"Taxol"
  concentration<-c(0,0.0125,.025,.05,0.1,1,5,10) 
  type<-c("S1","S2","S3","S4","S5","S6","S7","S8")
  dilution<-5
  plate_meta<-list(compound=compound,concentration=round(concentration,2),type=type,
                   dilution=dilution)
}


if(plate =="p4"){
  compound<-c("Cisplatin")
  concentration<-c(0,0.5,2,4,8,16,64,"") 
  type<-c("S1","S2","S3","S4","S5","S6","S7","") 
  dilution <- 5
  plate_meta<-list(compound=compound,concentration=round(concentration,2),type=type,
                   dilution=dilution)
}
  return(plate_meta)
}

plate_details<-plate_info(filelist,1)
plate_details

## ----metadata compiling-------------------------------------------------------
metadata1<-plate_metadata(plate_details,metadata,mergeby="type")
head(metadata1)

## ----metafile-joining, warning=FALSE------------------------------------------
data_DF<- dplyr::inner_join(OD_df,metadata1,by=c("row","col","position"))
assign(paste("data",sep="_",nickname),data_DF) # create a copy of data with platename
head(data_DF)

## ----reduce-blank-------------------------------------------------------------
data_DF<-reduceblank(data_DF, x_vector =c("All"),blank_vector = c("Blank"), "value")
head(data_DF)
assign(paste("Blkmin",sep="_",nickname),data_DF) # create a copy of data with platename

## ----standard-curve-calculation, warning=FALSE--------------------------------
std<- dplyr::filter(data_DF, data_DF$id=="STD")  
std<- aggregate(std$blankminus ~ std$concentration, FUN = mean )
colnames (std) <-c("con", "OD")
head(std)

## ----nprc graph, fig.width= 3, fig.height=3,warning=FALSE---------------------

fit1<-nplr::nplr(std$con,std$OD,npars=3,useLog = FALSE) 
#npars = 3 for 3 parametric regression curve

#for graph
x1 <- nplr::getX(fit1); y1 <- nplr::getY(fit1)
x2 <- nplr::getXcurve(fit1); y2 <- nplr::getYcurve(fit1)
plot(x1, y1, pch=15, cex=1, col="red", xlab="Concentration",
     ylab="Mean OD", main=paste("Standard Curve: ", nickname), cex.main=1)
lines(x2, y2, lwd=3, col="seagreen4")

## ----evaluate linear fitting--------------------------------------------------
params<-nplr::getPar(fit1)$params
nplr::getGoodness(fit1)

## ----as a fxn estimate1, warning= FALSE---------------------------------------
estimated_nplr<-estimate(data_DF,colname="blankminus",fitformula=fit1,method="nplr")
head(estimated_nplr)

## ----linear-regression-plot, fig.width= 3, fig.height=3-----------------------

fit2<-stats::lm(formula = con ~ OD,data = std)
ggplot2::ggplot(std, ggplot2::aes(x=OD,y=con))+
ggplot2::ggtitle(paste("Std Curve:", nickname))+
ggplot2::geom_point(color="red",size=2)+
ggplot2::geom_line(data = ggplot2::fortify(fit2),ggplot2::aes(x=OD,y=.fitted),
          colour="seagreen4",size=1)+
ggplot2::theme_bw()

## ----linear-fit-summary, warning=FALSE----------------------------------------
conpred<-estimate(std,colname="OD",fitformula=fit2,method="linear")
compare<-conpred[,c(1,3)]
corAccuracy<-cor(compare)[1,2]
corAccuracy ## Correlation accuracy of regression line
summary(fit2)


## ----as a fxn estimate3, warning= FALSE---------------------------------------
estimated_lr<-estimate(data_DF,colname="blankminus",fitformula=fit2,method="linear")
head(estimated_lr)

## ----multiply by dilution1, warning=FALSE-------------------------------------
estimated_lr$estimated2 <- estimated_lr$estimated * estimated_lr$dilution
head(estimated_lr)

## ----plate-summary, eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE, out.width=50----

result<-dfsummary(estimated_lr,"estimated2",c("id","type"),
        c("STD","Blank"),"plate1", rm="FALSE",
        param=c(strict="FALSE",cutoff=40,n=12))

result

## ----t test 1-----------------------------------------------------------------
pval<-pvalue(result, control="S1", sigval=0.05)
head(pval)


## ----384 rawdata, eval=FALSE--------------------------------------------------
#  rawdata2<-read.csv("384.csv",stringsAsFactors = FALSE,strip.white = TRUE,
#                    na.strings = c("NA",""),header = TRUE,skip=1)
#  dim(rawdata2)
#  head(rawdata2)
#  

## ----384 rawdata2, echo= FALSE------------------------------------------------
data(rawdata384)
rawdata2<-rawdata384
dim(rawdata2)
head(rawdata2)

## ----384 metadata, eval=FALSE-------------------------------------------------
#  metadata2<-read.csv("metafile_384_plate.csv",stringsAsFactors = FALSE,strip.white = TRUE,
#                     na.strings = c("NA",""),header = TRUE)
#  head(metadata2)
#  

## ----384 metadata2, echo=FALSE------------------------------------------------
data(metafile384)
metadata2<-metafile384
head(metadata2)

## ----384 rearrange------------------------------------------------------------

rawdata2<-data2plateformat(rawdata2,platetype = 384)
head(rawdata2)


## ----384 plate2df-------------------------------------------------------------
OD_df2 <- plate2df(rawdata2)
head(OD_df2)

## ----384overview1, echo = TRUE, eval=TRUE, fig.width=4, fig.height=3.5--------

data2<-matrix96(OD_df2,"value",rm="TRUE")
heatplate(data2,"Plate 384", size=1.5)

## ----384 innerjoin------------------------------------------------------------
data_DF2<- dplyr::inner_join(OD_df2,metadata2,by=c("row","col","position"))
head(data_DF2)

## ----384overview2, echo = TRUE, eval=TRUE, fig.width=4, fig.height=3.5--------
data3<-matrix96(data_DF2,"cell",rm="TRUE")
heatplate(data3,"Plate 384", size=2)

## ----384overview3, echo = TRUE, eval=TRUE, fig.width=4, fig.height=3.5--------
data4<-matrix96(data_DF2,"compound",rm="TRUE")
heatplate(data4,"Plate 384", size=2)

## ----384 blank----------------------------------------------------------------
data_blk<-reduceblank(data_DF2, 
x_vector=c("drug1","drug2","drug3","drug4"),
blank_vector = c("blank1","blank2","blank3","blank4"), "value")
dim(data_blk)

## ----384 blank2---------------------------------------------------------------
head(data_blk)

## ----384 summary 1------------------------------------------------------------
result2<-dfsummary(data_blk,"blankminus",
        c("cell","compound","concentration","type"),
        c("blank1","blank2","blank3","blank4"),
        nickname="384well", 
        rm="FALSE",param=c(strict="FALSE",cutoff=40,n=12))
head (result2)
dim (result2)


## ----384 summary 2------------------------------------------------------------
result3<-dfsummary(data_blk,"blankminus",
        c("cell","compound","concentration"),
        c("B","drug2","huh7"),
        nickname="", 
        rm="FALSE",param=c(strict="FALSE",cutoff=40,n=12))
head (result3)
dim (result3)


## ----384 ttest, echo=TRUE-----------------------------------------------------
pvalue<-pvalue(result3,"C3",sigval=0.05)
pvalue


