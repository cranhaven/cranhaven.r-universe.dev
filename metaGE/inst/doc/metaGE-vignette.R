## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = '70%'
)

## ----intall metaGE------------------------------------------------------------
if (!require('metaGE')){
  install.packages('metaGE')
} 
library('metaGE')

## ----setup,warning=FALSE,message=FALSE----------------------------------------

library(tidyverse)
library(DT)

## For graphical displays
library(data.table)
library(corrplot)
library(ggplot2)


## ----listing files of GWAS results--------------------------------------------
## Get the folder containing the association file
RepData <- system.file("extdata", package = "metaGE")

## Get the complete list of association files
File.list <- list.files(RepData ,full.names = TRUE) %>% 
  tibble(Names = .)%>% 
  mutate(ShortNames = Names %>%
           str_remove(pattern = paste0(RepData,"/")) %>%
           str_remove(pattern = "_3DF.txt"))  %>%
  select(ShortNames,Names) %>% 
  deframe

File.list

## ----looking at single file---------------------------------------------------
## Have a look at the first one
fread(File.list[1]) %>% head()  %>% datatable()

## ----metaGE collect-----------------------------------------------------------
###Build the dataset
## First provide the list of variable names 
Names.list <- list(MARKER="Marker_Name",
                   CHR="Chromosome",
                   POS="Marker_Position",
                   FREQ="Maf",
                   EFFECT="SNP_Weight",
                   PVAL="Pvalue",
                   ALLELE0="Allele1",
                   ALLELE1="Allele2")

MinFreq <- 0.07

## Now collect
MetaData <- metaGE.collect(FileNames = File.list, VariableNames = Names.list, MinFreq = MinFreq)
head(MetaData$Data) %>% datatable()


## ----metaGE collect folders---------------------------------------------------
## Get the list of the association files
File.list1 <- File.list[1:2]

## Get the list of the other association files
File.list2 <- File.list[3]

File.listc <- list("List1" = File.list1, "List2" = File.list2)
File.listc

###Build the dataset
## First provide the list of variable names 
Names.listc <- list(Names.list, Names.list)

MinFreq <- 0.07

## Now collect
MetaData <- metaGE.collect(FileNames = File.listc, VariableNames = Names.listc, MinFreq = MinFreq)

head(MetaData$Data)  %>% datatable()

## ----import data--------------------------------------------------------------
# Import the data
data("metaData")

## ----build matcorr------------------------------------------------------------
Threshold <- 0.8
MatCorr <- metaGE.cor(metaData, Threshold = Threshold)

## ----show matcorr,fig.align = 'center',fig.height=8,fig.width=8---------------
corrplot(MatCorr,order = "hclust")

## ----metaGE fit---------------------------------------------------------------
## Fixed Effect
FeDF <- metaGE.fit(metaData, MatCorr, Method = "Fe")
head(FeDF %>% select(CHR, POS, MARKER, Mu, Tau, PVALUE))  %>% datatable()

## Random Effect
ReDF <- metaGE.fit(metaData, MatCorr, Method = "Re")
head(ReDF %>% select(CHR, POS, MARKER, Mu, Tau, PVALUE))  %>% datatable()

## ----metaGE pvalplot,fig.height=3,fig.width=5,out.width='45%'-----------------
Pvalue.list <- list('PVALUE.Fe'= FeDF$PVALUE, 'PVALUE.Re'= ReDF$PVALUE)
plott <- map(names(Pvalue.list),~metaGE.pvalplot(Pvalue.list[[.x]], Main= .x) )

## ----correcting pvalues-------------------------------------------------------
CorrectingPValues <- function(Pvalues){

  ## Get a p0 estimate
  p0 = min(2*sum(Pvalues > 0.5)/length(Pvalues),1-1/length(Pvalues))

  ## Get the corrected p-values
  CorrectedPval <- stats::p.adjust(Pvalues, method = 'BH')*p0

  return(CorrectedPval)
}

## ----FDR control--------------------------------------------------------------
## FDR control
Alpha <- 0.05
Signif <- lapply(Pvalue.list, FUN = function(i){
                  return(CorrectingPValues(i) %>% 
                    `<`(Alpha) %>% 
                    which)})

lapply(X = Signif,FUN = length)

## ----manhattan plot,fig.align='center',fig.height=6,fig.width=10--------------

PvalThresholdFe <-FeDF[Signif$PVALUE.Fe,]$PVALUE%>% max %>% max(.,0)
manhattanFe <- metaGE.manhattan(Data = FeDF,VarName = 'PVALUE', Threshold = PvalThresholdFe,Main = '-log10(Pval) alongside the chromosome Fe method' )
print(manhattanFe)

PvalThresholdRe <- ReDF[Signif$PVALUE.Re,]$PVALUE%>% max %>% max(.,0)
manhattanRe <- metaGE.manhattan(Data = ReDF,VarName = 'PVALUE', Threshold = PvalThresholdRe,Main = '-log10(Pval) alongside the chromosome Re method')
print(manhattanRe)


## ----heatmap,fig.align='center',fig.height=6,fig.width=10---------------------
heatmapFe <- metaGE.heatmap(Data = FeDF[Signif$PVALUE.Fe,],Prefix = "Z.",Main = "Z-scores of Fe significant markers across environments")


heatmapRe <- metaGE.heatmap(Data = ReDF[Signif$PVALUE.Re,],Prefix = "Z.", Main = "Z-scores of Re significant markers across environments")


## ----local score FE-----------------------------------------------------------
x <- 3
FeDF_ls <- metaGE.lscore(Data = FeDF, PvalName = "PVALUE", xi = x)

## ----sigzones Fe--------------------------------------------------------------
FeDF_ls$SigZones %>% datatable()

## ----manhattan Fe lscore,fig.align='center',fig.height=6,fig.width=10---------
manhattanFe_lscore <- metaGE.manhattan(Data = FeDF_ls$Data,
                              VarName = "SCORE",
                              Score = T,
                              SigZones = FeDF_ls$SigZones )
print(manhattanFe_lscore+ggtitle('Score local alongside the chromosome Fe method'))


## ----import environment data--------------------------------------------------
data("envDesc")
envDesc %>% datatable()

## ----incidence matrix---------------------------------------------------------
## Build the incidence matrix 
(Incidence.Temp <- metaGE.incidence(VarName = "Temp",Covariate = envDesc,EnvName = "ShortName", Data = metaData))

## ----metaGE  contrast test----------------------------------------------------
## Build the list of Incidence
Incidence.list <- list(Temp = Incidence.Temp,
                       Diff.Temp = Incidence.Temp)

#Build the list of Contrast
Contrast.list <- list(Temp = NULL,
                      Diff.Temp = matrix(c(1,-1,0,0,1,-1),2,byrow = T)) 


ContrastDF <- metaGE.test(Data = metaData, MatCorr = MatCorr,
                          Incidence = Incidence.list,
                          Contrast = Contrast.list)

## ----FDR control test contrast------------------------------------------------
Alpha <- 0.05
SignifContrast <- apply(ContrastDF %>% select(contains("PVALUE.")),2,
                        FUN = function(i){return(CorrectingPValues(i) %>% 
                                                    `<`(Alpha) %>% 
                                                      which)})

lapply(SignifContrast,length)


## ----contrast heatmap,fig.align='center',fig.height=6,fig.width=10------------
# Specify the groups of environment
heatmap_Temp <- metaGE.heatmap(Data = ContrastDF[SignifContrast$PVALUE.Temp,],EnvGroups = envDesc[,1:2],Prefix = "Z.",Main = "Z-scores of markers with contrasted effect according the temperature")



## ----regression test----------------------------------------------------------
RegressionDF <- metaGE.test(Data=metaData,MatCorr = MatCorr,Covariate = envDesc[,c(1,5,6)],EnvName = "ShortName")

## ----FDR control test regression----------------------------------------------
SignifRegression <- apply(RegressionDF %>% select(contains("PVALUE.")),2,
                        FUN = function(i){return(CorrectingPValues(i) %>% 
                                                    `<`(Alpha) %>% 
                                                      which)})

lapply(SignifRegression,length)


## ----significant marker test regression---------------------------------------
RegressionDF[SignifRegression$PVALUE.Tnight.mean,] %>% select(CHR, POS, MARKER, PVALUE.Tnight.mean) %>% arrange(PVALUE.Tnight.mean) %>% head()  %>% datatable()

## ----metaGE regplot,fig.align='center',fig.height=7,fig.width=10--------------
metaGE.regplot(Data = metaData, Covariate = envDesc,VarName = "Tnight.mean",EnvName = "ShortName", MarkerName = "AX-90548528", aesCol = "Classification")

