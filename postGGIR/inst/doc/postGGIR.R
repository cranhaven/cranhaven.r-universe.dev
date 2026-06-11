## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)

## ----pdfplot,  echo=F, out.width="700px", out.height="700px"------------------
include_graphics("figure1.workflow.pdf")  

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("postGGIR", dependencies = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(postGGIR)
#  create.postGGIR()

## ----eval=FALSE---------------------------------------------------------------
#  options(width=2000)
#  argv = commandArgs(TRUE);
#  print(argv)
#  print(paste("length=",length(argv),sep=""))
#  mode<-as.numeric(argv[1])
#  print(c("mode =", mode))
#  # (Note) Please remove the above lines if you are running this within R console
#  #        instead of submitting jobs to a cluster.
#  
#  #########################################################################
#  # (user-define 1) you need to redefine this according different study!!!!
#  #########################################################################
#  # example 1
#  filename2id.1<-function(x)  unlist(strsplit(y1,"\\."))[1]
#  
#  #  example 2 (use csv file =c("filename","ggirID"))
#  filename2id.2<-function(x) {
#    d<-read.csv("./postGGIR/inst/example/filename2id.csv",head=1,stringsAsFactors=F)
#    y1<-which(d[,"filename"]==x)
#    if (length(y1)==0) stop(paste("Missing ",x," in filename2id.csv file",sep=""))
#    if (length(y1)>=1) y2<-d[y1[1],"newID"]
#    return(as.character(y2))
#  }
#  
#  
#  #########################################################################
#  #  main call
#  #########################################################################
#  
#  call.afterggir<-function(mode,filename2id=filename2id.1){
#  
#  library(postGGIR)
#  #########################################################################
#  # (user-define 2) Fill in parameters of your ggir output
#  ##########################################################################
#  
#  currentdir =
#  studyname =
#  bindir =
#  outputdir =
#  setwd(currentdir)
#  
#  rmDup=FALSE   # keep all subjects in postGGIR
#  PA.threshold=c(50,100,400)
#  part5FN="WW_L50M125V500_T5A5"
#  epochIn = 5
#  epochOut = 5
#  flag.epochOut = 60
#  use.cluster = FALSE
#  log.multiplier = 9250
#  QCdays.alpha = 7
#  QChours.alpha = 16
#  useIDs.FN<-NULL
#  Rversion="R"
#  desiredtz="US/Eastern"
#  RemoveDaySleeper=FALSE
#  part5FN=part5FN,
#  NfileEachBundle=20
#  trace=FALSE
#  #########################################################################
#  #   remove duplicate sample IDs for plotting and feature extraction
#  #########################################################################
#  if (mode==3 & rmDup){
#  # step 1: read ./summary/*remove_temp.csv file (output of mode=2)
#  keep.last<-TRUE #keep the latest visit for each sample
#  sumdir<-paste(currentdir,"/summary",sep="")
#  setwd(sumdir)
#  inFN<-paste(studyname,"_samples_remove_temp.csv",sep="")
#  useIDs.FN<-paste(sumdir,"/",studyname,"_samples_remove.csv",sep="")
#  
#  #########################################################################
#  # (user-define 3 as rmDup=TRUE)  create useIDs.FN file
#  #########################################################################
#  # step 2: create the ./summary/*remove.csv file manually or by R commands
#  d<-read.csv(inFN,head=1,stringsAsFactors=F)
#  d<-d[order(d[,"Date"]),]
#  d<-d[order(d[,"newID"]),]
#  d[which(is.na(d[,"newID"])),]
#  S<-duplicated(d[,"newID"],fromLast=keep.last) #keep the last copy for nccr
#  d[S,"duplicate"]<-"remove"
#  write.csv(d,file=useIDs.FN,row.names=F)
#  
#  }
#  
#  #########################################################################
#  #   call afterggir
#  #########################################################################
#  
#  setwd(currentdir)
#  afterggir(mode=mode,
#            useIDs.FN=useIDs.FN,
#            currentdir=currentdir,
#            studyname=studyname,
#            bindir=bindir,
#            outputdir=outputdir,
#            epochIn=epochIn,
#            epochOut=epochOut,
#            flag.epochOut=flag.epochOut,
#            log.multiplier=log.multiplier,
#            use.cluster=use.cluster,
#            QCdays.alpha=QCdays.alpha,
#            QChours.alpha=QChours.alpha,
#            QCnights.feature.alpha=QCnights.feature.alpha,
#            Rversion=Rversion,
#            filename2id=filename2id,
#            PA.threshold=PA.threshold,
#            desiredtz=desiredtz,
#            RemoveDaySleeper=RemoveDaySleeper,
#            part5FN=part5FN,
#            NfileEachBundle=NfileEachBundle,
#            trace=trace)
#  
#  }
#  #########################################################################
#            call.afterggir(mode)
#  #########################################################################
#  
#  #   Note:   call.afterggir(mode)
#  #        mode = 0 : creat sw/Rmd file
#  #        mode = 1 : data transform using cluster or not
#  #        mode = 2 : summary
#  #        mode = 3 : clean
#  #        mode = 4 : impu

## ----eval=FALSE---------------------------------------------------------------
#  call.afterggir(mode,filename2id)

## ----eval=FALSE---------------------------------------------------------------
#  #!/bin/bash
#  #
#  #$ -cwd
#  #$ -j y
#  #$ -S /bin/bash
#    source ~/.bash_profile
#  
#     cd /postGGIR/inst/example/afterGGIR;
#     module load R ;
#       R --no-save --no-restore --args  < studyname_ggir9s_postGGIR.pipeline.maincall.R  0
#       R --no-save --no-restore --args  < studyname_ggir9s_postGGIR.pipeline.maincall.R  1
#       R --no-save --no-restore --args  < studyname_ggir9s_postGGIR.pipeline.maincall.R  2
#       R --no-save --no-restore --args  < studyname_ggir9s_postGGIR.pipeline.maincall.R  3
#       R --no-save --no-restore --args  < studyname_ggir9s_postGGIR.pipeline.maincall.R  4
#  
#       R -e "rmarkdown::render('part5_studyname_postGGIR.report.Rmd'   )"
#       R -e "rmarkdown::render('part6_studyname_postGGIR.nonwear.report.Rmd'   )"
#       R -e "rmarkdown::render('part7a_studyname_postGGIR_JIVE_1_somefeatures.Rmd'   )"
#       R -e "rmarkdown::render('part7b_studyname_postGGIR_JIVE_2_allfeatures.Rmd'   )"
#       R -e "rmarkdown::render('part7c_studyname_postGGIR_JIVE_3_excelReport.Rmd'   )"
#  

## ----echo=F-------------------------------------------------------------------
input<-rbind(c("timestamp","ENMO","anglez"),
             c("2017-11-30T00:00:00+0100",8e-04,-32.5758),
             c("2017-11-30T00:00:05+0100",0.0198,-25.5726),
             c("2017-11-30T00:00:10+0100",0.0177,3.7972),
             c("2017-11-30T00:00:15+0100",0.0118,6.7154),
             c("2017-11-30T00:00:20+0100",0.0106,10.0357),
             c("2017-11-30T00:00:25+0100",0.0341,21.0143),
             c("2017-11-30T00:00:30+0100",0.1708,19.5008),
             c("......","......","......"),
             c("2017-11-30T23:59:55+0100",0.1504,-0.596))


output<-rbind(c( "Date","0:00:00","0:00:05","0:00:10","0:00:15","0:00:20","0:00:25","0:00:30","......","23:59:55"),
              c( "11/30/2017","8.00E-04",0.0198,0.0177,0.0118,0.0106,0.0341,0.1708,"......",0.1504))
kable(input)

## ----echo=F-------------------------------------------------------------------
kable(output)

## ----echo=F-------------------------------------------------------------------
library(xlsx)   
library(knitr)  
library(kableExtra)  


feaFN<-system.file("template", "features.dictionary.xlsx", package = "postGGIR")   
  
dict<-read.xlsx(feaFN,head=1,sheetName="dictionary",stringsAsFactors=F)
dict.SL<-dict[which(dict[,"Domain"]=="SL"),c("Variable","Description")]
dict.PA<-dict[which(dict[,"Domain"]=="PA"),c("Variable","Description")] 
dict.CR<-dict[which(dict[,"Domain"]=="CR"),c("Variable","Description")]

row.names(dict.SL)<-NULL
row.names(dict.PA)<-NULL
row.names(dict.CR)<-NULL

kable(dict.SL) %>%
     kable_styling(bootstrap_options = c("striped", "hover"))

## ----echo=F-------------------------------------------------------------------
kable(dict.PA) %>%
     kable_styling(bootstrap_options = c("striped", "hover"))

## ----echo=F-------------------------------------------------------------------
kable(dict.CR) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))

## ----eval=FALSE,include=FALSE-------------------------------------------------
#  d1<-read.xlsx("postGGIR.output.description.xlsx",sheetName="output.format")
#  
#  cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/v2/postGGIR/vignettes
#  R -e "rmarkdown::render('postGGIR.Rmd'   )"

