\name{GUI Functions}
\alias{biclust.bimax.GUI}
\alias{biclustGUI_help}
\alias{biclust.quest.GUI}
\alias{biclust.robust.fuse}
\alias{biclust.robust.fuse.GUI}
\alias{fabia.biplot}
\alias{isa.extract}
\alias{isa.GUI}
\alias{isa.scoreplots}
\alias{isa.summary}
\alias{openGUIScripts}
\alias{openGUIVignette}
\alias{shinylinkwebsite}
\alias{shinylinkstandalone}
\alias{biclustguiwebsitelink}
\alias{parallelCoordinates2}
\alias{parallelCoordinates3}
\alias{plotSuper.GUI}
\alias{robust.reset}
\alias{spfabia.GUI}
\alias{superbiclust.GUI}
\alias{superbiclust.robust.GUI}
\alias{writeBic.GUI}
\alias{export.Biclust}
\alias{chooseresultsGUI}
\alias{export.fabia.threshL}
\alias{export.fabia}
\alias{rqubic.GUI}
\alias{bicare.GUI}
\alias{summaryBICARE}
\alias{bicare.residuplot}
\alias{bicare.genesetenrichment}
\alias{bicare.samplesenrichment}
\alias{bicare.pdata}
\alias{bicare.makereport}
\alias{ExtractBiclustersGUI_general}
\alias{ExtractBiclustersGUI}
\alias{FindBiclusters}
\alias{HeatmapBC.GUI}
\alias{bcdiaghighlight_WINDOW}
\alias{repeatanalysis.GUI}
\alias{bibit3_BC_GUI}
\alias{BiBitWorkflow.GUI}
\alias{BiBitDendrogram.GUI}
\alias{ClusterRowCoverage.GUI}
\alias{Update.GUI}
\alias{summaryworkflow.GUI}
\alias{saveworkflow.GUI}
\alias{resetworkflow.GUI}
\alias{ColNoiseBC.GUI}

\alias{isadefault_WIN_TEMPORARY}

\title{GUI Functions}

\description{List of GUI functions used for certain buttons.}
%\usage{
%
%bicare.genesetenrichment(result,setType,gene.from,gene.to)
%bicare.GUI(Data,k,pGene,pSample,r,N,M,t,blocGene,blocSample,eSetData.name)
%bicare.makereport(dirName)
%bicare.pdata(result)
%bicare.residuplot(result)
%bicare.samplesenrichment(result,covariates)
%biclust.bimax.GUI(x,method,minr,minc,number,maxc,backfit,n2,maxbicheck)
%biclustGUI_help()
%biclust.quest.GUI(x,method,ns,nd,sd,alpha,number,d,quant,vari)
%biclust.robust.fuse(CutTree,method_result,type="biclust")
%export.Biclust(biClust,x,baseName,clusterAssignments=NULL,clusterNames=NULL)
%export.fabia(fabiaRes,baseName,clusterAssignments=NULL,clusterNames=NULL,threshZ=0.5,threshL=NULL)
%export.fabia.threshL(fabiaRes,threshZ=0.5)
%ExtractBiclustersGUI(result,which="all",from=1,to=1,selection=c(1),dim="both",save=FALSE,save.name="Extract")
%ExtractBiclustersGUI_general(result,which="all",from=1,to=1,selection=c(1),dim="both",save=FALSE,save.name="Extract")
%fabia.biplot(x,dim1,dim2)
%FindBiclusters(results,cols=c(),rows=c(),data)
%isa.extract(modules)
%isa.GUI(data,thr.row.from,thr.row.to,thr.row.by,thr.col.from,thr.col.to,
%    thr.col.by,no.seeds,dir.row,dir.col)
%isa.scoreplots(ISA,type="row",biclust=c(1))
%isa.summary(ISA)
%openGUIScripts()
%openGUIVignette()
%parallelCoordinates2(x, bicResult, number, info = TRUE)
%parallelCoordinates3(x,bicResult,number,type2="default",plotBoth=FALSE,
%    plotcol=TRUE,compare=TRUE,info=FALSE,bothlab=c("Rows","Columns"),
%    order=FALSE,order2=0,ylab="Value",col=1,...)
%plotSuper.GUI(type,which.robust,CutTree)
%robust.reset(method_result)
%rqubic.GUI(x,eSetData.name,q,rank,minColWidth,report.no,tolerance,filter.proportion,check.disc)
%spfabia.GUI(X,p,alpha,cyc,spl,spz,non_negative,random,write_file,norm,
%    scale,lap,nL,lL,bL,samples,initL,iter,quant,lowerB,upperB,
%    dorescale,doini,eps,eps1,datatype,filename)
%summary.BICARE(x)
%superbiclust.GUI(x,index,type,method_result,extra.biclust=NULL,isa2=FALSE)
%superbiclust.robust.GUI(CutTree,show.which=FALSE)
%writeBic.GUI(dset,fileName,bicResult,bicname,mname=c("fabia","isa2","biclust"),append=TRUE,delimiter=" ")
%}
%\arguments{
%\item{x}{ }
%\item{method}{ }
%\item{minr}{ }
%\item{minc}{ }
%\item{number}{ }
%\item{maxc}{ }
%\item{backfit}{ }
%\item{n2}{ }
%\item{maxbicheck}{ }
%\item{ns}{ }
%\item{nd}{ }
%\item{sd}{ }
%\item{alpha}{ }
%\item{d}{ }
%\item{quant}{ }
%\item{vari}{ }
%\item{CutTree}{ }
%\item{method_result}{ }
%\item{type}{ }
%\item{dim1}{ }
%\item{dim2}{ }
%\item{modules}{ }
%\item{data}{ }
%\item{thr.row.from}{ }
%\item{thr.row.to}{ }
%\item{thr.row.by}{ }
%\item{thr.col.from}{ }
%\item{thr.col.to}{ }
%\item{thr.col.by}{ }
%\item{no.seeds}{ }
%\item{dir.row}{ }
%\item{dir.col}{ }
%\item{ISA}{ }
%\item{biclust}{ }
%\item{bicResult}{ }
%\item{info}{ }
%\item{type2}{ }
%\item{plotBoth}{ }
%\item{plotcol}{ }
%\item{compare}{ }
%\item{bothlab}{ }
%\item{order}{ }
%\item{order2}{ }
%\item{ylab}{ }
%\item{col}{ }
%\item{...}{ }
%\item{which.robust}{ }
%\item{X}{ }
%\item{p}{ }
%\item{cyc}{ }
%\item{spl}{ }
%\item{spz}{ }
%\item{non_negative}{ }
%\item{random}{ }
%\item{write_file}{ }
%\item{norm}{ }
%\item{scale}{ }
%\item{lap}{ }
%\item{nL}{ }
%\item{lL}{ }
%\item{bL}{ }
%\item{samples}{ }
%\item{initL}{ }
%\item{iter}{ }
%\item{lowerB}{ }
%\item{upperB}{ }
%\item{dorescale}{ }
%\item{doini}{ }
%\item{eps}{ }
%\item{eps1}{ }
%\item{datatype}{ }
%\item{filename}{ }
%\item{index}{ }
%\item{extra.biclust}{ }
%\item{isa2}{ }
%\item{show.which}{ }
%
%
%}

\details{This list of GUI functions is a mix of 2 types of functions. Some of them simply consist out of combining several functions from a package, linking them all to a single button by placing them together in a function.
The other type are special created functions to extract more bicluster information from certain object or bring more functionality to the GUI. 
Sometimes this last type is based on example code in package documentation, sometimes it is made from scratch.
More information of these function can be found throughout the vignette of \code{RcmdrPlugin.BiclustGUI}.

In general these functions are not meant for public use.
}


\author{Ewoud De Troyer \email{ewoud.detroyer@uhasselt.be}}

