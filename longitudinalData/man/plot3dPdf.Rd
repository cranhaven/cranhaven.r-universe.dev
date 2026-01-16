\name{plot3dPdf}
\Rdversion{1.1}
\alias{plot3dPdf}
\alias{plot3dPdf,LongData3d-method}
\alias{plot3dPdf,LongData3d,missing-method}
\alias{plot3dPdf,LongData3d,numeric-method}
\alias{plot3dPdf,LongData3d,Partition-method}

\title{
  ~ Function: plot3dPdf for LongData ~
}
\description{
  Given a \code{\linkS4class{LongData}} and a \code{\linkS4class{Partition}}, this
  function create 'Triangle objects' representing the 3D plot the
  clusters centers. Triangle object can latter be used to include
  dynamic rotating graph in a pdf file.
}
\usage{
\S4method{plot3dPdf}{LongData3d,missing}(x,y,varY=1,varZ=2)
\S4method{plot3dPdf}{LongData3d,numeric}(x,y,varY=1,varZ=2)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containing the trajectories to plot.}
  \item{y}{\code{[numeric]}: \code{Partition} that will be use to plot
    the object.}
  \item{varY}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the first variable to display. 1 by default.}
  \item{varZ}{\code{[numeric]} or \code{[character]}: either the
    number or the name of the second variable to display. 2 by default.}
}
\details{
  Create Triangle objects representing the 3D plot of the main
  trajectories of a \code{\linkS4class{LongData}}.

  The three functions \code{\link{plot3dPdf}}, \code{\link{saveTrianglesAsASY}} and \code{\link{makeLatexFile}}
  are design to export a 3D graph to a Pdf file. The process is the following:

  \enumerate{
    \item \code{\link{plot3dPdf}}: Create a scene, that is a collection of Triangle object that
    represent a 3D images.
    \item \code{\link{saveTrianglesAsASY}}: Export the scene in an '.asy' file.
    \item '.asy' can not be include in LaTeX file. LaTeX can read only
    '.pre' file. So the next step is to use the software
    \code{asymptote} to convert '.asy' to '.pre'. This is done by the command \code{asy -inlineimage -tex pdflatex
      scene.asy} (not in R, in a console).
    \item The previous step did produce a file \code{scene+0.prc} that can be include in a LaTeX file.
    \code{\link{makeLatexFile}} create a LaTeX file that is directly compilable (using \code{pdfLatex}).
    It produce a pdf file that contain the 3D object.
  }
}

\value{
  A Triangle object.
}

\seealso{
  \code{\link{saveTrianglesAsASY}},\code{\link{makeLatexFile}},\code{\link[misc3d:makeTriangles]{makeTriangles}}
}

\section{Author}{
  Christophe Genolini\cr
  1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
  2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\examples{
  ### Move to tempdir
  wd <- getwd()
  setwd(tempdir()); getwd()
  
  ### Generating the data
  data(artificialJointLongData)
  myLd <- longData3d(artificialJointLongData,timeInData=list(var1=2:12,var2=13:23))
  part <- partition(rep(1:3,each=50))
  plotTrajMeans3d(myLd,part)

  ### Creation of the scene
  scene <- plot3dPdf(myLd,part)
  drawScene.rgl(scene)

  ### Export in '.asy' file
  saveTrianglesAsASY(scene)

  ### Creation of a '.prc' file
  # Open a console, then run:
  # 'asy -inlineimage -tex pdflatex scene.asy'

  ### Creation of the LaTeX main document
  makeLatexFile()

  ### Creation of the '.pdf'
  # Open a console window, then run
  # pdfLatex main.tex
  
  ### Go back to current dir
  setwd(wd)
}
