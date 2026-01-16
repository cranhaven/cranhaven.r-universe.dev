\name{makeLatexFile}
\alias{makeLatexFile}

\title{
  ~ Function: makeLatexFile ~
}
\description{
  Create a LaTeX document that inclusde 3D objects into PDF documents.
}
\usage{
makeLatexFile(filename = "main.tex", asyToInclude = "scene+0.prc")
}

\arguments{
  \item{filename}{Name of the LaTeX file}
  \item{asyToInclude}{Name of the file holding the 3D graph to include.}
}

\details{
  Create a LaTeX document that inclusde 3D objects into PDF documents with PDF-1.5/1.6 compatibility.
}
\value{
  A LaTeX file, in the current directory.
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

\seealso{
  \link[misc3d:makeTriangles]{makeTriangles},\code{\link{plot3dPdf}}, \code{\link{saveTrianglesAsASY}.}
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


