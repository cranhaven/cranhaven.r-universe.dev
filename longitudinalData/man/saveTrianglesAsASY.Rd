\name{saveTrianglesAsASY}
\alias{saveTrianglesAsASY}

\title{
  ~ Function: saveTrianglesAsASY ~
}
\description{
  Export a \code{Triangle} object to an '.asy' file.
}
\usage{
saveTrianglesAsASY(scene, filename = "scene.asy")
}

\arguments{
  \item{scene}{\code{[Triangle]}: Object representing the graph to plot,
    probably produce by \code{\link{plot3dPdf}}.}
  \item{filename}{\code{[character]}: Name of exported file.}
}
\details{
  Export a \code{Triangle} object to an '.asy' file. See
  \code{\link{plot3dPdf}} for a summary of the overall procedure.
}
\value{
  An '.asy' file, in the current directory.
}

\seealso{
  \code{\link{plot3dPdf}},\code{\link{makeLatexFile}},\code{\link[misc3d:makeTriangles]{makeTriangles}}
}


\references{\url{https://homepage.divms.uiowa.edu/~luke/R/misc3d/misc3d-pdf/misc3d-pdf.pdf}}

\section{Author(s)}{
  Luke Tierney \cr
  Chair, Statistics and Actuarial Science \cr
  Ralph E. Wareham Professor of Mathematical Sciences\cr
  University of Iowa \cr
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

