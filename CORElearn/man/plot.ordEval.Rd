\name{plot.ordEval}
\alias{plotOrdEval}
\alias{plot.ordEval}
\alias{printOrdEval}
\title{ Visualization of ordEval results }
\description{
    The method \code{plot} visualizes the results of ordEval algorithm with an adapted 
    box-and-whiskers plots. The method \code{printOrdEval} prints summary of the results 
    in a text format.
}
\usage{
    plotOrdEval(file, rndFile, ...) 
    
    \method{plot}{ordEval}(x, graphType=c("avBar", "attrBar", "avSlope"), ...)
    
    printOrdEval(x)
}
\arguments{
    \item{x}{The object containing results of ordEval algorithm obtained by calling  \code{\link{ordEval}}.
            If this object is not given, it has to be constructed from files \code{file} and \code{rndFile}.}
    \item{file}{ Name of file where evaluation results of ordEval algorithm were written to. }
    \item{rndFile}{ Name of file where evaluation of random normalizing attributes by ordEval algorithm were written to. }
    \item{graphType}{ The type of the graph to produce. Can be any of \code{"avBar", "attrBar", "avSlope"}. }
    \item{\dots }{ Other options controlling graphical output, used by specific graphical methods. See details.}
    
}
\details{
    The output of function \code{\link{ordEval}} either returned directly or stored in files \code{file} and \code{rndFile}
    is read and visualized. The type of graph produced is controlled by \code{graphType} parameter:
    \itemize{
    \item \code{avBar} the positive and negative reinforcement of each value of each attribute is visualized 
                        as the length of the bar. For each value also a normalizing modified box and whiskers plot
                        is produced above it, showing the confidence interval of the same attribute value under the assumption 
                        that the attribute contains no information. If the length of the bar is outside the normalizing whiskers, this 
                        is a statistically significant indication that the value is important.
    \item \code{attrBar} the positive and negative reinforcement for each attribute is visualized 
                        as the length of the bar. This reinforcement is weighted sum of contributions of individual
                        values visualized with \code{avBar} graph type. 
    \item \code{avSlope} the positive and negative reinforcement of each value of each attribute is visualized 
                        as the slope of the line segment connecting consequent values
    }    
    The \code{avBar} and \code{avSlope} produce several graphs (one for each attribute). In order to see them all on
     an interactive device use \code{\link{devAskNewPage}}. On some platforms or in RStudio environment the graphical window stores the
     history and one can browse through recent pages. Alternatively use any of non-interactive devices
     such as \code{\link{pdf}} or \code{\link{postscript}}. Some support for opening and handling of these devices is provided 
     by function \code{\link{preparePlot}}. The user should take care to call \code{\link{dev.off}} after completion of the operations.  
      
    There are some additional optional parameters \bold{\dots } which are important to all or for some graph types.
    \itemize{
    \item \code{ciType} The type of the confidence interval in "avBar" and "attrBar" graph types. Can be \code{"two.sided"}, \code{"upper"}, \code{"lower"}, or \code{"none"}. 
      Together with \code{ordEvalNormalizingPercentile} parameter in \code{\link{ordEval}}, \code{ciType}, \code{ciDisplay}, 
      and \code{ciDecorate}  controls the type, length and display of confidence intervals for each value.    
    \item \code{ciDisplay} The way how confidence intervals are displayed. Can be \code{"box"} or \code{"color"}. The value \code{"box"} displays confidence interval as box and whiskers plot above the actual value with whiskers representing confidence percentiles.
         The value \code{"color"} displays only the upper limit of confidence interval, namely the value 
         (represented with a length of the bar) beyond the confidence interval is displayed with more intensive color or shade. 
 \item \code{ciDecorate} controls if the reinforcement factors stretching outside the confidence intervals of possible random effects are decorated by being circled with an ellipse. The default value NULL means that there are no decorations, other values are interpreted as colors in the function \code{draw.elipse}, e.g., \code{ciDecorate="red"} draws red ellipses around statisticaly significant reinforcemnets.
    \item \code{equalUpDown} a boolean specifying if upward and downward reinforcement of the same value are to be displayed 
    side by side on the same level; it usually makes sense to 
    set this parameter to \code{TRUE} when specifying a single value differences by setting \code{variant="attrDist1"} in 
    \code{\link{ordEval}} function.  
    \item \code{graphTitle} specifies text to incorporate into the title.
    \item \code{attrIdx} displays plot for a single attribute with specified index.
    \item \code{xlabel}  label of lower horizontal axis. 
    \item \code{ylabLeft} label of the left-hand vertical axis.
    \item \code{ylabRight} label of the right-hand vertical axis.
    \item \code{colors} a vector with four colors specifying colors of reinforcement bars for down, down_beyond, up, and up_beyond, respectively. If set to NULL this produces black and white graph with shades of gray.
     The colors down_beyond and up_beyond depict the confidence interval if parameter \code{ciDisplay="color"}. \cr
     The default values are \code{colors=c("green","lightgreen","blue","lightblue")}.
    }   
     
}
\value{
    The method returns no value.
}
\examples{
    # prepare a data set
    dat <- ordDataGen(200)

    # evaluate ordered features with ordEval
    oe <- ordEval(class ~ ., dat, ordEvalNoRandomNormalizers=200)
    plot(oe)
    # printOrdEval(oe)
    
    # the same effect we achieve by storing results to files
    tmp <- ordEval(class ~ ., dat, file="profiles.oe", 
                  rndFile="profiles.oer", ordEvalNoRandomNormalizers=200)   
    plotOrdEval(file="profiles.oe", rndFile="profiles.oer",
                graphType="attrBar")
    # clean up for the sake of R package checks
    file.remove("profiles.oe")
    file.remove("profiles.oer")

}
\references{ 
    Marko Robnik-Sikonja, Koen Vanhoof: Evaluation of ordinal attributes at value level. 
    \emph{Knowledge Discovery and Data Mining}, 14:225-243, 2007    
    
    Marko Robnik-Sikonja, Igor Kononenko: Theoretical and Empirical Analysis of ReliefF and RReliefF.
    \emph{Machine Learning Journal}, 53:23-69, 2003
    
    Some of the references are available also from \url{http://lkm.fri.uni-lj.si/rmarko/papers/}    
}

\author{ Marko Robnik-Sikonja}


\keyword{ classif }

\seealso{
    \code{\link{ordEval}},
    \code{\link{helpCore}},
    \code{\link{preparePlot}},
    \code{\link{CORElearn}}
     
}

