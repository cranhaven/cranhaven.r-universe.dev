\name{qrfactor}
\alias{qrfactor}
\alias{qrfactor.default}
\alias{print.qrfactor}
\alias{summary.qrfactor}
\alias{plot.qrfactor}
\title{Simultaneous Q-Mode and R-Mode Factor Analysis}
\description{The \code{qrfactor} function performs simultaneous Q-mode and
R-mode factor analysis on spatial and non-spatial data.}
\usage{
qrfactor(source, layer='', var=NULL, type='', p="Yes", scale="sd", t='',
         nf=2, m=NULL, f=NULL, rotation=FALSE, ...)
\method{qrfactor}{default}(source, layer='', var=NULL, type='', p="Yes",
         scale="sd", t='', nf=2, m=NULL, f=NULL, rotation=FALSE, ...)
\method{print}{qrfactor}(x,...)
\method{summary}{qrfactor}(object,...)
\method{plot}{qrfactor}(x,factors=c(1,2),type="loading",plot="",
cex="",pch=15,pos=3,main="",xlim="optimise",
ylim="optimise",abline=TRUE,legend="topright",legendvalues=c(100),
values=FALSE,nfactors=3,rowname=TRUE,par=c(1,2),verbose=FALSE,...)
}
\arguments{
\item{source}{Folder path of the layer. Please quote the full folder path with forward slash "/".  You can use R object as a source but you must set the layer parameter to "nofile"; see below}
\item{layer}{ The layer qrfactor in the folder that you want to work with. It is the file name of qrfactor. This is case sensitive, please. In case you want to use non spatial data such as ".csv", ".txt", "dat" or ".tab" insert the full file name as layer. In case of using R object as a source set "layer" parameter to "nofile"  }
\item{var}{The attributes or variables of the layer. In case of using non spatial data such as ".csv", ".txt", "dat" or ".tab"  var are variables or column names }
\item{type}{Types of plots 'mds'for multidimensional scale, 'coordinate' for principal coordinate analyse. Or The type of results one wants to plot. It takes "scores", "loadings", pca or eigenvectors. The deault is loadings. }
\item{p}{Determine whether prediction must be done:"Yes". The scores are appended to the GIS data  }
\item{t}{The list of variables that one wants to transform eg. transform=c("gold","diamond")}
\item{scale}{scale the data:"sd","pca","data". The default is "sd" that is the scaled data divided by the standard deviation. It can lso take "log" or "sqrt" and use the default "sd" for normal distribution transformation  }
\item{rotation}{Should the retained factors be rotated to simple structure? The default \code{FALSE} (equivalently \code{"none"}) returns the classical unrotated solution; \code{TRUE} (equivalently \code{"varimax"}) applies an orthogonal varimax rotation to the first \code{nf} factors. When rotation is requested the same orthogonal rotation is applied to the R-mode and Q-mode loadings, so both modes stay on one common set of axes and the scores, the combined loadings, the appended index columns and every biplot and map are rotated consistently. The raw eigenvectors, eigenvalues and PCA loadings (\code{eigen.vector}, \code{eigen.value}, \code{pca}) are never rotated. Rotation redistributes variance among the rotated factors, so their individual \code{variance} percentages change while their sum (and \code{cumvariance}) is preserved.}
\item{m}{the the match field: the common variable on both the table and spatial data. This name must be identical to both sets of data}
\item{f}{The full path of csv file and the name of csv eg. C:/Users/owusu/Documents/Rpackages/qrfactor14/inst/external/farms.csv}
\item{x}{an object of class \code{"qrfactor"}, i.e., a fitted model. }
\item{object}{an object of class \code{"qrfactor"}, i.e., a fitted model. }
\item{plot}{The type of plots one desires. It takes "all" for all the 3 plots or "q" for q plot  or "r" for r plot or 'qr' for both q and r plots}
\item{factors}{list of factors one wants to plot. The default is factors=c(1,2). Please do not forget "c" in the list. }
\item{cex}{A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default. It also accepts a vector of values which are recycled eg cex=c("gold")}
\item{nfactors}{The number of factors to extract}
\item{pch}{Either an integer specifying a symbol or a single character to be used as the default in plotting points.}
\item{pos}{The position of text labels}
\item{main}{Main title of the graph}
\item{xlim}{x-coordinates of the axis eg xlim=c(-1.5,1.5)}
\item{ylim}{y-coordinates of the axis eg ylim=c(-1.5,1.5)}
\item{abline}{the intercept and slope, single values of straight lines through the current plot. eg. abline(-0.5,0.5)}
\item{legend}{position of legend: it takes topright,topleft, bottomright,bottomleft, top, left, bottom, right }
\item{legendvalues}{The values of the legend}
\item{values}{Incase one wants to label the graph with another variables. eg. values=c("gold")}
\item{nf}{The number of factors to extract}
\item{rowname}{rownames of the data}
\item{par}{the layout setteing in a form of list}
\item{verbose}{logical; if \code{TRUE}, the analysis views (\code{type = "cluster"}, \code{"anova"}, \code{"region"}, \code{"admin"}, \code{"diagnose"}) print their statistical tables (cluster means, ANOVA, Kruskal-Wallis, Tukey HSD, outliers) to the console. The default \code{FALSE} draws the plots silently, as R plot methods should. The \code{plot()} method also restores the user's \code{par()} settings on exit.}
\item{...}{any other parameter can be added}
}
\value{
Objects of the class that basically list its elements
\item{data}{ Original data for the model. All records must be numeric. It also accepts continous data}
\item{gisdata}{ GIS data for the model incase you use shape files}
\item{x.standard}{ it is the scale matrix of the original data}
\item{correlation}{ The correlation matrix for the data }
\item{eigen.value}{ eigen value of correlation matrix of the data}
\item{eigen.vector}{ eigen vector of correlation matrix of the data}
\item{diagonal.matrix}{ diagonal matrix of eigen vector }
\item{pca}{ pca loadings}
\item{pcascores}{ PCA scores }
\item{r.loading}{ R-mode loadings}
\item{q.loading}{ Q-mode loadings }
\item{loadings}{ combined loadings of R and Q on the same axis}
\item{q.scores}{ computed Q-mode scores }
\item{scores}{ combined R-mode and Q-mode scores on the same axis }
\item{rownames}{ row names of the loadings}
\item{variables}{ variables names of the loadings, of the original data}
\item{rotation}{ the rotation actually applied to the loadings: \code{"varimax"} or \code{"none"}}
\item{unrotated.r.loading}{ the R-mode loadings before rotation (identical to \code{r.loading} when \code{rotation = FALSE})}
\item{unrotated.q.loading}{ the Q-mode loadings before rotation (identical to \code{q.loading} when \code{rotation = FALSE})}
}
\author{George Owusu}
\examples{
# --- CSV / data-frame workflow (fast, always runs) ---
csv <- system.file("external", "Africanfreshwater.csv", package = "qrfactor")
var <- c("Domestic", "Industry", "Agricultur", "Resources",
         "withdrawal", "perCapitaW")

mod0 <- qrfactor(csv, var = var)   # classical unrotated solution (default)
mod0$r.loading
plot(mod0, rowname = "COUNTRY")

# opt in to a varimax rotation of the retained factors
rot <- qrfactor(csv, var = var, rotation = TRUE)
rot$rotation             # "varimax"
rot$unrotated.r.loading  # the unrotated loadings, same as mod0$r.loading

\donttest{
# per-cluster ANOVA / Kruskal-Wallis tables are printed only on request
plot(mod0, type = "cluster", verbose = TRUE)

# --- spatial workflow on the bundled shapefile (needs 'sf' and 'sp') ---
if (requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("sp", quietly = TRUE)) {
  src   <- system.file("external", package = "qrfactor")
  layer <- "Africanfreshwater"
  mod1  <- qrfactor(src, layer, var = var)
  plot(mod1, rowname = "COUNTRY")

  # join a CSV table to the shapefile on a common key
  mod3 <- qrfactor(src, layer, var = var, m = "COUNTRY", f = csv)
  plot(mod3, rowname = "COUNTRY", cex = c("means"),
       legend = "topleft", values = c("cluster"), pch = 23)

  plot(mod1, type = "map")   # choropleth cascade of the factor indices
}
}
}
\references{Bivand, R. S., Pebesma, E. J., Gomez-Rubio, V. (2008) Applied Spatial Data Analysis with R. Springer
Kabacoff, I. R. (2011) R in Action. Data Analysis and Graphics with R. Manning Publications Co
Kaiser, H. F. (1958) The varimax criterion for analytic rotation in factor analysis. Psychometrika 23(3):187-200
}
\keyword{Factor Analysis}