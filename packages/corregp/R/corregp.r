#' Functions and Methods for Correspondence Regression
#'
#' This package provides functions and methods for performing correspondence regression, i.e. the correspondence analysis of the
#'   crosstabulation of a categorical variable Y in function of another one X, where X can in turn be made up of the combination of various
#'   categorical variables.  
#'   Consequently, correspondence regression can be used to analyze the effects for a polytomous or multinomial outcome variable.  
#'   The central function in the package is \code{\link{corregp}}, which enables methods for printing, summarizing and plotting the output.  
#'   Additionally, there are functions for computing confidence intervals, ellipses or 3D ellipsoids (by means of bootstrapping).  
#' @section Contents:
#' This package consists of the following datasets, functions, generics and methods (some internal functions are no longer exported in version 2):  
#' \subsection{Datasets}{
#'   \itemize{
#'     \item{\code{\link{HairEye}} }{Hair and eye color of statistics students (data frame).}
#'     \item{\code{\link{COMURE}} }{The use of linguistic variants in translations vs. non-translations and in six different registers.}
#'     \item{\code{\link{AVT}} }{The use of linguistic variants in audio-visual translation (subtitles).}
#'     \item{\code{\link{TSS}}} {The use of inflected or uninflected determiners in vernacular Belgian Dutch.}
#'   }
#' }
#' \subsection{Functions}{
#'   \itemize{
#'     \item{\code{\link{ci}} }{A helper function to compute confidence intervals on the basis of a numeric vector.}
#'     \item{\code{\link{corregp}} }{The basic function to perform correspondence regression. Typically, one starts here, and then one uses \code{print}, \code{summary}, \code{anova}, \code{screeplot} or \code{plot} methods.}
#'     \item{\code{\link{corregplicate}} }{A function for repeated correspondence regressions with bootstrapping in order to handle large data sets.}
#'   }
#' }
#' \subsection{Generics}{
#'   \itemize{
#'     \item{\code{\link{cint}} }{Compute confidence intervals.}
#'     \item{\code{\link{cell}} }{Compute confidence ellipses.}
#'     \item{\code{\link{cell3d}} }{Compute 3D confidence ellipsoids.}
#'     \item{\code{\link{ciplot}} }{Plot confidence intervals.}
#'     \item{\code{\link{pcplot}} }{Plot parallel coordinates.}
#'     \item{\code{\link{agplot}} }{Plot an association graph.}
#'     \item{\code{\link{plotag}} }{Plot an association graph.}
#'   }
#' }
#' \subsection{Methods}{
#'   \itemize{
#'     \item{\code{\link{print.corregp}} }{Print the output of a correspondence regression.}
#'     \item{\code{\link{summary.corregp}} }{Give a \code{summary} of a correspondence regression.}
#'     \item{\code{\link{print.summary.corregp}} }{Print the \code{summary} of a correspondence regression.}
#'     \item{\code{\link{screeplot.corregp}} }{Make a scree plot on the basis of the output of a correspondence regression.}
#'     \item{\code{\link{anova.corregp}} }{Give an \code{anova} table on the basis of a correspondence regression.}
#'     \item{\code{\link{print.anova.corregp}} }{Print an \code{anova} table on the basis of a correspondence regression.}
#'     \item{\code{\link{coef.corregp}} }{Give the coefficients on the basis of a correspondence regression.}
#'     \item{\code{\link{coefficients.corregp}} }{Give the coefficients on the basis of a correspondence regression.}
#'     \item{\code{\link{fitted.corregp}} }{Give the fitted values on the basis of a correspondence regression.}
#'     \item{\code{\link{fitted.values.corregp}} }{Give the fitted values on the basis of a correspondence regression.}
#'     \item{\code{\link{residuals.corregp}} }{Give the residuals on the basis of a correspondence regression.}
#'     \item{\code{\link{resid.corregp}} }{Give the residuals on the basis of a correspondence regression.}
#'     \item{\code{\link{cint.corregp}} }{Compute confidence intervals on the basis of the output of a correspondence regression. Typically, this function is not so much used directly as it is called by a \code{ciplot.corregp} command.}
#'     \item{\code{\link{ciplot.corregp}} }{Plot confidence intervals on the basis of the output of a correspondence regression.}
#'     \item{\code{\link{pcplot.corregp}} }{Make a parallel coordinate plot on the basis of the output of a correspondence regression.}
#'     \item{\code{\link{cell.corregp}} }{Compute confidence ellipses on the basis of the output of a correspondence regression. Typically, this function is not so much used directly as it is called by a \code{plot.corregp} command.}
#'     \item{\code{\link{plot.corregp}} }{Plot the output (and the optional confidence ellipses) of a correspondence regression.}
#'     \item{\code{\link{cell3d.corregp}} }{Compute 3D confidence ellipsoids on the basis of a correspondence regression. Typically, this function is not so much used directly as it is called by a \code{plot3d.corregp} command.}
#'     \item{\code{\link{plot3d.corregp}} }{Plot the 3D output (and the optional confidence ellipsoids) of a correspondence regression.}
#'     \item{\code{\link{agplot.corregp}} }{Make an association graph on the basis of the output of a correspondence regression.}
#'     \item{\code{\link{plotag.corregp}} }{Make an association graph on the basis of the output of a correspondence regression.}
#'   }
#' }
#' 
#' @section Future prospects:
#' \itemize{
#'   \item Specify a \code{predict} method for a.o. supplementary points.
#'   \item Specify a \code{plot} method for an \code{anova} table.
#'   \item Enable scale transformations for all plots (and corresponding confidence regions).
#'   \item Provide the possibility for so-called "calibration lines".
#' }
#'
#' @section Author:
#' Koen Plevoets, \email{koen.plevoets@@ugent.be}  
#'
#' @section Acknowledgements:
#' This package has benefited greatly from the helpful comments of Isabelle Delaere and Gert De Sutter. Thanks to Kurt Hornik and Uwe Ligges for proofing this package.  
#' @docType package
#' @name corregp-package
NULL

#' @import data.table
NULL

#' @import graphics
NULL

#' @import rgl
NULL

#' @import utils
NULL

#' @import stats
NULL

#' @importFrom stats anova
NULL

#' @importFrom ellipse ellipse
NULL

#' @importFrom gplots barplot2
NULL

#' @importFrom gplots plotCI
NULL

#' @importFrom diagram openplotmat
NULL

#' @importFrom diagram coordinates
NULL

#' @importFrom diagram straightarrow
NULL

#' @importFrom diagram textellipse
NULL

#' @importFrom diagram textrect
NULL

utils::globalVariables(c("B_", "D_chi", "D_num", "E_den", "E_num", "F_", "F_t_s", "F_x_s", "F_x_t", "F_y_s", "F_y_t", "N_"))

#' Hair and Eye Color of Statistics Students (Data Frame)
#'
#' The distribution of hair color, eye color and sex among 592 statistics students (from Snee 1974 and Friendly 1992).  
#' @format A data frame with 592 rows and 3 variables.
#' @source This is simply a data frame version of the in-built data set \code{\link[datasets]{HairEyeColor}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' haireye.crg
#' summary(haireye.crg, parm = "b", add_ci = TRUE)
#' screeplot(haireye.crg, add_ci = TRUE)
#' anova(haireye.crg, nf = 2)
#' plot(haireye.crg, x_ell = TRUE, xsub = c("Hair", "Sex"))
#' }
#' @docType data
#' @name HairEye
NULL

#' The Use of Linguistic Variants in Translations vs. Non-translations and in Six Different Registers
#'
#' This data set was a case study in the COMURE project ("\strong{co}rpus-based, \strong{mu}ltivariate research of \strong{re}gister variation in translated and
#'   non-translated Belgian Dutch") which was conducted at the Department of Translation, Interpreting and Communication of Ghent University between 2010 and 2014.  
#' @format A data frame with 3762 rows and 5 variables.
#' \itemize{
#'   \item{\code{Variant} }{The linguistic variant used in a set of alternatives (27 levels).}
#'   \item{\code{Variable} }{The linguistic variable specifying a set of alternatives (13 levels).}
#'   \item{\code{Variety} }{The dichotomization of \code{Variant} into standard and non-standard.}
#'   \item{\code{Register} }{The register or "Text type" of the data (6 levels).}
#'   \item{\code{Language} }{The language (and source language) of the data (3 levels).}
#' }
#' @source
#' Delaere, I., G. De Sutter and K. Plevoets (2012) Is translated language more standardized than non-translated language? \emph{Target} \strong{24} (2), 203--224.
#' @examples
#' \donttest{
#' data(COMURE)
#' # The execution of corregp may be slow, due to bootstrapping:  
#' comure.crg <- corregp(Variant ~ Register * Language, data = COMURE, part = "Variable", b = 3000)
#' comure.crg
#' summary(comure.crg, parm = "b", add_ci = TRUE)
#' screeplot(comure.crg, add_ci = TRUE)
#' anova(comure.crg, nf = 2)
#' comure.col <- ifelse( xtabs(~ Variant + Variety, data = COMURE)[, "Standard"] > 0, "blue", "red")
#' plot(comure.crg, x_ell = TRUE, xsub = c("Register", "Language"), col_btm = comure.col, 
#'   col_top = "black")
#' }
#' @docType data
#' @name COMURE
NULL

#' The Use of Linguistic Variants in Audio-Visual Translation (Subtitles)
#'
#' This data set was a follow-up study to the \code{\link{COMURE}} project and was conducted at the Department of Translation, Interpreting and Communication of
#'   Ghent University between 2014 and 2018.  
#' @format A data frame with 3302 rows and 7 variables.
#' \itemize{
#'   \item{\code{Variant} }{The linguistic variant used in a set of alternatives (27 levels).}
#'   \item{\code{Variable} }{The linguistic variable specifying a set of alternatives (13 levels).}
#'   \item{\code{Variety} }{The dichotomization of \code{Variant} into standard and non-standard.}
#'   \item{\code{Speaker} }{The role of the speaker in the data (2 levels).}
#'   \item{\code{Language} }{The language (and source language) of the data (3 levels).}
#'   \item{\code{Language2} }{The same as \code{Language} but with the observations of level \code{intra.nl} set to \code{NA}.}
#'   \item{\code{Genre} }{The genre or register of the data (2 levels).}
#' }
#' @source
#' Prieels, L., I. Delaere, K. Plevoets and G. De Sutter (2015) A corpus-based multivariate analysis of linguistic norm-adherence in audiovisual and written
#'   translation. \emph{Across Languages and Cultures} \strong{16} (2), 209--231.
#' @examples
#' \donttest{
#' data(AVT)
#' # The execution of corregp may be slow, due to bootstrapping:  
#' avt.crg <- corregp(Variant ~ Speaker * Language * Genre, data = AVT, part = "Variable", b = 3000)
#' avt.crg
#' summary(avt.crg, parm = "b", add_ci = TRUE)
#' screeplot(avt.crg, add_ci = TRUE)
#' anova(avt.crg, nf = 2)
#' avt.col <- ifelse( xtabs(~ Variant + Variety, data = AVT)[, "Standard"] > 0, "blue", "red")
#' plot(avt.crg, x_ell = TRUE, xsub = c("Speaker", "Language", "Genre"), col_btm = avt.col, 
#'   col_top = "black")
#' }
#' @docType data
#' @name AVT
NULL

#' The Use of Inflected or Uninflected Determiners in the Belgian Dutch Vernacular
#'
#' The distribution of the Belgian Dutch \emph{-e(n)}-suffix with 14 determiners in 14 registers and for several speaker characteristics.  
#' @format A data frame with 40778 rows and 13 variables.
#' \itemize{
#'   \item{\code{Variant} }{The linguistic variant used in a set of alternatives (35 levels).}
#'   \item{\code{Variable} }{The linguistic variable specifying a set of alternatives (14 levels).}
#'   \item{\code{Inflected} }{Numeric variable specifying whether the linguistic variant is inflected (\code{1}) or not (\code{0}).}
#'   \item{\code{Register} }{The register of the data in the Spoken Dutch Corpus (14 levels: see 
#'     \href{http://lands.let.ru.nl/cgn/doc_English/topics/version_1.0/overview.htm}{here} for their definition).}
#'   \item{\code{Register2} }{The dichotomization of \code{Register} into private and public.}
#'   \item{\code{SpeakerID} }{The ID of the speaker in the Spoken Dutch Corpus (1144 levels).}
#'   \item{\code{Region} }{The region in which the speaker lived until the age of 18 (4 levels).}
#'   \item{\code{Sex} }{The sex of the speaker (2 levels).}
#'   \item{\code{BirthYear} }{The year in which the speaker was born (63 levels).}
#'   \item{\code{Decade} }{The decade in which the speaker was born (7 levels).}
#'   \item{\code{Generation} }{The generation cohort in which the speaker was born (5 levels).}
#'   \item{\code{Education} }{The level of education of the speaker (3 levels).}
#'   \item{\code{Occupation} }{The level of occupation of the speaker (10 levels: see 
#'     \href{http://lands.let.ru.nl/cgn/doc_English/topics/version_1.0/metadata/speakers.htm}{here} for their definition).}
#' }
#' @source
#' Plevoets, K. (2008) \emph{Tussen spreek- en standaardtaal}. Leuven, Doctoral dissertation. Available online \href{https://biblio.ugent.be/publication/1168055/file/1168056}{here}.
#' @examples
#' \donttest{
#' data(TSS)
#' # The execution of corregp may be slow, due to bootstrapping:  
#' tss.crg <- corregp(Variant ~ Register2 * Region, data = TSS, part = "Variable", b = 3000)
#' tss.crg
#' summary(tss.crg, parm = "b", add_ci = TRUE)
#' screeplot(tss.crg, add_ci = TRUE)
#' anova(tss.crg, nf = 2)
#' tss.col <- ifelse( xtabs(~ Variant + Inflected, data = TSS)[, 1] > 0, "blue", "red")
#' plot(tss.crg, x_ell = TRUE, xsub = c("Register2", "Region"), col_btm = tss.col, col_top = "black")
#' }
#' @docType data
#' @name TSS
NULL

#' Correspondence Regression
#'
#' This is the basic function for \emph{correspondence regression}, i.e. the correspondence analysis of a contingency table formed
#'   by the categorical variables Y and X, where X can be in turn made up of the combinations of various categorical variables.  
#' @param formula A \code{\link[stats]{formula}} specification of which factors to cross with each other. The left-hand (\code{y}) side must be a single factor.
#'   The right-hand side (\code{x}) can involve all the usual specifications of interactions and/or nested analyses.
#' @param data The data frame containing the variables specified in the \code{formula}.
#' @param part Character vector specifying the names of conditional factors (e.g. a factor partioning the levels of the left-hand side \code{y} into groups).
#'   This argument is relevant for analyses in which one wants to remove between-item variation.
#' @param b Number of the bootstrap replications (simulations). If \code{0} (i.e. the default), then the analysis is exploratory.
#' @param xep Logical specifying whether to output the separate terms in the right-hand side (\code{x}) as components in a list.
#'   If \code{FALSE}, then all \code{x} output is collected in a matrix.
#' @param std Logical specifying whether to output the standardized coordinates. Defaults to \code{FALSE}.
#' @param rel Logical specifying whether to divide the coordinates by the \code{sqrt} of their totals, so that one obtains coordinates for
#'   the relative frequencies (as is customary in correspondence analysis). Defaults to \code{TRUE}.
#' @param phi Logical specifying whether to compute the output on the scale of the \emph{Chi-squared} value of the contingency table or of the \emph{Phi-squared} value
#'   (which is \emph{Chi-squared} divided by \emph{N}). Reminiscent of \code{\link[MASS]{corresp}} in package \pkg{MASS}, defaults to \code{FALSE}.
#' @param chr Character specifying the separator string for constructing the interaction terms.
#' @param b_scheme Character specifying the sampling scheme for bootstrapping. Must match either \code{"multinomial"} (the default) or \code{"product-multinomial"}.
#' @details
#' Correspondence regression rests on the idea, described by Gilula and Haberman (1988), of using a correspondence analysis to model a polytomous or multinomial (i.e.
#'   'multi-category') response variable (\code{Y}) in terms of other (possibly interacting) factors (\code{X}) (see also 3.2 in Van der Heijden et al. 1989). These are
#'   specified in the argument \code{formula}, which can be constructed in all the usual ways of specifying a model formula: e.g.
#' \itemize{
#'   \item \code{Y ~ X1 + X2 + X1 : X2} or \code{Y ~ X1 * X2}
#'   \item \code{Y ~ (X1 + X2 + X3) ^ 2}
#'   \item \code{Y ~ X1 * X2 * X3 - X1 : X2 : X3}
#'   \item \ldots
#' }
#' Correspondence regression then crosstabulates the \code{Y} factor with all the combinations in \code{X}, thus producing a typical contingency table, on which a simple
#'   correspondence analysis is performed (see Greenacre 2017: 121-128 for the outline of this approach). The more general effects in \code{X} are obtained by aggregating
#'   the combinations.  
#'
#' Correspondence regression also allows for inferential validation of the effects, which is done by means of the bootstrap (in fact, Monte Carlo simulation). Setting the argument
#'   \code{b} to a number \eqn{> 0}, \code{b} replicates of the contingency table are generated with multinomial sampling. From these, \code{b} new values are derived for the
#'   coordinates in both \code{Y} and \code{X} as well as for the eigenvalues (also called the "principal inertias").  On the basis of the replicate/simulated values,
#'   confidence intervals, ellipses or ellipsoids can be computed.  CAUTION: bootstrapping/simulation is computationally quite intensive, so it can take a while to reach
#'   results, especially with a large \code{b}.  
#'
#' The argument \code{parm} can be used when one wants to perform a correspondence regression of \code{Y} onto \code{X} conditional on other factors. These conditioning factors are
#'   therefore equivalent to \emph{random factors}, and \code{corregp} always conditions on the joint variable of all the specified factors. One such use of conditioning factors is
#'   a so-called \emph{lectometric} analysis in linguistics, where the levels of \code{Y} are grouped/partitioned/nested into clusters and one wants to exclude the heterogeneity
#'   between the clusters.  
#' @return An object of class "corregp", i.e. a list with components:
#' \item{\code{eigen} }{A vector of eigenvalues of the correpondence regression.}
#' \item{\code{y} }{The coordinates (matrix) of the Y levels.}
#' \item{\code{x} }{The coordinates of the X levels. If \code{xep} is \code{TRUE}, then this is a list with a component for each term name.}
#' \item{\code{freq} }{A list of the frequencies of every Y and X level.}
#' \item{\code{conf} }{If \eqn{b>0}. A list of bootstrap replicates for the eigenvalues, the coordinates of Y levels, the coordinates of X levels and the frequencies of both the Y levels and the X levels.}
#' \item{\code{aux} }{A list of auxiliary information (such as the U and V matrices of the SVD, the specified values for all the arguments) to be passed to other functions and methods.}
#' @references
#' Gilula, Z. and S.J. Haberman (1988) The analysis of multivariate contingency tables by restricted canonical and restricted association models.
#'   \emph{Journal of the American Statistical Association} \strong{83} (403), 760--771.
#'
#' Greenacre, M. (2017) \emph{Correspondence analysis in practice, Third edition}. Boca Raton: Chapman and Hall/CRC.
#'
#' Van der Heijden, P.G.M., A. de Falguerolles and J. de Leeuw (1989) A combined approach to contingency table analysis using correspondence analysis and log-linear analysis.
#'   \emph{Applied Statistics} \strong{38} (2), 249--292.
#' @seealso \code{\link{print.corregp}}, \code{\link{summary.corregp}}, \code{\link{screeplot.corregp}}, \code{\link{anova.corregp}}, \code{\link{plot.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' haireye.crg
#' }
#' @export
corregp <- function(formula,data,part=NULL,b=0,xep=TRUE,std=FALSE,rel=TRUE,phi=FALSE,chr=".",b_scheme="multinomial") {
	f.var <- all.vars(formula,functions=FALSE,unique=TRUE)
	x.var <- f.var[-1]
	y.var <- f.var[1]
	if (!is.null(part) && is.na(part)) {
		part <- NULL
		}
	f.dat <- data.table::data.table(data[,c(x.var,y.var,part)])
	f.trm <- strsplit(labels(terms(formula,keep.order=FALSE)),split=":")
	chr <- chr[1]
	names(f.trm) <- sapply(f.trm,paste,collapse=chr)
	y.aux <- f.dat[,list(N_=.N),keyby=c(y.var,part)]
	x.aux <- f.dat[,list(N_=.N),keyby=c(x.var,part)]
	t.aux <- lapply(f.trm,function(t01){f.dat[,list(N_=.N),keyby=t01]})
	f.tab <- f.dat[,list(N_=as.double(.N)),by=names(f.dat)]
	if (is.null(part)) {
		f.tab <- f.tab[
			merge(data.table::data.table(x.aux[,x.var,with=FALSE],Tmp_=1),data.table::data.table(y.aux[,y.var,with=FALSE],Tmp_=1),by="Tmp_",allow.cartesian=TRUE)[,-"Tmp_",with=FALSE],
			on=c(x.var,y.var)][is.na(N_), N_:=0]
		}
	else {
		f.tab <- f.tab[merge(x.aux[,c(x.var,part),with=FALSE],y.aux[,c(y.var,part),with=FALSE],by=part,allow.cartesian=TRUE), on=c(x.var,y.var,part)][is.na(N_), N_:=0]
		y.aux <- y.aux[,list(N_=sum(N_)),by=y.var]
		x.aux <- x.aux[,list(N_=sum(N_)),by=x.var]
		}
	n.tot <- f.tab[,sum(N_)]
	b <- ifelse(is.null(b)||is.na(b),0,b)
	if (b>0) {
		b.sch <- pmatch(tolower(b_scheme),c("multinomial","product-multinomial","product_multinomial"))
		if (b.sch==1) {
			f.rep <- matrix(rmultinom(b,n.tot,f.tab$N_/n.tot),ncol=b,dimnames=list(NULL,paste("B",1:b,sep="")))
			storage.mode(f.rep) <- "double"
			f.tab <- data.table::melt(cbind(f.tab,f.rep), id.vars=c(x.var,y.var,part), measure.vars=c("N_",paste("B",1:b,sep="")), variable.name="B_", value.name="F_")
			}
		if (b.sch>1) {
			f.tab <- rbind(
				f.tab[,B_:="N_"],
				f.tab[,c( lapply(.SD,function(j){rep(j,times=b)}), list( B_=paste("B",rep(1:b,each=length(N_)),sep=""), N_=as.double(rmultinom(b,sum(N_),N_)) ) ),
					by=c(x.var,part),.SDcols=y.var]
				)[,F_:=N_]
			}
		if (is.na(b.sch)) {
			warning("argument 'b_scheme' has to match either 'multinomial' or 'product-multinomial', so no bootstrapping was done",call.=FALSE)
			f.tab[,c("B_","F_") := list("N_",as.double(N_))]
			}
		}
	else {
		f.tab[,c("B_","F_") := list("N_",as.double(N_))]
		}
	data.table::setkeyv(f.tab,c("B_",x.var,y.var,part))
	f.tab[,F_x_s := sum(F_), by=c("B_",x.var,part)]
	f.tab[,F_y_s := sum(F_), by=c("B_",y.var,part)]
	f.tab[,F_t_s := sum(F_), by=c("B_",part)]
	f.tab[,F_x_t := sum(F_), by=c("B_",x.var)]
	f.tab[,F_y_t := sum(F_), by=c("B_",y.var)]
	f.tab[,E_num := F_x_s*F_y_s/F_t_s]
	f.tab[,E_den := F_x_t*F_y_t]
	f.tab[,D_num := F_-E_num]
	f.tab[,D_chi := D_num/sqrt(E_den)]
	f.tab[!is.finite(D_chi), D_chi:=0]
	n.rot <- ifelse(phi[1],1,sqrt(n.tot))
	f.chi <- n.rot*as.matrix(data.table::dcast(f.tab[B_=="N_"],formula(paste(paste(x.var,collapse="+"),"~",y.var,sep="")),fun.aggregate=sum,value.var="D_chi")[,-x.var,with=FALSE])
	f.svd <- svd(f.chi)
	s.svd <- f.svd$d[f.svd$d != 0]
	r.ank <- 1:length(s.svd)
	u.svd <- f.svd$u[,r.ank]
	v.svd <- f.svd$v[,r.ank]
	f.tab <- merge(f.tab,
		data.table::data.table(x.aux[,x.var,with=FALSE], matrix(u.svd,nrow=nrow(u.svd),dimnames=list(NULL,paste("U_",r.ank,sep="")))),
		by.x=x.var, by.y=x.var, all.x=TRUE, sort=FALSE)
	f.tab <- merge(f.tab,
		data.table::data.table(y.aux[,y.var,with=FALSE], matrix(v.svd,nrow=nrow(v.svd),dimnames=list(NULL,paste("V_",r.ank,sep="")))),
		by.x=y.var, by.y=y.var, all.x=TRUE, sort=FALSE)
	f.tab[,paste("S_",r.ank,sep="") := eval(parse(text=paste("list(",paste("U_",r.ank,"*D_chi*V_",r.ank,sep="",collapse=","),")",sep="")))]
	if (std[1]) {
		f.tab[,paste("U_",r.ank,sep="") := eval(parse(text=paste("list(",paste(paste("U_",r.ank,sep=""),paste("/s.svd[",r.ank,"]",sep=""),sep="",collapse=","),")",sep="")))]
		f.tab[,paste("V_",r.ank,sep="") := eval(parse(text=paste("list(",paste(paste("V_",r.ank,sep=""),paste("/s.svd[",r.ank,"]",sep=""),sep="",collapse=","),")",sep="")))]
		}
	f.tab[,paste("Y_",r.ank,sep="") := eval(parse(text=paste("list(",paste("D_chi*U_",r.ank,sep="",collapse=","),")",sep="")))]
	f.tab[,paste("X_",r.ank,sep="") := eval(parse(text=paste("list(",paste("D_num*V_",r.ank,sep="",collapse=","),")",sep="")))]
	if (rel[1]) {
		f.tab[,paste("W_",y.var,sep="") := sqrt(1/sum(F_)),by=c("B_",y.var)]
		for (t02 in f.trm) {
			f.tab[,paste("W_",paste(t02,collapse=chr),sep="") := sqrt(1/F_y_t)*(1/sum(F_)),by=c("B_",t02)]
			}
		}
	else {
		f.tab[,paste("W_",y.var,sep="") := 1]
		for (t02 in f.trm) {
			f.tab[,paste("W_",paste(t02,collapse=chr),sep="") := sqrt(1/(F_y_t*sum(F_))),by=c("B_",t02)]
			}
		}
	y.lab <- levels(y.aux[,eval(parse(text=y.var))])
	x.lab <- lapply(f.trm,function(t03){apply(t.aux[[paste(t03,collapse=chr)]][,t03,with=FALSE],1,paste,collapse=chr)})
	y.loc <- n.rot*matrix(do.call(cbind, 
			f.tab[B_=="N_",eval(parse(text=paste("list(",paste("sum(W_",y.var,"*Y_",r.ank,",na.rm=TRUE)",sep="",collapse=","),")",sep=""))),
			keyby=y.var][,-y.var,with=FALSE]),
		ncol=length(r.ank), dimnames=list(y.lab,r.ank))
	x.loc <- lapply(f.trm,function(t04){
		n.rot*matrix(do.call(cbind,
			f.tab[B_=="N_", eval(parse(text=paste("list(",paste("sum(W_",paste(t04,collapse=chr),"*X_",r.ank,",na.rm=TRUE)",sep="",collapse=","),")",sep=""))),
			keyby=c(t04)][,-t04,with=FALSE]),
		ncol=length(r.ank), dimnames=list(x.lab[[paste(t04,collapse=chr)]],r.ank))
		})
	y.frq <- y.aux[,N_]
	names(y.frq) <- y.lab
	x.frq <- lapply(f.trm,function(t05){t.aux[[paste(t05,collapse=chr)]][,N_]})
	for (t06 in names(f.trm)) {
		names(x.frq[[t06]]) <- x.lab[[t06]]
		}
	xep <- ifelse(length(x.var)==1,FALSE,xep[1])
	if (!xep) {
		x.loc <- do.call(rbind,x.loc)
		names(x.frq) <- NULL
		x.frq <- do.call("c",x.frq)
		}
	f.out <- list(eigen=s.svd^2,y=y.loc,x=x.loc,freq=list(y=y.frq,x=x.frq))
	if (b>0) {
		y.con <- sapply(y.lab,function(yl){
			yl=n.rot*matrix(do.call(cbind,
				f.tab[B_!="N_" & eval(parse(text=paste(y.var,"==\"",yl,"\"",sep=""))),
				eval(parse(text=paste("list(",paste("sum(W_",y.var,"*Y_",r.ank,",na.rm=TRUE)",sep="",collapse=","),")",sep=""))),by="B_"][,-"B_",with=FALSE]),
				ncol=length(r.ank),dimnames=list(NULL,r.ank))
			},simplify=FALSE,USE.NAMES=TRUE)
		x.con <- lapply(f.trm,function(t07){
			sapply(x.lab[[paste(t07,collapse=chr)]],function(xl){
				n.rot*matrix(do.call(cbind,
					f.tab[B_!="N_" & eval(parse(text=paste(t07,paste("\"",unlist(strsplit(xl,split=chr,fixed=TRUE)),"\"",sep=""),sep="==",collapse="&"))),
					eval(parse(text=paste("list(",paste("sum(W_",paste(t07,collapse=chr),"*X_",r.ank,",na.rm=TRUE)",sep="",collapse=","),")",sep=""))),
					by="B_"][,-"B_",with=FALSE]),
				ncol=length(r.ank),dimnames=list(NULL,r.ank))
				},simplify=FALSE,USE.NAMES=TRUE)
			})
		y.rep <- as.matrix(data.table::dcast(f.tab[B_!="N_",c("B_",y.var,"F_"),with=FALSE],formula(paste("B_~",y.var,sep="")),fun.aggregate=sum,value.var="F_")[,-"B_",with=FALSE])
		x.rep <- lapply(f.trm,function(t08){
			as.matrix(data.table::dcast(f.tab[B_!="N_",c("B_",t08,"F_"),with=FALSE],formula(paste("B_~",paste(t08,collapse="+"),sep="")),
				fun.aggregate=sum,sep=chr,value.var="F_")[,-"B_",with=FALSE])
			})
		s.con <- (n.rot*matrix(do.call(cbind,
				f.tab[B_!="N_",eval(parse(text=paste("list(",paste("sum(S_",r.ank,")",sep="",collapse=","),")",sep=""))),by="B_"][,-"B_",with=FALSE]),
			nrow=b,dimnames=list(NULL,r.ank)))^2
		if (!xep) {
			names(x.con) <- NULL
			x.con <- do.call("c",x.con)
			x.rep <- do.call(cbind,x.rep)
			}
		f.out$conf <- list(eigen=s.con,y=y.con,x=x.con,freq=list(y=y.rep,x=x.rep))
		}
	colnames(u.svd) <- r.ank
	colnames(v.svd) <- r.ank
	if (is.null(part)) {
		part <- NA
		}
	f.out$aux <- list(U=u.svd,V=v.svd,formula=formula,data=as.list(match.call())$data,part=part,b=b,std=std,rel=rel,phi=phi,chr=chr)
	class(f.out) <- "corregp"
	f.out
	}

#' Repeated Correspondence Regression
#'
#' A function for repeated correspondence regressions with bootstrapping in order to handle large data sets. This is essentially a wrapper
#'   \code{replicate(n = r, expr = corregp(...), simplify = FALSE)}, so it may dissappear in the future.  
#' @param formula A \code{\link[stats]{formula}} specification of which factors to cross with each other. The left-hand (\code{y}) side must be a single factor.
#'   The right-hand side (\code{x}) can involve all the usual specifications of interactions and/or nested analyses.
#' @param data The data frame containing the variables specified in the \code{formula}.
#' @param part Character vector specifying the names of conditional factors (e.g. a factor partioning the levels of the left-hand side \code{y} into groups).
#'   This argument is relevant for analyses in which one wants to remove between-item variation.
#' @param b Number of the bootstrap replications (simulations).
#' @param r Number of repeated calls to \code{\link{corregp}}.
#' @param xep Logical specifying whether to output the separate terms in the right-hand side (\code{x}) as components in a list.
#'   If \code{FALSE}, then all \code{x} output is collected in a matrix.
#' @param std Logical specifying whether to output the standardized coordinates. Defaults to \code{FALSE}.
#' @param rel Logical specifying whether to divide the coordinates by the \code{sqrt} of their totals, so that one obtains coordinates for
#'   the relative frequencies (as is customary in correspondence analysis). Defaults to \code{TRUE}.
#' @param phi Logical specifying whether to compute the output on the scale of the \emph{Chi-squared} value of the contingency table or of the \emph{Phi-squared} value
#'   (which is \emph{Chi-squared} divided by \emph{N}). Reminiscent of \code{\link[MASS]{corresp}} in package \pkg{MASS}, defaults to \code{FALSE}.
#' @param chr Character specifying the separator string for constructing the interaction terms.
#' @param b_scheme Character specifying the sampling scheme for bootstrapping. Must match either \code{"multinomial"} (the default) or \code{"product-multinomial"}.
#' @return An object of class "corregp" in which the bootstrap replications of all the repeated calls to \code{corregp} are put together.
#' @seealso \code{\link{corregp}}.
#' @export
corregplicate <- function(formula,data,part=NULL,b=100,r=10,xep=TRUE,std=FALSE,rel=TRUE,phi=FALSE,chr=".",b_scheme="multinomial") {
	if (b==0 || r==0) {
		stop("both b and r must be > 0",call.=FALSE)
		}
	r.epl <- replicate(n=r,corregp(formula=formula,data=data,part=part,b=b,xep=xep,std=std,rel=rel,phi=phi,chr=chr,b_scheme=b_scheme),simplify=FALSE)
	r.out <- r.epl[[1]][c("eigen","y","x","freq")]
	s.con <- do.call(rbind,lapply(r.epl,function(r01){r01$conf$eigen}))
	y.lab <- names(r.epl[[1]]$conf$y)
	y.con <- sapply(y.lab,function(yl){
		do.call(rbind,lapply(r.epl,function(r02){r02$conf$y[[yl]]}))
		},simplify=FALSE,USE.NAMES=TRUE)
	y.rep <- do.call(rbind,lapply(r.epl,function(r03){r03$conf$freq$y}))
	if (xep) {
		x.lab <- sapply(r.epl[[1]]$conf$x,function(x){names(x)},simplify=FALSE,USE.NAMES=TRUE)
		x.con <- sapply(names(x.lab),function(tl){
			sapply(x.lab[[tl]],function(xl){
				do.call(rbind,lapply(r.epl,function(r04){r04$conf$x[[tl]][[xl]]}))
				},simplify=FALSE,USE.NAMES=TRUE)
			},simplify=FALSE,USE.NAMES=TRUE)
		x.rep <- sapply(names(x.lab),function(tl){
			do.call(rbind,lapply(r.epl,function(r05){r05$conf$freq$x[[tl]]}))
			},simplify=FALSE,USE.NAMES=TRUE)
		}
	else {
		x.lab <- names(r.epl[[1]]$conf$x)
		x.con <- sapply(x.lab,function(xl){
			do.call(rbind,lapply(r.epl,function(r04){r04$conf$x[[xl]]}))
			},simplify=FALSE,USE.NAMES=TRUE)
		x.rep <- do.call(rbind,lapply(r.epl,function(r05){r05$conf$freq$x}))
		}
	r.out$conf <- list(eigen=s.con,y=y.con,x=x.con,freq=list(y=y.rep,x=x.rep))
	r.out$aux <- r.epl[[1]]$aux
	class(r.out) <- "corregp"
	r.out
	}

#' Printing Correspondence Regression
#'
#' Method to print the output of \code{\link{corregp}}.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param nf The number of dimensions to print. Defaults to the first two dimensions.
#' @param ... Further arguments passed to or from other methods.
#' @return The output of a call to \code{\link{corregp}}.
#' @seealso \code{\link{corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' haireye.crg
#' print(haireye.crg, nf = 3)
#' }
#' @export
print.corregp <- function(x,nf=2,...) {
	crg <- x
	o.rnk <- sum(crg$eigen>1e-08,na.rm=TRUE)
	nf <- ifelse(is.null(nf),o.rnk,nf)
	nf <- ifelse(is.character(nf),match(nf,table=colnames(crg$y)),nf)[1]
	if(nf>o.rnk || is.na(nf)) {
		nf <- o.rnk
		if (o.rnk>1) {
			warning(paste(as.list(match.call())$x,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
			}
		}
	f.var <- all.vars(crg$aux$formula,functions=FALSE,unique=TRUE)
	cat("Correspondence regression of ",format(crg$aux$formula)," in ",crg$aux$data,"\n",sep="",fill=FALSE)
	if (all(!is.na(crg$aux$part))) {
		cat("Conditioning factor(s): ",crg$aux$part,"\n",sep="  ",fill=FALSE)
		}
	cat("\nEigenvalues: ",crg$eigen[1:nf],"\n",sep=" ",fill=FALSE)
	cat("\nY (",f.var[1],"):\n",sep="",fill=FALSE)
	print(crg$y[,1:nf],...)
	if (is.list(crg$x)) {
		p.var <- names(crg$x)
		cat("\nX:\n",sep="",fill=FALSE)
		for (p in 1:length(crg$x)) {
			cat(p.var[p],":\n",sep="",fill=FALSE)
			print(crg$x[[p]][,1:nf],...)
			cat("\n",sep="",fill=FALSE)
			}
		}
	else {
		cat("\nX",ifelse(length(f.var)==2,paste(" (",f.var[2],")",sep="",collapse=NULL),""),":\n",sep="",fill=FALSE)
		print(crg$x[,1:nf],...)
		cat("\n",sep="",fill=FALSE)
		}
	invisible()
	}

#' Summarizing Correspondence Regression
#'
#' Method to produce a summary of a correspondence regression.  
#' @param object The outout of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to compute the contributions \code{contrib}. Can be either \code{"y"} for the Y contributions, \code{"x"} for the X contributions, \code{"both"}
#'   which can be abbreviated to \code{"b"}, or a vector of term names in X. Defaults to \code{"b"}.
#' @param contrib The type of contributions to be computed: either \emph{from points to axes} (absolute contributions) or \emph{from axes to points} (squared correlations).
#'   The specification can be \code{"pnts_to_axes"} or \code{"axes_to_pnts"}, \code{"pts2axs"} or \code{"axs2pts"}, \code{"p_a"} or \code{"a_p"}, or any other reasonable abbreviation.
#' @param nf The number of dimensions to be retained in the reduced space. Defaults to all dimensions (no reduction).
#' @param add_ci Logical specifying whether to compute confidence intervals for the eigenvalues (and eigenvalues only). Defaults to \code{FALSE}.
#' @param cl The confidence level for the confidence intervals. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence intervals.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence intervals are computed directly with the \code{\link[stats]{quantile}} function.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class "summary.corregp", providing a summary of a correspondence regression, i.e. a list with components:
#' \item{\code{formula} }{The \code{\link[stats]{formula}} specified to the \code{formula} argument in the call to \code{corregp}.}
#' \item{\code{data} }{The name of the data frame specified to the \code{data} argument in the call to \code{corregp}.}
#' \item{\code{part} }{The name of the factor specified to the \code{part} argument in the call to \code{corregp}.}
#' \item{\code{chi_squared} }{The chi-squared value of the correspondence regression.}
#' \item{\code{phi_squared} }{The phi-squared value of the correspondence regression, i.e. the chi-squared value divided by \code{N}.}
#' \item{\code{N} }{The total number of observations.}
#' \item{\code{eigen} }{Depending on \code{add_ci}: if \code{FALSE}, a matrix of the actual eigenvalues, their percentages and cumulative percentages; if \code{TRUE}, a list of the actual eigenvalues, their percentages and cumulative percentages together with the lower and upper confidence limits for each.}
#' \item{\code{y} }{If \code{parm} is \code{"y"} or \code{"b"}. A list of components \code{p_a} for the absolute contributions and//or \code{a_p} for the squared correlations, depending on \code{contrib}.}
#' \item{\code{x} }{If \code{parm} is \code{"y"}, \code{"b"} or any of the term names in X. A list of components \code{p_a} for the absolute contributions and/or \code{a_p} for the squared correlations, depending in \code{contrib}.}
#' @seealso \code{\link{corregp}}, \code{\link{print.summary.corregp}}, \code{\link{anova.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' summary(haireye.crg, add_ci = TRUE)
#' summary(haireye.crg, parm = "y", contrib = "pts_axs", nf = 2)
#' }
#' @export
summary.corregp <- function(object,parm=NULL,contrib=NULL,nf=NULL,add_ci=FALSE,cl=0.95,nq=TRUE,...) {
	crg <- object
	if (is.null(parm) && !is.null(contrib)) {
		parm <- "b"
		}
	if (!is.null(parm) && is.null(contrib)) {
		contrib <- "b"
		}
	parm <- ifelse(is.null(parm),NA,parm)
	contrib <- tolower(contrib)
	contrib <- ifelse(contrib %in% c("p_a","pts_axs","pts2axs","ptstoaxs","pts_to_axs","pnts_axes","pnts2axes","pntstoaxes","pnts_to_axes"),"p_a",contrib)
	contrib <- ifelse(contrib %in% c("a_p","axs_pts","axs2pts","axstopts","axs_to_pts","axes_pnts","axes2pnts","axestopnts","axes_to_pnts"),"a_p",contrib)
	o.eig <- crg$eigen
	o.rnk <- sum(o.eig>1e-08,na.rm=TRUE)
	nf <- ifelse(is.null(nf),o.rnk,nf)
	nf <- ifelse(is.character(nf),match(nf,table=colnames(crg$y)),nf)[1]
	if(nf>o.rnk || is.na(nf)) {
		nf <- o.rnk
		warning(paste(as.list(match.call())$object,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
		}
	o.tot <- sum(crg$freq$y)
	o.phi <- ifelse(crg$aux$phi,o.tot,1)
	s.out <- list(formula=crg$aux$formula,data=crg$aux$data,part=crg$aux$part,chi_squared=sum(o.eig)*o.phi,phi_squared=sum(o.eig)*o.phi/o.tot,N=o.tot)
	o.lab <- colnames(crg$y[,1:nf])
	if (add_ci && is.null(crg$conf)) {
		add_ci <- FALSE
		warning(paste("no bootstrapping was done in",as.list(match.call())$object,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (add_ci) {
		s.out$eigen$value <- as.matrix(rbind(o.eig[1:nf],apply(matrix(crg$conf$eigen[,1:nf],ncol=nf),2,ci,cl=cl,nq=nq)))
		s.out$eigen$"%" <- as.matrix(rbind(o.eig[1:nf]/sum(o.eig),apply(sweep(matrix(crg$conf$eigen[,1:nf],ncol=nf),1,apply(crg$conf$eigen,1,sum),"/"),2,ci,cl=cl,nq=nq)))
		s.out$eigen$"cum_%" <- as.matrix(rbind(cumsum(o.eig[1:nf])/sum(o.eig),apply(sweep(matrix(apply(matrix(crg$conf$eigen[,1:nf],ncol=nf),1,cumsum),ncol=nf),1,apply(crg$conf$eigen,1,sum),"/"),2,ci,cl=cl,nq=nq)))
		dimnames(s.out$eigen$value) <- list(c("","lower","upper"),o.lab)
		dimnames(s.out$eigen$"%") <- list(c("","lower","upper"),o.lab)
		dimnames(s.out$eigen$"cum_%") <- list(c("","lower","upper"),o.lab)
		}
	else {
		s.out$eigen <- as.matrix(rbind(o.eig[1:nf],o.eig[1:nf]/sum(o.eig),cumsum(o.eig[1:nf])/sum(o.eig)),ncol=nf)
		dimnames(s.out$eigen) <- list(c("value","%","cum_%"),o.lab)
		}
	if (!is.na(parm)) {
		if (crg$aux$std) {
			o.std <- rep(1,times=length(o.eig))
			}
		else {
			o.std <- o.eig
			}
		}
	if (parm %in% c("y","b","both")) {
		if (contrib %in% c("p_a","b","both")) {
			if (crg$aux$rel) {
				y.rel <- crg$freq$y
				}
			else {
				y.rel <- rep(1,times=nrow(crg$y))
				}
			s.out$y$p_a <- sweep(sweep(matrix(crg$y[,1:nf]^2,ncol=nf,dimnames=list(rownames(crg$y),1:nf)),1,y.rel,"*"),2,o.std[1:nf],"/")
			}
		if (contrib %in% c("a_p","b","both")) {
			s.out$y$a_p <- sweep(sweep(matrix(crg$y[,1:nf]^2,ncol=nf,dimnames=list(rownames(crg$y),1:nf)),2,(o.eig/o.std)[1:nf],"*"),1,apply(sweep(crg$y^2,2,(o.eig/o.std),"*"),1,sum),"/")
			}
		}
	if (!is.list(crg$x) && parm %in% c("x","b","both")) {
		if (contrib %in% c("p_a","b","both")) {
			if (crg$aux$rel) {
				x.rel <- crg$freq$x
				}
			else {
				x.rel <- rep(1,times=nrow(crg$x))
				}
			s.out$x$p_a <- sweep(sweep(matrix(crg$x[,1:nf]^2,ncol=nf,dimnames=list(rownames(crg$x),1:nf)),1,x.rel,"*"),2,o.std[1:nf],"/")
			}
		if (contrib %in% c("a_p","b","both")) {
			s.out$x$a_p <- sweep(sweep(matrix(crg$x[,1:nf]^2,ncol=nf,dimnames=list(rownames(crg$x),1:nf)),2,(o.eig/o.std)[1:nf],"*"),1,apply(sweep(crg$x^2,2,(o.eig/o.std),"*"),1,sum),"/")
			}
		}
	if (is.list(crg$x) && all(parm %in% c("x","b","both",names(crg$x)))) {
		if (parm %in% c("x","b","both")) {
			parm <- names(crg$x)
			}
		if (contrib %in% c("p_a","b","both")) {
			if (crg$aux$rel) {
				x.rel <- crg$freq$x[parm]
				}
			else {
				x.rel <- lapply(crg$x[parm],function(p1){rep(1,times=nrow(p1))})
				}
			s.out$x$p_a <- lapply(parm,function(p2){
				sweep(sweep(matrix(crg$x[[p2]][,1:nf]^2,ncol=nf,dimnames=list(rownames(crg$x[[p2]]),1:nf)),1,x.rel[[p2]],"*"),2,o.std[1:nf],"/")
				})
			names(s.out$x$p_a) <- parm
			}
		if (contrib %in% c("a_p","b","both")) {
			s.out$x$a_p <- lapply(parm,function(p3){
				sweep(sweep(matrix(crg$x[[p3]][,1:nf]^2,ncol=nf,dimnames=list(rownames(crg$x[[p3]]),1:nf)),2,(o.eig/o.std)[1:nf],"*"),1,apply(sweep(crg$x[[p3]]^2,2,(o.eig/o.std),"*"),1,sum),"/")
				})
			names(s.out$x$a_p) <- parm
			}
		}
	class(s.out) <- "summary.corregp"
	s.out
	}


#' Printing the Summary of Correspondence Regression
#'
#' Method to print the output of \code{\link{summary.corregp}}.  
#' @param x The output of a call to \code{summary} on a "corregp" object (i.e. an object of class "summary.corregp").
#' @param ... Further arguments passed to or from other methods.
#' @return The output of a call to \code{summary} on a "corregp" object. The eigenvalues and contributions are printed with \code{TOTAL}s.
#' @seealso \code{\link{summary.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' summary(haireye.crg, add_ci = TRUE)
#' summary(haireye.crg, parm = "y", contrib = "pts_axs", nf = 2)
#' }
#' @export
print.summary.corregp <- function(x,...) {
	crs <- x
	f.var <- all.vars(crs$formula,functions=FALSE,unique=TRUE)
	cat("Summary of correspondence regression of ",format(crs$formula)," in ",crs$data,"\n",sep="",fill=FALSE)
	if (all(!is.na(crs$part))) {
		cat("Conditioning factor(s): ",crs$part,"\n",sep="  ",fill=FALSE)
		}
	cat("\nChi-squared: ",crs$chi_squared,"\nPhi-squared: ",crs$phi_squared,"\nN: ",crs$N,"\n",sep="",fill=FALSE)
	cat("\n\nEigenvalues:\n",sep="",fill=FALSE)
	if (is.list(crs$eigen)) {
		cat(" Value:\n",sep="",fill=FALSE)
		print(cbind(crs$eigen$value,TOTAL=c(sum(crs$eigen$value[1,]),NA,NA)),na.print="",...)
		cat("\n Percentage (%):\n",sep="",fill=FALSE)
		print(cbind(crs$eigen$"%",TOTAL=c(sum(crs$eigen$"%"[1,]),NA,NA)),na.print="",...)
		cat("\n Cumulative percentage (cum_%):\n",sep="",fill=FALSE)
		print(cbind(crs$eigen$"cum_%",TOTAL=c(sum(crs$eigen$"%"[1,]),NA,NA)),na.print="",...)
		cat("\n",sep="",fill=FALSE)
		}
	else {
		print(cbind(crs$eigen,TOTAL=c(apply(as.matrix(crs$eigen[1:2,],ncol=ncol(crs$eigen)),1,sum),sum(crs$eigen[2,]))),...)
		cat("\n",sep="",fill=FALSE)
		}
	if ("y" %in% names(crs) || "x" %in% names(crs)) {
		cat("\nContributions:\n\n",sep="",fill=FALSE)
		}
	if ("y" %in% names(crs)) {
		cat("Y (",f.var[1],"):\n",sep="",fill=FALSE)
		if ("p_a" %in% names(crs$y)) {
			cat(" Points to axes (Absolute contributions):\n",sep="",fill=FALSE)
			print(rbind(crs$y$p_a,TOTAL=apply(crs$y$p_a,2,sum)),...)
			cat("\n",sep="",fill=FALSE)
			}
		if ("a_p" %in% names(crs$y)) {
			cat(" Axes to points (Squared correlations):\n",sep="",fill=FALSE)
			print(cbind(crs$y$a_p,TOTAL=apply(crs$y$a_p,1,sum)),...)
			cat("\n",sep="",fill=FALSE)
			}
		}
	if ("x" %in% names(crs)) {
		cat("X",ifelse(length(f.var)==2,paste(" (",f.var[2],")",sep="",collapse=NULL),""),":\n",sep="",fill=FALSE)
		if ("p_a" %in% names(crs$x)) {
			cat(" Points to axes (Absolute contributions):\n",sep="",fill=FALSE)
			if (is.list(crs$x$p_a)) {
				p.var <- names(crs$x$p_a)
				for (p1 in 1:length(p.var)) {
					cat("\n   ",p.var[p1],":\n",sep="",fill=FALSE)
					print(rbind(crs$x$p_a[[p1]],TOTAL=apply(as.matrix(crs$x$p_a[[p1]][complete.cases(crs$x$p_a[[p1]]),],ncol=ncol(crs$x$p_a[[p1]])),2,sum)),...)
					}
				cat("\n",sep="",fill=FALSE)
				}
			else {
				print(rbind(crs$x$p_a,TOTAL=apply(as.matrix(crs$x$p_a[complete.cases(crs$x$p_a),],ncol=ncol(crs$x$p_a)),2,sum)),...)
				cat("\n",sep="",fill=FALSE)
				}
			}
		if ("a_p" %in% names(crs$x)) {
			cat(" Axes to points (Squared correlations):\n",sep="",fill=FALSE)
			if (is.list(crs$x$a_p)) {
				p.var <- names(crs$x$a_p)
				for (p2 in 1:length(p.var)) {
					cat("\n   ",p.var[p2],":\n",sep="",fill=FALSE)
					print(cbind(crs$x$a_p[[p2]],TOTAL=apply(as.matrix(crs$x$a_p[[p2]],ncol=ncol(crs$x$a_p[[p2]])),1,sum)),...)
					}
				cat("\n",sep="",fill=FALSE)
				}
			else {
				print(cbind(crs$x$a_p,TOTAL=apply(as.matrix(crs$x$a_p,ncol=ncol(crs$x$a_p)),1,sum)),...)
				cat("\n",sep="",fill=FALSE)
				}
			}
		}
	invisible()
	}

#' Confidence Interval
#'
#' This is the basic function for computing a confidence interval on the basis of a sample of data values.  
#' @param x A numeric vector.
#' @param cl The confidence level for the confidence interval. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence interval.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence interval is computed directly with the \code{\link[stats]{quantile}} function.
#' @return A vector with two components \code{Lower} and \code{Upper} giving the lower and upper confidence limits respectively.
#' @seealso \code{\link{ciplot.corregp}}, \code{\link{anova.corregp}}, \code{\link{agplot.corregp}}, \code{\link[stats]{confint}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' ci(haireye.crg$conf$eigen[, 1])
#' ci(haireye.crg$conf$eigen[, 2])
#' }
#' @export
ci <- function(x,cl=0.95,nq=TRUE) {
	x <- x[complete.cases(x)]
	if (nq) {
		avg <- mean(x)
		s.e <- sd(x)*abs(qnorm((1-cl)/2))
		out <- c(Lower=avg-s.e,Upper=avg+s.e)
		}
	else {
		out <- c(Lower=quantile(x,probs=(1-cl)/2,names=FALSE),Upper=quantile(x,probs=1-(1-cl)/2,names=FALSE))
		}
	return(ifelse(!is.na(out),out,c(Lower=NA,Upper=NA)))
	}

#' Scree Plotting
#'
#' Method to produce a \emph{scree plot}, i.e. a bar chart of the eigenvalues.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param type A character specification  of which type of values to plot: either \code{"value"} for the \emph{actual eigenvalues}, \code{"\%"} for \emph{percentages} or
#'   \code{"cum_\%"} for \emph{cumulative percentages}. Defaults to \code{"value"}.
#' @param add_ci Logical specifying whether to include the confidence intervals. Defaults to \code{FALSE}.
#' @param cl The confidence level for the confidence intervals. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence intervals.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence intervals are computed directly with the \code{\link[stats]{quantile}} function.
#' @param ... Further arguments passed to or from other methods.
#' @details \code{screeplot} (of a \code{corregp} output) makes use of \code{\link[gplots]{barplot2}} from the package \pkg{gplots}.  
#' @return A plot window containing the scree plot.
#' @seealso \code{\link{corregp}}, \code{\link{summary.corregp}}, \code{\link{anova.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' screeplot(haireye.crg, add_ci = TRUE)
#' }
#' @export
screeplot.corregp <- function(x,type="value",add_ci=FALSE,cl=0.95,nq=TRUE,...) {
	crg <- x
	if (add_ci && is.null(crg$conf)) {
		add_ci <- FALSE
		warning(paste("no bootstrapping was done in",as.list(match.call())$x,sep=" ",collapse=NULL),call.=FALSE)
		}
	type <- tolower(type)
	if (add_ci) {
		r.int <- summary(object=crg,parm=NULL,contrib=NULL,nf=NULL,add_ci=TRUE,cl=cl,nq=nq)$eigen[[type]]
		r.val <- r.int[1,]
		}
	else {
		r.val <- summary(object=crg,parm=NULL,contrib=NULL,nf=NULL,add_ci=FALSE)$eigen[type,]
		}
	gplots::barplot2(r.val,plot.ci=add_ci,ci.l=r.int[2,],ci.u=r.int[3,],...)
	invisible()
	}

#' Building an ANOVA Table for Correspondence Regression
#'
#' Method to construct an ANOVA table for correspondence regression, i.e. a table with the Chi-squared deviation for each term in the \code{formula} of the
#'   \code{\link{corregp}} call (or of each individual level in X in case \code{xep = FALSE}).  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param nf The number of dimensions to be retained in the reduced space. Defaults to all dimensions (no reduction).
#' @param cl The confidence level for the confidence intervals. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence interval.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence interval is computed directly with the \code{\link[stats]{quantile}} function.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' If \code{object} was made with bootstrap replications, then \code{anova.corregp} will automatically compute confidence intervals for the Chi-squared
#'   deviations by means of the \code{\link{ci}} function.  
#' @return A matrix with the Chi-squared deviations for all the terms in the \code{formula} of \code{object}, based on the selected number of dimensions. If
#'   \code{object} was made with the argument \code{xep = FALSE}, then the output contains the Chi-squared deviation for every individual level in X.
#' @seealso \code{\link{print.anova.corregp}}, \code{\link{ci}}, \code{\link{summary.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' anova(haireye.crg, nf = 2)
#' }
#' @export
anova.corregp <- function(object,nf=NULL,cl=0.95,nq=TRUE,...) {
	crg <- object
	o.rnk <- sum(crg$eigen>1e-08,na.rm=TRUE)
	nf <- ifelse(is.null(nf),o.rnk,nf)
	nf <- ifelse(is.character(nf),match(nf,table=colnames(crg$y)),nf)[1]
	if(nf>o.rnk || is.na(nf)) {
		nf <- o.rnk
		warning(paste(as.list(match.call())$object,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
		}
	if (crg$aux$std) {
		o.std <- crg$eigen[1:nf]
		}
	else {
		o.std <- rep(1,times=nf)
		}
	if (crg$aux$rel) {
		o.rel <- crg$freq$x
		if (!is.null(crg$conf)) {
			c.rel <- crg$conf$freq$x
			}
		}
	else {
		if (is.list(crg$x)) {
			o.rel <- lapply(crg$x,function(p1){rep(1,times=nrow(p1))})
			if (!is.null(crg$conf)) {
				c.rel <- lapply(crg$x,function(p2){matrix(1,nrow=crg$aux$b,ncol=nrow(p2))})
				}
			}
		else {
			o.rel <- rep(1,times=nrow(crg$x))
			if (!is.null(crg$conf)) {
				c.rel <- matrix(1,nrow=crg$aux$b,ncol=nrow(crg$x))
				}
			}
		}
	o.phi <- ifelse(!crg$aux$phi,1,sum(crg$freq$y))
	if (is.list(crg$x)) {
		o.trm <- labels(terms(crg$aux$formula,keep.order=FALSE))
		o.cmb <- lapply(strsplit(o.trm,split=":"),function(p3){
			unlist(lapply(1:length(p3),function(l3){
				match(utils::combn(p3,l3,paste,collapse=":"),table=o.trm)*rep((-1)^l3,times=choose(length(p3),l3))
				}))*(-1)^length(p3)
			})
		o.cmb <- lapply(1:length(o.cmb),function(p4){o.cmb[[p4]][!is.na(o.cmb[[p4]])]})
		a.out <- o.phi*do.call(what=rbind,args=lapply(1:length(o.cmb),function(p5){
			sum(unlist(lapply(names(crg$x[abs(o.cmb[[p5]])]),function(o5){
				sum(sweep(sweep(matrix(crg$x[[o5]][,1:nf]^2,ncol=nf),2,o.std,"*"),1,o.rel[[o5]],"*"),na.rm=TRUE)
				}))*sign(o.cmb[[p5]]),na.rm=TRUE)
			}))
		dimnames(a.out) <- list(names(crg$x),"X^2")
		if (!is.null(crg$conf)) {
			c.ssq <- lapply(crg$conf$x,function(p6){
				do.call(what=cbind,args=lapply(1:length(p6),function(o6){
					apply(sweep(matrix(p6[[o6]][,1:nf]^2,ncol=nf),2,o.std,"*"),1,sum)
					}))
				})
			c.csq <- o.phi*do.call(what=cbind,args=lapply(names(crg$x),function(p7){apply(c.ssq[[p7]]*c.rel[[p7]],1,sum,na.rm=TRUE)}))
			c.int <- do.call(what=rbind,args=lapply(o.cmb,function(p8){
				ci(apply(sweep(matrix(c.csq[,abs(p8)],ncol=length(p8)),2,sign(p8),"*"),1,sum),cl=cl,nq=nq)
				}))
			a.out <- cbind(a.out,c.int)
			}
		}
	else {
		a.out <- matrix(o.phi*apply(sweep(sweep(matrix(crg$x[,1:nf]^2,ncol=nf),2,o.std,"*"),1,o.rel,"*"),1,sum),ncol=1)
		dimnames(a.out) <- list(rownames(crg$x),"X^2")
		if (!is.null(crg$conf)) {
			c.int <- t(apply(o.phi*do.call(what=cbind,args=lapply(crg$conf$x,function(p9){
				apply(sweep(matrix(p9[,1:nf]^2,ncol=nf),2,o.std,"*"),1,sum)
				}))*c.rel,2,ci,cl=cl,nq=nq))
			a.out <- cbind(a.out,c.int)
			}
		}
	class(a.out) <- "anova.corregp"
	a.out
	}

#' Printing the ANOVA Table of Correspondence Regression
#'
#' Method to print the output of \code{\link{anova.corregp}}.  
#' @param x The output of a call to \code{anova} on a "corregp" object (i.e. an object of class "anova.corregp").
#' @param ... Further arguments passed to or from other methods.
#' @return The output of a call to \code{anova} on a "corregp" object.
#' @seealso \code{\link{anova.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' anova(haireye.crg, nf = 2)
#' }
#' @export
print.anova.corregp <- function(x,...) {
	cra <- x
	cat("ANOVA Table\n(Type III Tests)\n\n",sep="",fill=FALSE)
	print(unclass(cra),...)
	cat("\n",sep="",fill=FALSE)
	invisible()
	}

#' Extracting Coefficients from Correspondence Regression
#'
#' Method to extract the coefficients (i.e. scores) of a correspondence regression.  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to extract the coefficients. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names in X or
#'   level names in Y. Defaults to \code{"x"}.
#' @param axes The axes for which to extract the coefficients: a vector of indices. Defaults to all the axes.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The coefficients in correspondence regression are the same as the coordinate scores.  
#' @return A matrix or vector with coefficients (i.e. scores) for the parameters and axes of interest.
#' @seealso \code{\link{fitted.corregp}}, \code{\link{residuals.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' coef(haireye.crg, parm = c("Hair", "Sex"), axes = 1:2)
#' coefficients(haireye.crg, parm = c("Hair", "Sex"), axes = 1:2)
#' }
#' @export
coef.corregp <- function(object,parm="x",axes=NULL,...) {
	crg <- object
	o.rnk <- sum(crg$eigen>1e-08,na.rm=TRUE)
	if (is.null(axes)) {
		axes <- 1:o.rnk
		}
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))
		}
	if (any(axes>o.rnk)) {
		axes <- 1:o.rnk
		warning(paste(as.list(match.call())$object,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
		}
	out <- NULL
	if (length(parm)==1 && tolower(parm)=="x") {
		if (is.list(crg$x)) {
			out <- do.call(what=rbind,args=crg$x)[,axes]
			}
		else {
			out <- crg$x[,axes]
			}
		}
	if (is.list(crg$x) && all(parm %in% names(crg$x))) {
		out <- do.call(what=rbind,args=crg$x[parm])[,axes]
		}
	if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
		out <- do.call(what=rbind,args=crg$x)[parm,axes]
		}
	if (all(parm %in% rownames(crg$x))) {
		out <- crg$x[parm,axes]
		}
	if (all(parm %in% rownames(crg$y))) {
		out <- crg$y[parm,axes]
		}
	if (length(parm)==1 && tolower(parm)=="y") {
		out <- crg$y[,axes]
		}
	out
	}

#' @rdname coef.corregp
#' @export
coefficients.corregp <- function(object,parm="x",axes=NULL,...) {
	coef(object,parm,axes,...)
	}

#' Extracting Fitted Values from Correspondence Regression
#'
#' Method to extract the fitted values of a correspondence regression.  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to extract the fitted values. Can be \code{"all"}, \code{"both"} (or abbreviations), \code{"y"} or \code{"x"} for
#'   the fitted values of every cell in the data, but it can also be any vector of term names in X or level names in X. Defaults to \code{"all"}.
#' @param nf The number of dimensions to be retained in the reduced space. Defaults to all dimensions (no reduction).
#' @param ... Further arguments passed to or from other methods.
#' @details
#' If all dimensions are retained, then the fitted values will only be equal to the observed counts if no conditioning factors were specified with the argument
#'   \code{"part"} in the \code{\link{corregp}} call. This is because the associations with the conditioning factors (in \code{"part"}) are not taken into
#'   account.  
#' @return A matrix or vector with the fitted values for the parameters of interest, based on the selected number of dimensions.
#' @seealso \code{\link{coef.corregp}}, \code{\link{residuals.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' fitted(haireye.crg, parm = c("Hair", "Sex"), nf = 2)
#' fitted.values(haireye.crg, parm = c("Hair", "Sex"), nf = 2)
#' }
#' @export
fitted.corregp <- function(object,parm="all",nf=NULL,...) {
	crg <- object
	o.rnk <- sum(crg$eigen>1e-08,na.rm=TRUE)
	nf <- ifelse(is.null(nf),o.rnk,nf)
	nf <- ifelse(is.character(nf),match(nf,table=colnames(crg$y)),nf)[1]
	if(nf>o.rnk || is.na(nf)) {
		nf <- o.rnk
		warning(paste(as.list(match.call())$object,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
		}
	if (crg$aux$std) {
		o.std <- sqrt(crg$eigen[1:nf])
		}
	else {
		o.std <- 1/sqrt(crg$eigen[1:nf])
		}
	x.mat <- NULL
	if (crg$aux$rel) {
		y.rel <- crg$freq$y
		if (nf!=1) {
			y.mat <- sweep(crg$y[,1:nf],1,y.rel,"*")
			}
		else {
			y.mat <- matrix(y.rel*crg$y[,1],ncol=1)
			}
		if (length(parm)==1 && tolower(parm) %in% c("x","y","a","b","all","both")) {
			if (is.list(crg$x)) {
				x.frq <- crg$freq$x
				names(x.frq) <- NULL
				x.rel <- unlist(x.frq)
				if (nf!=1) {
					x.mat <- sweep(do.call(what=rbind,args=crg$x)[,1:nf],1,x.rel,"*")
					}
				else {
					x.mat <- matrix(x.rel*do.call(what=rbind,args=crg$x)[,1],ncol=1)
					}
				}
			else {
				x.rel <- crg$freq$x
				if (nf!=1) {
					x.mat <- sweep(crg$x[,1:nf],1,x.rel,"*")
					}
				else {
					x.mat <- matrix(x.rel*crg$x[,1],ncol=1)
					}
				}
			}
		if (is.list(crg$x) && all(parm %in% names(crg$x))) {
			x.frq <- crg$freq$x[parm]
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)
			if (nf!=1) {
				x.mat <- sweep(do.call(what=rbind,args=crg$x[parm])[,1:nf],1,x.rel,"*")
				}
			else {
				x.mat <- matrix(x.rel*do.call(what=rbind,args=crg$x[parm])[,1],ncol=1)
				}
			}
		if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
			x.frq <- crg$freq$x
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)[parm]
			if (nf!=1) {
				x.mat <- sweep(matrix(do.call(what=rbind,args=crg$x)[parm,1:nf],ncol=nf),1,x.rel,"*")
				}
			else {
				x.mat <- matrix(x.rel*do.call(what=rbind,args=crg$x)[parm,1],ncol=1)
				}
			}
		if (all(parm %in% rownames(crg$x))) {
			x.rel <- crg$freq$x[parm]
			if (nf!=1) {
				x.mat <- sweep(matrix(crg$x[parm,1:nf],ncol=nf),1,x.rel,"*")
				}
			else {
				x.mat <- matrix(x.rel*crg$x[parm,1],ncol=1)
				}
			}
		}
	else {
		y.rel <- crg$freq$y
		if (nf!=1) {
			y.mat <- sweep(crg$y[,1:nf],1,sqrt(y.rel),"*")
			}
		else {
			y.mat <- matrix(sqrt(y.rel)*crg$y[,1],ncol=1)
			}
		if (length(parm)==1 && tolower(parm) %in% c("x","y","a","b","all","both")) {
			if (is.list(crg$x)) {
				x.frq <- crg$freq$x
				names(x.frq) <- NULL
				x.rel <- unlist(x.frq)
				if (nf!=1) {
					x.mat <- sweep(do.call(what=rbind,args=crg$x)[,1:nf],1,sqrt(x.rel),"*")
					}
				else {
					x.mat <- matrix(sqrt(x.rel)*do.call(what=rbind,args=crg$x)[,1],ncol=1)
					}
				}
			else {
				x.rel <- crg$freq$x
				if (nf!=1) {
					x.mat <- sweep(crg$x[,1:nf],1,sqrt(x.rel),"*")
					}
				else {
					x.mat <- matrix(sqrt(x.rel)*crg$x[,1],ncol=1)
					}
				}
			}
		if (is.list(crg$x) && all(parm %in% names(crg$x))) {
			x.frq <- crg$freq$x[parm]
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)
			if (nf!=1) {
				x.mat <- sweep(do.call(what=rbind,args=crg$x[parm])[,1:nf],1,sqrt(x.rel),"*")
				}
			else {
				x.mat <- matrix(sqrt(x.rel)*do.call(what=rbind,args=crg$x[parm])[,1],ncol=1)
				}
			}
		if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
			x.frq <- crg$freq$x
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)[parm]
			if (nf!=1) {
				x.mat <- sweep(matrix(do.call(what=rbind,args=crg$x)[parm,1:nf],ncol=nf),1,sqrt(x.rel),"*")
				}
			else {
				x.mat <- matrix(sqrt(x.rel)*do.call(what=rbind,args=crg$x)[parm,1],ncol=1)
				}
			}
		if (all(parm %in% rownames(crg$x))) {
			x.rel <- crg$freq$x[parm]
			if (nf!=1) {
				x.mat <- sweep(matrix(crg$x[parm,1:nf],ncol=nf),1,sqrt(x.rel),"*")
				}
			else {
				x.mat <- matrix(sqrt(x.rel)*crg$x[parm,1],ncol=1)
				}
			}
		}
	if (!is.null(x.mat)) {
		o.tot <- sum(crg$freq$y)
		out <- outer(x.rel,y.rel/o.tot)+sqrt(ifelse(crg$aux$phi,o.tot,1)/o.tot)*x.mat%*%diag(o.std,nrow=nf)%*%t(y.mat)
		}
	else {
		out <- NULL
		}
	out
	}

#' @rdname fitted.corregp
#' @method fitted.values corregp
#' @export
fitted.values.corregp <- function(object,parm="all",nf=NULL,...) {
	fitted(object,parm,nf,...)
	}

#' Extracting Residuals from Correspondence Regression
#'
#' Method to extract the residuals of a correspondence regression.  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to extract the residuals. Can be \code{"all"}, \code{"both"} (or abbreviations), \code{"y"} or \code{"x"} for
#'   the residuals of every cell in the data, but it can also be any vector of term names in X or level names in X. Defaults to \code{"all"}.
#' @param nf The number of dimensions to be retained in the reduced space. Defaults to all dimensions (no reduction).
#' @param ... Further arguments passed to or from other methods.
#' @details
#' If all dimensions are retained, then the residuals will only be exactly zero to the observed counts if no conditioning factors were specified with the argument
#'   \code{"part"} in the \code{\link{corregp}} call. This is because the associations with the conditioning factors (in \code{"part"}) are not taken into
#'   account.  
#' @return A matrix or vector with the residuals for the parameters of interest, based on the selected number of dimensions.
#' @seealso \code{\link{coef.corregp}}, \code{\link{fitted.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' residuals(haireye.crg, parm = c("Hair", "Sex"), nf = 2)
#' resid(haireye.crg, parm = c("Hair", "Sex"), nf = 2)
#' }
#' @export
residuals.corregp <- function(object,parm="all",nf=NULL,...) {
	crg <- object
	o.rnk <- sum(crg$eigen>1e-08,na.rm=TRUE)
	nf <- ifelse(is.null(nf),o.rnk,nf)
	nf <- ifelse(is.character(nf),match(nf,table=colnames(crg$y)),nf)[1]
	if(nf>o.rnk || is.na(nf)) {
		nf <- o.rnk
		warning(paste(as.list(match.call())$object,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
		}
	if (nf<o.rnk) {
		if (crg$aux$std) {
			o.std <- sqrt(crg$eigen[-(1:nf)])
			}
		else {
			o.std <- 1/sqrt(crg$eigen[-(1:nf)])
			}
		}
	else {
		o.std <- 0
		}
	x.mat <- NULL
	if (crg$aux$rel) {
		if (nf<o.rnk) {
			y.mat <- sweep(crg$y[,-(1:nf)],1,crg$freq$y,"*")
			}
		else {
			y.mat <- matrix(rep(0,times=nrow(crg$y)),ncol=1,dimnames=list(rownames(crg$y),NULL))
			}
		if (length(parm)==1 && tolower(parm) %in% c("x","y","a","b","all","both")) {
			if (is.list(crg$x)) {
				x.frq <- crg$freq$x
				names(x.frq) <- NULL
				x.rel <- unlist(x.frq)
				if (nf<o.rnk) {
					x.mat <- sweep(do.call(what=rbind,args=crg$x)[,-(1:nf)],1,x.rel,"*")
					}
				else {
					x.mat <- matrix(rep(0,times=length(x.rel)),ncol=1,dimnames=list(names(x.rel),NULL))
					}
				}
			else {
				x.rel <- crg$freq$x
				if (nf<o.rnk) {
					x.mat <- sweep(crg$x[,-(1:nf)],1,x.rel,"*")
					}
				else {
					x.mat <- matrix(rep(0,times=length(x.rel)),ncol=1,dimnames=list(names(x.rel),NULL))
					}
				}
			}
		if (is.list(crg$x) && all(parm %in% names(crg$x))) {
			x.frq <- crg$freq$x[parm]
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)
			if (nf<o.rnk) {
				x.mat <- sweep(do.call(what=rbind,args=crg$x[parm])[,-(1:nf)],1,x.rel,"*")
				}
			else {
				x.mat <- matrix(rep(0,times=length(x.rel)),ncol=1,dimnames=list(names(x.rel),NULL))
				}
			}
		if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
			x.frq <- crg$freq$x
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)[parm]
			if (nf<o.rnk) {
				x.mat <- sweep(matrix(do.call(what=rbind,args=crg$x)[parm,-(1:nf)],nrow=length(parm),dimnames=list(parm,NULL)),1,x.rel,"*")
				}
			else {
				x.mat <- matrix(rep(0,times=length(parm)),ncol=1,dimnames=list(parm,NULL))
				}
			}
		if (all(parm %in% rownames(crg$x))) {
			x.rel <- crg$freq$x[parm]
			if (nf<o.rnk) {
				x.mat <- sweep(matrix(crg$x[parm,-(1:nf)],nrow=length(parm),dimnames=list(parm,NULL)),1,x.rel,"*")
				}
			else {
				x.mat <- matrix(rep(0,times=length(parm)),ncol=1,dimnames=list(parm,NULL))
				}
			}
		}
	else {
		if (nf<o.rnk) {
			y.mat <- sweep(crg$y[,-(1:nf)],1,sqrt(crg$freq$y),"*")
			}
		else {
			y.mat <- matrix(rep(0,times=nrow(crg$y)),ncol=1,dimnames=list(rownames(crg$y),NULL))
			}
		if (length(parm)==1 && tolower(parm) %in% c("x","y","a","b","all","both")) {
			if (is.list(crg$x)) {
				x.frq <- crg$freq$x
				names(x.frq) <- NULL
				x.rel <- unlist(x.frq)
				if (nf<o.rnk) {
					x.mat <- sweep(do.call(what=rbind,args=crg$x)[,-(1:nf)],1,sqrt(x.rel),"*")
					}
				else {
					x.mat <- matrix(rep(0,times=length(x.rel)),ncol=1,dimnames=list(names(x.rel),NULL))
					}
				}
			else {
				x.rel <- crg$freq$x
				if (nf<o.rnk) {
					x.mat <- sweep(crg$x[,-(1:nf)],1,sqrt(x.rel),"*")
					}
				else {
					x.mat <- matrix(rep(0,times=length(x.rel)),ncol=1,dimnames=list(names(x.rel),NULL))
					}
				}
			}
		if (is.list(crg$x) && all(parm %in% names(crg$x))) {
			x.frq <- crg$freq$x[parm]
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)
			if (nf<o.rnk) {
				x.mat <- sweep(do.call(what=rbind,args=crg$x[parm])[,-(1:nf)],1,sqrt(x.rel),"*")
				}
			else {
				x.mat <- matrix(rep(0,times=length(x.rel)),ncol=1,dimnames=list(names(x.rel),NULL))
				}
			}
		if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
			x.frq <- crg$freq$x
			names(x.frq) <- NULL
			x.rel <- unlist(x.frq)[parm]
			if (nf<o.rnk) {
				x.mat <- sweep(matrix(do.call(what=rbind,args=crg$x)[parm,-(1:nf)],nrow=length(parm),dimnames=list(parm,NULL)),1,sqrt(x.rel),"*")
				}
			else {
				x.mat <- matrix(rep(0,times=length(parm)),ncol=1,dimnames=list(parm,NULL))
				}
			}
		if (all(parm %in% rownames(crg$x))) {
			x.rel <- crg$freq$x[parm]
			if (nf<o.rnk) {
				x.mat <- sweep(matrix(crg$x[parm,-(1:nf)],nrow=length(parm),dimnames=list(parm,NULL)),1,sqrt(x.rel),"*")
				}
			else {
				x.mat <- matrix(rep(0,times=length(parm)),ncol=1,dimnames=list(parm,NULL))
				}
			}
		}
	if (!is.null(x.mat)) {
		o.tot <- sum(crg$freq$y)
		out <- sqrt(ifelse(crg$aux$phi,o.tot,1)/o.tot)*x.mat%*%diag(o.std,nrow=length(o.std))%*%t(y.mat)
		}
	else {
		out <- NULL
		}
	out
	}

#' @rdname residuals.corregp
#' @export
resid.corregp <- function(object,parm="all",nf=NULL,...) {
	residuals(object,parm,nf,...)
	}

#' Getting \code{conf} Components from \code{corregp} Objects
#'
#' Internal function for retrieving the \code{conf} component(s) in a \code{corregp} object.  
#' @param crg The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to retrieve the \code{conf} components. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names
#'   in X or level names in Y.
#' @details
#' \code{confGet} is an internal function to be called by \code{\link{cint.corregp}}, \code{\link{cell.corregp}} or \code{\link{cell3d.corregp}}, but not by users.  
#' @return A list of components selected with \code{parm}.
confGet <- function(crg,parm) {
	stopifnot(class(crg)=="corregp")
	c.out <- NULL
	if (length(parm)==1 && tolower(parm)=="x") {
			if(is.list(crg$x)) {
				c.out <- do.call(what=c,args=crg$conf$x)
				names(c.out) <- unlist(lapply(crg$x,rownames))
				}
			else {
				c.out <- crg$conf$x
				}
		}
	if (is.list(crg$x) && all(parm %in% names(crg$x))) {
		c.out <- do.call(what=c,args=crg$conf$x[parm])
		names(c.out) <- unlist(lapply(crg$x[parm],rownames))
		}
	if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
		c.out <- do.call(what=c,args=crg$conf$x)
		names(c.out) <- unlist(lapply(crg$x,rownames))
		c.out <- c.out[parm]
		}
	if (all(parm %in% rownames(crg$x))) {
		c.out <- crg$conf$x[parm]
		}
	if (all(parm %in% rownames(crg$y))) {
		c.out <- crg$conf$y[parm]
		}
	if (length(parm)==1 && tolower(parm)=="y") {
		c.out <- crg$conf$y
		}
	c.out
	}

#' Confidence Intervals for Correspondence Regression
#'
#' Method to compute confidence intervals for coordinates in correspondence regression.  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to compute the confidence intervals. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names in X or
#'   level names in Y. Defaults to \code{"x"}.
#' @param axis The axis for which to compute the confidence intervals.
#' @param cl The confidence level for the confidence interval. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence intervals.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence intervals are computed directly with the \code{\link[stats]{quantile}} function.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' \code{cint} (of a \code{corregp} output) makes use of \code{\link{ci}}.  
#'
#' Typically, \code{cint} is not so much used directly as it is called by a \code{\link{ciplot.corregp}} command.  
#' @return A matrix with \code{Lower} and \code{Upper} confidence limits for the coordinates of interest.
#' @seealso \code{\link{ci}}, \code{\link{ciplot.corregp}}, \code{\link{agplot.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' cint(haireye.crg, parm = "y", axis = 1)
#' cint(haireye.crg, parm = c("Hair", "Sex"), axis = 1)
#' }
#' @export
cint.corregp <- function(object,parm="x",axis,cl=0.95,nq=TRUE,...) {
	crg <- object
	if (is.null(crg$conf)) {
		stop(paste("no bootstrapping was done in",as.list(match.call())$object,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (is.character(axis)) {
		axis <- match(axis,table=colnames(crg$y))[1]
		}
	p.con <- confGet(crg=crg,parm=parm)
	do.call(what=rbind,args=lapply(p.con,function(p){ci(p[,axis],cl=cl,nq=nq)}))
	}

#' @rdname cint.corregp
#' @export
cint <- function(object,...) {
	UseMethod("cint")
	}

#' Plotting Confidence Intervals for Correspondence Regression
#'
#' Method to plot confidence intervals for coordinates in correspondence regression.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to plot the confidence intervals. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names in X or
#'   level names in Y. Defaults to \code{"x"}.
#' @param axis The axis for which to plot the confidence intervals.
#' @param cl The confidence level for the confidence intervals. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence intervals.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence intervals are computed directly with the \code{\link[stats]{quantile}} function.
#' @param horiz Logical specifying whether the confidence intervals should be plotted horizontally or not. Defaults to \code{FALSE}.
#' @param na.rm Logical specifying whether to omit \code{NA} coordinates from the plot. Defaults to \code{FALSE}.
#' @param type The type of plot: see \code{\link[graphics]{plot.default}}. For correspondence regression, there is an additional option \code{"labs"} which
#'   plots the text labels at the centers of the confidence intervals. Defaults to \code{"p"}.
#' @param col Color of the text labels: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param cex Character expansion factor: a number to specify the size of the text labels.
#' @param font Font of the text labels: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic. Defaults to \code{1}.
#' @param family Font family of the text labels: can be \code{"serif"}, \code{"sans"}, \code{"mono"} or one of the \code{\link[grDevices]{Hershey}} fonts.
#' @param alim Vector of two values specifying the lower and upper limit between which to plot the axis.
#' @param adir Reading direction of the text labels on the (horizontal) axis: either a \code{numeric} value between \code{0} and \code{3} (see the \code{las} argument in
#'   the graphical parameters \code{\link[graphics]{par}}) or a \code{character} value matching either \code{"horizontal"} or \code{"vertical"}. Defaults to \code{1} (horizontal).
#' @param ecol Color of the error bars: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param ewid Width of the error bars: a number to specify the line width.
#' @param etyp Line type of the error bars: \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"}, \code{3} or \code{"dotted"},
#'   \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param psym The symbol (or "plotting character") to use for the centers of the confidence intervals.
#' @param pcol Color of the center symbol: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param pcex Character expansion factor of the center symbol.
#' @param pbgc Background color of the center symbol: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param lwd Width of all lines except for the error bars, e.g. the connecting lines: a number to specify the line width.
#' @param lty Line type of all lines except for the error bars, e.g. the connecting lines: \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"},
#'   \code{3} or \code{"dotted"}, \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param sfrac Width of "crossbar" at the end of error bar as a fraction of the x plotting region. Defaults to 0.01.
#' @param gap Space left between the center of the error bar and the lines marking the error bar in units of the height (width) of the letter "O". Defaults to 0.
#' @param main The main title of the plot.
#' @param sub The subtitle of the plot.
#' @param ... Further arguments passed to or from other methods.
#' @details \code{ciplot} (of a \code{corregp} output) makes use of \code{\link[gplots]{plotCI}} from the package \pkg{gplots}.  
#' @return A plot window containing the confidence intervals.
#' @seealso \code{\link{ci}}, \code{\link[gplots]{plotCI}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' ciplot(haireye.crg, parm = "y", axis = 1)
#' ciplot(haireye.crg, parm = c("Hair", "Sex"), axis = 1)
#' }
#' @export
ciplot.corregp <- function(x,parm="x",axis,cl=0.95,nq=TRUE,horiz=FALSE,na.rm=FALSE,type="p",col="darkgrey",cex=par("cex"),font=par("font"),family=par("family"),alim=NULL,adir=1,ecol="darkgrey",ewid=par("lwd"),etyp=par("lty"),psym=16,pcol=par("col"),pcex=cex,pbgc=par("bg"),lwd=ewid,lty=etyp,sfrac=0.01,gap=0,main=NULL,sub=NULL,...) {
	crg <- x
	if (is.null(crg$conf)) {
		stop(paste("no bootstrapping was done in",as.list(match.call())$x,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (type=="labs" && horiz) {
		horiz <- FALSE
		warning("type='labs' will plot the confidence intervals vertically",call.=FALSE)
		}
	if (is.character(axis)) {
		axis <- match(axis,table=colnames(crg$y))[1]
		}
	if (is.character(adir)) {
		adir <- pmatch(tolower(adir),table=c("horizontal","vertical"))
		}
	a.lab <- colnames(crg$y)[axis]
	p.loc <- NULL
	if (length(parm)==1 && tolower(parm)=="x") {
			if(is.list(crg$x)) {
				p.loc <- do.call(what=rbind,args=crg$x)[,axis]
				}
			else {
				p.loc <- crg$x[,axis]
				}
		}
	if (is.list(crg$x) && all(parm %in% names(crg$x))) {
		p.loc <- do.call(what=rbind,args=crg$x[parm])[,axis]
		}
	if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
		p.loc <- do.call(what=rbind,args=crg$x)[parm,axis]
		if (length(p.loc)==1) {
			names(p.loc) <- parm
			}
		}
	if (all(parm %in% rownames(crg$x))) {
		p.loc <- crg$x[parm,axis]
		if (length(p.loc)==1) {
			names(p.loc) <- parm
			}
		}
	if (all(parm %in% rownames(crg$y))) {
		p.loc <- crg$y[parm,axis]
		if (length(p.loc)==1) {
			names(p.loc) <- parm
			}
		}
	if (length(parm)==1 && tolower(parm)=="y") {
		p.loc <- crg$y[,axis]
		}
	p.int <- cint(object=crg,parm=parm,axis=axis,cl=cl,nq=nq)
	if (na.rm) {
		p.int <- p.int[!is.na(p.loc),]
		p.loc <- p.loc[!is.na(p.loc)]
		}
	p.mai <- par("mai")
	n.mai <- p.mai
	if (horiz) {
		a.err <- "x"
		x.arg <- p.loc
		y.arg <- 1:length(p.loc)
		if (is.null(alim)) {
			x.lim <- signif(range(range(p.int[,1],na.rm=TRUE),range(p.int[,2],na.rm=TRUE)))
			}
		else {
			x.lim <- alim
			}
		y.lim <- c(1,length(p.loc))
		x.axt <- "s"
		y.axt <- "n"
		x.lab <- a.lab
		y.lab <- ""
		a.num <- 2
		a.las <- 1
		n.mai[2] <- max(c(p.mai[2] - 0.3,strwidth(names(p.loc),units="inches",cex=cex,font=font,family=family))) + 0.3
		}
	else {
		a.err <- "y"
		x.arg <- 1:length(p.loc)
		y.arg <- p.loc
		x.lim <- c(1,length(p.loc))
		if (is.null(alim)) {
			y.lim <- signif(range(range(p.int[,1],na.rm=TRUE),range(p.int[,2],na.rm=TRUE)))
			}
		else {
			y.lim <- alim
			}
		x.axt <- "n"
		y.axt <- "s"
		x.lab <- ""
		y.lab <- a.lab
		a.num <- 1
		a.las <- adir
		if (adir > 1) {
			n.mai[1] <- max(c(p.mai[1] - 0.3,strwidth(names(p.loc),units="inches",cex=cex,font=font,family=family))) + 0.3
			}
		}
	par(mai=n.mai)
	gplots::plotCI(x=x.arg,y=y.arg,uiw=NULL,ui=p.int[,2],li=p.int[,1],err=a.err,xlim=x.lim,ylim=y.lim,pch=NA,barcol=ecol,lwd=ewid,lty=etyp,sfrac=sfrac,gap=gap,xaxt=x.axt,yaxt=y.axt,labels=FALSE,add=FALSE,xlab=x.lab,ylab=y.lab,main=main,sub=sub,...)
	if (type=="labs") {
		text(x=x.arg,y=y.arg,labels=names(p.loc),col=col,cex=cex,font=font,family=family)
		}
	else {
		points(x=x.arg,y=y.arg,type=type,pch=psym,col=pcol,cex=pcex,bg=pbgc,lwd=lwd,lty=lty)
		axis(side=a.num,at=1:length(p.loc),labels=names(p.loc),col.axis=col,cex.axis=cex,font.axis=font,las=a.las)
		}
	par(mai=p.mai)
	invisible()
	}

#' @rdname ciplot.corregp
#' @export
ciplot <- function(x,...) {
	UseMethod("ciplot")
	}

#' Parallel Coordinate Plotting for Correspondence Regression
#'
#' Method to produce a \emph{parallel coordinate plot} of the output of a correspondence regression.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to plot the coordinates. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names in X or
#'   level names in Y. Defaults to \code{"x"}.
#' @param axes The axes to plot.
#' @param add_ci Logical specifying whether to include the confidence intervals. Defaults to \code{FALSE}.
#' @param cl The confidence level for the confidence intervals. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence intervals.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence intervals are computed directly with the \code{\link[stats]{quantile}} function.
#' @param col Color of the text labels: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param cex Character expansion factor: a number to specify the size of the text labels.
#' @param font Font of the text labels: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic. Defaults to \code{1}.
#' @param family Font family of the text labels: can be \code{"serif"}, \code{"sans"}, \code{"mono"} or one of the \code{\link[grDevices]{Hershey}} fonts.
#' @param lwd Width of the connecting lines: a number to specify the line width.
#' @param lty Line type of the connecting lines: \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"}, \code{3} or
#'   \code{"dotted"}, \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param lcol Color of the connecting lines: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param psym The symbol (or "plotting character") for the values of the coordinates on the axes.
#' @param pcol Color of the symbol for the values on the axes: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param pcex Character expansion factor of the symbol for the values on the axes.
#' @param ecol Color of the error lines (connecting the confidence intervals on each axis): either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param ewid Width of the error lines (connecting the confidence intervals on each axis): a number to specify the line width.
#' @param etyp Line type of the error lines (connecting the confidence intervals on each axis): \code{0} or \code{"blank"}, \code{1} or \code{"solid"},
#'   \code{2} or \code{"dashed"}, \code{3} or \code{"dotted"}, \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or
#'   \code{"twodash"}. Defaults to \code{2}.
#' @param acol Color of the parallel axes: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param awid Width of the parallel axes: a number to specify the line width.
#' @param atyp Line type of the parallel axes: \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"}, \code{3} or
#'   \code{"dotted"}, \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param acex Character expansion factor for the labels of the parallel axes.
#' @param afnt Font for the labels of the parallel axes: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic.
#' @param adir Reading direction of the labels on the parallel axes: either a \code{numeric} value between \code{0} and \code{3} (see the \code{las}
#'   argument in the graphical parameters \code{\link[graphics]{par}}) or a \code{character} value matching either \code{"horizontal"} or
#'   \code{"vertical"}. Defaults to \code{1} (horizontal).
#' @param add_scale Logical specifying whether to add a scale for the parallel axes (which are normalised).
#' @param main The main title of the plot.
#' @param sub The subtitle of the plot.
#' @param ... Further arguments passed to or from other methods.
#' @details Although adding lines for confidence intervals is possible, it is not recommended, as it typically leads to an unreadable plot.  
#' @return A parallel coordinate plot containing the output of a correspondence regression.
#' @seealso \code{\link{ciplot.corregp}}, \code{\link{plot.corregp}}, \code{\link{plot3d.corregp}}, \code{\link{agplot.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' pcplot(haireye.crg, parm = "y", axes = 1:3)
#' pcplot(haireye.crg, parm = c("Hair", "Sex"), axes = 1:3)
#' }
#' @export
pcplot.corregp <- function(x,parm="x",axes,add_ci=FALSE,cl=0.95,nq=TRUE,col="darkgrey",cex=par("cex"),font=par("font"),family=par("family"),lwd=par("lwd"),lty=par("lty"),lcol=col,psym=NULL,pcol=col,pcex=cex,ecol="red",ewid=1,etyp=2,acol="black",awid=1,atyp=1,acex=cex,afnt=font,adir=1,add_scale=FALSE,main=NULL,sub=NULL,...) {
	crg <- x
	if (add_ci && is.null(crg$conf)) {
		add_ci <- FALSE
		warning(paste("no bootstrapping was done in",as.list(match.call())$x,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (length(axes)==1) {
		stop("pcplot is suited for more than 1 axis; use 'ciplot' instead",call.=FALSE)
		}
	else {
		a.len <- length(axes)
		}
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))
		}
	if (is.character(adir)) {
		adir <- pmatch(tolower(adir),table=c("horizontal","vertical"))
		}
	if (length(parm)==1 && tolower(parm)=="x") {
			if(is.list(crg$x)) {
				p.loc <- do.call(what=rbind,args=crg$x)[,axes]
				}
			else {
				p.loc <- crg$x[,axes]
				}
		}
	if (is.list(crg$x) && all(parm %in% names(crg$x))) {
		p.loc <- do.call(what=rbind,args=crg$x[parm])[,axes]
		}
	if (is.list(crg$x) && all(parm %in% unlist(lapply(crg$x,rownames)))) {
		p.loc <- do.call(what=rbind,args=crg$x)[parm,axes]
		if (nrow(p.loc)==1) {
			rownames(p.loc) <- parm
			}
		}
	if (all(parm %in% rownames(crg$x))) {
		p.loc <- rbind(crg$x[parm,axes])
		if (nrow(p.loc)==1) {
			rownames(p.loc) <- parm
			}
		}
	if (all(parm %in% rownames(crg$y))) {
		p.loc <- rbind(crg$y[parm,axes])
		if (nrow(p.loc)==1) {
			rownames(p.loc) <- parm
			}
		}
	if (length(parm)==1 && tolower(parm)=="y") {
		p.loc <- crg$y[,axes]
		}
	p.min <- apply(p.loc,2,min,na.rm=TRUE)
	p.max <- apply(p.loc,2,max,na.rm=TRUE)
	if (add_ci) {
		a.int <- lapply(axes,function(a){cint(object=crg,parm=parm,axis=a,cl=cl,nq=nq)})
		p.min <- pmin(p.min,sapply(a.int,function(a1){min(a1[,1],na.rm=TRUE)}))
		p.max <- pmax(p.max,sapply(a.int,function(a2){max(a2[,2],na.rm=TRUE)}))
		}
	a.loc <- sweep(sweep(p.loc,2,p.min,"-"),2,(p.max-p.min),"/")
	p.mai <- par("mai")
	n.mai <- p.mai
	n.mai[2] <- max(strwidth(rownames(a.loc),units="inches",cex=cex,font=font,family=family)) + 0.3
	if (adir > 1) {
		n.mai[1] <- max(c(p.mai[1] - 0.3,strwidth(colnames(a.loc),units="inches",cex=acex,font=afnt))) + 0.3
		}
	par(mai=n.mai)
	matplot(t(a.loc),type="n",xlim=c(1,a.len),ylim=c(0,1),ylab="",main=main,sub=sub,axes=FALSE,add=FALSE,...)
	axis(side=1,at=1:a.len,labels=colnames(a.loc),col.axis=acol,cex.axis=acex,font.axis=afnt,las=adir)
	if (add_scale) {
		axis(side=4,col.axis=acol,cex.axis=acex,font.axis=afnt,las=1)
		}
	abline(v=1:a.len,col=acol,lty=atyp,lwd=awid)
	matlines(t(a.loc),col=lcol,lty=lty,lwd=lwd)
	if (add_ci) {
		matlines(t(sweep(sweep(sapply(a.int,function(a3){a3[,1]}),2,p.min,"-"),2,(p.max-p.min),"/")),col=ecol,lty=etyp,lwd=ewid)
		matlines(t(sweep(sweep(sapply(a.int,function(a4){a4[,2]}),2,p.min,"-"),2,(p.max-p.min),"/")),col=ecol,lty=etyp,lwd=ewid)
		}
	psym <- ifelse(is.null(psym),NA,psym)
	if (!is.na(psym)) {
		matpoints(t(a.loc),pch=psym,col=pcol,cex=pcex)
		}
	mtext(text=rownames(a.loc),side=2,at=a.loc[,1],las=1,col=col,cex=cex,font=font,family=family)
	par(mai=p.mai)
	invisible()
	}

#' @rdname pcplot.corregp
#' @export
pcplot <- function(x,...) {
	UseMethod("pcplot")
	}

#' Confidence Ellipses for Correspondence Regression
#'
#' Method to compute confidence ellipses for coordinates in correspondence regression.  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to compute the confidence ellipses. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names in X or
#'   level names in Y. Defaults to \code{"x"}.
#' @param axes The axes for which to compute the confidence ellipses: a vector of two values. Defaults to the first two axes.
#' @param cl The confidence level for the confidence ellipses. Defaults to \code{0.95}.
#' @param np The number of points to represent the confidence ellipses. Defaults to \code{100}.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' \code{cell} (of a \code{corregp} output) makes use of \code{\link[ellipse]{ellipse}} from the package \pkg{ellipse}.  
#'
#' Typically, \code{cell} is not so much used directly as it is called by a \code{\link{plot.corregp}} command.  
#' @return A list containing \code{np} points for each confidence ellipse of interest.
#' @seealso \code{\link{plot.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' cell(haireye.crg, parm = "y")
#' cell(haireye.crg, parm = c("Hair", "Sex"))
#' }
#' @export
cell.corregp <- function(object,parm="x",axes=1:2,cl=0.95,np=100,...) {
	crg <- object
	if (is.null(crg$conf)) {
		stop(paste("no bootstrapping was done in",as.list(match.call())$object,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))[1:2]
		}
	p.con <- confGet(crg=crg,parm=parm)
	p.con <- p.con[unlist(lapply(p.con,function(p0){any(complete.cases(p0))}))]
	lapply(p.con,function(p1){ellipse::ellipse(cov(p1[complete.cases(p1),axes]),centre=apply(p1[complete.cases(p1),axes],2,mean),level=cl,npoints=np)})
	}

#' @rdname cell.corregp
#' @export
cell <- function(object,...) {
	UseMethod("cell")
	}

#' Plotting Correspondence Regression
#'
#' Basic method to plot the output of a correspondence regression.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param axes The axes to plot: a vector of two values. Defaults to the first two axes.
#' @param y_btm Logical specifying whether the Y levels should be plotted first ("at the bottom") and then be overlaid by the X levels. Defaults to \code{TRUE}.
#' @param y_ell Logical specifying whether the confidence ellipses of the Y levels should be plotted. Defaults to \code{FALSE}.
#' @param x_ell Logical specifying whether the confidence ellipses of the X levels should be plotted. Defaults to \code{FALSE}.
#' @param ysub Vector of indices to select a subset of the Y levels.
#' @param xsub Vector of indices to select a subset of the X levels.
#' @param hlim Vector of two values specifying the lower and upper limit between which to plot the horizontal axis.
#' @param vlim Vector of two values specifying the lower and upper limit between which to plot the vertical axis.
#' @param expa_btm Expansion factor for the bottom coordinates: a number to rescale the axes.
#' @param expa_top Expansion factor for the top coordinates: a number to rescale the axes.
#' @param asp The aspect ratio for the whole plot. See \code{\link[graphics]{plot.window}}.
#' @param asp_btm The aspect ratio for the bottom coordinates. See \code{\link[graphics]{plot.window}}.
#' @param asp_top The aspect ratio for the top coordinates. See \code{\link[graphics]{plot.window}}.
#' @param col_btm Color of the bottom levels: either \code{numeric} or see \code{\link[grDevices]{colors}}. Defaults to \code{"darkgrey"}.
#' @param col_top Color of the top levels: either \code{numeric} or see \code{\link[grDevices]{colors}}. Defaults to \code{"red"}.
#' @param cex_btm Character expansion factor of the bottom levels: a number to specify the size of the text labels.
#' @param cex_top Character expansion factor of the top levels: a number to specify the size of the text labels.
#' @param font_btm Font of the bottom levels: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic. Defaults to \code{1}.
#' @param font_top Font of the top levels: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic. Defaults to \code{1}.
#' @param fam_btm Font family of the bottom levels: can be \code{"serif"}, \code{"sans"}, \code{"mono"} or one of the \code{\link[grDevices]{Hershey}} fonts.
#' @param fam_top Font family of the top levels: can be \code{"serif"}, \code{"sans"}, \code{"mono"} or one of the \code{\link[grDevices]{Hershey}} fonts.
#' @param col_ell Color of the confidence ellipses: either a number or see \code{\link[grDevices]{colors}}.
#' @param lwd_ell Width of the confidence ellipses: a number to specify the line width.
#' @param lty_ell Line type of the confidence ellipses: \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"}, \code{3} or \code{"dotted"},
#'   \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param col_ori Color of the lines through the origin: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param lwd_ori Width of the lines through the origin: a number to specify the line width.
#' @param lty_ori Line type of the lines through the origin: \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"},
#'   \code{3} or \code{"dotted"}, \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param main The main title of the plot.
#' @param sub The subtitle of the plot.
#' @param hlab The title of the horizontal axis.
#' @param vlab The title of the vertical axis.
#' @param cl The confidence level for the confidence ellipses. Defaults to \code{0.95}.
#' @param np The number of points to represent the confidence ellipses. Defaults to \code{100}.
#' @param add_ori Logical specifying whether to add lines through the origin. Defaults to \code{TRUE}.
#' @param ... Further arguments passed to or from other methods.
#' @details The plot of a correspondence regression is by definition a \code{\link[stats]{biplot}}.  
#' @return A plot window containing the output of a correspondence regression.
#' @references
#' Gower, J., S. Lubbe and N. Le Roux (2011) \emph{Understanding biplots}. Chichester: Wiley.
#'
#' Greenacre, M. (2010) \emph{Biplots in practice}. Bilbao: Fundacion BBVA.
#' @seealso \code{\link{corregp}}, \code{\link{summary.corregp}}, \code{\link{screeplot.corregp}}, \code{\link{anova.corregp}}, \code{\link[stats]{biplot}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' plot(haireye.crg, x_ell = TRUE, xsub = c("Hair", "Sex"))
#' }
#' @export
plot.corregp <- function(x,axes=1:2,y_btm=TRUE,y_ell=FALSE,x_ell=FALSE,ysub=NULL,xsub=NULL,hlim=NULL,vlim=NULL,expa_btm=1,expa_top=1,asp=1,asp_btm=asp,asp_top=asp,col_btm="darkgrey",col_top="red",cex_btm=par("cex"),cex_top=cex_btm,font_btm=par("font"),font_top=font_btm,fam_btm=par("family"),fam_top=fam_btm,col_ell=par("col"),lwd_ell=par("lwd"),lty_ell=par("lty"),col_ori=par("col"),lwd_ori=par("lwd"),lty_ori=1,main=NULL,sub=NULL,hlab=NULL,vlab=NULL,cl=0.95,np=100,add_ori=TRUE,...) {
	crg <- x
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))[1:2]
		}
	y.mat <- crg$y[,axes]
	if (is.list(crg$x)) {
		x.mat <- do.call(what=rbind,args=crg$x)[,axes]
		if (is.numeric(xsub) || all(xsub %in% names(crg$x))) {
			xsub <- unlist(lapply(crg$x[xsub],rownames))
			}
		}
	else {
		x.mat <- crg$x[,axes]
		}
	if (is.character(ysub)) {
		ysub <- match(ysub,table=rownames(y.mat))
		}
	if (is.null(ysub)) {
		ysub <- 1:nrow(y.mat)
		}
	if (is.character(xsub)) {
		xsub <- match(xsub,table=rownames(x.mat))
		}
	if (is.null(xsub)) {
		xsub <- 1:nrow(x.mat)
		}
	ell_btm <- NULL
	ell_top <- NULL
	if ((y_ell || x_ell) && is.null(crg$conf)) {
		y_ell <- x_ell <- FALSE
		warning(paste("no bootstrapping was done in",as.list(match.call())$x,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (y_btm) {
		loc_btm <- y.mat
		loc_top <- x.mat
		sub_btm <- ysub
		sub_top <- xsub
		if (y_ell) {
			ell_btm <- cell(object=crg,parm=rownames(y.mat)[ysub],axes=axes,cl=cl,np=np)
			}
		if (x_ell) {
			ell_top <- cell(object=crg,parm=rownames(x.mat)[xsub],axes=axes,cl=cl,np=np)
			}
		}
	else {
		loc_btm <- x.mat
		loc_top <- y.mat
		sub_btm <- xsub
		sub_top <- ysub
		if (y_ell) {
			ell_top <- cell(object=crg,parm=rownames(y.mat)[ysub],axes=axes,cl=cl,np=np)
			}
		if (x_ell) {
			ell_btm <- cell(object=crg,parm=rownames(x.mat)[xsub],axes=axes,cl=cl,np=np)
			}
		}
	if (!is.null(hlim) && !is.null(vlim)) {
		asp <- asp_btm <- asp_top <- NA
		}
	if (is.null(hlim)) {
		hlim <- signif(range(range(y.mat[,1],na.rm=TRUE),range(x.mat[,1],na.rm=TRUE)))
		}
	if (is.null(vlim)) {
		vlim <- signif(range(range(y.mat[,2],na.rm=TRUE),range(x.mat[,2],na.rm=TRUE)))
		}
	if (is.null(hlab)) {
		hlab <- colnames(y.mat)[1]
		}
	if (is.null(vlab)) {
		vlab <- colnames(y.mat)[2]
		}
	p.pty <- par("pty")
	par(pty="s")
	plot(x=loc_btm[sub_btm,1],y=loc_btm[sub_btm,2],type="n",xlim=(hlim/expa_btm),ylim=(vlim/expa_btm),main=main,sub=sub,xlab=hlab,ylab=vlab,asp=asp_btm,...)
	if (add_ori) {
		abline(h=0,v=0,col=col_ori,lwd=lwd_ori,lty=lty_ori)
		}
	if (length(col_ell)==1) {
		col_ell <- rep(col_ell,times=2)
		}
	if (length(lwd_ell)==1) {
		lwd_ell <- rep(lwd_ell,times=2)
		}
	if (length(lty_ell)==1) {
		lty_ell <- rep(lty_ell,times=2)
		}
	if (!is.null(ell_btm)) {
		lapply(ell_btm,function(p1){lines(x=p1[,1],y=p1[,2],col=col_ell[1],lwd=lwd_ell[1],lty=lty_ell[1])})
		}
	text(x=loc_btm[sub_btm,1],y=loc_btm[sub_btm,2],labels=rownames(loc_btm)[sub_btm],col=col_btm,cex=cex_btm,font=font_btm,family=fam_btm)
	par(new=TRUE)
	plot(x=loc_top[sub_top,1],y=loc_top[sub_top,2],type="n",xlim=(hlim/expa_top),ylim=(vlim/expa_top),main=NA,sub=NA,xlab=NA,ylab=NA,asp=asp_top,axes=FALSE)
	if (expa_btm != expa_top) {
		axis(side=3,col.ticks=col_top,...)
		axis(side=4,col.ticks=col_top,...)
		}
	if (!is.null(ell_top)) {
		lapply(ell_top,function(p2){lines(x=p2[,1],y=p2[,2],col=col_ell[2],lwd=lwd_ell[2],lty=lty_ell[2])})
		}
	text(x=loc_top[sub_top,1],y=loc_top[sub_top,2],labels=rownames(loc_top)[sub_top],col=col_top,cex=cex_top,font=font_top,family=fam_top)
	par(pty=p.pty)
	invisible()
	}

#' 3D Confidence Ellipsoids for Correspondence Regression
#'
#' Method to compute 3D confidence ellipsoids for coordinates in correspondence regression.  
#' @param object The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param parm The parameter for which to compute the confidence ellipsoids. Can be either \code{"y"}, \code{"x"}, or any vector of term names in X, level names in X or
#'   level names in Y. Defaults to \code{"x"}.
#' @param axes The axes for which to compute the confidence ellipsoids: a vector of three values. Defaults to the first three axes.
#' @param cl The confidence level for the confidence ellipsoids. Defaults to \code{0.95}.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' \code{cell3d} (of a \code{corregp} output) makes use of \code{\link[rgl]{ellipse3d}} from the package \pkg{rgl}.  
#' 
#' Typically, \code{cell3d} is not so much used directly as it is called by a \code{\link{plot3d.corregp}} command.  
#' @return A list containing coordinate points for each confidence ellipsoid of interest.
#' @seealso \code{\link{plot3d.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' cell3d(haireye.crg, parm = "y")
#' cell3d(haireye.crg, parm = c("Hair", "Sex"))
#' }
#' @export
cell3d.corregp <- function(object,parm="x",axes=1:3,cl=0.95,...) {
	crg <- object
	if (is.null(crg$conf)) {
		stop(paste("no bootstrapping was done in",as.list(match.call())$object,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))[1:3]
		}
	p.con <- confGet(crg=crg,parm=parm)
	p.con <- p.con[unlist(lapply(p.con,function(p0){any(complete.cases(p0))}))]
	lapply(p.con,function(p1){rgl::ellipse3d(cov(p1[complete.cases(p1),axes]),centre=apply(p1[complete.cases(p1),axes],2,mean),level=cl)})
	}

#' @rdname cell3d.corregp
#' @export
cell3d <- function(object,...) {
	UseMethod("cell3d")
	}

#' 3D Plotting for Correspondence Regression
#'
#' Method to produce a 3D plot for a correspondence regression.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param axes The axes to plot: a vector of three values. Defaults to the first three axes.
#' @param y_btm Logical specifying whether the Y levels should be plotted first ("at the bottom") and then be overlaid by the X levels. Defaults to \code{TRUE}.
#' @param y_ell Logical specifying whether the confidence ellipsoids of the Y levels should be plotted. Defaults to \code{FALSE}.
#' @param x_ell Logical specifying whether the confidence ellipsoids of the X levels should be plotted. Defaults to \code{FALSE}.
#' @param ysub Vector of indices to select a subset of the Y levels.
#' @param xsub Vector of indices to select a subset of the X levels.
#' @param hlim Vector of two values specifying the lower and upper limit between which to plot the horizontal axis.
#' @param vlim Vector of two values specifying the lower and upper limit between which to plot the vertical axis.
#' @param dlim Vector of two values specifying the lower and upper limit between which to plot the "depth" axis.
#' @param asp The aspect ratio for the whole plot. See \code{\link[rgl]{aspect3d}}.
#' @param col_btm Color of the bottom levels: either \code{numeric} or see \code{\link[grDevices]{colors}}. Defaults to \code{"darkgrey"}.
#' @param col_top Color of the top levels: either \code{numeric} or see \code{\link[grDevices]{colors}}. Defaults to \code{"red"}.
#' @param cex_btm Character expansion factor of the bottom levels: a number to specify the size of the text labels.
#' @param cex_top Character expansion factor of the top levels: a number to specify the size of the text labels.
#' @param font_btm Font of the bottom levels: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic.
#' @param font_top Font of the top levels: \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic.
#' @param fam_btm Font family of the bottom levels: can be \code{"serif"}, \code{"sans"}, \code{"mono"} or code{"symbol"}.
#' @param fam_top Font family of the top levels: can be \code{"serif"}, \code{"sans"}, \code{"mono"} or \code{"symbol"}.
#' @param col_ell Color of the confidence ellipsoids: either a number or see \code{\link[grDevices]{colors}}. Defaults to \code{"black"}.
#' @param lwd_ell Width of the confidence ellipsoids: a number to specify the line width.
#' @param lty_ell Line type of the confidence ellipsoids: either \code{"shade"}, \code{"wire"}, or \code{"dots"}. Defaults to \code{"shade"}.
#' @param opa_ell Opaqueness of the confidence ellipsoids: a number between \code{0} for fully transparent and \code{1} for fully opaque. Defaults to \code{0.2}.
#' @param col_ori Color of the lines through the origin: either a number or see \code{\link[grDevices]{colors}}. Defaults to \code{"grey"}.
#' @param lwd_ori Width of the lines through the origin: a number to specify the line width. Defaults to \code{1}.
#' @param main The main title of the plot.
#' @param sub The subtitle of the plot.
#' @param hlab The title of the horizontal axis.
#' @param vlab The title of the vertical axis.
#' @param dlab The title of the "depth" axis.
#' @param cl The confidence level for the confidence ellipsoids. Defaults to \code{0.95}.
#' @param add_ori Logical specifying whether to add lines through the origin. Defaults to \code{TRUE}.
#' @param ... Further arguments passed to or from other methods.
#' @details \code{plot3d} (of a \code{corregp} output) makes use of \code{\link[rgl]{plot3d}} (and \code{\link[rgl]{text3d}} and \code{\link[rgl]{abclines3d}}) from the package \pkg{rgl}.  
#' @return A 3D plot window containing the output of a correspondence regression.
#' @seealso \code{\link{corregp}}, \code{\link{pcplot.corregp}}, \code{\link{agplot.corregp}}, \code{\link[rgl]{plot3d}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' plot3d(haireye.crg, x_ell = TRUE, xsub = c("Hair", "Sex"))
#' }
#' @export
plot3d.corregp <- function(x,axes=1:3,y_btm=TRUE,y_ell=FALSE,x_ell=FALSE,ysub=NULL,xsub=NULL,hlim=NULL,vlim=NULL,dlim=NULL,asp=par3d("scale"),col_btm="darkgrey",col_top="red",cex_btm=par3d("cex"),cex_top=cex_btm,font_btm=par3d("font"),font_top=font_btm,fam_btm=par3d("family"),fam_top=fam_btm,col_ell="black",lwd_ell=1,lty_ell="shade",opa_ell=0.2,col_ori="grey",lwd_ori=1,main=NULL,sub=NULL,hlab=NULL,vlab=NULL,dlab=NULL,cl=0.95,add_ori=TRUE,...) {
	crg <- x
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))[1:3]
		}
	axes <- c(axes[1],axes[3],axes[2])
	y.mat <- crg$y[,axes]
	if (is.list(crg$x)) {
		x.mat <- do.call(what=rbind,args=crg$x)[,axes]
		if (is.numeric(xsub) || all(xsub %in% names(crg$x))) {
			xsub <- unlist(lapply(crg$x[xsub],rownames))
			}
		}
	else {
		x.mat <- crg$x[,axes]
		}
	if (is.character(ysub)) {
		ysub <- match(ysub,table=rownames(y.mat))
		}
	if (is.null(ysub)) {
		ysub <- 1:nrow(y.mat)
		}
	if (is.character(xsub)) {
		xsub <- match(xsub,table=rownames(x.mat))
		}
	if (is.null(xsub)) {
		xsub <- 1:nrow(x.mat)
		}
	ell_btm <- NULL
	ell_top <- NULL
	if ((y_ell || x_ell) && is.null(crg$conf)) {
		y_ell <- x_ell <- FALSE
		warning(paste("no bootstrapping was done in",as.list(match.call())$x,sep=" ",collapse=NULL),call.=FALSE)
		}
	if (y_btm) {
		loc_btm <- y.mat
		loc_top <- x.mat
		sub_btm <- ysub
		sub_top <- xsub
		if (y_ell) {
			ell_btm <- cell3d(object=crg,parm=rownames(y.mat)[ysub],axes=axes,cl=cl)
			}
		if (x_ell) {
			ell_top <- cell3d(object=crg,parm=rownames(x.mat)[xsub],axes=axes,cl=cl)
			}
		}
	else {
		loc_btm <- x.mat
		loc_top <- y.mat
		sub_btm <- xsub
		sub_top <- ysub
		if (y_ell) {
			ell_top <- cell3d(object=crg,parm=rownames(y.mat)[ysub],axes=axes,cl=cl)
			}
		if (x_ell) {
			ell_btm <- cell3d(object=crg,parm=rownames(x.mat)[xsub],axes=axes,cl=cl)
			}
		}
	if (is.null(hlim)) {
		hlim <- signif(range(range(y.mat[,1],na.rm=TRUE),range(x.mat[,1],na.rm=TRUE)))
		}
	if (is.null(vlim)) {
		vlim <- signif(range(range(y.mat[,2],na.rm=TRUE),range(x.mat[,2],na.rm=TRUE)))
		}
	if (is.null(dlim)) {
		dlim <- signif(range(range(y.mat[,3],na.rm=TRUE),range(x.mat[,3],na.rm=TRUE)))
		}
	if (is.null(hlab)) {
		hlab <- colnames(y.mat)[1]
		}
	if (is.null(vlab)) {
		vlab <- colnames(y.mat)[2]
		}
	if (is.null(dlab)) {
		dlab <- colnames(y.mat)[3]
		}
	rgl::plot3d(x=loc_btm[sub_btm,1],y=loc_btm[sub_btm,2],z=loc_btm[sub_btm,3],type="n",xlim=hlim,ylim=vlim,zlim=dlim,main=main,sub=sub,xlab=hlab,ylab=vlab,zlab=dlab,aspect=asp,...)
	if (add_ori) {
		rgl::abclines3d(x=0,y=0,z=0,a=1,b=0,c=0,col=col_ori,lwd=lwd_ori)
		rgl::abclines3d(x=0,y=0,z=0,a=0,b=1,c=0,col=col_ori,lwd=lwd_ori)
		rgl::abclines3d(x=0,y=0,z=0,a=0,b=0,c=1,col=col_ori,lwd=lwd_ori)
		}
	if (length(col_ell)==1) {
		col_ell <- rep(col_ell,times=2)
		}
	if (length(lwd_ell)==1) {
		lwd_ell <- rep(lwd_ell,times=2)
		}
	if (length(lty_ell)==1) {
		lty_ell <- rep(lty_ell,times=2)
		}
	if (length(opa_ell)==1) {
		opa_ell <- rep(opa_ell,times=2)
		}
	if (!is.null(ell_btm)) {
		lapply(ell_btm,rgl::plot3d,add=TRUE,xlim=hlim,ylim=vlim,zlim=dlim,lit=FALSE,col=col_ell[1],lwd=lwd_ell[1],type=lty_ell[1],alpha=opa_ell[1])
		}
	rgl::text3d(x=loc_btm[sub_btm,1],y=loc_btm[sub_btm,2],z=loc_btm[sub_btm,3],texts=rownames(loc_btm)[sub_btm],col=col_btm,cex=cex_btm,font=font_btm,family=fam_btm)
	rgl::plot3d(x=loc_top[sub_top,1],y=loc_top[sub_top,2],z=loc_top[sub_top,3],type="n",xlim=hlim,ylim=vlim,zlim=dlim,main="",sub="",xlab="",ylab="",zlab="",add=TRUE)
	if (!is.null(ell_top)) {
		lapply(ell_top,rgl::plot3d,add=TRUE,xlim=hlim,ylim=vlim,zlim=dlim,lit=FALSE,col=col_ell[2],lwd=lwd_ell[2],type=lty_ell[2],alpha=opa_ell[2])
		}
	rgl::text3d(x=loc_top[sub_top,1],y=loc_top[sub_top,2],z=loc_top[sub_top,3],texts=rownames(loc_top)[sub_top],col=col_top,cex=cex_top,font=font_top,family=fam_top)
	invisible()
	}

#' Plotting an Association Graph for Correspondence Regression
#'
#' Function to make an association graph of the (significant) coordinate scores in correspondence regression.  
#' @param x The output of a call to \code{\link{corregp}} (i.e. an object of class "corregp").
#' @param axes The axes for which to plot the association graph: a vector of indices. Defaults to all the axes.
#' @param ysub Vector of indices to select a subset of the Y levels.
#' @param xsub Vector of indices to select a subset of the X levels. Can also be \code{"all"} or \code{"both"} (or abbreviations).
#' @param sort Vector of axes for which to sort the coordinate scores. The default (\code{NULL}) plots all levels in the order in which they appear in the
#'   correspondence regression \code{x}.
#' @param na.rm Logical specifying whether to omit \code{NA} coordinates from the plot. Defaults to \code{FALSE}.
#' @param col Color of the association graph: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param cex Character expansion factor: a number to specify the size of the text labels.
#' @param font Font of the text labels (levels): \code{1} for plain, \code{2} for bold, \code{3} for italic, and \code{4} for bold italic. Defaults to \code{1}.
#' @param family Font family of the text labels (levels): can be \code{"serif"}, \code{"sans"}, \code{"mono"} or one of the \code{\link[grDevices]{Hershey}} fonts.
#' @param lwd Line width of the association graph: a number to specify the line width.
#' @param lty Line type of the association graph (i.e. linking edges): \code{0} or \code{"blank"}, \code{1} or \code{"solid"}, \code{2} or \code{"dashed"},
#'   \code{3} or \code{"dotted"}, \code{4} or \code{"dotdash"}, \code{5} or \code{"longdash"}, \code{6} or \code{"twodash"}. Defaults to \code{1}.
#' @param ycol Color of the levels in Y: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param xcol Color of the levels in X: either \code{numeric} or see \code{\link[grDevices]{colors}}.
#' @param ncol Fill color of the nodes: either \code{numeric} or see \code{\link[grDevices]{colors}}. Defaults to \code{c("white","lightgray")}: the first value
#'   is for the nodes of the axes and the second value is for the nodes of the X and Y levels.
#' @param nwid Line width of the nodes: a number to specify the line width. If a vector of two values is specified, then the first width is for the nodes of
#'   the axes and the second width is for the nodes of the X and Y levels.
#' @param lcol Color of the links (edges): either \code{numeric} or see \code{\link[grDevices]{colors}}. If a vector of two values is specified, then
#'   the first color is for the scores \code{> 0} and the second color is for the scores \code{< 0}.
#' @param lwid Line width of the links (edges): a number to specify the line width. If a vector of two values is specified, then the first width is for
#'   the scores \code{> 0} and the second width is for the scores \code{< 0}.
#' @param pcol Color of the pointer (arrow head): either \code{numeric} or see \code{\link[grDevices]{colors}}. If a vector of two values is specified, then
#'   the first color is for the scores \code{> 0} and the second color is for the scores \code{< 0}.
#' @param ppos Relative position of the pointer (arrow head): a vector of values between 0 and 1 for each axis.
#' @param ptyp Type of of the pointer (arrow head): can be \code{"simple"}, \code{"curved"}, \code{"triangle"}, \code{"circle"}, \code{"ellipse"} or
#'   \code{"T"}. Defaults to \code{"simple"}.
#' @param zoom Zoom factor of the association graph. Defaults to \code{1}.
#' @param hshft Horizontal shift of the association graph. Defaults to \code{0}.
#' @param vshft Vertical shift of the association graph. Defaults to \code{0}.
#' @param main The main title of the association graph.
#' @param cl The confidence level for the confidence intervals. Defaults to \code{0.95}.
#' @param nq Logical specifying whether to use a normal quantile (i.e. apply \code{\link[stats]{qnorm}}) in the computation of the confidence interval.
#'   Defaults to \code{TRUE}. If \code{FALSE}, then the confidence interval is computed directly with the \code{\link[stats]{quantile}} function.
#' @param digits Integer specifying the number of decimals for the scores as labels of the links (edges). Defauls to \code{2}.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' Association graphs (of a \code{corregp} output) in the \pkg{corregp} package make use of various functionalities of the package \pkg{diagram}.  
#' @return A plot window containing the association graph.
#' @seealso \code{\link{corregp}}, \code{\link{cint.corregp}}, \code{\link{pcplot.corregp}}, \code{\link{plot3d.corregp}}.
#' @examples
#' \donttest{
#' data(HairEye)
#' haireye.crg <- corregp(Eye ~ Hair * Sex, data = HairEye, b = 3000)
#' agplot(haireye.crg, axes = 1:2, xsub = c("Hair", "Sex"))
#' plotag(haireye.crg, axes = 1:2, xsub = c("Hair", "Sex"))
#' }
#' @export
agplot.corregp <- function(x,axes=NULL,ysub=NULL,xsub=NULL,sort=NULL,na.rm=FALSE,col="black",cex=par("cex"),font=par("font"),family=par("family"),lwd=par("lwd"),lty=par("lty"),ycol=col,xcol=col,ncol=c("white","lightgray"),nwid=lwd,lcol=col,lwid=lwd,pcol=lcol,ppos=NULL,ptyp="simple",zoom=1,hshft=0,vshft=0,main=NULL,cl=0.95,nq=TRUE,digits=2,...) {
	crg <- x
	if (is.null(crg$conf)) {
		stop(paste("no bootstrapping was done in",as.list(match.call())$x,sep=" ",collapse=NULL),call.=FALSE)
		}
	o.rnk <- sum(crg$eigen>1e-08,na.rm=TRUE)
	if (is.null(axes)) {
		axes <- 1:o.rnk
		}
	if (is.character(axes)) {
		axes <- match(axes,table=colnames(crg$y))
		}
	if (any(axes>o.rnk)) {
		axes <- 1:o.rnk
		warning(paste(as.list(match.call())$object,"only has",o.rnk,"axes",sep=" ",collapse=NULL),call.=FALSE)
		}
	if (is.null(ysub)) {
		ysub <- rownames(crg$y)
		}
	if (is.null(xsub)) {
		xsub <- "x"
		}
	a.len <- length(axes)
	y.mat <- round(matrix(crg$y[ysub,axes],nrow=length(ysub),ncol=a.len,dimnames=list(ysub,axes)),digits=digits)
	y.mat[as.matrix(do.call(what=cbind,args=lapply(axes,function(a1){apply(cint(crg,parm=ysub,axis=a1,cl=cl,nq=nq),1,prod)<0})),ncol=a.len)] <- 0
	if (length(xsub)==1 && tolower(xsub) %in% c("x","a","b","all","both")) {
		if (is.list(crg$x)) {
			x.mat <- round(as.matrix(do.call(what=rbind,args=crg$x)[,axes],ncol=a.len),digits=digits)
			}
		else {
			x.mat <- round(as.matrix(crg$x[,axes],ncol=a.len),digits=digits)
			}
		xsub <- "x"
		}
	if (is.list(crg$x) && all(xsub %in% names(crg$x))) {
		x.mat <- round(as.matrix(do.call(what=rbind,args=crg$x[xsub])[,axes],ncol=a.len),digits=digits)
		}
	if (is.list(crg$x) && is.numeric(xsub)) {
		x.mat <- round(as.matrix(do.call(what=rbind,args=crg$x[xsub])[,axes],ncol=a.len),digits=digits)
		xsub <- names(crg$x)[xsub]
		}
	if (is.list(crg$x) && all(xsub %in% unlist(lapply(crg$x,rownames)))) {
		x.mat <- round(matrix(do.call(what=rbind,args=crg$x)[xsub,axes],nrow=length(xsub),ncol=a.len,dimnames=list(xsub,axes)),digits=digits)
		}
	if (all(xsub %in% rownames(crg$x))) {
		x.mat <- round(matrix(crg$x[xsub,axes],nrow=length(xsub),ncol=a.len,dimnames=list(xsub,axes)),digits=digits)
		}
	x.mat[as.matrix(do.call(what=cbind,args=lapply(axes,function(a2){apply(cint(crg,parm=xsub,axis=a2,cl=cl,nq=nq),1,prod)<0})),ncol=a.len)] <- 0
	if (na.rm) {
		x.mat <- as.matrix(x.mat[complete.cases(x.mat),],ncol=a.len)
		y.mat <- as.matrix(y.mat[complete.cases(y.mat),],ncol=a.len)
		}
	y.len <- nrow(y.mat)
	x.len <- nrow(x.mat)
	if (!is.null(sort) && !any(is.na(sort))) {
		if (is.character(sort)) {
			sort <- match(sort,table=colnames(crg$y))
			}
		if (any(!sort %in% axes)) {
			sort <- axes
			warning("the values in argument 'sort' do not match the values in argument 'axes'",call.=FALSE)
			}
		y.ord <- do.call(order,c(data.frame(y.mat[,sort]),decreasing=TRUE))
		x.ord <- do.call(order,c(data.frame(x.mat[,sort]),decreasing=TRUE))
		}
	else {
		y.ord <- 1:y.len
		x.ord <- 1:x.len
		}
	y.mat <- y.mat[y.ord,]
	x.mat <- x.mat[x.ord,]
	a.pos <- matrix(c(rep(0.5,times=a.len),seq(from=a.len,to=1)/(a.len+1)),ncol=2,dimnames=list(axes,NULL))
	y.pos <- matrix(c(rep(0.3,times=y.len),seq(from=y.len,to=1)/(y.len+1)),ncol=2,dimnames=list(rownames(y.mat),NULL))
	x.pos <- matrix(c(rep(0.7,times=x.len),seq(from=x.len,to=1)/(x.len+1)),ncol=2,dimnames=list(rownames(x.mat),NULL))
	c.pos <- rbind(y.pos,x.pos)
	c.mat <- rbind(y.mat,x.mat)
	l.mat <- do.call(what=rbind,args=lapply(1:a.len,function(a3){cbind(rep(a3,times=sum(c.mat[,a3]!=0,na.rm=TRUE)),which(c.mat[,a3]!=0))}))
	p.mat <- matrix(nrow=nrow(l.mat),ncol=2,dimnames=list(as.vector(c.mat[!c.mat%in%c(0,NA)]),NULL))
	a.rad <- max(c(strwidth(rownames(a.pos),units="inches",cex=cex,font=font,family=family),strheight(rownames(a.pos),units="inches",cex=cex,font=font,family=family))+0.2)/10
	c.rad <- max(strwidth(rownames(c.pos),units="inches",cex=cex,font=font,family=family)+0.01)/10
	v.rad <- max(strheight(c(rownames(a.pos),rownames(c.pos)),units="inches",cex=cex,font=font,family=family)+0.05)/10
	if (is.null(ppos)) {
		p.pos <- seq(from=0.8,to=0.3,length.out=a.len)
		}
	else {
		p.pos <- rep(ppos,length.out=a.len)
		}
	lcol <- rep(lcol,length.out=2)
	pcol <- rep(pcol,length.out=2)
	lwid <- rep(lwid,length.out=2)
	nwid <- rep(nwid,length.out=2)
	glim <- c(0,1)
	if (zoom != 1) {
		glim <- c(-1/zoom+1,1/zoom)
		}
	p.mar <- par("mar")
	par(mar=rep(1,times=4))
	diagram::openplotmat(xlim=glim,ylim=glim,main=main,...)
	diagram::coordinates(pos=rbind(a.pos,c.pos),mx=hshft,my=vshft,hor=FALSE,relsize=zoom)
	for (l4 in 1:nrow(l.mat)) {
		l.col <- ifelse(as.numeric(rownames(p.mat)[l4])>0,lcol[1],lcol[2])
		p.col <- ifelse(as.numeric(rownames(p.mat)[l4])>0,pcol[1],pcol[2])
		l.wid <- ifelse(as.numeric(rownames(p.mat)[l4])>0,lwid[1],lwid[2])
		p.mat[l4,] <- diagram::straightarrow(from=a.pos[l.mat[l4,1],],to=c.pos[l.mat[l4,2],],lcol=l.col,lwd=l.wid,lty=lty,arr.col=p.col,arr.pos=p.pos[l.mat[l4,1]],arr.type=ptyp)
		diagram::textellipse(a.pos[l.mat[l4,1],],radx=a.rad,rady=a.rad,lab=rownames(a.pos)[l.mat[l4,1]],shadow.size=0,box.col=ncol[1],lcol="black",lwd=nwid[1],col=col,cex=cex,font=font,family=family,xpd=TRUE)
		}
	ycol <- rep(ycol,length.out=y.len)[y.ord]
	lapply(1:nrow(y.pos),function(c5){diagram::textrect(y.pos[c5,]-c(c.rad-0.00001,0),radx=c.rad,rady=v.rad,lab=rownames(y.pos)[c5],shadow.size=0,box.col="lightgray",lcol="black",lwd=nwid[2],col=ycol[c5],cex=cex,font=font,family=family,xpd=TRUE)})
	xcol <- rep(xcol,length.out=x.len)[x.ord]
	lapply(1:nrow(x.pos),function(c6){diagram::textrect(x.pos[c6,]+c(c.rad-0.00001,0),radx=c.rad,rady=v.rad,lab=rownames(x.pos)[c6],shadow.size=0,box.col=ncol[2],lcol="black",lwd=nwid[2],col=xcol[c6],cex=cex,font=font,family=family,xpd=TRUE)})
	lapply(1:nrow(p.mat),function(l7){text(p.mat[l7,1],p.mat[l7,2],labels=rownames(p.mat)[l7],pos=3,offset=0.5,col=ifelse(as.numeric(rownames(p.mat)[l7])>0,pcol[1],pcol[2]),cex=cex,font=font,family=family)})
	par(mar=p.mar)
	invisible()
	}

#' @rdname agplot.corregp
#' @export
plotag.corregp <- function(x,axes=NULL,ysub=NULL,xsub=NULL,sort=NULL,na.rm=FALSE,col="black",cex=par("cex"),font=par("font"),family=par("family"),lwd=par("lwd"),lty=par("lty"),ycol=col,xcol=col,ncol=c("white","lightgray"),nwid=lwd,lcol=col,lwid=lwd,pcol=lcol,ppos=NULL,ptyp="simple",zoom=1,hshft=0,vshft=0,main=NULL,cl=0.95,nq=TRUE,digits=2,...) {
	agplot.corregp(x,axes,ysub,xsub,sort,na.rm,col,cex,font,family,lwd,lty,ycol,xcol,ncol,nwid,lcol,lwid,pcol,ppos,ptyp,zoom,hshft,vshft,main,cl,nq,digits,...)
	}

#' @rdname agplot.corregp
#' @export
agplot <- function(x,...) {
	UseMethod("agplot")
	}

#' @rdname agplot.corregp
#' @export
plotag <- function(x,...) {
	UseMethod("plotag")
	}
