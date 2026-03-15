% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:40 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/anova.haplo.glm.Rd,v 1.1 2011/11/10 15:29:40 sinnwell Exp $ -->
% $Locker:  $ -->
% $Log: anova.haplo.glm.Rd,v $
% Revision 1.1  2011/11/10 15:29:40  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.4  2008/04/04 16:12:33  sinnwell
% add note about returned value by invisible()
%
% Revision 1.3  2008/03/24 21:14:44  sinnwell
% add show.missing argument
%
% Revision 1.2  2008/01/09 21:32:16  sinnwell
% change comments to %
%
% Revision 1.1  2007/11/07 21:46:59  sinnwell
% Initial revision
%
% Revision 1.2  2004/03/16 17:02:24  sinnwell
% add return char to usage

\name{anova.haplo.glm}
\alias{anova.haplo.glm}
\alias{anova.haplo.glmlist}
\title{
  Analysis of variance for haplo.glm model fit
}
\description{
  Perform an analysis of variance between two haplo.glm model fits
  using the deviances from the fitted objects 
}
\usage{
\method{anova}{haplo.glm}(object, ..., dispersion=NULL, test="Chisq")
}
\arguments{
\item{object}{
  A haplo.glm or glm object
}
\item{\dots}{
  More model fits to compare against the fit in the first argument
}
\item{dispersion }{
  the dispersion parameter for the fitting family. By default it is
  obtained from the object(s)
}
\item{test}{
  character string for the test of model comparison. Only "Chisq"
  supported for haplo.glm objects
}
}
\value{
A data.frame of the anova class, with these columns: Df, Deviance, Resid.Df,
Resid.Dev, p-value
}
\details{
  Uses print.anova for the displayed result
}
\seealso{
  \code{\link{haplo.glm}}
}
\examples{
 data(hla.demo)
 geno <- as.matrix(hla.demo[,c(17,18,21:24)])
 keep <- !apply(is.na(geno) | geno==0, 1, any) # SKIP THESE THREE LINES
 hla.demo <- hla.demo[keep,]                   # IN AN ANALYSIS
 geno <- geno[keep,]                           # 
 attach(hla.demo)
 label <-c("DQB","DRB","B")
 y <- hla.demo$resp
 y.bin <- 1*(hla.demo$resp.cat=="low")

# set up a genotype array as a model.matrix for inserting into data frame
# Note that hla.demo is a data.frame, and we need to subset to columns
# of interest. Also also need to convert to a matrix object, so that
# setupGeno can code alleles and convert geno to 'model.matrix' class.

 geno <- setupGeno(geno, miss.val=c(0,NA))

  # geno now has an attribute 'unique.alleles' which must be passed to
  # haplo.glm as allele.lev=attributes(geno)$unique.alleles, see below

 my.data <- data.frame(geno=geno, age=hla.demo$age, male=hla.demo$male,
                        y=y, y.bin=y.bin)


 fit.gaus <- haplo.glm(y ~ male + geno, family = gaussian,  na.action=
               "na.geno.keep", data=my.data, locus.label=label,
               control = haplo.glm.control(haplo.freq.min=0.02))
 glmfit.gaus <- glm(y~male, family=gaussian, data=my.data)

 anova.haplo.glm(glmfit.gaus, fit.gaus)

}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
