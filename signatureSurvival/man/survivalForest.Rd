\name{survivalForest}
\alias{survivalForest}

\title{
Forestplot for result of multivariate Cox proportional hazard survival analysis
}
\description{
This function provides a publishable forestplot figure that shows results of
multivariate Cox proportional hazard regression analysis of patients in a
cohort.
}
\usage{
survivalForest(sdata,xtick)
}

\arguments{
  \item{sdata}{
 results of multivariate survival analyses,outputted by performing MKMplot.
 }
 \item{xtick}{
 x-coordinate size in forestplot. For example, user can set xtick=c(-2,-1,0,1,2)
 }
}

\details{
The data includes column "model" and/or stage", "variable","HR","SE", and
"p_value". In inputing data, stage may be stage IA, stageIB, stageII, stageIIA,
stageIIB, stageIII, etc. The Variable may contain signature, sex, age, smoking,
genotype,etc.. The variable column may be named with "Variate","variate",
"Variable","variable", "factor" or "category" word.  "HR" is hazard risk, also 
called coefficient or beta of Cox proportional hazard regression. So, HR column 
may be named with "HR","hazard risk", "coefficient","beta" or "Beta" word. "SE" 
is standard error and may be named with "se","SE" or "standard error". "p_value" 
is p-value for Ward test/ranksum test, may be named with "pvalue","p_value",
"p-value","p value","PV" or "pv" word. The rows include signature (gene or 
biomarker), age, sex and covariate(s).
}
\value{
output a plot figure.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}\cr
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}
\note{
sdata may contain multiple models such as model1, model2, model3. Model1:
signature, age,sex. Model2: signature, age, sex, smoking. Model 3: signature,
age, sex, smoking, and genotype. User can use empty row to separate these
models.
}

\seealso{
\code{\link{forestplot}}
}

\examples{

#library(forestplot)
data(TCGA_forestplt)
xtick=c(-1,-0.5,0,0.5,1)
#pdf(file="TCGA_survival_forestplot.pdf")
survivalForest(sdata=TCGA_forestplt,xtick=xtick)
#dev.off()
}

\keyword{signature }
\keyword{forestplot}


