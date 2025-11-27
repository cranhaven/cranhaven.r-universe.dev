\name{ICFCoreSetCWP}
\alias{ICFCoreSetCWP}
\title{ICF core set for chronic widespread pain}
\description{The data set contains observed levels of ICF categories from
the (comprehensive) ICF Core Set for chronic widespread pain (CWP) and a
physical health component summary measure for n = 420 patients.}

\usage{
data(ICFCoreSetCWP)
}

\format{
The data frame has 420 rows and 68 columns. The first 67 columns
contain observed levels of ICF categories from the (comprehensive) ICF Core Set
for chronic widespread pain (CWP). In the last column, the physical health
component summary measure is given. Each row corresponds to one patient
with CWP. ICF categories have discrete ordinal values between 0 and 4
(columns 1 - 50 and 67), or between -4 and 4 (columns 51 - 66). See the given
references for details.
}

\details{
The original data set contained some missing values, which have been imputed 
using R package \code{Amelia}.

The data were collected within the study \emph{Validation of ICF Core Sets for
chronic conditions}, which was a collaboration effort between the ICF Research 
Branch of the collaborating centers for the Family of International
Classifications in German, the Classification, Terminology and standards Team
from the World Health Organization and the International Society for Physical
and Rehabilitation Medicine.

Special thanks go to the following participating study centers:
Ankara University, Turkey; Azienda Ospedaliera di Sciacca, Italy; Donauspital,
Vienna, Austria; Drei-Burgen-Klinik, Bad Muenster, Germany; Edertal Klinik,
Bad Wildungen, Germany; Fachklinik Bad Bentheim, Germany; Hospital das
Clinicas, School of Medicine, University of Sao Paulo, Brazil; Hospital San
Juan Bautista, Catamarca, Argentina; Istituto Scientifico di Montescano, Italy;
Istituto Scientifico di Veruno, Italy; Kaiser-Franz-Josef-Spital, Vienna,
Austria; Klinik am Regenbogen, Nittenau, Germany; Klinik Bavaria Kreischa,
Germany; Klinik Hoher Meissner, Bad Sooden-Allendorf, Germany; Klinikum
Berchtesgadener Land, Schoenau, Germany; Kuwait Physical Medicine and
Rehabilitation Society, Safat, Kuwait; National Institute for Medical
Rehabilitation, Budapest, Hungary; Neuro-Orthopaedisches Krankenhaus und
Zentrum fuer Rehabilitative Medizin Soltau, Germany; Praxis fuer Physikalische
Medizin und Rehabilitation, Goettingen, Germany; Rehabilitationsklinik Seehof
der Bundesversicherungsanstalt fuer Angestellte, Teltow, Germany; Rehaklinik
Rheinfelden, Switzerland; Spanish Society of Rheumatology, Madrid, Spain;
University Hospital Zurich, Switzerland; University of Santo Tomas, Quelonchy,
Philippines.

Most special thanks go to all the patients participating in the study.

If you use the data, please cite the following two references.
}

\references{
Cieza, A., G. Stucki, M. Weigl, L. Kullmann, T. Stoll, L. Kamen, N. Kostanjsek,
and N. Walsh (2004). \emph{ICF Core Sets for chronic widespread pain}. Journal
of Rehabilitation Medicine, Suppl. 44, 63-68.

Gertheiss, J., S. Hogger, C. Oberhauser and G. Tutz (2011). \emph{Selection
of ordinally scaled independent variables with applications to international
classification of functioning core sets}. Journal of the Royal Statistical 
Society C (Applied Statistics), 60, 377-395.
}

\examples{
# load the data
data(ICFCoreSetCWP)

# available variables
names(ICFCoreSetCWP)

# adequate coding of x matrix (using levels 1,2,...)
p <- ncol(ICFCoreSetCWP) - 1
n <- nrow(ICFCoreSetCWP)
add <- c(rep(1,50),rep(5,16),1)
add <- matrix(add,n,p,byrow=TRUE)
x <- ICFCoreSetCWP[,1:p] + add

# make sure that also a coefficient is fitted for levels
# that are not observed in the data
addrow <- c(rep(5,50),rep(9,16),5)
x <- rbind(x,addrow)
y <- c(ICFCoreSetCWP$phcs,NA)

# some lambda values
lambda <- c(600,500,400,300,200,100)

# smoothing and selection
modelICF <- ordSelect(x = x, y = y, lambda = lambda)

# results
plot(modelICF)

# plot a selected ICF category (e.g. e1101 'drugs')
# with adequate class labels
plot(modelICF, whx = 51, xaxt = "n")
axis(side = 1, at = 1:9, labels = -4:4)
}

\keyword{models}
\keyword{regression}
