\docType{data}

\name{data.comp}

\alias{data.comp}

\title{Monthly Rentals and Qualitative Characteristics of Computers}

\format{
A data frame with 34 observations on the following four variables:
\tabular{ll}{
\code{rent} \tab monthly rental (in dollar).\cr
\code{mem} \tab memory capacity computed from three different computer characteristics.\cr
\code{access} \tab average time required to access information from memory.\cr
\code{mult} \tab average time required to obtain and complete multiplication instruction.\cr
}
}

\source{
The dataset was originally published by Chow (1967). For the purpose of \pkg{desk} it was imported from 3.5 inch floppy disk in ASCII format included in Berndt (1990). The dataset also available in the original format on \href{https://github.com/BenjaminKay/berndt-econometrics/blob/master/data/floppy_ver/CHAP4.DAT/CHOW}{Github}.
}

\usage{
data.comp
}

\description{
This is a data set on the monthly rentals of computers of different quality during the 1960s.
}

\details{
In Auer et al. (2023, Chaps. 13 & 14) these data are used to illustrate the specification of a multivariate regression model.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).

Chow, G.C. (1967): Technological Change and the Demand for Computers. The American Economic Review, 57, 1117–1130.

Berndt, E.R. (1990): The Practice of Econometrics: Classic and Contemporary. Addison-Wesley, 136-142.
}

\keyword{datasets}
