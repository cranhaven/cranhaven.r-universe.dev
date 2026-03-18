\name{triangular_parameters_U}
\encoding{WINDOWS-1252}
\alias{triangular_parameters_U}
\title{Return the parameters of the
fitted triangular distribution of the random variable
"capitalization factor" \eqn{U}.
}
\usage{
triangular_parameters_U(data)
}
\arguments{\item{data}{ A vector of interest rates expressed as percentage. }


}

\description{
Return the parameters of the
fitted triangular distribution of the random variable
"capitalization factor" \eqn{U}.

}

\author{
Salvador Cruz Rambaud, Fabrizio Maturo, Ana María Sánchez Pérez 
}


\examples{

# example 1
data = c(1.77,1.85,1.85,1.84,1.84,1.83,1.85,1.85,1.88,1.85,1.80,1.84,1.91,1.85,1.84,1.85,
1.86,1.85,1.88,1.86)
triangular_parameters_U(data)

}

