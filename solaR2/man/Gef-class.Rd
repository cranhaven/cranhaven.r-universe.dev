\name{B4_Gef-class}
\Rdversion{1.1}
\encoding{UTF-8}
\docType{class}
\alias{Gef-class}
\alias{show,Gef-method}

\title{Class "Gef": irradiation and irradiance on the generator plane.}
\description{This class contains the global, diffuse and direct
  irradiation and irradiance on the horizontal plane, and ambient
  temperature. }

\section{Objects from the Class}{
  Objects can be created by the function \code{\link{calcGef}}.
}
\section{Slots}{
  \describe{
    \item{\code{GefI}:}{Object of class \code{data.table} created by
      \code{\link{fInclin}}. It contains these components:
      \describe{
        \item{Bo: }{Extra-atmospheric irradiance on the inclined surface
	  (W/m²)}
	\item{Bn: }{Direct normal irradiance (W/m²)}
        \item{G, B, D, Di, Dc, R: }{Global, direct, diffuse (total, isotropic
          and anisotropic) and albedo irradiance incident on an inclined
          surface (W/m²)}
        \item{Gef, Bef, Def, Dief, Dcef, Ref: }{Effective global, direct,
          diffuse (total, isotropic and anisotropic) and albedo irradiance
          incident on an inclined surface (W/m²)}
        \item{FTb, FTd, FTr: }{Factor of angular losses for the direct,
          diffuse and albedo components}}}

    \item{\code{GefD}:}{Object of class \code{data.table} with daily values
      of global, diffuse and direct irradiation.}

    \item{\code{Gefdm}:}{Object of class \code{data.table} with monthly means
      of daily global, diffuse and direct irradiation.}

    \item{\code{Gefy}:}{Object of class \code{data.table} with yearly sums of
      global, diffuse and direct irradiation.}

    \item{\code{Theta}:}{Object of class \code{data.table} created by
      \code{\link{fTheta}}. It contains these components:
      \describe{
        \item{\code{Beta}: }{numeric, inclination angle of the surface
          (radians). When \code{modeTrk='fixed'} it is the value of the
          argument \code{beta} converted from degreesto radians.}
        \item{\code{Alpha}: }{numeric, azimuth angle of the surface
          (radians). When \code{modeTrk='fixed'} it is the value of the
          argument \code{alpha} converted from degrees to radians.}
        \item{\code{cosTheta}: }{numeric, cosine of the incidence angle of the solar
          irradiance on the surface}
    }}
    \item{\code{iS}:}{numeric, degree of dirtiness.}
    \item{\code{alb}:}{numeric, albedo reflection coefficient.}
    \item{\code{modeTrk}:}{character, mode of tracking.}
    \item{\code{modeShd}:}{character, mode of shadows.}
    \item{\code{angGen}:}{A list with the values of \code{alpha},
      \code{beta} and \code{betaLim}.}
    \item{\code{struct}:}{A list with the dimensions of the structure. }
    \item{\code{distances}:}{A data.frame with the distances between structures.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{G0}"}, directly.
  Class \code{"\linkS4class{Meteo}"}, by class "G0", distance 2.
  Class \code{"\linkS4class{Sol}"}, by class "G0", distance 2.
}
\section{Methods}{
  \describe{
    \item{as.data.tableD}{\code{signature(object = "Gef")}: conversion
      to a data.table with daily values.}
    \item{as.data.tableI}{\code{signature(object = "Gef")}: conversion
      to a data.table with intradaily values.}
    \item{as.data.tableM}{\code{signature(object = "Gef")}: conversion
      to a data.table with monthly values.}
    \item{as.data.tableY}{\code{signature(object = "Gef")}: conversion
      to a data.table with yearly values.}
    \item{indexD}{\code{signature(object = "Gef")}: index of the
      \code{solD} slot. }
    \item{indexI}{\code{signature(object = "Gef")}: index of the
      \code{solI} slot. }
    \item{getLat}{\code{signature(object = "Gef")}: latitude of the inherited
      \code{\linkS4class{Sol}} object. }
    \item{xyplot}{\code{signature(x = "Gef", data = "missing")}: display
      the time series of daily values of irradiation.}
    \item{xyplot}{\code{signature(x = "formula", data = "Gef")}:
      displays the contents of a \code{Gef} object with the
      \code{xyplot} method for formulas.}
  }
}

\references{\itemize{
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2015.
    (\url{https://oscarperpinan.github.io/esf/})
  \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \doi{10.18637/jss.v050.i09}
  }}

\author{
  Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.
}

\seealso{
  \code{\linkS4class{Sol}},
  \code{\linkS4class{G0}}.
}
\keyword{classes}
