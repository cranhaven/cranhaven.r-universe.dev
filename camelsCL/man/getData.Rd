%% File getData.Rd
%% Part of 'camelsCL' R package, https://gitlab.com/hzambran/camelsCL ; 
%%                               https://cran.r-project.org/package=camelsCL
%% Copyright 2024-2025 Hector Garces-Figueroa and Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{getData}
\Rdversion{1.1}
\alias{getData}
\alias{getData.default}

\title{
getData
}
\description{
Download data for a specific catchment from the CAMELS-CL dataset.
}
\usage{
%getData(x, ...)
getData(x, tscale, from, to, na.rm, na.rm.max, include.shp, include.attr,
        include.extra.P, verbose)

\method{getData}{default}(x, tscale=c("daily", "monthly", "annual"),
           from="auto", to="auto", na.rm=TRUE, na.rm.max=0, include.shp=FALSE,
           include.attr=FALSE, include.extra.P=FALSE, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric, indicating the ID of the catchment for which all the data will be 
downloaded
}
  \item{tscale}{
Character, indicating the temporal scale to be used for the output time series.
Valid values of \code{tscale} are:

1) \kbd{"daily"}: All the output time series have a daily temporal frequency, 
directly read from the CAMELS-CL dataset.

2) \kbd{"monthly"}: All the output time series have a monthly temporal frequency,
aggregated from the original daily time series in the CAMELS-CL dataset.

3) \kbd{"annual"}: All the output time series have an annual temporal frequency,
aggregated from the original daily time series in the CAMELS-CL dataset.
}
  \item{from}{
Date, indicating the starting date of the time series to be 
returned. Its default value is \code{"auto"}, which automatically selects the first 
date with at least one non-missing value among all variables.
}
\item{to}{
Date, indicating the ending date of the time series to be 
returned. Its default value is \code{"auto"}, which automatically selects the 
last date with at least one non-missing value among all variables.
}
  \item{na.rm}{
Logical. Should missing values be removed? \cr
-) \kbd{TRUE} : the monthly/annual values are computed only for months/years 
with a percentage of missing values less than \code{na.rm.max} \cr
-) \kbd{FALSE}: if there is AT LEAST one NA within a month/year, the corresponding 
monthly/annual values in the output object will be \code{NA}.
}
  \item{na.rm.max}{
Numeric in [0, 1]. It is used to define the maximum percentage of missing values 
allowed in each month/year to keep the temporally aggregated value (i.e., monthly/annual) 
in the output object of this function. In other words, if the percentage of missing 
values in a given month/year is larger or equal than \code{na.rm.max} the corresponding 
monthly or annual value will be \code{NA}.
}
  \item{include.shp}{
Optional. Logical, indicating whether to include a shapefile of the catchment in 
the function's output. Its default value is \kbd{FALSE}. 
}
  \item{include.attr}{
Optional. Logical, indicating whether to include all the attributes of the catchment 
in the function's output. Its default value is \kbd{FALSE}. 
}
  \item{include.extra.P}{
Optional. Logical, indicating whether to include all the
additional precipitation datasets available in the CAMELS-CL database
(i.e., MSWEP, CHIRPSv2, and TMPA). Its default value is \kbd{FALSE}.
}
  \item{verbose}{
Logical; if \kbd{TRUE}, progress messages are printed.
}
%  \item{\dots}{
%additional arguments, not used yet.
%}
}
\details{
Currently it works with CAMELS-CL version '2022 enero'.
}
\value{
The daily time series output contains a single zoo object with the following columns:

1) \kbd{P_mm}: precipitation obtained fromt the CR2MET dataset, [mm/day].

2) \kbd{Tmin_degC}: minimum air temperature obtained from CR2METv2 dataset, [mm/day].

3) \kbd{Tavg_degC}: average air temperature obtained from CR2METv2 dataset, [mm/day].

4) \kbd{Tmax_degC}: maximum air temperature obtained from CR2METv2 dataset, [mm/day].

5) \kbd{PET_mm}: potential evapotranspiration computed with the Hargreaves-Samani 
equation, [mm/day].

6) \kbd{Qobs_mm}: streamflow obtained from 'Direccion General de Aguas (DGA)', [mm/day].

7) \kbd{Qobs_m3s}: streamflow obtained from 'Direccion General de Aguas (DGA)', [m3/s].

If \code{include.extra.P = TRUE}, the following columns will also be included:

8) \kbd{P_CHIRPSv2_mm}: precipitation obtained from Climate Hazards Group InfraRed 
Precipitation with Station (CHIRPS) version 2 dataset, [mm/day].

9) \kbd{P_MSWEP_mm}: precipitation obtained from Multi-Source Weighted-Ensemble 
Precipitation (MSWEP) v1.1 dataset, [mm/day].

10) \kbd{P_TMPA_mm}: precipitation obtained from TMPA 3B42v7 dataset, [mm/day].


}
\references{
\url{https://camels.cr2.cl}

\cite{Alvarez-Garreton, C.; Mendoza, P. A.; Boisier, J. P.; Addor, N.; Galleguillos, 
M.; Zambrano-Bigiarini, M.; Lara, A.; Puelma, C.; Cortes, G.; Garreaud, R.; McPhee, J.; 
and Ayala, A. (2018). The CAMELS-CL dataset: catchment attributes and meteorology 
for large sample studies - Chile dataset, Hydrology and Earth System Sciences 22, 
5817-5846. doi:10.5194/hess-22-5817-2018.}
}
\author{
Hector Garces-Figueroa <hegarcesf@gmail.com> and Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}


\examples{
##################
\donttest{
# Example 1: Getting only daily time series data for the 'Rio Trancura Antes Rio Llafenco'
# catchment
x1 <- getData(9414001) 
head(x1)

# Example 2: Getting only daily time series of P [mm/day], Tmin [degC], Tavg [degC],
# Tmax [degC], PET [mm/day] and Q [mm/day] data for the 'Rio Trancura Antes Rio Llafenco'
# catchment
var.names <- c("P_mm_day", "Tmin_degC_day", "Tavg_degC_day", 
               "Tmax_degC_day", "PET_mm_day", "Qobs_mm_day")
x2 <- getData(9414001)
x2 <- x2[, var.names]
names(x2) <- c("P", "Tmin", "Tavg", "Tmax", "PET", "Q")

# Example 3: Getting only daily time series and catchment borders data for the 
# 'Rio Trancura Antes Rio Llafenco' catchment
x3 <- getData(9414001, include.shp=TRUE)
plot(x3[["CatchmentBorders"]]) 

# Example 4: Getting only daily time series and catchment attributes data for the 
# 'Rio Trancura Antes Rio Llafenco' catchment
x4 <- getData(9414001, include.attr=TRUE) 
head(x4[["CatchmentAttributes"]])

# Example 5: Getting daily time series, catchment borders and catchment attributes data for the 
# 'Rio Trancura Antes Rio Llafenco' catchment
x5 <- getData(9414001, include.shp=TRUE, include.attr=TRUE) 
head(x5[["CatchmentTS"]])
plot(x5[["CatchmentBorders"]]) 
head(x5[["CatchmentAttributes"]])

# Example 6: Getting daily time series, catchment borders, catchment attributes and additional
# precipitation datasets for the 'Rio Trancura Antes Rio Llafenco' catchment
x6 <- getData(9414001, include.shp=TRUE, include.attr=TRUE,
              include.extra.P=TRUE)
head(x6[["CatchmentTS"]])
plot(x6[["CatchmentBorders"]]) 
head(x6[["CatchmentAttributes"]])

# Example 7: Getting only monthly time series data for the 'Rio Trancura Antes Rio Llafenco'
# catchment
x7 <- getData(9414001, tscale="monthly") 
head(x7)

# Example 8: Getting only annual time series data for the 'Rio Trancura Antes Rio Llafenco'
# catchment
x8 <- getData(9414001, tscale="annual")
head(x8)

# Example 9: Getting only annual time series data between 2008 and 2015 for the
# 'Rio Trancura Antes Rio Llafenco' catchment
x9 <- getData(9414001, tscale="annual", from = as.Date("2008-01-01"),
              to = as.Date("2015-12-31"))
head(x9)

# Example 10: Getting only monthly time series data for the 'Rio Mapocho en Los Almendros'
# and 'Rio Trancura Antes Rio Llafenco' catchments

target.ids <- c(5722002, 9414001)
nelements  <- length(target.ids)
x10 <- vector(mode = "list", length = nelements)

for (i in seq_len(nelements)) {
  x10[[i]] <- getData(target.ids[[i]], tscale="monthly") 
}
str(x10)
}
}

\keyword{ data }
