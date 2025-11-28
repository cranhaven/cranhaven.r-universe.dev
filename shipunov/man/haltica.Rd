\name{haltica}
\alias{haltica}

\title{Haltica flea beetles}

\usage{haltica}

\format{
These data frame contains the following columns:
\describe{
\item{\code{Species}}{Species epithet}
\item{\code{No}}{Number of sample (see below)}
\item{\code{x5}}{The distance of the transverse groove from the posterior border of the prothorax, in microns}
\item{\code{x14}}{The length of the elytram, in 0.01 mm}
\item{\code{x17}}{The length of the second antennal joint, in microns}
\item{\code{x18}}{The length of the third antennal joint, in microns}
}
}

\description{
Lubischew data (1962, pp. 460--461, table 2): 39 Haltica flea beetles specimens which belong to two cryptic species.

Sources of specimens:

Haltica oleracea:

1, 2, 3, 4, 6: Western Europe (Germany, France, Italy);
5: Leningrad;
7, 8: Perm;
11, 12: Kiev;
15, 16: Middle Volga (Kuibyshev);
17: Orel district, Middle Russia;
9, 10: Northern Caucasus;
13, 14, 18, 19: Transcaucasia (Delizhan, Akstafa).

H. carduorum:

6: Northern Russia (Elabuga);
1, 5, 9, 10, 11, 12, 14, 16, 20: Middle Russia (Penza, Orel, Voronezh districts);
3, 4, 17; Northern Caucasus;
2, 15, 18, 19: Black Sea Coast of the Caucasus;
13: Transcaucasia (Armenia);
7, 8: Middle Asia (Schachriziabs).
}

\source{
Lubischew A.A. 1962. On the use of discriminant functions in taxonomy. Biometrics. 18:455--477.
}

\examples{
plot(prcomp(haltica[, -(1:2)])$x[, 1:2], col=haltica$Species)

haltica.qj <- Classproj(haltica[, -(1:2)], haltica$Species, method="QJ")
plot(haltica.qj$proj, col=haltica$Species)
text(haltica.qj$centers, levels(haltica$Species), col=1:2)
}

\keyword{datasets}
