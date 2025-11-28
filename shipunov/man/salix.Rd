\name{salix_leaves}
\alias{salix_leaves}
\alias{salix_plants}
\alias{salix_pop}

\title{salix_leaves}

\description{
Morphometry on willows (Salix).

Three files (datasets):
'salix_pop' localities,
'salix_plants' measures on whole plants,
'salix_leaves' measures on leaves from from these plants.
}

\usage{
salix_leaves
}

\format{
These data frames contain the following columns:
\describe{
\item{\code{POP}}{Location ID}
\item{\code{WHERE}}{Geography}
\item{\code{SPECIES}}{Species}
\item{\code{PLN}}{Plant ID}
\item{\code{HEIGHT}}{Height, m}
\item{\code{SEX}}{Plant sex (willows are dioeciuos)}
\item{\code{PID}}{Shoot ID}
\item{\code{N.CIRCLES}}{Number of circles of the imaginary spiral between two leaves (below)}
\item{\code{N.LEAVES}}{Number of leaves between the chosen one and the next in the same position}
\item{\code{INTERNODE}}{Internode length, average, mm}
\item{\code{DIAM}}{Stem diameter in the middle of shoot, mm}
\item{\code{NL}}{Leaf ID}
\item{\code{LL}}{Maximal length of the leaf, mm (along midvein from blade basement to blade top)}
\item{\code{LW}}{Maximal width of the leaf, mm}
\item{\code{PW}}{Position of maximal width, mm (along midvein)}
\item{\code{PTL}}{Length of the petiole, mm (from the place of attachment to blade base)}
\item{\code{STPL}}{Stipules present?}
\item{\code{SL}}{Maximal width of maximal stipule, mm (0 if no stipule present)}
\item{\code{SW}}{Maximal width of maximal stipule, mm (0 if no stipule present)}
\item{\code{TL}}{Length of maximal marginal tooth, mm (0 in no teeth)}
\item{\code{ADC}}{Color of the adaxial (upper) leaf surface: 1  glaucous, 2 other shades of green}
\item{\code{ABC}}{Color of the abaxial (lower) leaf surface: 1  glaucous, 2 other shades of green}
\item{\code{ADP}}{Pubescence of the adaxial (upper) leaf surface under magnification: 1 absent, 2 rare (epidermis surface visible), 3 dense (epidermis surface is not visible or barely visible)}
\item{\code{ABP}}{Pubescence of the abaxial (lower) leaf surface under magnification: 1 absent, 2 rare (epidermis surface visible), 3 dense (epidermis surface is not visible or barely visible)}
}
}

% \source{
% ...
% }

% \examples{
% ...
% }

\keyword{datasets}
