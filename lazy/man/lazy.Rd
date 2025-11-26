% ------------------------------------------------------------------------- %
% lazy.Rd                                Lazy learning for local regression %
% ------------------------------------------------------------------------- %
                                                                             
% ========================================================================= %
% Lazy learning for local regression                                        %
% ------------------------------------------------------------------------- %
% Copyright (C) 1999, 2003 Mauro Birattari and Gianluca Bontempi            %
% ========================================================================= %
% This program is free software; you can redistribute it and/or modify it   %
% under the terms of the GNU General Public License as published by the     %
% Free Software Foundation; either version 2 of the License, or (at your    %
% option) any later version.                                                %
%                                                                           %
% This program is distributed in the hope that it will be useful, but       %
% WITHOUT ANY WARRANTY; without even the implied warranty of                %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU         %
% General Public License for more details.                                  %
%                                                                           %
% You should have received a copy of the GNU General Public License along   %
% with this program; if not, write to the Free Software Foundation, Inc.,   %
% 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.                  %
% ========================================================================= %

% ========================================================================= %
%             Mauro Birattari                     Gianluca Bontempi         %
%                IRIDIA                     Departement d'Informatique      %
%    Universite' Libre de Bruxelles       Universite' Libre de Bruxelles    %
%             mbiro@ulb.ac.be                     gbonte@ulb.ac.be          %
% ========================================================================= %

% $Id: lazy.Rd,v 1.11 2003/11/18 15:36:29 mbiro Exp $ %

\name{lazy}
\alias{lazy}
\alias{print.lazy}
\alias{print.summary.lazy}
\alias{summary.lazy}

\title{Lazy learning for local regression}

\description{By combining constant, linear, and quadratic local models,
  \code{lazy} estimates the value of an unknown multivariate function on
  the basis of a set of possibly noisy samples of the function itself.
  This implementation of lazy learning automatically adjusts the
  bandwidth on a query-by-query basis through a leave-one-out
  cross-validation.}

\usage{
lazy(formula, data=NULL, weights, subset, na.action,
        control=lazy.control(\dots), \dots)
}

\arguments{
  \item{formula}{A formula specifying the response and some numeric
          predictors.}

  \item{data}{An optional data frame within which to look first for the
          response, predictors, and weights (the latter will be
          ignored).}

  \item{weights}{Optional weights for each case (ignored).}

  \item{subset}{An optional specification of a subset of the data to be
          used.}

  \item{na.action}{The action to be taken with missing values in the response
          or predictors.  The default is to stop.}

  \item{control}{Control parameters: see \code{\link{lazy.control}}}.

  \item{\dots}{Control parameters can also be supplied directly.}
}

\details{For one or more query points, \code{lazy} estimates the value of
  an unknown multivariate function on the basis of a set of possibly
  noisy samples of the function itself.  Each sample is an input/output
  pair where the input is a vector and the output is a number.  For each
  query point, the estimation of the function is obtained by combining
  different local models.  Local models considered for combination by
  \code{lazy} are polynomials of zeroth, first, and second degree that
  fit a set of samples in the neighborhood of the query point. The
  neighbors are selected according to either the Manhattan or the
  Euclidean distance. It is possible to assign weights to the different
  directions of the input domain for modifying their importance in the
  computation of the distance.  The number of neighbors used for
  identifying local models is automatically adjusted on a query-by-query
  basis through a leave-one-out validations of models, each fitting a
  different numbers of neighbors.  The local models are identified using
  the recursive least-squares algorithm, and the leave-one-out
  cross-validation is obtained through the PRESS statistic.

  As the name \code{lazy} suggests, this function does not do
  anything\ldots apart from checking the options and properly packing
  the data. All the actual computation is done when a prediction is
  request for a specific query point, or for a set of query points: see
  \code{\link{predict.lazy}}.
}

\value{An object of class \code{lazy}.}

\references{
   D.W. Aha (1997) Editorial. \emph{Artificial Intelligence Review},
   \bold{11}(1--5), pp. 1--6. Special Issue on Lazy Learning.

   C.G. Atkeson, A.W. Moore, and S. Schaal (1997) Locally Weighted
   Learning. \emph{Artificial Intelligence Review}, \bold{11}(1--5),
   pp. 11--73. Special Issue on Lazy Learning.

   W.S. Cleveland, S.J. Devlin, and S.J. Grosse (1988) Regression by
   Local Fitting: Methods, Prospectives and Computational
   Algorithms. \emph{Journal of Econometrics}, \bold{37}, pp. 87--114.

   M. Birattari, G. Bontempi, and H. Bersini (1999) Lazy learning meets
   the recursive least squares algorithm. \emph{Advances in Neural
   Information Processing Systems 11}, pp. 375--381. MIT Press.

   G. Bontempi, M. Birattari, and H. Bersini (1999) Lazy learning for
   modeling and control design. \emph{International Journal of Control},
   \bold{72}(7/8), pp. 643--658.

   G. Bontempi, M. Birattari, and H. Bersini (1999) Local learning for
   iterated time-series prediction. \emph{International Conference on
     Machine Learning}, pp. 32--38. Morgan Kaufmann.
 
}

\author{Mauro Birattari and Gianluca Bontempi}

\seealso{\code{\link{lazy.control}}, \code{\link{predict.lazy}}}

\examples{
library("lazy")
data(cars)
cars.lazy <- lazy(dist ~ speed, cars)
predict(cars.lazy, data.frame(speed = seq(5, 30, 1)))
}

\keyword{regression}



