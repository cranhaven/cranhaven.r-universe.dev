% ------------------------------------------------------------------------- %
% predict.lazy.Rd                         Predict method for lazy learning  %
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

% $Id: predict.lazy.Rd,v 1.3 2003/12/09 15:32:27 mbiro Exp $ %

\name{predict.lazy}
\alias{predict.lazy}

\title{Predict method for lazy learning}

\description{Obtains predictions from a lazy learning object}

\usage{%
\method{predict}{lazy}(object, newdata=NULL,
          t.out=FALSE, k.out=FALSE,
            S.out=FALSE, T.out=FALSE, I.out=FALSE, \dots)
}

\arguments{
  \item{object}{Object of class inheriting from \code{lazy}.}

  \item{newdata}{Data frame (or matrix, vector, etc\ldots) defining of the
    query points for which a prediction is to be produced.}
  
  \item{t.out}{Logical switch indicating if the function should return
    the parameters of the local models used to perform each estimation.}

  \item{k.out}{Logical switch indicating if the function should return
    the number of neighbors used to perform each estimation.}

  \item{S.out}{Logical switch indicating if the function should return
    the estimated variance of the prediction suggested by \bold{all} the
    models identified for each query point.}

  \item{T.out}{Logical switch indicating if the function should return
    the parameters of \bold{all} the models identified for each query point.}
  
  \item{I.out}{Logical switch indicating if the function should return
    the index \code{i} of all the samples \code{(X[i,],Y[i])} used to
    perform each estimation.}

  \item{\dots}{Arguments passed to or from other methods.}
}

\value{The output of the method is a list containing the following
  components:
  
  \item{h}{Vector of \code{q} elements, where \code{q} is the number of
    rows in \code{newdata}, i.e. the number of query points. The element
    in position \code{i} is the estimate of the value of the unknown function
    in the query point \code{newdata[i,]}.  The component \code{h} is
    always returned.}
  
  \item{t}{Matrix of \code{z*q} elements, where \code{z=z2} i.e., number of
    parameters of a quadratic model if at least one model of degree 2
    was identified (see \code{quaIdPar} in \code{\link{lazy.control}}),
    otherwise \code{z=z1} i.e.,
    number of parameters of a linear model if at least one model of
    degree 1 was identified (see \code{linIdPar} in
    \code{\link{lazy.control}}), or \code{z=1} if only
    models of degree 0 where considered. In the general case,
    the elements of the vector \code{t[,j]=c(a0, a1,\ldots, an, a11,
      a12,\ldots, a22, a23,\ldots, a33, a34,\ldots, ann)} are
    the parameters of the local  model used for estimating
    the function in the \code{j}th query point: the cross-terms terms
    \code{a11,a12,\ldots,ann} wil be missing if no quadratic model is
    identified and the terms \code{a1,\ldots,an}, will be missing if
    no linear model is identified. If, according to \code{cmbPar} (see
    \code{\link{lazy.control}}), estimations are to be performed by a
    combination of models, the elements of \code{t[,j]} are a weighted
    average of the parameters
    of the selected models where the weight of each model is the
    inverse of the a leave-one-out estimate of the variances of the
    model itself. \bold{REMARK:} a translation of the axes is considered 
    which centers all the local models in the respective query point.}
  
  \item{k}{Vector of \code{q} elements. Selected number of neighbors
    for each query point. If, according to \code{cmbPar} (see
    \code{\link{lazy.control}}), a local
    combination of models is considered, \code{k[j]} is the largest
    value among the number of neighbors used by the selected models
    for estimating the value in the \code{j}th query point.}
  
  \item{S}{List of up to 3 components: Each component is a matrix
    containing an estimate, obtained through a leave-one-out
    cross-valication, of the variance of local models.
    \describe{
      \item{\code{con}}{Matrix of \code{idM0*q} elements, where
        \code{idM0} is the maximum number of neighbors used to fit local
        polynomial models of degree 0 (see \code{\link{lazy.control}}):
        Estimated
        variance of \bold{all} the constant
        models identified for each query point. If no constant model
	is identified (see \code{conIdPar} and \code{cmbPar} in
        \code{\link{lazy.control}}) \code{S$con} is not returned.}
      \item{\code{lin}}{Matrix of \code{idM1*q} elements, where
        \code{idM1} is the maximum number of neighbors used to fit local
        polynomial models of degree 1 (see \code{\link{lazy.control}}):
        Estimated
        variance of \bold{all} the linear
        models identified for each query point. If no linear model
	is identified (see \code{linIdPar} and \code{cmbPar} in
        \code{\link{lazy.control}}) \code{S$lin} is not returned.}
      \item{\code{qua}}{Matrix of \code{idM2*q} elements, where
        \code{idM1} is the maximum number of neighbors used to fit local
        polynomial models of degree 1 (see \code{\link{lazy.control}}):
        Estimated variance of \bold{all} the quadratic
        models identified for each query point. If no quadratic model
	is identified (see \code{quaIdPar} and \code{cmbPar} in
        \code{\link{lazy.control}}) \code{S$qua} is not returned.}}
    The component \code{S} is returned only if \code{S.out=TRUE} in
    the function call.}
  
  \item{T}{List of up to 3 components:
    \describe{
      \item{\code{con}}{Array of \code{z0*idM0*q} elements, where
	\code{z0=1} is the number of parameters of a model of degree
	0. The element \code{T$con[1,i,j]=a0} is the single parameter of
	the local model identified on \code{i} neighbors of the
	\code{q}th query point.}
      \item{\code{lin}}{Array of \code{z1*idM1*q} elements where,  if
	\code{n} is the dimensionality of the input space,
	\code{z1=n+1} is the number of parameter of a model of degree
	1. The vector \code{T$lin[,i,j]=c(a0,a1,\ldots,an)} is the
	vector of parameters of
	the local model identified on \code{i} neighbors of the
	\code{q}th query point. In particular, \code{a0} is the
	constant term, \code{a1} is the parameter associated with the
	first input variable and so on.}
      \item{\code{qua}}{Array of \code{z2*idM2*q} elements where, if
	\code{n} is the dimensionality of the input space,
	\code{z2=(n+1)*(n+2)/2} is the number of parameter of a model
	of degree 2. The vector
	\code{T$qua[,i,j]=c(a0, a1,\ldots, an, a11, 
	  a12,\ldots, a22, a23,\ldots, a33, a34,\ldots, ann)}
	is the vector of parameters of the local quadratic model
	identified on \code{i} neighbors of the \code{q}th query
	point. In particular, \code{a0,\ldots,a1} are the constant and
	liner parameters as in \code{T$lin}, while
	\code{a11,a12,\ldots,ann} are the quadratic ones: \code{a11}
	is associated with the quadratic term \code{x1^2}, \code{a12}
	with the cross-term \code{x1*x2}, and so on.}}
    \bold{REMARK:} a translation of the axes is considered 
    which centers all the local models in the respective query
    point. The component \code{T} is returned only if
    \code{T.out=TRUE} in the function call.}
  
  \item{I}{Matrix of \code{idM*q} elements, where \code{idM} is the
    largest of \code{idM0}, \code{idM1}, and \code{idM2}. Contains the
    index of the neighbors of each query point in \code{newdata}.
    In particular, \code{I[i,j]} is the \code{i}th nearest neighbor of
    the \code{q}th query point.}
}

\author{Mauro Birattari and Gianluca Bontempi}

\seealso{\code{\link{lazy}}, \code{\link{lazy.control}}}

\examples{
library("lazy")
data(cars)
cars.lazy <- lazy(dist ~ speed, cars)
predict(cars.lazy, data.frame(speed = seq(5, 30, 1)))
}


\keyword{regression}
