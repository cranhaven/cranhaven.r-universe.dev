% ------------------------------------------------------------------------- %
% lazy.control.Rd                           Set parameters fo lazy learning %
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

% $Id: lazy.control.Rd,v 1.1 2003/10/02 16:19:31 mbiro Exp $ %

\name{lazy.control}
\alias{lazy.control}

\title{Set parameters for lazy learning}

\description{Set control parameters for a lazy learning object.}

\usage{
lazy.control(conIdPar=NULL, linIdPar=1, quaIdPar=NULL,
                distance=c("manhattan","euclidean"), metric=NULL,
                   cmbPar=1, lambda=1e+06)
}


\arguments{

  \item{conIdPar}{Parameter controlling the number of neighbors to be used
    for identifying and validating constant models. \code{conIdPar} can assume
    different forms:
    \describe{
      \item{\code{conIdPar=c(idm0,idM0,valM0)}:}{ In this case,
	\code{idm0:idM0} is the range in which the best number of
	neighbors is searched when identifying the local polynomial
	models of degree 0 and where \code{valM0} is the maximum
	number of neighbors used for their validation.  This means
	that the constant models identified with \code{k} neighbors,
	are validated on the first \code{v} neighbors, where
	\code{v=min(k,valM0)}.  If \code{valM0=0}, \code{valM0} is set
	to \code{idMO}: see next case for details.}

      \item{\code{conIdPar=c(idm0,idM0)}:}{ Here \code{idm0} and
	\code{idM0} have the same role as in previous case, and
	\code{valM0} is by default set to \code{idM0}: each model is
	validated on all the neighbors used in identification.}

      \item{\code{conIdPar=p}:}{ Here \code{idmO} and \code{idMO} are
        obtained according to the following formulas: \code{idm0=3}
        and \code{idMX=5*p}. Recommended choice: \code{p=1}. As far as
        the quantity \code{valM0} is concerned, it gets the default
        value as in previous case.}

      \item{\code{conIdPar=NULL}:}{ No constant model is considered.}}}

  \item{linIdPar}{Parameter controlling the number of neighbors to be used
    for identifying and validating linear models. \code{linIdPar} can assume
    different forms:
    \describe{
      \item{\code{linIdPar=c(idm1,idM1,valM1)}:}{ In this case,
	\code{idm1:idM1} is the range in which the best number of
	neighbors is searched when identifying the local polynomial
	models of degree 1 and where \code{valM1} is the maximum
	number of neighbors used for their validation.  This means
	that the linear models identified with \code{k} neighbors, are
	validated on the first \code{v} neighbors, where
	\code{v=min(k,valM1)}.  If \code{valM1=0}, \code{valM1} is set
	to \code{idM1}: see next case for details.}

      \item{\code{linIdPar=c(idm1,idM1)}:}{ Here
	\code{idm1} and \code{idM1} have the same role as in previous
	case, and \code{valM1} is by default set to \code{idM1}: each
	model is validated on all the neighbors used in identification.}

      \item{\code{linIdPar=p}:}{ Here \code{idmO} and \code{idMO} are
        obtained according to the following formulas:
        \code{idm1=3*noPar} and \code{idM1=5*p*noPar}, where
        \code{noPar=nx+1} is the number of parameter of the polynomial
        model of degree 1, and \code{nx} is the dimensionality of the
        input space. Recommended choice: \code{p=1}. As far as the
        quantity \code{valM1} is concerned, it gets the default value
        as in previous case.}

      \item{\code{linIdPar=NULL}:}{ No linear model is considered.}}}

  \item{quaIdPar}{Parameter controlling the number of neighbors to be
    used for identifying and validating quadratic
    models. \code{quaIdPar} can assume different forms:
    \describe{
      \item{\code{quaIdPar=c(idm2,idM2,valM2)}:}{ In this case,
	\code{idm2:idM2} is the range in which the best number of
	neighbors is searched when identifying the local polynomial
	models of degree 2 and where \code{valM2} is the maximum
	number of neighbors used for their validation.  This means
	that the quadratic models identified with \code{k} neighbors, are
	validated on the first \code{v} neighbors, where
	\code{v=min(k,valM2)}.  If \code{valM2=0}, \code{valM2} is set
	to \code{idM2}: see next case for details.}

      \item{\code{quaIdPar=c(idm2,idM2)}:}{ Here
	\code{idm2} and \code{idM2} have the same role as in previous
	case, and \code{valM2} is by default set to \code{idM2}: each
	model is validated on all the neighbors used in identification.}

      \item{\code{quaIdPar=p}:}{ Here \code{idmO} and \code{idMO} are
        obtained according to the following formulas:
        \code{idm2=3*noPar} and \code{idM2=5*p*noPar}, where in this
	case the number of parameters is 
        \code{noPar=(nx+1)*(nx+2)/2}, and \code{nx} is the
        dimensionality of the input space. Recommended choice:
        \code{p=1}. As far as the quantity \code{valM2} is concerned,
        it gets the default value as in previous case.}

      \item{\code{quaIdPar=NULL}:}{ No quadratic model is considered.}}}

  \item{distance}{The distance metric: can be \code{manhattan} or
    \code{euclidean}.}

  \item{metric}{Vector of \code{n} elements. Weights used to evaluate
    the distance between query point and neighbors.}
  
  \item{cmbPar}{Parameter controlling the local combination of
    models. \code{cmbPar} can assume different forms:
    \describe{
      \item{\code{cmbPar=c(cmb0,cmb1,cmb2)}:}{ In this case, \code{cmbX}
         is the number of polynomial models of degree \code{X} that will
         be included in the local combination. Each local model will be
         therfore a combination of \emph{the best} \code{cmb0}
         \emph{models of degree 0, the best} \code{cmb1} \emph{models of
         degree 1, and the best} \code{cmb2} \emph{models of degree 2}
         identified as specified by \code{idPar}.}

      \item{\code{cmbPar=cmb}:}{ Here \code{cmb} is the number of models
         that will be combined, disregarding any constraint on the
         degree of the models that will be considered.  Each local model
         will be therfore a combination of \emph{the best} \code{cmb}
         \emph{models}, identified as specified by \code{id_par}.}}}

  \item{lambda}{Initialization of the diagonal elements of the local
    variance/covariance matrix for Ridge Regression.}
}


\value{The output of \code{lazy.control} is a list containing the
  following components: \code{conIdPar}, \code{linIdPar}, \code{quaIdPar},
  \code{distance}, \code{metric}, \code{cmbPar}, \code{lambda}.
}

\author{Mauro Birattari and Gianluca Bontempi}

\seealso{\code{\link{lazy}}, \code{\link{predict.lazy}}}

\keyword{regression}



