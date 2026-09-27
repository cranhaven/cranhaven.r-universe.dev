% $Author: sinnwell $ 
% $Date: 2011/10/26 15:03:59 $ 
% $Header: /projects/genetics/cvs/cvsroot/mgenet/man/Ginv.Rd,v 1.2 2011/10/26 15:03:59 sinnwell Exp $ 
% $Locker:  $ 
% $Log: Ginv.Rd,v $
% Revision 1.2  2011/10/26 15:03:59  sinnwell
% take out unused sections in Rd
%
% Revision 1.1  2011/04/28 19:49:59  sinnwell
% Ginv.Rd
%
% Revision 1.2  2008/01/08 20:26:29  sinnwell
% change comment symbol
%
% Revision 1.1  2008/01/08 19:52:27  sinnwell
% Initial revision
% 
%revision 1.9
%date: 2006/08/07 15:37:41;  author: sinnwell;  state: Exp;  lines: +6 -5
%add eps to <usage>, fix a few comments
%----------------------------
%revision 1.8
%date: 2006/04/26 13:44:50;  author: sinnwell;  state: Exp;  lines: +3 -0
%add eps
%----------------------------
%revision 1.7
%date: 2006/03/07 22:01:36;  author: sinnwell;  state: Exp;  lines: +4 -11
%made example runable
%----------------------------
%revision 1.6
%date: 2005/03/18 17:06:10;  author: sinnwell;  state: Exp;  lines: +10 -7
%notify use of LAPACK instead of LINPACK
%
%revision 1.5
%date: 2004/03/01 21:08:28;  author: sinnwell;  state: Exp;  lines: +8 -6
%comment example
%
%revision 1.4
%date: 2003/03/12 20:46:25;  author: sinnwell;  state: Exp;  lines: +3 -7
%remove sgml comments
%
%revision 1.3
%date: 2003/03/07 22:15:57;  author: sinnwell;  state: Exp;  lines: +4 -4
%add keywords and fix syntax
%----------------------------
%revision 1.2
%date: 2003/03/07 22:10:01;  author: sinnwell;  state: Exp;  lines: +9 -6
%fix keywords
%----------------------------

\name{Ginv}
\alias{Ginv}
\title{
  Compute Generalized Inverse of Input Matrix
}
\description{
Singular value decomposition (svd) is used to compute a
generalized inverse of input matrix.
}
\usage{
Ginv(x, eps=1e-6)
}
\arguments{

\item{x}{
A matrix.
}
\item{eps}{
minimum cutoff for singular values in svd of x
}
}
\value{
List with components:

\item{Ginv}{
Generalized inverse of x.
}
\item{rank}{
Rank of matrix x.
}
}
\details{
The svd function uses the LAPACK standard library to compute the 
singular values of the input matrix, and the rank of the matrix is 
determined by the number of singular values  that  are  at  least  as
large   as  max(svd)*eps,  where  eps  is  a  small  value.  
For S-PLUS, the Matrix library is required (Ginv loads Matrix if not already
done so).
}
\section{References}{
Press  WH,  Teukolsky  SA,  Vetterling  WT,  Flannery  BP.
Numerical  recipes  in C. The art of scientific computing.
2nd ed. Cambridge University Press,  Cambridge.1992.  page
61.

Anderson, E., et al. (1994). LAPACK User's Guide, 
2nd edition, SIAM, Philadelphia.
}
\seealso{
svd
}
\examples{
# for matrix x, extract the generalized inverse and 
# rank of x as follows
    x <- matrix(c(1,2,1,2,3,2),ncol=3)
    save <- Ginv(x)
    ginv.x <- save$Ginv
    rank.x <- save$rank
}
\keyword{matrix}
% docclass is function
% Converted by Sd2Rd version 37351.
