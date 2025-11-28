\name{Read.tri.nts}
\alias{Read.tri.nts}
\title{Read 'NTSYSpc' files}

\description{Read a lower triangular matrix}

\usage{Read.tri.nts(file, ...)}

\arguments{
\item{file}{File to read}
\item{...}{Arguments to 'scan()'}
}

\details{
Reads a lower triangular matrix which at least in my practice, typically come from 'NTSYSpc' program.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
write(file=file.path(tempdir(), "tmp.nts"), x=c(
'" Procrustes distances between all pairs:
2 12 12 0
0.000E+000 
4.058E-002 0.000E+000 
5.753E-002 6.489E-002 0.000E+000 
6.445E-002 8.124E-002 9.509E-002 0.000E+000 
2.610E-001 2.395E-001 2.317E-001 3.051E-001 0.000E+000 
2.719E-001 2.508E-001 2.461E-001 3.132E-001 4.531E-002 0.000E+000 
2.563E-001 2.357E-001 2.278E-001 3.008E-001 4.414E-002 6.510E-002 0.000E+000 
8.003E-002 6.611E-002 7.738E-002 9.885E-002 2.206E-001 2.270E-001 2.161E-001 
0.000E+000 
6.838E-002 8.893E-002 6.691E-002 1.018E-001 2.585E-001 2.704E-001 2.497E-001 
1.019E-001 0.000E+000 
6.233E-002 6.756E-002 4.079E-002 8.329E-002 2.396E-001 2.507E-001 2.338E-001 
5.519E-002 5.932E-002 0.000E+000 
2.504E-001 2.313E-001 2.230E-001 2.967E-001 8.714E-002 1.080E-001 6.522E-002 
2.205E-001 2.323E-001 2.281E-001 0.000E+000 
2.590E-001 2.688E-001 2.424E-001 2.757E-001 3.698E-001 3.926E-001 3.689E-001 
3.051E-001 2.280E-001 2.603E-001 3.312E-001 0.000E+000 '
))

## interactive
file.show(file=file.path(tempdir(), "tmp.nts"))

Read.tri.nts(file=file.path(tempdir(), "tmp.nts"), skip=2)
}

\keyword{manip}
