\encoding{latin1}
\name{ads-internal}
\alias{adjust.marks.size}
\alias{area.poly}
\alias{convert}       
\alias{convert2}
\alias{in.circle}
\alias{in.poly}
\alias{in.rectangle}
\alias{in.triangle}
\alias{is.poly} 
\alias{overlap.poly}
\alias{overlap.trapez}
\alias{overlapping.polygons}
\alias{print.fads}
\alias{print.fads.k12fun}          
\alias{print.fads.kfun}
\alias{print.fads.kp.fun}
\alias{print.fads.kpqfun}
\alias{print.fads.kmfun}
\alias{print.fads.ksfun}
\alias{print.fads.krfun}
\alias{print.vads}
\alias{print.vads.dval}
\alias{print.vads.k12val}
\alias{print.vads.kval}
\alias{read.tri}
\alias{sortmat}
\alias{summary.vads}
\alias{summary.vads.dval}
\alias{summary.vads.k12val}
\alias{summary.vads.kval}
\alias{testIC}
\alias{testInteger}
\alias{transpose}
\alias{subsetdist}       
\title{Internal ads functions}
\description{
  Internal \code{ads} functions.
}
\usage{
adjust.marks.size(marks,window,maxsize=NULL)
area.poly(xp, yp)
convert(x)
convert2(x)
in.circle(x, y, x0, y0, r0, bdry=TRUE)
in.poly(x, y, poly, bdry=FALSE)
in.rectangle(x, y, xmin, ymin, xmax, ymax, bdry=TRUE)
in.triangle(x, y, ax, ay, bx, by, cx, cy, bdry=TRUE)
is.poly(p)
overlap.poly(P, Q)
overlap.trapez(xa, ya, xb, yb, verb=FALSE)
overlapping.polygons(listpoly)
\method{print}{fads}(x,\dots)
\method{print}{vads}(x,\dots)
read.tri(X)
\method{summary}{vads}(object,\dots)
sortmat(dis,ind)
subsetdist(dis,sub)
testIC(nbSimu, lev)
testInteger(i)
transpose(x, y)
}
\details{
  These are usually not to be called by the user.
}
\keyword{internal}
