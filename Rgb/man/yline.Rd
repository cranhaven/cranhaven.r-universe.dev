\name{yline}
\alias{yline}
\title{
  Compute Y line to avoid collision
}

\description{
  This function handles the collision between objects to be drawn, also accounting for labels. It is provided as a convenient component of custom drawing functions, and is currently in use in \code{\link{draw.boxes}} and \code{\link{draw.steps}}.
}

\usage{
  yline(boxes, start, end, label, labelStrand, labelCex, labelSrt, labelAdj,
    labelOverflow, maxDepth)
}

\arguments{
  \item{boxes}{
    A \code{data.frame} with "start.plot" and "end.plot" integer columns defining the boundaries of the graphical elements to be drawn and a "label" character column. In a typical use case, they correspond to the "start", "end" and "name" columns from the \code{slice} to be drawn.
}
  \item{start}{
    Single integer value, the left boundary of the window, in base pairs.
}
  \item{end}{
    Single integer value, the right boundary of the window, in base pairs.
}
  \item{label}{
    Single logical value, whether to print labels on boxes or not.
}
  \item{labelStrand}{
    Single logical value, whether to add the strand at the end of labels or not.
}
  \item{labelCex}{
    Single numeric value, character expansion factor for labels.
}
  \item{labelSrt}{
    Single numeric value, string rotation angle for labels.
}
  \item{labelAdj}{
    'left', 'right' or 'center', the horizontal adjustment of the labels on the boxes.
}
  \item{labelOverflow}{
    Single logical value, whether to write labels on boxes too narrow to host them or not.
}
  \item{maxDepth}{
    Single integer value, the maximum amount of box heights allowed on the plot to avoid overlaps (if exhausted an error message will be ploted, turning \code{label} to \code{FALSE} might help).
}
}

\value{
  Returns \code{boxes} with an additional "yline" integer column defining the y coordinate at which the box should be drawn to avoid collision. If an error occurs, a simpleError object will be returned instead and the drawing should be aborted (see \code{\link{draw.boxes}} code for a functional example).
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.boxes}}, \code{\link{draw.steps}}
}

