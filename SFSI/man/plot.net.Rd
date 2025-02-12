\name{Graphical network plot}
\alias{plot.net}
\title{Plotting a network}
\usage{
\method{plot}{net}(x, i = NULL, show.names = FALSE,
        group = NULL, group.shape = NULL,
        set.color = NULL, set.size = NULL,
        axis.labels = TRUE, curve = FALSE,
        bg.color = "white", unified = TRUE, ni = 36,
        line.color = "gray70", line.width = 0.3,
        legend.pos = "right", point.color = "gray20",
        sets = c("Testing","Supporting","Non-active"),
        circle = FALSE, ...)
}
\arguments{
\item{x}{An object of the 'net' class as per the \code{net} function}

\item{i}{(integer vector) Index subjects in rows to be shown in plot. Default \code{i=NULL} will consider all elements in rows}

\item{show.names}{\code{TRUE} or \code{FALSE} to whether show node names given by the row/column names of the matrix used to make the net (see \code{help(net)})}

\item{group}{(data.frame) Column grouping for the subjects}

\item{group.shape}{(integer vector) Shape of each level of the grouping column provided as \code{group}}

\item{bg.color}{(character) Plot background color}

\item{line.color}{(character) Color of lines connecting nodes in rows with those in columns}

\item{line.width}{(numeric) Width of lines connecting nodes in rows with those in columns}

\item{curve}{\code{TRUE} or \code{FALSE} to whether draw curve lines connecting nodes in rows with those in columns}

\item{set.color}{(character vector) Color point of each type of node: row, 'active' column, and 'non-active' column, respectively}

\item{set.size}{(numeric vector) Size of each type of node: row, 'active' column, and 'non-active' column, respectively}

\item{axis.labels}{\code{TRUE} or \code{FALSE} to whether show labels in both axes}

\item{unified}{\code{TRUE} or \code{FALSE} to whether show an unified plot or separated for each individual in 'testing'}

\item{point.color}{(character) Color of the points in the plot}

\item{ni}{(integer) Maximum number of row nodes that are plotted separated as indicated by \code{unified=FALSE}}

\item{legend.pos}{(character) Either "right", topright","bottomleft","bottomright","topleft", or "none" indicating where the legend is positioned in the plot}

\item{sets}{(character vector) Names of the types of node: row, 'active' column, and 'non-active' column, respectively}

\item{circle}{\code{TRUE} or \code{FALSE} to whether draw a circle for each trait in a multi-trait 'SGP'}

\item{...}{Other arguments for method \code{plot}: 'xlab', 'ylab', 'main'}

}
\description{
Plot a Graphical Network obtained from a numeric matrix
}
\details{
Plot a Graphical Network from a matrix where nodes are subjects in the rows and columns, and edges are obtained from the matrix entries. This Network is obtained using \code{net} function 
}
\examples{
  # See examples in
  # help(net, package="SFSI")
}

