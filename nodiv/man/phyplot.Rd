\name{phyplot}
\alias{phyplot}
\title{Plot Phylogenies}
\description{
  This is just the ape::plot.phylo method, which is imported and exported directly by the nodiv package. These functions plot phylogenetic trees on the current graphical
  device.
}
\usage{
  phyplot(x, type = "phylogram", use.edge.length = TRUE,
    node.pos = NULL, show.tip.label = TRUE, show.node.label = FALSE,
    edge.color = "black", edge.width = 1, edge.lty = 1, font = 3,
    cex = par("cex"), adj = NULL, srt = 0, no.margin = FALSE,
    root.edge = FALSE, label.offset = 0, underscore = FALSE,
    x.lim = NULL, y.lim = NULL, direction = "rightwards",
    lab4ut = NULL, tip.color = "black", plot = TRUE,
    rotate.tree = 0, open.angle = 0, node.depth = 1, align.tip.label = FALSE, ...)
}
\arguments{
  \item{x}{an object of class \code{"phylo"} }
  \item{type}{a character string specifying the type of phylogeny to be
    drawn; it must be one of "phylogram" (the default), "cladogram",
    "fan", "unrooted", "radial" or any unambiguous abbreviation of
    these.}
  \item{use.edge.length}{a logical indicating whether to use the edge
    lengths of the phylogeny to draw the branches (the default) or not
    (if \code{FALSE}). This option has no effect if the object of class
    \code{"phylo"} has no `edge.length' element.}
  \item{node.pos}{a numeric taking the value 1 or 2 which specifies the
    vertical position of the nodes with respect to their descendants. If
    \code{NULL} (the default), then the value is determined in relation
    to `type' and `use.edge.length' (see details).}
  \item{show.tip.label}{a logical indicating whether to show the tip
    labels on the phylogeny (defaults to \code{TRUE}, i.e. the labels
    are shown).}
  \item{show.node.label}{a logical indicating whether to show the node
    labels on the phylogeny (defaults to \code{FALSE}, i.e. the labels
    are not shown).}
  \item{edge.color}{a vector of mode character giving the colours used
    to draw the branches of the plotted phylogeny. These are taken to be
    in the same order than the component \code{edge} of \code{phy}. If
    fewer colours are given than the length of \code{edge}, then the
    colours are recycled.}
  \item{edge.width}{a numeric vector giving the width of the branches of
    the plotted phylogeny. These are taken to be in the same order than
    the component \code{edge} of \code{phy}. If fewer widths are given
    than the length of \code{edge}, then these are recycled.}
  \item{edge.lty}{same than the previous argument but for line types;
    1: plain, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, 6: twodash.}
  \item{font}{an integer specifying the type of font for the labels: 1
    (plain text), 2 (bold), 3 (italic, the default), or 4 (bold
    italic).}
  \item{cex}{a numeric value giving the factor scaling of the tip and
    node labels (Character EXpansion). The default is to take the
    current value from the graphical parameters.}
  \item{adj}{a numeric specifying the justification of the text strings
    of the labels: 0 (left-justification), 0.5 (centering), or 1
    (right-justification). This option has no effect if \code{type =
      "unrooted"}. If \code{NULL} (the default) the value is set with
    respect of \code{direction} (see details).}
  \item{srt}{a numeric giving how much the labels are rotated in degrees
    (negative values are allowed resulting in clock-like rotation); the
    value has an effect respectively to the value of
    \code{direction} (see Examples). This option has no effect if
    \code{type = "unrooted"}.}
  \item{no.margin}{a logical. If \code{TRUE}, the margins are set to
    zero and the plot uses all the space of the device (note that this
    was the behaviour of \code{plot.phylo} up to version 0.2-1 of `ape'
    with no way to modify it by the user, at least easily).}
  \item{root.edge}{a logical indicating whether to draw the root edge
    (defaults to FALSE); this has no effect if `use.edge.length = FALSE'
    or if `type = "unrooted"'.}
  \item{label.offset}{a numeric giving the space between the nodes and
    the tips of the phylogeny and their corresponding labels. This
    option has no effect if \code{type = "unrooted"}.}
  \item{underscore}{a logical specifying whether the underscores in tip
    labels should be written as spaces (the default) or left as are (if
    \code{TRUE}).}
  \item{x.lim}{a numeric vector of length one or two giving the limit(s)
    of the x-axis. If \code{NULL}, this is computed with respect to
    various parameters such as the string lengths of the labels and the
    branch lengths. If a single value is given, this is taken as the
    upper limit.}
  \item{y.lim}{same than above for the y-axis.}
  \item{direction}{a character string specifying the direction of the
    tree. Four values are possible: "rightwards" (the default),
    "leftwards", "upwards", and "downwards".}
  \item{lab4ut}{(= labels for unrooted trees) a character string
    specifying the display of tip labels for unrooted trees (can be
    abbreviated): either \code{"horizontal"} where all labels are
    horizontal (the default if \code{type = "u"}), or \code{"axial"}
    where the labels are displayed in the axis of the corresponding
    terminal branches. This option has an effect if \code{type = "u"},
    \code{"f"}, or \code{"r"}.}
  \item{tip.color}{the colours used for the tip labels, eventually
    recycled (see examples).}
  \item{plot}{a logical controlling whether to draw the tree. If
    \code{FALSE}, the graphical device is set as if the tree was
    plotted, and the coordinates are saved as well.}
  \item{rotate.tree}{for "fan", "unrooted", or "radial" trees: the
    rotation of the whole tree in degrees (negative values are
    accepted).}
  \item{open.angle}{if \code{type = "f"} or \code{"r"}, the angle in
    degrees left blank. Use a non-zero value if you want to call
    \code{\link[ape]{axisPhylo}} after the tree is plotted.}
  \item{node.depth}{an integer value (1 or 2) used if branch lengths are
    not used to plot the tree; 1: the node depths are proportional to
    the number of tips descending from each node (the default and was the
    only possibility previously), 2: they are evenly spaced.}
  \item{align.tip.label}{a logical value or an integer. If TRUE, the tips 
    are aligned and dotted lines are drawn between the tips of the tree and
    the labels. If an integer, the tips are aligned and this gives the type
    of the lines (lty).}
  \item{\dots}{further arguments to be passed to \code{plot} or to
    \code{plot.phylo}.}
}
\details{

  The font format of the labels of the nodes and the tips is the same.

  If \code{no.margin = TRUE}, the margins are set to zero and are not
  restored after plotting the tree, so that the user can access the
  coordinates system of the plot.

  The option `node.pos' allows the user to alter the vertical position
  (i.e., ordinates) of the nodes. If \code{node.pos = 1}, then the
  ordinate of a node is the mean of the ordinates of its direct
  descendants (nodes and/or tips). If \code{node.pos = 2}, then the
  ordinate of a node is the mean of the ordinates of all the tips of
  which it is the ancestor. If \code{node.pos = NULL} (the default),
  then its value is determined with respect to other options: if
  \code{type = "phylogram"} then `node.pos = 1'; if \code{type =
    "cladogram"} and \code{use.edge.length = FALSE} then `node.pos = 2';
  if \code{type = "cladogram"} and \code{use.edge.length = TRUE} then
  `node.pos = 1'. Remember that in this last situation, the branch
  lengths make sense when projected on the x-axis.

  If \code{adj} is not specified, then the value is determined with
  respect to \code{direction}: if \code{direction = "leftwards"} then
  \code{adj = 1} (0 otherwise).

  If the arguments \code{x.lim} and \code{y.lim} are not specified by the
  user, they are determined roughly by the function. This may not always
  give a nice result: the user may check these values with the
  (invisibly) returned list (see ``Value:'').

  If you resize manually the graphical device (windows or X11) you may
  need to replot the tree.
}
\note{
  The argument \code{asp} cannot be passed with \code{\dots}.
}
\value{
  \code{plot.phylo} returns invisibly a list with the following
  components which values are those used for the current plot:

  \item{type}{}
  \item{use.edge.length}{}
  \item{node.pos}{}
  \item{node.depth}{}
  \item{show.tip.label}{}
  \item{show.node.label}{}
  \item{font}{}
  \item{cex}{}
  \item{adj}{}
  \item{srt}{}
  \item{no.margin}{}
  \item{label.offset}{}
  \item{x.lim}{}
  \item{y.lim}{}
  \item{direction}{}
  \item{tip.color}{}
  \item{Ntip}{}
  \item{Nnode}{}
  \item{root.time}{}
}
\author{Emmanuel Paradis}
\seealso{
  \code{\link[ape]{plot.phylo}}
}
