\name{plot.select}
\alias{plot.select}

\title{ Plot function for S3 class "select" }

\description{
Plot the optimal graph by model selection
}

\usage{
\method{plot}{select}(x, vis= NULL, xlab= NULL, ylab= NULL, n.mem= NULL, vertex.label= FALSE
, ..., layout= NULL, label.vertex= "all", vertex.size= NULL, vertex.color= NULL,
edge.color= "gray29", sel.nod.label= NULL, label.size = NULL, w.btw= 800,
 w.within = 10, sign.edg= TRUE, edge.width= NULL, edge.label= NULL,
max.degree= NULL, layout.tree= NULL, root.node= NULL, degree.node= NULL,
curve= FALSE, delet.v =TRUE, pos.legend= "bottomleft", cex.legend= 0.8,
iterl = NULL, temp = NULL, tk.width = NULL, tk.height= NULL)
}

\arguments{
	\item{x}{An object with S3 class "select" }
	\item{vis}{Visualizing the results as a graph (network) or as a matrix. There are 4 options to visulize the selected graph: (i) "CI": plotting conditional independence (CI) relationships between variables, (ii) "interactive": plotting the conditional independence network, where opens a new windows with interactive graph drawing facility, and (iii) "parcor.network": plots the estimated graph based on partial correlation values. (iv) "parcor.interactive": plots the estimated graph based on partial correlation matrix with an interactive graph drawing facility. Default is "CI". \cr
Also, there are 3 options to visulaze the selected graph as a matrix: (i) \code{vis}= "image.parcorMatrix" plots the image of partial correlation matrix, (ii)  \code{vis} = "image.adj" draws the adjacency matrix (only presence and absence of links), (iii) \code{vis} = "image.precision" plots the selected precision matrix.}
	\item{xlab}{ONLY applicable when \code{vis = "CI"}, "image.parcorMatrix", "image.adj", or "image.precision".}
	\item{ylab}{ONLY applicable when \code{vis = "CI"}, "image.parcorMatrix", "image.adj", or "image.precision".}
	\item{n.mem}{A vector of memberships. For example, the CviCol dataset, which is provided in the package, contain 5 chromosomes which the total number of markers is \eqn{p = 90}, where the first 24 markers belong into chromosome 1, the next 14 markers into chromosome 2, ..., and chromosome 5 contains 19 markers. Thus, n.mem = c(24,14,17,16,19). If \code{n.mem = NULL} and \code{vis = "CI"} all vertices are coloured the same.}
	\item{vertex.label}{ONLY applicable when \code{vis}= "CI". Assign names to the vertices. Default is FALSE.}
	\item{\dots}{ONLY applicable when \code{vis}= "CI". System reserved (No specific usage)}
	\item{layout}{ONLY applicable when \code{vis}= "interactive" or "parcor.network". The layout specification. Some graph layouts examples: layout_with_fr, layout_in_circle, layout_as_tree, and layout.fruchterman.reingold. The default layout is layout_with_fr.}
	\item{label.vertex}{ONLY applicable when \code{vis}= "interactive". Assign names to the vertices. There are three options: "none", "some", "all". Specify "none" to omit vertex labels in the graph; using \code{label.vertex} = "some" you must provide a vector of vertex IDs or a single vertex ID to the \code{sel.label} argument, which you would like to be shown in the graph. Specify "all" to include all vertex labels in the graph. Default is "all".}
	\item{vertex.size}{ Optional. The size of vertices in the graph visualization. The default value is 7.}
	\item{vertex.color}{ONLY applicable when \code{vis}= "interactive" or "parcor.network". Optional vector (or a color name) giving the colors of the vertices. The default is "red" }
	\item{edge.color}{ONLY applicable when \code{vis}= "interactive". Optional. The default is "gray".}
	\item{sel.nod.label}{ONLY applicable when \code{vis}= "interactive" or "parcor.network". A vector of vertex IDs or a single vertex ID, which you would like to be shown in the graph. ONLY applicable when \code{label.vertex}="some".}
	\item{label.size}{ONLY applicable for \code{vis}= "interactive" or \code{vis}= "parcor.network". The font size of the vertex labels.}
	\item{w.btw}{Distance between nodes from different memberships of \code{n.mem} in layout.}
	\item{w.within}{Distance of nodes within one membership of \code{n.mem} in layout.}
	\item{sign.edg}{Optional. ONLY applicable when \code{vis}= "parcor.network". If \code{TRUE} then edges are colored as red and blue, where red stands for positive and blue negative partial correlation values. If \code{FASLE} all edeges are colored as gray. Default is TRUE.}
	\item{edge.width}{Optional. ONLY applicable when \code{vis}= "parcor.network". Based on the strength of partial correlation values, edges will shown with different line type. Default is FALSE.}
	\item{edge.label}{Optional. ONLY applicable when \code{vis}= "parcor.network". If TRUE then the partial correlation values will be shown on top of each edge. Default is FALSE.}
	\item{max.degree}{Optional. ONLY applicable when \code{vis}= "parcor.network". A number showing degree of a node. This can be used to print those vertex labels that the correspondence vertex have at least e.g. 1 degree.}
	\item{layout.tree}{Optional. ONLY applicable when \code{vis}= "parcor.network". If TRUE then it uses \code{layout_as_tree} from \code{igraph} package. Default is FALSE.}
	\item{root.node}{Optional. ONLY applicable when \code{vis}= "parcor.network". The index of the root vertex or root vertices. If this is a non-empty vector then the supplied vertex ids are used as the roots of the trees . If it is an empty vector, then the root vertices are automatically calculated based on topological sorting, performed with the opposite mode than the mode argument. After the vertices have been sorted, one is selected from each component.}
	\item{degree.node}{Optional. ONLY applicable when \code{vis}= "parcor.network". It is related to the vertex label degree. It controls the position of the labels with respect to the vertices. Value are for example -pi/2, 0, pi/2, pi sets above, to the right, below, to the left of a node, respectively.}
	\item{curve}{Optional. ONLY applicable when \code{vis}= "parcor.network". Edge curvature, range between 0 and 1 (FALSE sets it to 0, TRUE to 0.5). Default is FALSE.}
	\item{delet.v}{ Delete vertices with no edges. Default is "TRUE".}
	\item{pos.legend}{Applicable when \code{vis}= "parcor.network" or \code{vis}= "CI". The x and y co-ordinates to be used to position the legend. They can be specified by keywords like "topright", "topleft", and etc. Default is "bottomleft".}
	\item{cex.legend}{Applicable when \code{vis}= "parcor.network" or \code{vis}= "CI".}
\item{iterl}{Optional. ONLY applicable when \code{vis}= "parcor.interactive". integer scalar, the number of iterations to perform for layout_with_fr layout.}
\item{temp}{Optional. ONLY applicable when \code{vis}= "parcor.interactive". Real scalar, the start temperature for layout_with_fr layout.}
\item{tk.width}{Optional. The size of the drawing area of interactive plot.}
\item{tk.height}{Optional. The size of the drawing area of interactive plot.}
}

\value{
An object with S3 class \code{"select"} is returned:
 \item{network}{ Plot of a selected graph,  when \code{vis}= "CI".}
 \item{adjacency}{ Conditional independence (CI) relationships between variables, when \code{vis}= "CI"}
 \item{network}{Interactive plot of a selected graph with .eps format, when \code{vis}= "interactive"}
}

\author{
  Pariya Behrouzi \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com} \cr
}

\references{
Behrouzi, P., and Wit, E. C. (2017c). netgwas: An R Package for Network-Based Genome-Wide Association Studies. arXiv preprint, arXiv:1710.01236.
}

%\seealso{\code{\link{select}}}


\examples{
	\donttest{
		#simulate data
		data(CviCol)
		out <- netsnp(CviCol)
		sel <- selectnet(out)

		cl <- c(rep("palegoldenrod", 24), rep("white",14), rep("tan1",17),
            rep("gray",16), rep("lightblue2",19))
    plot(sel, vis= "parcor.network", sign.edg = TRUE, layout = NULL, vertex.color = cl)
	}
}
