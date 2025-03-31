#' Pairwise dissimilarity plot
#' @title Pairwise dissimilarity plot
#' @author Antton Alberdi, \email{anttonalberdi@gmail.com}
#' @keywords hill numbers diversity partitioning pairwise
#' @description Visualisation of pairwise dissimilarities
#' @param distance Matrix of pairwise dissimilarities, usually one of the matrices listed in the output object of the pair_dis() function.
#' @param hierarchy The first column lists the sample names while the second lists the groups. If provided, group profiles are plotted instead of individual profiles.
#' @param type Whether to plot a NMDS or qgraph chart. type="NMDS".
#' @param level If '1', samples are not grouped (each sample is a different colour), while if '2', samples are coloured by groups. level=1.
#' @param colour he number of vector items (colours, e.g. '#34k235'), must equal the number of samples or groups that are intended to plot.
#' @param magnify Only relevant for qgraph. Whether the pairwise dissimilarity values are transformed to 0-1 scale, 0 corresponding to the minimum dissimilarity and 1 to the maximum dissimilarity value. magnify=FALSE.
#' @return An NMDS or network plot.
#' @seealso \code{\link{pair_dis}}, \code{\link{beta_dis}}
#' @import ggplot2
#' @import qgraph
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom stats as.dist
#' @importFrom vegan metaMDS
#' @examples
#' data(bat.diet.otutable)
#' data(bat.diet.tree)
#' data(bat.diet.hierarchy)
#' pairdisres <- pair_dis(bat.diet.otutable,qvalue=0,hierarchy=bat.diet.hierarchy,level="2")
#' pair_dis_plot(pairdisres$L2_CqN,hierarchy=bat.diet.hierarchy,type="NMDS",level=2)
#' pair_dis_plot(pairdisres$L2_CqN,hierarchy=bat.diet.hierarchy,type="qgraph",level=2)
#' @references
#' Alberdi, A., Gilbert, M.T.P. (2019). A guide to the application of Hill numbers to DNA-based diversity analyses. Molecular Ecology Resources, 19, 804-817.\cr\cr
#' Chao, A., Chiu, C.H., & Hsieh, T. C. (2012). Proposing a resolution to debates on diversity partitioning. Ecology, 93, 2037-2051.\cr\cr
#' Jost, L. (2007). Partitioning diversity into independent alpha and beta components. Ecology, 88, 2427-2439.
#' @export

pair_dis_plot <- function(distance,hierarchy,type,level,colour,magnify){

if(missing(type)){type = "NMDS"}
if(missing(level)){level = "1"}
if(missing(magnify)){magnify = "FALSE"}

#Declare colours

if(missing(colour) || (length(colour) != length(unique(hierarchy[,2])))){
getPalette <- colorRampPalette(brewer.pal(length(unique(hierarchy[,2])), "Paired"))
colour <- getPalette(length(unique(hierarchy[,2])))
}

if(type == "NMDS"){
#NMDS plot
values.NMDS<-metaMDS(as.dist(distance), k = 2, trymax = 400)
if(level == 1){
NMDS=data.frame(x=values.NMDS$point[,1],y=values.NMDS$point[,2],Sample=as.factor(rownames(values.NMDS$point)))
NMDS=merge(NMDS,hierarchy,by="Sample")
colnames(NMDS) <- c("Sample","x","y","Group")
}
if(level == 2){
NMDS=data.frame(x=values.NMDS$point[,1],y=values.NMDS$point[,2],Group=as.factor(unique(hierarchy[,2])))
}

nmds.plot <- ggplot() +
	geom_point(data = NMDS, aes_(x=~x, y=~y, colour=~Group), size = 2, alpha = 0.5) +
	scale_colour_manual(values = colour) +
	scale_shape_manual(values=16) +
	theme(panel.background = element_rect(fill = 'white', colour = 'grey'))
print(nmds.plot)
}

if(type == "qgraph"){
#qgraph plot
normal <- 1-as.matrix(distance)
forced <- (normal - min(normal,na.rm=TRUE))/(max(normal,na.rm=TRUE)-min(normal,na.rm=TRUE))
if(magnify == TRUE){
	if(level == 1){
	qgraph.plot <- qgraph(as.dist(forced), layout = "circular", posCol = "grey", vsize=6, groups=hierarchy[,2], color = colour, borders=FALSE)
	}
	if(level == 2){
	qgraph.plot <- qgraph(as.dist(forced), layout = "circular", posCol = "grey", vsize=6, groups=unique(hierarchy[,2]), color = colour, borders=FALSE)
	}
}else{
	if(level == 1){
	qgraph.plot <- qgraph(as.dist(normal), layout = "circular", posCol = "grey", vsize=6, groups=hierarchy[,2], color = colour, borders=FALSE)
	}
	if(level == 2){
	qgraph.plot <- qgraph(as.dist(normal), layout = "circular", posCol = "grey", vsize=6, groups=unique(hierarchy[,2]), color = colour, borders=FALSE)
	}
}
print(qgraph.plot)
}

}
