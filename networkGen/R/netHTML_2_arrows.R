#' @export
#' @import igraph
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics grconvertX
#' @importFrom graphics grconvertY
#' @importFrom graphics plot
#' @param nodeLogic This is the connections between the nodes.
#' @param wd is the working directory to save the HTML source code in. If not given, the file will be saved in the default working directory.
#' @param names This allows you to put in your own names in the nodes when generating the maze.
#' @param concerto Choose between concerto 4 or concerto 5. So if you are not using concerto, you might want to change the default option to concerto 4 instead.
#' @description This function generates an network Maze with 2 arrows.
#' @details This function creates a maze and is saved into your working directory. This is regardless of whether it is a trail or circuit type maze. 2 arrows per maze is generated.
#' @author Aiden Loe
#' @title Generate Network Maze (2 arrows)
#' @examples
#' #create random names
#' countries <- c("Croatia","Cyprus","Denmark","Finland","France","Germany",
#' "Greece","Hungary","Iceland","UK","US")
#'
#' #create node logic
#' logic <- nodeLogic(value = 8, type= "circuit", itemFamily= 1)
#'
#' #Folder to save html/
#' #setwd("~/desktop")
#' #filePath<- getwd()
#'
#' #Generate item
#' set.seed(1)
#' netHTML2arrows(logic, wd=NULL, names = countries,concerto="C5")
#'
#'


netHTML2arrows <- function(nodeLogic=NULL, wd=NULL, names=NULL,concerto="C5"){

  if(is.null(nodeLogic)){
    warnings("Please insert nodeLogic.")
  }

  if(is.null(wd)){
    message("HTML file is saved in default working directory.")
  }

  if(concerto != "C4" && concerto !="C5") stop("Please use select either C4 or C5 for the concerto argument.")

##### html ####
  if(is.null(wd)){
    wd = getwd()
  }

htmlfile = file.path(paste0(wd, "/maze.html"))

cat("\n<html><head>",file=htmlfile)
if(concerto=="C5"){
  button<- cssC5()
}else{
  button<- cssC4()
}

cat("\n<html><head>",file=htmlfile)
cat(button, append=TRUE, file=htmlfile)
cat("\n</head>", append=TRUE, file = htmlfile)
cat("\n<br>", append=TRUE, file = htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><span style=\"color: white;\">Level {{level}} out of {{t_question}}.</span></p>",append=TRUE, file = htmlfile)
cat("\n<body>", append = TRUE, file = htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:14px;\"><font color=\"white\">To solve the puzzle, travel on every path. You can return to the same country but you can only use each path once. </font></p> ", append=TRUE, file=htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:14px;\"><font color=\"white\">You can only go in one direction for those paths with an arrow. </font></p>", append=TRUE, file=htmlfile)
cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:14px;\"><font color=\"white\">Click on any country to begin.</font></p>", append=TRUE, file=htmlfile)


####### Create Node coordinates

# countries <- c("Albania","Andorra","Armenia","Austria","Azerbajian","Belarus","Belgium","Bulgaria","Croatia","Cyprus","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Kazakhstan","Kosovo","Latvia","Malta","Moldova","Monaco","Norway","Poland","Portugal","Romania","Russia","Serbia","Slovakia","Solvenia","Spain","Sweden","Turkey","Ukraine","UK","US")

#countries<- as.data.frame(countries)
o <- suppressWarnings(logicMap(nodeLogic ,base.colour=3, start.colour=9,end.colour= 9,names=names,newValue=9,default.colour=FALSE, no.label=FALSE))
o
#Other graph layouts: add_layout_; layout.bipartite, layout_as_bipartite; as_star, layout.star, layout_as_star; as_tree, layout_as_tree; component_wise; in_circle, layout_in_circle; layout.auto, layout_nicely, nicely; layout.davidson.harel, layout_with_dh, with_dh; layout.gem, layout_with_gem, with_gem; layout.graphopt, layout_with_graphopt, with_graphopt; layout.grid, layout.grid.3d, layout.grid.3d, layout_on_grid, on_grid; layout.mds, layout_with_mds, with_mds; layout.merge, layout_components, merge_coords, piecewise.layout, piecewise.layout; layout.norm, norm_coords; layout.sugiyama, layout_with_sugiyama, with_sugiyama; layout_on_sphere, on_sphere; layout_randomly, randomly; layout_with_fr, with_fr; layout_with_kk, with_kk; layout_with_lgl, with_lgl; normalize

#coords <- layout_with_dh(o)
#coords <- layout.auto(o)
#plot(o, layout = coords)
#plot(o)
#tkplot(E.trail)

#Must normalise coordinates
#grconvertX and gconvertY create the right pixel coordinates.
coordinates <- layout_with_dh(o)
coordinates.1 <- layout.norm(coordinates)


# Plot Graph
#save empty .png
png(filename="map.png", height=1000, width=1000)
#plot graph, png must always be forced.
plot.igraph(o,
            layout=layout_with_dh,
            vertex.shape='square',
            vertex.size=10,
            vertex.label.cex=0.5) # plot using desired coordinates)

# combind vector of coordinates
coord. <- cbind(grconvertX(coordinates.1[, 1], "user", "device"), cbind(grconvertY(coordinates.1[, 2], "user", "device")))
coord.1 <- apply(coord., 1:2, function(x) x/1.8) #adjust node coordinates
dev.off()

cat("\n<div align=center>", append = TRUE, file=htmlfile)
cat("<div class=box>", append=TRUE, file=htmlfile)

#margin:0 auto puts container in the center when it is a fixed width.
cat("\n<div style= 'position:relative;width:auto; height:auto;margin:0 auto' id = 'graphContainer'>", append=TRUE, file=htmlfile)  #position:relative(parent)

#Position of the buttons is relative to the div position
#z index puts the div tag at the top, so the nodes are above the edges.
n.name <-  unlist(V(o)$name)
buttons = ""
for (j in 1:nrow(coord.)){
  buttons <- paste0(buttons,"\n<div onClick='nodeClick(this)' id = '", j,"'", " class = 'myButton' style = 'z-index:1; left:",(coord.1[j,1]),"px;top:",(coord.1[j,2]),"px'>",n.name[j],"</div>")
}
cat(buttons, append=TRUE, file=htmlfile)
buttons
ed<- ends(o, E(o), names=FALSE)


##### Extract start node coordinates.
#Number of rows we need from vector.
start.index <- ed[,1]

#Number of columns we need from matrix.
start.coord <- ncol(coord.)
start.coord
#Empty Matrix
start.node.coord <- matrix(NA, nrow = length(start.index), ncol = start.coord)

#get coordinates
for(i in 1:length(start.index)){
  start.node.coord[i,]<- coord.[start.index[i],]
}
start.node.coord.1 <- apply(start.node.coord,1:2, function(x) x/1.8)



##### Extract end node coordinates.
end.index<- ed[,2]
end.coord <- ncol(coord.)

#Create empty matrix
end.node.coord <- matrix(NA, nrow = length(end.index), ncol= end.coord)
for(i in 1:length(end.index)){
  end.node.coord[i,] <- coord.[end.index[i],]
}
end.node.coord.1 <- apply(end.node.coord,1:2, function(x) x/1.8) #adjust edge coord

ed
#### Number of Arrows per maze #####
# selecting rows randomly to swap around for arrows
# This needs to change for the java script so that the arrows are highlighted.
(x <- nrow(end.node.coord.1))
(rowNumber <- sample(x, size=2, replace=FALSE)) # unidirection

uniDirection = 2
revDirection = 3
(direction <- rep(1,times=nrow(end.node.coord.1)))
(direction[rowNumber] = c(uniDirection, revDirection))

direction<- cbind(direction)
direction
arrowDirect <- cbind.data.frame(ed,direction)
arrowDirect
arrowDirect[rowNumber[1],][1] #Start
arrowDirect[rowNumber[1],][2] #Stop
arrowDirect[rowNumber[2],][2] #Start
arrowDirect[rowNumber[2],][1] #Stop
arrowDirect
o
#### Edges of Nodes
arrowDirect
connections = ""
for (i in 1:nrow(ed)){
  if(arrowDirect$direction[i]==3){
    connections <- paste0(connections,"
                          <defs>
                          <marker id=\"arrow",i,"\" markerWidth=\"100\" markerHeight=\"50\" refx=\"40\" refy=\"6\" orient=\"auto\">
                          <path id=\"colourArrow",i,"\"d=\"M2,1 L2,10 L10,6 L2,2\" style=\"fill:blue\" />
                          </marker>
                          </defs>
                          <path id=",paste0('"',ed[i,1],'_',ed[i,2],'"'), " d=","\"M",end.node.coord.1[i,1],' ',end.node.coord.1[i,2],' L',start.node.coord.1[i,1],' ',start.node.coord.1[i,2],"\"  style=\"stroke:black; stroke-width: 3.25px; fill: none ;marker-end: url(#arrow",i,");\" >
                          </path>  ")
  }else if(arrowDirect$direction[i]==2){
    connections <- paste0(connections,"
                          <defs>
                          <marker id=\"arrow",i,"\" markerWidth=\"100\" markerHeight=\"50\" refx=\"40\" refy=\"6\" orient=\"auto\">
                          <path id=\"colourArrow",i,"\" d=\"M2,1 L2,10 L10,6 L2,2\" style=\"fill:blue\" />
                          </marker>
                          </defs>
                          <path id=",paste0('"',ed[i,1],'_',ed[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],' ',start.node.coord.1[i,2],' L',end.node.coord.1[i,1],' ',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;marker-end: url(#arrow",i,");\" >
                          </path> ")
  }else{
    connections <- paste0(connections,"
                          <path id=",paste0('"',ed[i,1],'_',ed[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],' ',start.node.coord.1[i,2],' L',end.node.coord.1[i,1],' ',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;\" >
                          </path> ")
  }
  }
connections

# Step 1 Break into col vectors
start.node <- ed[,1]
start.node<- cbind(start.node)

end.node<- ed[,2]
end.node <- cbind(end.node)

travelled = 0


cat("\n<div>", append = TRUE, file=htmlfile)
cat("\n <svg height=\"610\" width=\"600\">", append=TRUE, file=htmlfile)
cat(connections, append=TRUE, file=htmlfile)
cat("\n </svg>", append=TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n<div id=\"hidden\">&nbsp;</div>", append=TRUE, file=htmlfile) #saves all the info into the id=hidden. concerto will automatically save all this info in a form.
cat("\n<div id=\"hidden2\">&nbsp;</div>", append=TRUE, file=htmlfile)
cat("\n<input name=\"next\" style=\"display: none;\" type=\"Submit\" value=\"next\" />",append=TRUE, file=htmlfile) # For concerto
#cat("\n<input type = 'submit' value = 'next' style='display: none'> ", append=TRUE, file = htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n<p style =\"width:150px; text-align: center; height:20px; background-color:#fff; border: 1px solid #999\" id=\"output\" hidden></p>", append=TRUE, file=htmlfile)




# Include the position of which row to be reverse directions or unidirections
##### Step 2 for loop across number of col vectors
  edge.list <- "\n var edgeArray=["

for (i in 1:length(start.node)){
  if (i != 1) {
    edge.list <- paste0(edge.list,",[",start.node[i,1],",",end.node[i,1],",", travelled[],",",direction[i,1], "]")
  } else {
    edge.list <- paste0(edge.list,"[",start.node[i,1],",",end.node[i,1],",", travelled[],",",direction[i,1],"]")
  }
}
edge.list <- paste0(edge.list,"];")
edge.list



cat("\n<script>", append = TRUE, file = htmlfile)
cat(edge.list, append=TRUE, file=htmlfile)
javaScript <- javaScript2Arrows(arrowDirect, rowNumber)
cat(javaScript, append=TRUE, file=htmlfile)


cat("\n</script>", append = TRUE, file = htmlfile)
cat("\n</body>", append = TRUE, file = htmlfile)
cat("\n</html>", append = TRUE, file = htmlfile)

}


