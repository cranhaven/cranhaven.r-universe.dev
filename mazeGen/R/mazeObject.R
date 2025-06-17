#' @export
#' @import igraph
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics grconvertX
#' @importFrom graphics grconvertY
#' @importFrom graphics plot
#' @param rank This is the Rank of the maze.
#' @param satPercent The saturation of the number of black dots created for a given grid. Range between 0-1.
#' @param seed To make sure that the randomness of the created black dots is captured and not repeated.
#' @param grid is the grid of the maze
#' @param background The background colour of the page.
#' @param boxBackground The background colour of the box.
#' @param fontColour The font colour of the instructions.
#' @param Timer If True, a time limit of 4 mintues is given per question.
#' @param concerto The code varies between concerto version "C4" and "C5".
#' @description This function generates the html template of the Elithorn Maze in an R object.
#' @details This function creates a plot with the maze blueprint into your working directory.
#' A grid object needs to be called out first before runing the maze function.
#' The grid object needs to be the same as the rank given.
#' @author Aiden Loe
#' @title Generate Elithorn Maze
#' @seealso \code{\link{mazeAbility}}, \code{\link{mazeDiff}}, \code{\link{np}}
#' @examples
#'
#' rank <- 3
#' satPercent <- 0.5
#'
#' #Grid must be same as rank
#' grid <- gridThreeUp
#'
#' #Generate item
#' mazeObject(rank,satPercent,seed=5,grid = grid,
#' background="#7abcff",boxBackground="#66CDAA", fontColour="white ",
#' Timer=TRUE, concerto="C5")
#'
#'
#'


mazeObject <- function(rank = 3,
                     satPercent = 0.5,
                     seed = 1,
                     grid = NULL,
                     background="#7abcff",
                     boxBackground = "#66CDAA",
                     fontColour="white",
                     Timer=TRUE,
                     concerto="C5"){

  if(is.null(grid)){
    stop("Please select a grid of a specific rank to construct the maze.")
  }

  if(concerto != "C4" && concerto !="C5") stop("Please use select either C4 or C5 for the concerto argument.")

  if(!is.logical(Timer)){
    stop("Please set Timer as TRUE or FALSE.")
  }

# require(mazeGen)
#   require(igraph)
#     rank = 8
#     satPercent = 0.5
#     seed = 1
#     grid = gridEightUp
#     background="#7abcff"
#     boxBackground = "#66CDAA"
#     fontColour="white"
#     Timer=FALSE

  G <- graph(genMaze(rank), directed = TRUE )

  #Node length
  nodeLength <- topNodes(rank)
  nodeLength

  #lowerGrid
  lowerGridCombind<- lowerGrid(rank)

  #### Calculate Path to node length ####
  allPaths <- all_simple_paths(G, 1,nodeLength)
  allPaths

  #saturation and node Position
  nodePosition <- np(rank, satPercent,seed)

  # max score
  maxScore <- maxScore(nodePosition)


  nodePosition <- nodePosition$nodePosition

  #minimum steps to achieve maximum score
  # mazeGen:::minStep(nodeLength)
  #
  # # number of optimised Routes
  # mazeGen:::maxScoreRoutes(nodePosition)
  #
  # #Complete solution
  # mazeGen:::solution(nodePosition)
  #
  # blackNodeRoutes(rank,nodePosition)
  # lookUniqueSolution(rank,saturation,seed)



  ##### From Here (HTML) ####
  # htmlfile = file.path(paste0(wd, "/seed",seed,".html"))



  htmlOne <- paste0("\n<html><head>",
         css(background,boxBackground),
         "\n</head>
         \n<br>
         \n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;\"><span style=\"color: white;font-size:25px\">Level {{level}} out of {{t_question}}.</span></p>
         \n<body>
         \n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><font color=\"white\">The goal is to collect as many gold coins as possible as you plan your route up to the {{direction}}.</font></p>
         \n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><font color=\"white\">To start, click on the first node at the bottom of the maze.</font></p>")

  if(Timer==TRUE){
    htmlTimer <- paste0("\n<input id=\"countdown\" name=\"timeLeft\" type=\"hidden\" />
           \n<div style=\"text-align: center; font-size:35px\">
           <font color=\"",fontColour,"\">Timer: <span id=\"countdown2\">&nbsp;</span></font></div>")
  }else{
    htmlTimer<-paste0("")
  }

htmlTwo <- paste0(htmlOne, htmlTimer)

  coordinates.1 <- grid
  plot(coordinates.1)
  coordinates.2<- coordinates.1[which(1:nrow(coordinates.1) %in% lowerGridCombind),]
  plot(coordinates.2)

  png(filename=paste0("map_",seed,".png"), height=1000, width=1000)
   tryCatch(
     {plot(G, layout=coordinates.1)
     },
     warning=function(w) {
       stop("\n Stop: Rank and grid coordinates are not the same. (e.g, rank = 3, grid = gridThreeUp) ", message(w))
     }
   )
  coord.node <- cbind(grconvertX(coordinates.1[, 1], "user", "device"), cbind(grconvertY(coordinates.1[, 2], "user", "device")))
  coord. <- cbind(grconvertX(coordinates.1[lowerGridCombind, 1], "user", "device"), cbind(grconvertY(coordinates.1[lowerGridCombind, 2], "user", "device")))
  coord.1 <- apply(coord., 1:2, function(x) x/1.2) #adjust node coordinates
  dev.off()


  #### TO here (CSS) ####
   cssOne <- paste0("\n<div align=center>
                       <div class=box>
                       \n<div style= 'position:relative;width:auto; height:auto;margin:0 auto' id = 'graphContainer'>")

  htmlThree <- paste0(htmlTwo, cssOne)

  #Position of the buttons is relative to the div position
  #z index puts the div tag at the top, so the nodes are above the edges.
  colouredPoint <- ifelse(lowerGridCombind %in% nodePosition,"black","non-black")
  n.name <-  lowerGridCombind

  buttons = ""
  for (j in 1:nrow(coord.)){
    buttons <- paste0(buttons,"\n<div onClick='nodeClick(this)' id = '", n.name[j],"'", " class =",if(colouredPoint[j]=="non-black"){'\"myButton\"'}else{'\"myButtonTwo\"'}, " style = 'z-index:1; left:",(coord.1[j,1]),"px;top:",(coord.1[j,2]),"px'> <span class=colour>",n.name[j],"</span></div>")
  }

  buttons

  htmlFour <- paste0(htmlThree,buttons)

  ed<- ends(G, E(G), names=FALSE)
  lowerGridCombind
  ed <- as.data.frame(ed)
  ed.1 <- ed[ed$V1 %in% lowerGridCombind,]
  ed.2 <- ed.1[ed.1$V2 %in% lowerGridCombind,]
  ed.2

  ##### Extract start node coordinates.
  #Number of rows we need from vector.
  start.index <- ed.2[,1]

  #Number of columns we need for matrix.
  start.coord <- ncol(coord.)

  #Empty Matrix
  start.node.coord <- matrix(NA, nrow = length(start.index), ncol = start.coord)
  length(start.index)
  for(i in 1:length(start.index)){
    start.node.coord[i,]<- coord.node[start.index[i],]
  }
  start.node.coord.1 <- apply(start.node.coord,1:2, function(x) x/1.2)
  start.node.coord

  ##### Extract end node coordinates.
  end.index<- ed.2[,2]
  end.coord <- ncol(coord.)

  #Create empty matrix
  end.node.coord <- matrix(NA, nrow = length(end.index), ncol= end.coord)
  for(i in 1:length(end.index)){
    end.node.coord[i,] <- coord.node[end.index[i],]
  }
  end.node.coord.1 <- apply(end.node.coord,1:2, function(x) x/1.2) #adjust edge coord

  end.node.coord.1


  # This needs to change for the java script so that the arrows are highlighted.
  x <- nrow(ed.2)
  uniDirection = 2
  direction <- rep(2,times=x)
  arrowDirect <- cbind.data.frame(ed.2,direction)
  arrowDirect
  #### Edges of Nodes
  connections = ""
  for (i in 1:nrow(ed.2)){
    if(arrowDirect$direction[i]==2){
      connections <- paste0(connections,"
                            <defs>
                            <marker id=\"arrow\" markerWidth=\"100\" markerHeight=\"50\" refx=\"15\" refy=\"6\" orient=\"auto\">
                            <path id=\"colourArrow\" d=\"M2,1 L2,10 L10,6 L2,2\" style=\"fill:blue\" />
                            </marker>
                            </defs>
                            <path id=",paste0('"',ed.2[i,1],'_',ed.2[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],',',start.node.coord.1[i,2],',L',end.node.coord.1[i,1],',',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;marker-end: url(#arrow",i,");\" >
                            </path> ")
    }else{
      connections <- paste0(connections,"
                            <path id=",paste0('"',ed.2[i,1],'_',ed.2[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],',',start.node.coord.1[i,2],',L',end.node.coord.1[i,1],',',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;\" >
                            </path> ")
    }
    }
  connections

connect <- paste0("\n<div>
       \n <svg height=\"1000\" width=\"1000\">",
       connections,
       "\n </svg>
       \n</div>
       \n</div>
       \n</div>
       \n</div>
       \n<div id=\"hidden\">&nbsp;</div>
       \n<div id=\"hidden2\">&nbsp;</div>
       \n<div id=\"hidden3\">&nbsp;</div>
       \n<input name=\"next\" style=\"display: none;\" type=\"Submit\" value=\"next\" />
       \n</div>
       \n<p style =\"width:150px; text-align: center; height:20px; background-color:#fff; border: 1px solid #999\" id=\"output\" hidden></p>")


htmlFive <- paste0(htmlFour,connect)

# Step 1 Break into col vectors
  start.node <- ed[,1]
  start.node<- cbind(start.node)

  end.node<- ed[,2]
  end.node <- cbind(end.node)

  travelledFirstNode = 0
  travelledBlackNode = 0
  # Include the position of which row to be reverse directions or unidirections
  ##### Step 2 for loop across number of col vectors

  edge.list <- "\n var edgeArray=["
  arrowDirect
  start.index
  for (i in 1: 1:nrow(ed.2)){
    if (i != 1) {
      edge.list <- paste0(edge.list,",[",start.index[i],",",end.index[i],",", travelledFirstNode[],",", travelledBlackNode[],",",direction[i], "]")
    } else {
      edge.list <- paste0(edge.list,"[",start.index[i],",",end.index[i],",", travelledFirstNode[],",", travelledBlackNode[],",",direction[i],"]")
    }
  }

  edge.list <- paste0(edge.list,"];")
  edge.list

  edge <- paste0("\n<script>",edge.list)

  htmlSix<- paste0(htmlFive,edge)

  #### TO GET NODE POSITIION ####
  v <- paste0("if(")
  e <- NULL
  for(i in 1:(length(nodePosition)-1)){
    e <- paste0(e,paste0("(nodeclicked.id == ",nodePosition[i],") || "))
  }
  colourNodePosition <- paste0(v,e,paste0("(nodeclicked.id == ",nodePosition[length(nodePosition)],"))"))

  ##### TO CALUCLATE SUM SCORE ####
  end.index.df<- as.data.frame(end.index)
  index <- 1:nrow(end.index.df)
  end.index.df <- cbind.data.frame(index,end.index.df)
  nodeLength
  which(end.index.df$end.index == nodeLength)

  test1 <- NULL
  for(i in 1:length(nodeLength)){
    test1[[i]] <- end.index.df[which(end.index.df$end.index == nodeLength[i]),]
  }
  lastRow <- do.call("rbind",test1)

  v <- paste0("if(")
  e<- NULL
  for(i in 1:(nrow(lastRow)-1)){
    e <- paste0(e,paste0("(nodeclicked.id == edgeArray[",lastRow$index[i]-1,"][1]) || "))
  }
  e
  finalRow <- paste0(v,e,paste0("(nodeclicked.id == edgeArray[",lastRow$index[nrow(lastRow)]-1,"][1]))"))


  ##### javaScript1 Timer ####
  if(Timer==TRUE){
    if(concerto== "C4"){
    javaScript <- javaScriptTimerC4(colourNodePosition=colourNodePosition,maxScore=maxScore)
    }else{
      javaScript <- javaScriptTimerC5(colourNodePosition=colourNodePosition,maxScore=maxScore)
    }
  }else{
    javaScript <- javaScriptNoTimer(colourNodePosition=colourNodePosition,maxScore=maxScore)
  }


  ##### javascript 2 #####
  if(concerto=="C4"){
    javaScript2 <- javaScriptTwoC4(finalRow=finalRow)
  }else{
    javaScript2 <- javaScriptTwoC5(finalRow=finalRow)
  }
 javaScript3 <-  paste0(javaScript, javaScript2,
         "\n</script>
         \n</body>
         \n</html>" )
 htmlSeven <- paste0(htmlSix,javaScript3)
 htmlSeven
 return(htmlSeven)

}

#htmlSeven
#
# htmlfile = file.path(paste0('~/Desktop', "/seed",seed,".html"))
#
# cat(htmlSeven,file=htmlfile)
# ?png
# png(filename=paste0("map_",seed,".png"), height=1000, width=1000)
# plot(G, layout=coordinates.1)
# dev.off()
