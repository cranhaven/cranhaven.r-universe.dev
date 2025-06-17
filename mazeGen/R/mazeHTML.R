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
#' @param wd is the working directory to save the HTML source code in. If not given, the file will be saved in the default working directory.
#' @param background The background colour of the page.
#' @param boxBackground The background colour of the box.
#' @param fontColour The font colour of the instructions.
#' @param Timer If True, a time limit of 1 mintues and 30 seconds is given per question.
#' @param concerto The code varies between concerto version "C4" and "C5".
#' @description This function generates an Elithorn Maze
#' @details This function creates a maze and is saved into your working directory.
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
#' #Folder to save html/
#' #setwd("~/desktop")
#' #filePath<- getwd()
#'
#' #Generate item
#' mazeHTML(rank,satPercent,seed=5,grid = grid,wd=NULL,
#' background="#7abcff",boxBackground="#66CDAA", fontColour="white ",
#' Timer=TRUE, concerto="C5")
#'


mazeHTML <- function(rank = 3,
                 satPercent = 0.5,
                 seed = 1,
                 grid = NULL,
                 wd = NULL,
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

  if(is.null(wd)){
    message("HTML file is saved in default working directory.")
  }






# require(igraph)
#   rank = 8
#   satPercent = 0.5
#   seed = 1
#   grid = gridEightUp
#   wd = setwd("~/desktop")
#   background="#7abcff"
#   boxBackground = "#66CDAA"
#   fontColour="white"
#   Timer=FALSE

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

  # maxscore
  # maxScore <- max(maxscore$totalScore)
  # maxScore

  nodePosition <- nodePosition$nodePosition

  #minimum steps to achieve maximum score
  # mazeGen:::minStep( nodeLength)
  #
  # # number of optimised Routes
  # mazeGen:::maxScoreRoutes( nodePosition)
  #
  # #Complete solution
  # mazeGen:::solution( nodePosition)
  #
  # blackNodeRoutes(nodePosition)
  # lookUniqueSolution(rank,saturation,seed)

  ##### From Here (HTML) ####
  if(is.null(wd)){
    wd = getwd()
  }

  ##### From Here (HTML) ####
  htmlfile = file.path(paste0(wd, "/maze_",seed,".html"))


  cat("\n<html><head>",file=htmlfile)
  # CSS
  cat(css(background,boxBackground), append= TRUE, file = htmlfile)
  cat("\n</head>", append=TRUE, file = htmlfile)
  cat("\n<br>", append=TRUE, file = htmlfile)
  cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;\"><span style=\"color: white;font-size:25px\">Level {{level}} out of {{t_question}}.</span></p>",append=TRUE, file = htmlfile)
  cat("\n<body>", append = TRUE, file = htmlfile)

  cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><font color=\"white\">The goal is to collect as many gold coins as possible as you plan your route up to the {{direction}}.</font></p>", append=TRUE, file=htmlfile)
  cat("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><font color=\"white\">To start, click on the first node at the bottom of the maze.</font></p>", append=TRUE, file=htmlfile)
  if(Timer==TRUE){
    cat("\n<input id=\"countdown\" name=\"timeLeft\" type=\"hidden\" />", append=TRUE, file=htmlfile)
    cat(paste0("\n<div style=\"text-align: center; font-size:35px\"><font color=\"",fontColour,"\">Timer: <span id=\"countdown2\">&nbsp;</span></font></div>"), append=TRUE, file=htmlfile)
  }


  coordinates.1 <- grid
  plot(coordinates.1)
  coordinates.2<- coordinates.1[which(1:nrow(coordinates.1) %in% lowerGridCombind),]
  plot(coordinates.2)


  # Plot Graph
  png(filename=paste0("map_",seed,".png"), height=1000, width=1000)

  tryCatch(
    {plot(G, layout=coordinates.1)
    },
    warning=function(w) {
      stop("\n Stop: Rank and grid coordinates are not the same. (e.g, rank = 3, grid = gridThreeUp) ", message(w))
    }
  )
  # combind vector of coordinates
  #coord. <- cbind(grconvertX(coordinates.2[, 1], "user", "device"), cbind(grconvertY(coordinates.2[, 2], "user", "device")))
  coord.node <- cbind(grconvertX(coordinates.1[, 1], "user", "device"), cbind(grconvertY(coordinates.1[, 2], "user", "device")))
  coord. <- cbind(grconvertX(coordinates.1[lowerGridCombind, 1], "user", "device"), cbind(grconvertY(coordinates.1[lowerGridCombind, 2], "user", "device")))
  coord.1 <- apply(coord., 1:2, function(x) x/1.2) #adjust node coordinates
  dev.off()
  coordinates.1

  #### TO here (CSS) ####
  cat("\n<div align=center>", append = TRUE, file=htmlfile)
  cat("<div class=box>", append=TRUE, file=htmlfile)

  #margin:0 auto puts container in the center when it is a fixed width.
  cat("\n<div style= 'position:relative;width:auto; height:auto;margin:0 auto' id = 'graphContainer'>", append=TRUE, file=htmlfile)  #position:relative(parent)

  #Position of the buttons is relative to the div position
  #z index puts the div tag at the top, so the nodes are above the edges.
  colouredPoint <- ifelse(lowerGridCombind %in% nodePosition,"black","non-black")
  n.name <-  lowerGridCombind

  buttons = ""
  for (j in 1:nrow(coord.)){
    buttons <- paste0(buttons,"\n<div onClick='nodeClick(this)' id = '", n.name[j],"'", " class =",if(colouredPoint[j]=="non-black"){'\"myButton\"'}else{'\"myButtonTwo\"'}, " style = 'z-index:1; left:",(coord.1[j,1]),"px;top:",(coord.1[j,2]),"px'> <span class=colour>",n.name[j],"</span></div>")
  }

  buttons
  cat(buttons, append=TRUE, file=htmlfile)

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
                            <path id=",paste0('"',ed.2[i,1],'_',ed.2[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],' ',start.node.coord.1[i,2],' L',end.node.coord.1[i,1],' ',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;marker-end: url(#arrow",i,");\" >
                            </path> ")
    }else{
      connections <- paste0(connections,"
                            <path id=",paste0('"',ed.2[i,1],'_',ed.2[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],' ',start.node.coord.1[i,2],' L',end.node.coord.1[i,1],' ',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;\" >
                            </path> ")
    }
    }
  connections


  cat("\n<div>", append = TRUE, file=htmlfile)
  cat("\n <svg height=\"1000\" width=\"1000\">", append=TRUE, file=htmlfile)
  cat(connections, append=TRUE, file=htmlfile)
  cat("\n </svg>", append=TRUE, file=htmlfile)
  cat("\n</div>", append = TRUE, file=htmlfile)
  cat("\n</div>", append = TRUE, file=htmlfile)
  cat("\n</div>", append = TRUE, file=htmlfile)
  cat("\n</div>", append = TRUE, file=htmlfile)
  cat("\n<div id=\"hidden\">&nbsp;</div>", append=TRUE, file=htmlfile) #saves all the info into the id=hidden. concerto will automatically save all this info in a form.
  cat("\n<div id=\"hidden2\">&nbsp;</div>", append=TRUE, file=htmlfile)
  cat("\n<div id=\"hidden3\">&nbsp;</div>", append=TRUE, file=htmlfile)
  cat("\n<input name=\"next\" style=\"display: none;\" type=\"Submit\" value=\"next\" />",append=TRUE, file=htmlfile) # For concerto
  #cat("\n<input type = 'submit' value = 'next' style='display: none'> ", append=TRUE, file = htmlfile)
  cat("\n</div>", append = TRUE, file=htmlfile)
  cat("\n<p style =\"width:150px; text-align: center; height:20px; background-color:#fff; border: 1px solid #999\" id=\"output\" hidden></p>", append=TRUE, file=htmlfile)


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
  cat("\n<script>", append = TRUE, file = htmlfile)
  cat(edge.list, append=TRUE, file=htmlfile)

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
  if(concerto == "C4"){
    javaScript <- javaScriptTimerC4(colourNodePosition=colourNodePosition,maxScore=maxScore)
  }else{
    javaScript <- javaScriptTimerC5(colourNodePosition=colourNodePosition,maxScore=maxScore)
  }
}else{
  javaScript <- javaScriptNoTimer(colourNodePosition=colourNodePosition,maxScore=maxScore)
}


  ##### javascript 2 #####
  if(concerto =="C4"){
  javaScript2 <- javaScriptTwoC4(finalRow=finalRow)
  }else{
    javaScript2 <- javaScriptTwoC5(finalRow=finalRow)
  }
  cat(javaScript, append=TRUE, file=htmlfile)
  cat(javaScript2, append=TRUE, file=htmlfile)
  cat("\n</script>", append = TRUE, file = htmlfile)
  cat("\n</body>", append = TRUE, file = htmlfile)
  cat("\n</html>", append = TRUE, file = htmlfile)

}
