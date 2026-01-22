#' @export
#' @importFrom igraph graph_from_literal
#' @param value seed value
#' @param type select either 'circuit' or 'trail' type network maze.
#' @param itemFamily There are 9 item family for circuit and 6 item family for trail type network maze.
#' @description This function generates the node logic for circuit (9 item family) or trail (6 item family) type maze. Please refer to details for more information
#' @details Currently, there are 9 item families for circuit type items and 6 item families for trail type mazes. They are by no means based on increasing difficulty. This is based on the uniquness of each pattern.
#'
#' Circuit (radical 1). 2 Same even number nodes
#' \itemize{
#'  \item{Item Family 1: }{In total 4 moves}
#'  \item{Item Family 2: }{In total 8 moves}
#'  \item{Item Family 3: }{In total 12 Moves}
#'  \item{Item Family 4: }{In total 16 of moves}
#'  }
#'
#'  Circuit (radical 2).  Different even number nodes
#'  \itemize{
#'  \item{Item Family 5: }{In total 6 moves. 1 node with 4 edges (Sample Item 2).}
#'  \item{Item Family 6: }{In total 9 moves. 2 nodes with 4 edges}
#'  \item{Item Family 7: }{In total 12 moves. 3 nodes with 4 edges}
#'  \item{Item Family 8: }{In total 15 moves. 1 node with 6 edges, 2 nodes with 4 edges, the rest with 2 edges}
#'  \item{Item Family 9: }{In total 12 moves. 1 node with 6 edges, 1 node with 4 edges, the rest with 2 edges.}
#' }
#'
#' Trail.  Same uneven number of nodes
#' \itemize{
#' \item{Item Family 1: }{In total 6 moves}
#' \item{Item Family 2: }{In total 10 moves}
#' \item{Item Family 3: }{In total 14 moves}
#' \item{Item Family 4: }{In total 9 moves}
#' \item{Item Family 5: }{In total 13 moves}
#' \item{Item Family 6: }{In total 10 move}
#' }
#' @author Aiden Loe
#' @title Node Logic
#' @examples
#' nodeLogic(value = 1, type= "circuit", itemFamily= 1)
#'


nodeLogic <- function(value, type, itemFamily){

  if(type != "circuit" && type != "trail"){
    stop("Please choose the either 'circuit' or 'trail'")
  }

  if(any(itemFamily >= 10) && type == "circuit"){
    stop("Select a smaller item family for circuit type nodes")
  }

  if(any(itemFamily >= 7) && type == "trail"){
    stop("Select a smaller item family for trail type nodes")
  }

  set.seed(value)
  if(type == "circuit"){
    if(itemFamily == 1 ){
      node <- graph_from_literal(1:2 -- 3:4) # 4 moves
    }else if(itemFamily == 2){
      node <- graph_from_literal(1:2:3:4 -- 5:6) # 8 moves
    }else if(itemFamily == 3){
      node <- graph_from_literal(1:2:3:4:5:6 -- 7:8) # 12 Moves
    }else if(itemFamily == 4){
      node <- graph_from_literal(1:2:3:4:5:6:7:8 -- 9:10) # 16 moves
    }else if(itemFamily == 5){
      node<- graph_from_literal(1--2:3:4:5, 2--3,3--2, 4--5) #6 of moves
    }else if(itemFamily == 6){
      node <- graph_from_literal(1-- 2:3:4:5, 2--3:6:7, 4--5, 6--7) # 9 moves
    }else if(itemFamily == 7){
      node <- graph_from_literal(1-- 2:3:4:5, 2--3:6:7, 4--5:8:9, 6--7, 8--9) # 12 moves
    }else if(itemFamily == 8){
      node<- graph_from_literal(1--2:3:4:5:6:7, 2--3:8:9, 3--10:11,4--5, 6--7, 8--9, 10-11) # 15 moves
    }else{
      node <- graph_from_literal(1-- 2:3:4:5:6:7, 2--3:8:9, 4--5, 6--7, 8--9) # 12 moves
    }
  }else{
    if(itemFamily == 1 ){
      node <- graph_from_literal(1:2:3 -- 4:5)
    }else if(itemFamily == 2){
      node <- graph_from_literal(1:2:3:4:5 -- 6:7) #10 moves
    }else if(itemFamily == 3){
      node <- graph_from_literal(1:2:3:4:5:6:7 -- 8:9) # 14 moves
    }else if(itemFamily == 4){
      node <- graph_from_literal(1:2:3:4 -- 5:6, 1--2) # 9 moves
    }else if(itemFamily == 5){
      node <- graph_from_literal(1:2:3:4:5:6 -- 7:8, 1--2) # 13 moves
    }else{
      node <- graph_from_literal(1:2:3:4 -- 5:6, 1--2, 1--3) # 10 moves
    }
  }
  return(node)
}

