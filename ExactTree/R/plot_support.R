# functions supporting main plot.ETrees() function

## function to make indices for node assignment of levels of a factor variable (used in to.party)
indexID <- function(nodeID, ETrees.out) {
  splitlevels <- unlist(strsplit(as.character(ETrees.out$si[ETrees.out$si[,1]==nodeID, 5]), split = ", "))
  everylevel <- ETrees.out$Desc[[(which(ETrees.out$var.names == ETrees.out$si[ETrees.out$si[,1]==nodeID, 4]))-1]]#We subtract one because Y is not in Desc. #Old: sapply(ETrees.out$origData[ETrees.out$si[ETrees.out$si[,1]==nodeID, 4]], FUN = function(p) sort(unique(p)))#sapply(ETrees.out$data[ETrees.out$si[ETrees.out$si[,1]==nodeID, 3]], FUN = function(p) sort(unique(p)))#This does not seem fine.


  index1 <- numeric(length(splitlevels)) # index factor levels used for split
  for (i in 1:length(index1)) {
    index1[i] <- which(everylevel == splitlevels[i])
  }

  #Temporary fix until different categories can be placed together as splitpoints.
  if(sapply(ETrees.out$origData[colnames(ETrees.out$origData)==ETrees.out$si[ETrees.out$si[,1]==nodeID,4]], is.factor) == TRUE){
    index1<-index1-1 #We take the previous category because we split with using < & >=
  }
  index1<-1:index1

  # indices of all levels not used in split
  index2 <- 1:length(everylevel)
  index2 <- index2[-index1]

  Ls <- integer(length(everylevel))
  Ls[index1] <- 1L; Ls[index2] <- 2L

  return(Ls)
}


## conversion function ##
to.party <- function(nodeID,ETrees.out, ...){
  if("ETree" %in% is(nodeID)){ETrees.out <- nodeID; nodeID <- 1L}
  if(is.null(ETrees.out$var.names)) ETrees.out$var.names <- colnames(ETrees.out$data)
  if(nodeID %in% ETrees.out$li[,1]) return(partynode(id=as.integer(nodeID),
                                                     info = ETrees.out$li[ETrees.out$li[,1]==nodeID,]
  ))

  if(nodeID %in% ETrees.out$si[,1]) {
    #If the variable used for splitting is numeric, use breaks.
    if(sapply(ETrees.out$origData[colnames(ETrees.out$origData)==ETrees.out$si[ETrees.out$si[,1]==nodeID,4]], is.factor) == FALSE){
      split_number<-which(ETrees.out$si[,1]==nodeID)
      return(partynode(id=as.integer(nodeID),
                       split =  partysplit(varid = as.integer(which(ETrees.out$var.names==ETrees.out$si[ETrees.out$si[,1]==nodeID,4])),
                                           breaks = as.numeric(ETrees.out$si[ETrees.out$si[,1]==nodeID,5]), right=FALSE), #right=FALSE added 10/08/2022
                       kids = lapply(c(ETrees.out$si[split_number,2],ETrees.out$si[split_number,3]),to.party,ETrees.out)#MISSING, check this line. Old line: lapply(c(nodeID*2,nodeID*2+1),to.party,ETrees.out)
      )
      )}

    #If the variable used for splitting is a factor, use index.
    if(sapply(ETrees.out$origData[colnames(ETrees.out$origData)==ETrees.out$si[ETrees.out$si[,1]==nodeID,4]], is.factor) == TRUE){
      split_number<-which(ETrees.out$si[,1]==nodeID)
      return(partynode(id=as.integer(nodeID),
                       split =  partysplit(varid = as.integer(which(ETrees.out$var.names==ETrees.out$si[ETrees.out$si[,1]==nodeID,4])),
                                           index = as.integer(indexID(nodeID=nodeID, ETrees.out=ETrees.out))),#,right=FALSE), #right=FALSE added 10/08/2022
                       kids = lapply(c(ETrees.out$si[split_number,2],ETrees.out$si[split_number,3]),to.party,ETrees.out))#MISSING: OLD CODE: lapply(c(nodeID*2,nodeID*2+1),to.party,ETrees.out))
      )}
  }
}



#' Transformation of a Exact Tree object to party object
#'
#'
#' @param obj tree of class \code{ETree}.
#' @param nodeID Node identification.
#' @param \dots additional arguments to be passed.
#' @returns object transformed to a \code{constparty} object.
#' @keywords as.party
#'
#' @importFrom partykit party
#'
#' @export
## conversion method
as.party.ETree <- function(obj, nodeID=1L, ...){
  ETrees.out <- obj
  ETreesNodes <- to.party(nodeID,ETrees.out,...)
  # browser() ##########
  # Old code:
  # party.object <- party(ETreesNodes,ETrees.out$data
  #                       , fitted=data.frame(
  #                         "(fitted)"=colnames(ETrees.out$nind)[
  #                           apply(ETrees.out$nind==1,1,which)],
  #                         "(response)"=ETrees.out$data[,1],
  #                         check.names=FALSE)
  #                       , terms=terms(as.formula(paste(
  #                         colnames(ETrees.out$data)[1],"~",
  #                         paste(colnames(ETrees.out$data)[-1],collapse="+"))
  #                       )),
  #                       ...
  # )

  party.object <- party(ETreesNodes, ETrees.out$origData, #ETrees.out,
                        ...
  )
  party.object$ni <- ETrees.out$li
  class(party.object) <- c("constparty",class(party.object))
  return(party.object)
}


## ETrees inner node
node_ETrees <- function(obj, id = TRUE, abbreviate = FALSE, fill = "white", gp = gpar())
{
  meta <- obj$data
  nam <- names(obj)

  extract_label <- function(node) {
    if(is.terminal(node)) return(rep.int("", 2))

    varlab <- character_split(split_node(node), meta)$name
    if(abbreviate > 0) varlab <- abbreviate(varlab, as.numeric(abbreviate))

    plab <- ""
    return(c(varlab, plab))
  }

  maxstr <- function(node) {
    lab <- extract_label(node)
    klab <- if(is.terminal(node)) "" else unlist(lapply(kids_node(node), maxstr))
    lab <- c(lab, klab)
    lab <- unlist(lapply(lab, function(x) strsplit(x, "\n")))
    return(lab[which.max(nchar(lab))])
  }

  nstr <- maxstr(node_party(obj))
  if(nchar(nstr) < 6) nstr <- "aAAAAa"

  ### panel function for the inner nodes
  rval <- function(node) {
    node_vp <- viewport(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(1, "strwidth", nstr) * 1.3,
      height = unit(3, "lines"),
      name = paste("node_inner", id_node(node), sep = ""),
      gp = gp
    )
    pushViewport(node_vp)

    xell <- c(seq(0, 0.2, by = 0.01),
              seq(0.2, 0.8, by = 0.05),
              seq(0.8, 1, by = 0.01))
    yell <- sqrt(xell * (1-xell))

    lab <- extract_label(node)
    fill <- rep(fill, length.out = 2)

    grid.polygon(x = unit(c(xell, rev(xell)), "npc"),
                 y = unit(c(yell, -yell)+0.5, "npc"),
                 gp = gpar(fill = fill[1]))

    grid.text(lab[1], y = unit(1.5 + 0.5 * FALSE, "lines"))

    if(id) {
      nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1, "npc"),
                           width = max(unit(1, "lines"), unit(1.3, "strwidth", nam[id_node(node)])),
                           height = max(unit(1, "lines"), unit(1.3, "strheight", nam[id_node(node)])))
      pushViewport(nodeIDvp)
      popViewport()
    }
    upViewport()
  }
  return(rval)
}
class(node_ETrees) <- "grapcon_generator"


## ETrees terminal node plot ##
terminal_ETrees <- function(obj,
                            col = "black",
                            fill = "lightgray",
                            width = 0.5,
                            yscale = NULL,
                            ylines = 3,
                            cex = 0.5,
                            id = TRUE,
                            gp = gpar())
{
  ni <- obj$ni #leaf information
  ni.temp <- ni
  # d.CI <- data.frame(
  #   lower = ni[,8] - 1.96 * ni[,9],
  #   mean = ni[,8],
  #   upper = ni[,8] + 1.96 * ni[,9]
  # )
  #yscale <- c(0-max(abs(d.CI)),max(abs(d.CI))) + c(-0.1, 0.1) * max(abs(d.CI))
  rval <- function(node) { # core plotting function
    nid <- id_node(node)
    top_vp <- viewport(layout = grid.layout(nrow = 3, ncol = 3, # define viewport
                                            widths = unit(c(ylines, 1, 1),
                                                          c("lines", "null", "lines")),
                                            heights = unit(c(1, 1, 2), c("lines","null","lines"))),
                       width = unit(1, "npc"),
                       height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_ETrees", nid, sep = ""),
                       gp = gp)

    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "transparent", col =0))

    ind2 <- nid == nodeids(obj,terminal=TRUE)

    ## main title
    bottom <- viewport(layout.pos.col=2, layout.pos.row=3)
    pushViewport(bottom)
    if(id){
      grid.text(sprintf("Leaf %s",
                        which(nid==nodeids(obj,terminal=TRUE)))
      )
    }else{grid.text("")}
    popViewport()

    plot <- viewport(layout.pos.col = 2, layout.pos.row = 2,
                     xscale = c(0, 1), #yscale = yscale,
                     name = paste("node_ETrees", nid, "plot",
                                  sep = ""))
    pushViewport(plot)

    xl <- 0.5 - width/4
    xr <- 0.5 + width/4



    ## box
    grid.rect(gp = gpar(fill = "#DDD8C2"))

    ## Text
    text<-paste0("N = ", ni[ind2,2] ,"\ny = ", ni[ind2,3],"\nh = ",ni[ind2,4])
    grid.text(label = text)

    ##refline
    # grid.lines(unit(c(0, 1), "npc"),
    #            unit(0, "native"), gp = gpar(col = col,lwd=unit(width/4,"npc"),lty="dashed"))
    # grid.lines(unit(c(xl, xr), "npc"),
    #            unit(d.CI$lower[ind2], "native"), gp = gpar(col = col,lwd=unit(width,"npc")))
    # grid.lines(unit(0.5, "npc"),
    #            unit(c(d.CI$lower[ind2],d.CI$upper[ind2]), "native"), gp = gpar(col = col,lwd=unit(width/2,"npc")))
    # meanline=FALSE ## if FALSE mean point
    # if(meanline){
    #   grid.lines(unit(c(0.5 - width/2, 0.5+width/2), "npc"),
    #              unit(d.CI$mean[ind2], "native"), gp = gpar(col = col, lwd = 2))
    # }else{      grid.points(unit(0.5, "npc"),
    #                         unit(d.CI$mean[ind2], "native"), gp = gpar(col = col,lwd=1),size=unit(1,"lines"),pch=20)}
    # grid.lines(unit(c(xl, xr), "npc"), unit(d.CI$upper[ind2], "native"),
    #            gp = gpar(col = col,lwd=unit(width,"npc")))
    #
    #grid.yaxis(label=TRUE) ## TO DO only TRUE for left terminal
    #grid.rect(gp = gpar(fill = "transparent"))
    upViewport(2)
  }
  return(rval)
}
class(terminal_ETrees) <- "grapcon_generator"
