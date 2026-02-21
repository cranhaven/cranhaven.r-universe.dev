
#' Provides ranking of GO-terms according to distance
#'
#' @param goterm A list of GO-terms to be ordered
#' @param domain The ontology of the GO-terms. The default is "BP"
#' @param plot If FALSE the visualisation of the ordering is not provided. The default is TRUE.
#'
#' @importFrom stats median
#' @importFrom ggplot2 geom_line geom_point coord_flip xlab ylab scale_color_manual scale_y_discrete
#'
#' @description distRankingGO returns the ranking of GO-terms based on the GO-terms hierarchy level and the maximal
#' depth of paths in the GO-DAG passing through these GO-terms.
#'
#' @return
#' The function returns the ordered GO-terms, the indices corresponding to these GO-terms,
#' the distance between the GO-terms hierarchy level and the maximal depth of paths in the GO-DAG passing through these GO-terms and a plot of
#' the visualisaiton by default.
#' @export
#'
#' @examples
#' goterm <- c("GO:0000278","GO:0006414","GO:0022403","GO:0006415",
#' "GO:0045047","GO:0072599","GO:0006613","GO:0000184","GO:0070972",
#' "GO:0006413","GO:0000087","GO:0000280","GO:0000279","GO:0006612",
#' "GO:0000956","GO:0048285","GO:0019080","GO:0019083","GO:0043624",
#' "GO:0006402","GO:0032984","GO:0006401","GO:0072594","GO:0019058",
#' "GO:0051301","GO:0016071","GO:0006412","GO:0002682","GO:0022411",
#' "GO:0006614")
#'
#' #The GO-terms in the list are ordered
#' distRankingGO(goterm = goterm, domain = "BP", plot = TRUE)
#'
distRankingGO <- function(goterm, domain = "BP", plot = TRUE){
  z <- 0
  x <- goterm
  if(toupper(domain) == "BP"){
    y <- GOTermBPOnLevel(goterm = goterm)$Level
    ind <- order(y)
    x2 <- x[ind]
    y2 <- y[ind]
    df <- data.frame(x=x2, y=y2)
    L <- length(x)
    cc1 <- vector(mode="numeric", length=L)
    cc2 <- vector(mode="numeric", length=L)
    cc3 <- vector(mode="numeric", length=L)
    for(i in 1:L){
      goterms <- GO2DecBP(goterm = x[i])
      if(all(!is.na(goterms))){
        cc1[i] <- max(GOTermBPOnLevel(goterm = goterms)$Level)
      }else{
        cc1[i] <- GOTermBPOnLevel(goterm = x[i])$Level
      }

      #print(cc1[i])
    }
    maxLevel <- 19

  }else if(toupper(domain) == "MF"){
    y <- GOTermMFOnLevel(goterm = goterm)$Level

    ind <- order(y)
    x2 <- x[ind]
    y2 <- y[ind]
    df <- data.frame(x=x2, y=y2)
    L <- length(x)
    cc1 <- vector(mode="numeric", length=L)

    for(i in 1:L){
      goterms <- GO2DecMF(goterm = x[i])
      if(!is.null(goterms)){
        cc1[i] <- max(GOTermMFOnLevel(goterm = goterms)$Level)
      }
      else{
        cc1[i] <- GOTermMFOnLevel(goterm = x[i])$Level
      }
    }
    maxLevel <- 16
  }else if(toupper(domain) == "CC"){
    y <- GOTermCCOnLevel(goterm = goterm)$Level
    ind <- order(y)
    x2 <- x[ind]
    y2 <- y[ind]
    df <- data.frame(x=x2, y=y2)
    L <- length(x)
    cc1 <- vector(mode="numeric", length=L)
    cc2 <- vector(mode="numeric", length=L)
    cc3 <- vector(mode="numeric", length=L)
    for(i in 1:L){
      goterms <- GO2DecCC(goterm = x[i])
      if(all(!is.na(goterms))){
        cc1[i] <- max(GOTermCCOnLevel(goterm = goterms)$Level)
      }else{
        cc1[i] <- GOTermCCOnLevel(goterm = x[i])$Level
      }
      #print(cc1[i])
    }
    maxLevel <- 17
  }else{
    stop("The argument \"domain\" can only be \"BP\", \"MF\" or \"CC\".")
  }

  distance <- y-cc1
  ind <- order(y-cc1)
  distance <- distance[ind] * -1
  yo <- y[ind]
  cc1o <- cc1[ind]
  xo <- x[ind]
  zu1 <- c(rep(1, L))
  zu2 <- c(rep(2, L))
  zuo <- zu1[ind]
  zo <- c(zuo, zu2)
  df <- data.frame(x=c(xo, xo), y=c(yo, cc1o), z=zo)
  theme_dotplot <-
    theme(axis.text.y = element_text(size = rel(.75)),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = rel(.75)),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5),
          panel.grid.minor.x = element_blank())

 # maxLevel <- 19 # human

  df$x <- factor(df$x, levels = df$x[1:L])

  p <- ggplot(df, aes(x=x, y=y)) + theme_dotplot +
    coord_flip(ylim=c(1,maxLevel)) +
    scale_y_continuous(limits = c(1,maxLevel), breaks = c(1, 5, 10, 15, 19)) +
    ylab("level") +
    xlab("GO-terms") +
    geom_point(aes(color = as.factor(z)), show.legend = FALSE)  +
    geom_line(aes(group = x)) +
    scale_color_manual(values=c('purple3','red', '#56B4E9'))


  hierarchy_val <- lapply(as.vector(unique(df$x)), function(x) which(goterm == x))
  hierarchy_val <- unlist(hierarchy_val)
  hierachy_final = list("GO-terms_ranking" = as.vector(unique(df$x)), "indices_of_ranking" = hierarchy_val, "distance"= distance)

  if(plot){
    hierachy_final[["plot"]] <- p
    return(hierachy_final)

  }else{
    return(hierachy_final)
  }
}

#' Provides ranking of GO-terms according to a score
#'
#' @param goterm A list of GO-terms to be ordered
#' @param domain The ontology of the GO-terms. The default is "BP"
#' @param plot If FALSE the visualisation of the ordering is not provided. The default is TRUE.
#'
#' @importFrom stats median
#' @importFrom ggplot2 geom_line geom_point coord_flip xlab ylab scale_color_manual scale_y_discrete
#' @description scoreRankingGO provides ranking for a given list of GO-terms according to a score.
#'
#' @return
#' The function returns the ordered GO-terms, the indices corresponding to these GO-terms, the score of each GO-term
#' and a plot of the visualization by default.
#'
#' @export
#'
#' @examples
#' goterm <- c( "GO:0000278","GO:0006414","GO:0022403","GO:0006415",
#' "GO:0045047","GO:0072599","GO:0006613","GO:0000184","GO:0070972",
#' "GO:0006413","GO:0000087","GO:0000280","GO:0000279","GO:0006612",
#' "GO:0000956","GO:0048285","GO:0019080","GO:0019083",
#' "GO:0006402","GO:0032984","GO:0006401","GO:0072594","GO:0019058",
#' "GO:0051301","GO:0016071","GO:0006412","GO:0002682","GO:0022411",
#' "GO:0006614")
#'
#' #The GO-terms in the list are ordered according to a score
#' scoreRankingGO(goterm = goterm, domain = "BP", plot = TRUE)
#'
scoreRankingGO <- function(goterm, domain = "BP", plot = TRUE){
  x <- goterm
  y <- 0
  if(toupper(domain) == "BP"){
    y <- GOTermBPOnLevel(goterm = goterm)$Level
    ind <- order(y)
    x2 <- x[ind]
    y2 <- y[ind]
    df <- data.frame(x=x2, y=y2)
    L <- length(x)
    cc1 <- vector(mode="numeric", length=L)
    for(i in 1:L){
      goterms <- GO2DecBP(goterm = x[i])
      if(all(!is.null(goterms)) && all(!is.na(goterms))){
        cc1[i] <- max(GOTermBPOnLevel(goterm = goterms)$Level)
      }else{
        cc1[i] <- GOTermBPOnLevel(goterm = x[i])$Level
      }
      #print(cc1[i])
    }
    maxLevel <- 19

  }else if(toupper(domain) == "MF"){
    y <- GOTermMFOnLevel(goterm = goterm)$Level
    ind <- order(y)
    x2 <- x[ind]
    y2 <- y[ind]
    df <- data.frame(x=x2, y=y2)
    L <- length(x)
    cc1 <- vector(mode="numeric", length=L)
    for(i in 1:L){
      goterms <- as.vector(GO2DecMF(goterm = x[i]))
      if(all(!is.null(goterms)) && all(!is.na(goterms))){
        cc1[i] <- max(GOTermMFOnLevel(goterm = goterms)$Level)
      }
      else{
        cc1[i] <- GOTermMFOnLevel(goterm = x[i])$Level
      }
      #print(cc1[i])
    }
    maxLevel <- 16
  }else if(toupper(domain) == "CC"){
    y <- GOTermCCOnLevel(goterm = goterm)$Level
    ind <- order(y)
    x2 <- x[ind]
    y2 <- y[ind]
    df <- data.frame(x=x2, y=y2)
    L <- length(x)
    cc1 <- vector(mode="numeric", length=L)
    for(i in 1:L){
      goterms <- GO2DecCC(goterm = x[i])
      if(all(!is.null(goterms)) && all(!is.na(goterms))){
        cc1[i] <- max(GOTermCCOnLevel(goterm = goterms)$Level)
      }else{
        cc1[i] <- GOTermCCOnLevel(goterm = x[i])$Level
      }
      #print(cc1[i])
    }
    maxLevel <- 17
  }else{
    stop("The argument \"domain\" can only be \"BP\", \"MF\" or \"CC\".")
  }


  theme_dotplot <-
    theme(axis.text.y = element_text(size = rel(.75)),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = rel(.75)),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5),
          panel.grid.minor.x = element_blank())




  # maxLevel <- 19 # human

  df$x <- factor(df$x, levels = df$x[1:L])

  u <- (y/cc1)*(y/maxLevel)
  ind <- order(u)

  #plot(u[ind])
  #points(v[ind], col="red")


  df <- data.frame(x=x, y=u)
  df$x <- factor(df$x, levels = df$x[ind])

  p <- ggplot(df, aes(x=x, y=y)) + theme_dotplot +
    coord_flip(ylim=0:1) +
    ylab("score") +
    xlab("GO-terms") +
    geom_point(color="blue")

  hierachy_final = list("GO_terms_ranking" = goterm[ind], "indices_of_ranking" = ind, "score" = u[ind])

  if(plot){
    hierachy_final[["plot"]] <- p
    return(hierachy_final)

  }else{
    return(hierachy_final)
  }


}

