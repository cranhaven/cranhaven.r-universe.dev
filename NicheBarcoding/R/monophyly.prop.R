

#' Calculate the proportion of monophyletic group on a tree
#'
#' @description Calculate the proportion of monophyletic group on a tree given
#' species vector and a tree.
#'
#' @param  phy A tree of class phylo.
#' @param  sppVector Species vector.
#' @param  singletonsMono Logical. Should singletons (i.e. only a single
#' specimen representing that species) be treated as monophyletic?
#' Default of TRUE. Possible values of FALSE and NA.
#'
#' @return A list containing proportion and number of monophyly group.
#' @return A set monophyly and of non-monophyly group names.
#'
#' @keywords monophyly.prop
#' @export
#'
#' @author Cai-qing YANG (Email: yangcq_ivy(at)163.com) and Ai-bing ZHANG
#' (Email:zhangab2008(at)cnu.edu.cn), Capital Normal University (CNU), Beijing,
#' CHINA.
#'
#'
#' @examples
#' library(ape)
#' tree<-ape::rtree(20)
#' tree$tip.label<-sample(tree$tip.label[1:10],size=20,replace = TRUE)
#' plot(tree)
#' sppVector<-tree$tip.label
#'
#' MP<-monophyly.prop(tree,sppVector,singletonsMono = TRUE)
#' MP


monophyly.prop<-function(phy,sppVector,singletonsMono = TRUE){
  mono<-spider::monophyly(phy, sppVector, pp = NA, singletonsMono = singletonsMono)
  spp.unique<-unique(sppVector)

  mono.list<-spp.unique[mono]

  mono[mono==TRUE]<-1
  mono[mono==FALSE]<-0
  no.mono<-sum(mono)

  non.mono.list<-spp.unique[!mono]
  non.mono.list
  mono.prop<-no.mono/length(mono)
  OUT<-list(mono.prop=mono.prop,
            no.mono=no.mono,
            mono.list=mono.list,
            non.mono.list=non.mono.list
            )
  return(OUT)
}

# The end of monophyly.prop #

