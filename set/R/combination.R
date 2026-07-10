#' @title Combination of Characters or Vectors
#' @description Combination of characters or vectors.
#' @param ... one or more vectors
#'
#' @return binary combination
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' combination(A)
#'
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' D <- c("a","b","e")
#' E <- c("a","c")
#' combination(A,B)
#' combination(A,B,C)
#' combination(A,B,C,D)
#' combination(A,B,C,D,E)
combination <- function(...){
    x1=list(...)
    if (length(x1)==1){
        comb.binary(x1[[1]])
    }else{
        x=suppressWarnings(rbind(...,deparse.level = 2))
        vector.names=rownames(x)
        names(x1)=vector.names
        comb.df=comb.binary(vector.names)[-1,]
        for (i in 1:nrow(comb.df)) {
            if (i==1) {
                venn.c=c()
                venn.n=c()
            }
            if (sum(comb.df[i,])==1){
                stay.in=unlist(x1[vector.names[comb.df[i,]==1]])
                stay.out.all=unlist(x1[vector.names[comb.df[i,]==0]])
                stay.out=unique(stay.out.all)
                stay.both= as.character(stay.in[!(stay.in %in% stay.out)])
                venn.c.i=do::inner_Add_Symbol(stay.both,";")
                if (length(stay.both)==0) venn.c.i=NA
                venn.c=c(venn.c,venn.c.i)
                venn.n.i=length(stay.both)
                venn.n=c(venn.n,venn.n.i)
            }else if (i==nrow(comb.df)){
                stay.both= and(...)
                venn.c.i=do::inner_Add_Symbol(stay.both,";")
                if (length(stay.both)==0) venn.c.i=NA
                venn.c=c(venn.c,venn.c.i)
                venn.n.i=length(stay.both)
                venn.n=c(venn.n,venn.n.i)
            }else{
                stay.in.all=unlist(x1[vector.names[comb.df[i,]==1]])
                stay.in=names(table(stay.in.all))[table(stay.in.all)==sum(comb.df[i,])]
                stay.out.all=unlist(x1[vector.names[comb.df[i,]==0]])
                stay.out=unique(stay.out.all)
                stay.both= as.character(stay.in[!(stay.in %in% stay.out)])

                venn.c.i=do::inner_Add_Symbol(stay.both,";")
                if (length(stay.both)==0) venn.c.i=NA
                venn.c=c(venn.c,venn.c.i)
                venn.n.i=length(stay.both)
                venn.n=c(venn.n,venn.n.i)
            }
        }
        venn.data=cbind(comb.df,number=venn.n,characters=venn.c)
        row.names(venn.data)=NULL
        return(venn.data)
    }
}
