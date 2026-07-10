#' Convert Wide Data to Long
#' @description It is easy to convert wide data to long in this function. Be careful, id must be unique. prefix, suffix and var.names can be used together.
#'
#' @param data wide data
#' @param prefix prefix of value variables
#' @param suffix suffix of value variables
#' @param var.names names of value variables, do.value will be created as the name of value column
#' @importFrom reshape2 melt
#' @return long data
#'
reshape_toLong <- function (data, prefix=NULL, suffix=NULL, var.names=NULL){
   if (!is.null(var.names) & is.null(prefix) & is.null(suffix)){
      melt(data = data,
                     id.vars=setdiff(colnames(data),var.names)
      )
   }else{
      id.names=as.character(Sys.time())
      data2=cbind(1:nrow(data),data)
      colnames(data2)[1]=id.names
      #tans suffix and var.names to prefix
      if (!missing(suffix)) {
         suffix = suffix[order(nchar(suffix), decreasing = TRUE)]
         for (i in 1:length(suffix)) {
            suffix.char = right(colnames(data2), nchar(suffix[i]))
            suffix.loc = (1:length(suffix.char))[suffix[i] == 
                                                    suffix.char]
            name.suffix = Replace0(colnames(data2)[suffix.loc], 
                                   suffix[i])
            colnames(data2)[suffix.loc] = paste0(suffix[i], name.suffix)
         }
         if (missing(prefix)){
            prefix=suffix
         }else{
            prefix=c(prefix,suffix)
         }
      }
      if (!missing(var.names)) {
         names.loc = unlist(var.names %==% colnames(data2))
         colnames(data2)[names.loc] = paste0(var.names,"value")
         if (missing(prefix)){
            prefix=var.names
         }else{
            prefix=c(prefix,var.names)
         }
      }
      #order prefix from large to small
      prefix = prefix[order(nchar(prefix), decreasing = TRUE)]
      #get prefix colname number
      for (i in 1:length(prefix)) {
         if (i == 1) trans.no = NULL
         prefix.i = prefix[i]
         left.char = left(colnames(data2), nchar(prefix.i))
         trans.no = c(trans.no, prefix.i %==% left.char)
         if (i == length(prefix)) trans.no = unique(trans.no)
      }
      # get tans data2 and left data2 ---------------------------------------------
      trans.data2 = data2[, c(1,trans.no)]
      left.names = not(colnames(data2),colnames(data2)[trans.no])
      if (length(left.names)>1) left.data2 = data2[,left.names]
      
      # reshape trans.data2 to wide ----------------------------------------------
      for (i in 1:length(prefix)) {
         if (i==1) pre.list=NULL
         judge=left(colnames(trans.data2),
                    nchar(prefix[i]))==prefix[i]
         prefix.id=(1:ncol(trans.data2))[judge]
         pre.data=trans.data2[,c(1,prefix.id)]
         for (j in 1:length(prefix.id)) {
            if (j==1) pre.j.i=NULL
            pre.j=pre.data[,c(1,j+1)]
            pre.j=cbind(variable=Replace0(colnames(pre.j)[2],prefix[i]),pre.j)
            names(pre.j)[3]=prefix[i]
            pre.j.i=rbind(pre.j.i,pre.j)
         }
         rownames(pre.j.i)=paste0(pre.j.i[,1],pre.j.i[,2])
         pre.list[[i]]=pre.j.i
      }
      if (missing(var.names)){
         for (i in 1:length(prefix)) {
            if (i==1){
               long=pre.list[[i]]
            }else{
               long=merge(long,pre.list[[i]],all = TRUE)
            }
         }
      }else{
         for (i in 1:length(pre.list)) {
            if (i==1){
               long=pre.list[[i]]
               long[,1]=colnames(long)[ncol(long)]
               colnames(long)[ncol(long)]='value'
            }else{
               long.i=pre.list[[i]]
               long.i[,1]=colnames(long.i)[ncol(long.i)]
               colnames(long.i)[ncol(long.i)]='value'
               long=rbind(long,long.i)
            }
         }
      }
      rownames(long)=NULL
      # merge long and left.data2 ------------------------------------------------
      if (length(left.names)<=1){
         long2=long[,-2]
         return(long2)
      }
      long2=merge(left.data2,long,by = id.names)
      long3=long2[,-1]
      return(long3)
   }
   
}

