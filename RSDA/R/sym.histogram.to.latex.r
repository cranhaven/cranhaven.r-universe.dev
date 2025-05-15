#' optim.desv.fun.interval
#' @keywords internal
#' @importFrom xtable xtable
sym.histogram.to.latex<-function(datasym.V3.histogram){
  dim.histogram<-dim(datasym.V3.histogram)
  colnames.histogram<-colnames(datasym.V3.histogram)
  latex.histogram<-as.data.frame(matrix(rep("X",dim.histogram[1]*dim.histogram[2]), nrow = dim.histogram[1]))
  colnames(latex.histogram)<-colnames.histogram
  for(i in 1:dim.histogram[1]){
    for(j in 1:dim.histogram[2]){
      breaks.histogram<-length(datasym.V3.histogram[[j]][[i]]$props)
      sal<- paste0("{[",paste(datasym.V3.histogram[[j]][[i]]$breaks[1],",",datasym.V3.histogram[[j]][[i]]$breaks[2],"],", round(datasym.V3.histogram[[j]][[i]]$props[1], digits = 2)))
      if(breaks.histogram > 1){
        for(s in 2:breaks.histogram){
          sal_tmp<- paste0("[",paste0(datasym.V3.histogram[[j]][[i]]$breaks[s],",",datasym.V3.histogram[[j]][[i]]$breaks[s+1],"],",round(datasym.V3.histogram[[j]][[i]]$props[s], digits = 2)))
          sal<-paste0(sal,"; ",sal_tmp)
        }
      }
      sal<-paste0(sal,"}")
      latex.histogram[i,j]<-sal
    }
  }
  row.names(latex.histogram)<-row.names(datasym.V3.histogram)
  return(xtable::xtable(latex.histogram,digits = 2))
}
