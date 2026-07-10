#' Connect Duplicated Values
#'
#' @param data dataframe or matrix
#' @param id id column names or indexs
#' @param dup.var duplicated column names or indexs
#'
#' @return dataframe contains id and duplicated values
#' @export
#'
#' @examples
#' dup.connect(data = mtcars,id = 'am',dup.var = 'cyl')
#' dup.connect(data = mtcars,
#'              id = c('am','gear'),
#'              dup.var = c('cyl','qsec'))
dup.connect <- function(data,id,dup.var){
    left.check=0
    left.name=not(colnames(data),c(id,dup.var))
    if (length(left.name)==1){
        if (is.factor(data[,left.name])){
            left.check=1
            left.name.level=levels(data[,left.name])
            data[,left.name]=as.character(data[,left.name])
        }
    }
    for (i in 1:length(id)) {
        if (i==1){
            res.id=data[,id[i]]
        }else{
            res.id=paste0(res.id,data[,id[i]])
        }
    }
    data.id=data[,id]
    if (length(id)==1){
        data.id=data.frame(data.id)
        colnames(data.id)=id
    }
    data.u=row.freq(data.id)
    res.id.u=unique(res.id)
    for (i in 1:length(dup.var)) {
        dup.i=dup.var[i]
        for (j in 1:length(res.id.u)) {
            if (j==1) res.j=NULL
            x.j=data[res.id==res.id.u[j],dup.i]
            res.j=c(res.j,paste0(x.j,collapse = ';'))
        }
        if (i==1){
            res=res.j
        }else{
            res=cbind(res,res.j)
        }
    }
    res=data.frame(res)
    colnames(res)=dup.var
    res=equal_length(res,' ',colname = TRUE)
    if (left.check==1){
        res[,left.name]=factor(x = res[,left.name],
                                levels = left.name.level)
    }
    cbind(data.u,res)
}


