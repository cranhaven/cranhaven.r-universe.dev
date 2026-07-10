get_cutoff <- function(regress,data,x,cut.numb,n.per,include,round,y,y.per){
    if ((cut.numb+1) * n.per > 1){
        message('n.per or cut.cumb is so big')
        return(NULL)
    }
    data=delet_na_df(data)
    data=data[order(data[,x]),]
    x=data[,x]
    x.cut=unique(x)
    combn=t(combn(x.cut,cut.numb))
    message('\n1: all combination: ',nrow(combn),'\n')
    hx=ceiling(nrow(combn)/10^(nchar(nrow(combn))-1))
    int=1:hx * 10^(nchar(nrow(combn))-1)
    message('2: filter by n.per')
    cat('  ',paste0(rep('-',(hx-1)),collapse = ''),'\n')
    # n.per ---------------------------------------------------------------------
    for (i in 1:nrow(combn)) {
        if (i==1){
            tab=NULL
            prop=NULL
        }
        res.i=suppressMessages(cutit(x = x,
                                     cut_points = combn[i,],
                                     include = include))
        if (length(unique(res.i)) != (cut.numb+1)){
            combn[i,]=NA
        }else{
            tab.pn=table(res.i)
            prop.pn=prop.table(tab.pn)
            if (!all(prop.pn >= n.per)){
                combn[i,]=NA
            }else{
                tab=c(tab,paste0(tab.pn,collapse = '/'))
                prop.pn=digital(prop.pn,round)
                prop=c(prop,paste0(prop.pn,collapse = '/'))
            }
        }
        if (any(i== int)){
            if (i==int[1]) cat('   ')
            cat('=')
            }
    }
    comb.pn=data.frame(na.omit(combn))
    if (nrow(comb.pn)==0){
        message('n.per or cut.cumb is so big')
        return(NULL)
    }
    colnames(comb.pn)=paste0('cut',1:cut.numb)
    res.n=cbind(comb.pn,n=tab,n.per=prop)
    # if linear regression return res.n, else go on
    cat('\n   Combination: ',nrow(res.n),'\n')
    if (do::left('linear',nchar(regress))==regress){
        return(res.n)
    }
    res.cut=data.frame(res.n[,1:cut.numb])
    colnames(res.cut)=colnames(res.n)[1:cut.numb]
    if (nrow(res.cut)==0){
        message('cut.cumb is so big')
        return(NULL)
    }
# y.n ---------------------------------------------------------------------
    message('\n3: filter by y.per')

    for (i in 1:nrow(res.cut)) {
        if (i==1) {
            n.y=NULL
            prop=NULL
        }
        cut.lab=cutit(x,res.cut[i,],include)
        tab.y=table(cut.lab,y=data[,y])
        prop.y=tab.y[,2]/rowSums(tab.y)
        if (all((prop.y >= y.per))){
            n.y=c(n.y,paste0(tab.y[,2],collapse = '/'))
            prop.y=digital(prop.y,round)
            prop=c(prop,paste0(prop.y,collapse = '/'))
        }else{
            res.n[i,]=NA
        }
    }
    res.p=data.frame(na.omit(res.n))
    if (nrow(res.p)==0){
        message('y.per or cut.cumb is so big')
        return(NULL)
    }
    res.cut=cbind(res.p,y=n.y,y.per=prop)
    cat('   Combination: ',nrow(res.cut),'\n')
    return(res.cut)
}
