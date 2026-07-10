#' @title Join two dataframes together
#' @description Join two dataframes by the same id column.
#' @details join_inner(), join_full(), join_left(), join_right() and join_out() are five
#'     functons to joint two dataframes together. They are based on package 'data.table', 
#'     so they are more efficient and fast.
#' @param x one dataframe
#' @param y the other dataframe
#' @param by the id name in x and y dataframe
#' @importFrom data.table `.__T__[:base`
#' @name join
#' @return one joined dataframe.
#' @export
#'
#' @examples
#' df1=data.frame(x=rep(c('b','a','c'),each=3),
#'               y=c(1,3,6),
#'               v=1:9)
#' 
#' df2=data.frame(x=c('c','b','e'),
#'                v=8:6,
#'                foo=c(4,2,1))
#' join_inner(df1,df2,'x')
#' join_full(df1,df2,'x')
#' join_left(df1,df2,'x')
#' join_right(df1,df2,'x')
#' join_out(df1,df2,'x')

join_inner <- function(x,y,by=NULL){
    if (is.null(by)){
        by=(names(x)[names(x) %in% names(y)])[1]
        message('Joinging by: ',by)
    }
    if (data.table::is.data.table(x)) x=data.frame(x)
    if (is.matrix(x)) x=data.frame(x)
    if (data.table::is.data.table(y)) y=data.frame(y)
    if (is.matrix(y)) y=data.frame(y)
    if (is.factor(x[,by])) x[,by]=as.character(x[,by])
    if (is.factor(y[,by])) y[,by]=as.character(y[,by])
    xi=data.table::data.table(x)
    yi=data.table::data.table(y)
    data.frame(xi[yi,on=by,nomatch=0])
}
#' @rdname join
#' @export
join_full <- function(x,y,by=NULL){
    
    if (is.null(by)){
        by=(names(x)[names(x) %in% names(y)])[1]
        message('Joinging by: ',by)
    }
    if (data.table::is.data.table(x)) x=data.frame(x)
    if (data.table::is.data.table(y)) y=data.frame(y)
    if (is.factor(x[,by])) x[,by]=as.character(x[,by])
    if (is.factor(y[,by])) y[,by]=as.character(y[,by])
    ukey=unique(c(x[,by], y[,by]))
    ukey=data.table::data.table(ukey)
    names(ukey)=by
    xjoint=data.table::data.table(x)
    yjoint=data.table::data.table(y)
    string1=paste0('data.table::setkey(xjoint,',by,')')
    string2=paste0('data.table::setkey(yjoint,',by,')')
    eval(parse(text = string1))
    eval(parse(text = string2))
    data.frame(yjoint[xjoint[list(ukey), on=by]  ])
}
#' @rdname join
#' @export
join_left <- function(x,y,by=NULL){
    if (is.null(by)){
        by=(names(x)[names(x) %in% names(y)])[1]
        message('Joinging by: ',by)
    }
    if (data.table::is.data.table(x)) x=data.frame(x)
    if (data.table::is.data.table(y)) y=data.frame(y)
    if (is.factor(x[,by])) x[,by]=as.character(x[,by])
    if (is.factor(y[,by])) y[,by]=as.character(y[,by])
    xi=data.table::data.table(x)
    yi=data.table::data.table(y)
    data.frame(yi[xi,on=by])
}
#' @rdname join
#' @export
join_right <- function(x,y,by=NULL){
    if (is.null(by)){
        by=(names(x)[names(x) %in% names(y)])[1]
        message('Joinging by: ',by)
    }
    if (data.table::is.data.table(x)) x=data.frame(x)
    if (data.table::is.data.table(y)) y=data.frame(y)
    if (is.factor(x[,by])) x[,by]=as.character(x[,by])
    if (is.factor(y[,by])) y[,by]=as.character(y[,by])
    xi=data.table::data.table(x)
    yi=data.table::data.table(y)
    data.frame(xi[yi,on=by])
}
#' @rdname join
#' @export
join_out <- function(x,y,by=NULL){
    if (is.null(by)){
        by=(names(x)[names(x) %in% names(y)])[1]
        message('Joinging by: ',by)
    }
    if (data.table::is.data.table(x)) x=data.frame(x)
    if (data.table::is.data.table(y)) y=data.frame(y)
    if (is.factor(x[,by])) x[,by]=as.character(x[,by])
    if (is.factor(y[,by])) y[,by]=as.character(y[,by])
    ukey=unique(c(x[,by][! x[,by] %in% y[,by]], y[,by][! y[,by] %in% x[,by]]))
    ukey=data.table::data.table(ukey)
    names(ukey)=by
    xjoint=data.table::data.table(x)
    yjoint=data.table::data.table(y)
    string1=paste0('data.table::setkey(xjoint,',by,')')
    string2=paste0('data.table::setkey(yjoint,',by,')')
    eval(parse(text = string1))
    eval(parse(text = string2))
    data.frame(yjoint[xjoint[list(ukey), on=by]  ])
}
