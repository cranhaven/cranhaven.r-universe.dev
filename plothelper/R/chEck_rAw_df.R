chEck_rAw_df=function(x, must_matrix=FALSE, must_df=TRUE, column_num=2, must_numeric=FALSE, no_na=TRUE){
	cla=class(x)[1]
	if (! cla %in%  c("matrix", "data.frame", "tbl_df")) stop("The input must be of class data.frame or matrix or tbl_df.")	
	if (cla=="tbl_df") x=as.data.frame(x)
	if (cla=="matrix" & must_df==TRUE) x=data.frame(x, stringsAsFactors=FALSE)
	if (cla=="data.frame" & must_matrix==TRUE) x=as.matrix(x)
	if (nrow(x) == 0 ) stop("The input must have at least 1 row.")
	if (! is.null(column_num)){
		temp_ncol_x=ncol(x)
		if (temp_ncol_x > column_num){
			x=x[, 1: column_num, drop=FALSE]
		} else if (temp_ncol_x < column_num){
			stop("Data should have at least ", column_num, " columns, but the number of column of x is less than that.")
		}
	}
	if (must_numeric==TRUE){
		for (i in 1: ncol(x)){
			if (! is.numeric(x[, i])) stop("All columns must be numeric.")
		}
	}
	if (no_na==TRUE){
		for (i in 1: ncol(x)){
			if (anyNA(x[, i])) stop("Column ", i, " should not have NA.")
		}
	}	
	if (ncol(x) == 2) colnames(x)=c("x", "y")
	x
}

# x is a list of matrix, df, tibble; add_name is the name for the extra
# column for each df in result. 
# function to add group number for each df
add_group_number=function(x, add_name="g"){
	ADDGNUMBER=function(X, INDEX, ADD_NAME){
		if (class(X)[1] != "data.frame") X=as.data.frame(X)
		colna=colnames(X)
		Y=cbind(X, INDEX)
		colnames(Y)=c(colna, ADD_NAME)
		Y
	}
	mapply(ADDGNUMBER, X=x, INDEX=1: length(x), ADD_NAME=add_name, SIMPLIFY=FALSE)
}
# m=matrix(1:8, nr=2)
# df=data.frame(m)
# mm=list(m, m)
# dfdf=list(df, df)
# res=add_group_number(mm, "hh")
# res=add_group_number(dfdf, "hh")
