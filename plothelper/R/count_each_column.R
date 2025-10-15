#' Counting Each Column and Summarizing in a Matrix
#' 
#' This function counts the frequencies of each element of each 
#' column of a data frame or matrix. The frequencies of 
#' missing values and the 0 frequencies of non-existent 
#' values are also included in the final result.
#' 
#' @param x a data frame or matrix with at least 1 row and 
#' 1 column. NOTE: all column should belong to the same 
#' class (numeric, character). 
#' However, if \code{checks = TRUE}, character and 
#' factor variables can co-exist and logical values are also OK. 
#' If a column has nothing but 
#' NA, it should be remove; otherwise, an error will be raised.
#' @param answer the values whose frequencies you 
#' want to know, e. g., "agree" and "disagree" 
#' in your survey data. Default is NULL which means 
#' all possible answers in the whole data will be used.
#' @param checks whether to check the validity of the 
#' input data. Default is TRUE. Do not turn it off unless you 
#' are sure that your data has no logical variables or factor 
#' variables and each column has at least 1 non-missing value.
#'
#' @export
#' @examples
#' # values that do not appear in 
#' # the data can also be counted.
#' # a factor will be transformed into
#' # a character variable automatically.
#' x1=c("a", "b", "a", "b", NA)
#' x2=factor(x1)
#' x3=c("1", "3", "2", "1", "a")
#' dat=data.frame(x1, x2, x3, stringsAsFactors=FALSE)
#' res=count_each_column(dat, answer=c("c", "d", NA, "a"))
#' # logical value is OK.
#' x1=c(TRUE, TRUE, TRUE)
#' x2=c(TRUE, NA, NA)
#' dat=data.frame(x1, x2)
#' res=count_each_column(dat)
#' res=count_each_column(dat, c(TRUE, FALSE))
count_each_column=function(x, answer=NULL, checks=TRUE){
	cla=class(x)[1]
	if (! cla %in% c("matrix", "data.frame")) stop("x must be a matrix or data frame.")
	if (ncol(x) == 0 | nrow(x) == 0) stop("x must have at least 1 row and 1 column.")
	if (cla == "matrix") x=data.frame(x)
	if(checks==TRUE) x=check_n_c_f(x)
	real_answer=unique(unlist(plyr::alply(x, 2, .fun=unique)))
	if (is.null(answer)){
		char_real_answer=sort(as.character(real_answer), na.last=TRUE)
		ori_answer=char_real_answer
		enriched_real_answer=real_answer
	} else {
		if (anyDuplicated(answer) != 0) stop("Argument answer should not have duplicated values.")
		if (is.factor(answer)) answer=as.character(answer)
		if (is.logical(answer)) answer=as.numeric(answer)
		ori_answer=as.character(answer)
		enriched_real_answer=unique(c(real_answer, answer))
	}
	TABLE_ALL=function(x, ANSWER, USENA="always") table(c(x, ANSWER), useNA=USENA)-1
	m=apply(x, 2, FUN=TABLE_ALL, ANSWER=enriched_real_answer, USENA=if (anyNA(enriched_real_answer)) "always" else "no" )
	m=t(m)
	if (! is.null(colnames(x))) rownames(m)=colnames(x)
	mcolname=colnames(m)
	mcolname[is.na(mcolname)]="NA"
	colnames(m)=mcolname
	ori_answer[is.na(ori_answer)]="NA"
	m[, ori_answer, drop=FALSE]
}

check_n_c_f=function(x){
	## do NOT use apply(x, 2, class), for it fails to check class
	## do NOT use adply, it fails to handle
	nc=ncol(x)
	for (h in 1: nc){
		if (all(is.na(x[, h]))) stop(paste("All elements in column ", h, " are NAs. Please remove this column.", sep=""))
	}
	all_class=c()
	for (i in 1: nc) all_class=append(all_class, if(is.numeric(x[, i])) "numeric" else class(x[, i]))
	cha_as_fac=all_class
	which_fac=which(cha_as_fac=="factor")
	if (length(which_fac)>0) cha_as_fac[which_fac]="character"
	if (length(unique(cha_as_fac)) != 1) stop ("All columns should belong to the same class.")
	if (length(which_fac)>0){
		for (j in which_fac){
			x[, j]=as.character(x[, j])
		}
	}
	if ("logical" %in% all_class){
		for (k in 1: nc){
			x[, k]=as.numeric(x[, k])
		}
	}
	x
}
