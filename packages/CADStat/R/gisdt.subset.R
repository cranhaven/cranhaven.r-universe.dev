gisdt.subset <- function(subset.data, subset1.name=NULL, subset1.val=NULL, 
                         subset2.name=NULL, subset2.val=NULL,
                         na.check=NULL){
  if(is.null(subset1.val)) subset1.name=NULL
  if(is.null(subset2.val)) subset2.name=NULL

  na.check = c(na.check,subset1.name,subset2.name)
  if(any(!is.null(na.check))){
    na.check = na.check[!is.null(na.check)]
    # 5.28.2009 corrected bug here
    #subset.data = na.omit(subset.data[,na.check,drop=FALSE])
    for (i in 1:length(na.check)) {
      subset.data <- subset.data[!is.na(subset.data[,na.check[i]]),]
    }
  }

  if (!is.null(subset1.name) & !is.null(subset2.name)) {
   subset.data = subset.data[subset.data[,subset1.name]%in%subset1.val & subset.data[,subset2.name]%in%subset2.val,]
  } else if (!is.null(subset1.name)) {
    subset.data = subset.data[subset.data[,subset1.name]%in%subset1.val,]
  } else if (!is.null(subset2.name)) {
    subset.data = subset.data[subset.data[,subset2.name]%in%subset2.val,]
  } else {
    subset.data = subset.data
  }
  subset.data
}
