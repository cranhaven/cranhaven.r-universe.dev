#' @importFrom stats na.omit
delet_na_df<-function(data){
    if (any(is.na(data))){
        nrow.origin=nrow(data)
        data=na.omit(data)
        nrow.new=nrow(data)
        diff=nrow.origin-nrow.new
        message('\n')
        message(diff,' rows were deleted because of missing value.')
        message(nrow.new,' rows were analysised')
        message('\n\n')
        data
    }else{
        data
    }
}
