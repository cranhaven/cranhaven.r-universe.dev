setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("vetcorOrNULL", c("vector", "NULL"))

#' @title Outlier detection class for multiple methods
#'
#' @slot result List of data sets with outliers detected.
#' @slot mode Either ´TRUE´ for multiple species and FALSE for one species.
#' @slot varused The variable used for outlier detection, useful for univariate outlier detection methods.
#' @slot out Either outliers or clean dataset outputted.
#' @slot methodsused The methods used in outlier detection.
#' @slot dfname the dataframe name to aid tracking it during clean data extraction.
#' @slot excluded whether some columns were excluded during outlier detection. useful for multivariate methods where coordinates are removed from the data.
#' @slot pc parameters for principal component analysis.
#' @slot bootstrap parameters for bootstrapping for small data sets.
#' @slot nboots the number of bootstraps during bootstrapping.
#' @slot pcvariable variable to be considered during PCA.
#' @slot pcretained the number data columns retained. the default is 3.
#' @slot maxrecords the maximum number of records used for bootstrapping.
#'
#' @export
#'
setClass(Class = 'datacleaner',
         representation = list(result     ='list',
                               mode       = 'logical',
                               varused    = 'character',
                               out        ='character',
                               methodsused='vector',
                               dfname     ='character',
                               excluded   ='vetcorOrNULL',
                               pc         ='logical',
                               bootstrap  ='logical',
                               nboots     = 'numeric',
                               pcvariable ='CharacterOrNULL',
                               pcretained = 'numeric',
                               maxrecords = 'numeric')
)


#' @title set method for displaying output details after outlier detection.
#'
#' @param object The data model for outlier detection.
#'
#' @importFrom methods show
#'
#' @export
#'
#' @return prints the datacleaner class for this package.
#'
setMethod(f='show', signature = 'datacleaner', definition = function(object){

  if(object@mode==FALSE){
    cat(" ======================================",'\n',
        ' Outlier detection summary','\n',
        "======================================",'\n',
        'Number of variables      :',   1,'\n',
        'Number of methods        :',   length(object@result),'\n',
        'Methods used             :',   paste(object@methodsused, collapse = ','), '\n',
        'Multiple                 :',   object@mode,'\n',
        'Variable                 :',   noquote(object@varused),'\n',
        'Output                   :',   noquote(object@out),'\n',
        'Dataset Name             :',   noquote(object@dfname),'\n',
        'Excluded columns         :',   paste(object@excluded, collapse = ','), '\n',
        "--------------------------------------",'\n',
        'Principal component settings','\n',
        "--------------------------------------",'\n',
        'Principal components     :',   object@pc,'\n',
        'PCs retained             :',   if(isTRUE(object@pc)) object@pcretained else NA,'\n',
        'PC variable used         :',   if(isTRUE(object@pc)) object@pcvariable else NA,'\n',
        "--------------------------------------",'\n',
        'Bootsrapping settings','\n',
        "--------------------------------------",'\n',
        'Bootstrapping            :',   object@bootstrap,'\n',
        'Number of bootsraps      :',   if(isTRUE(object@bootstrap)) object@nboots else NA,'\n',
        'Maximum sample records   :',   if(isTRUE(object@bootstrap)) object@maxrecords else NA,'\n',
        "======================================")
  }else{
    cat(" ======================================",'\n',
        ' Data cleaning summary','\n',
        "======================================",'\n',
        'Number of variables      :',   length(object@result),'\n',
        'Number of methods        :',   length(object@result[[1]]),'\n',
        'Methods used             :',   paste(object@methodsused, collapse = ','), '\n',
        'Multiple                 :',   object@mode,'\n',
        'Variable                 :',   noquote(object@varused),'\n',
        'Output                   :',   noquote(object@out),'\n',
        'Dataset Name             :',   noquote(object@dfname),'\n',
        'Excluded columns         :',   paste(object@excluded, collapse = ','), '\n',
        "--------------------------------------",'\n',
        'Principal component settings','\n',
        "--------------------------------------",'\n',
        'Principal components     :',   object@pc,'\n',
        'PCs retained             :',   if(isTRUE(object@pc)) object@pcretained else NA,'\n',
        'PC variable used         :',   if(isTRUE(object@pc)) object@pcvariable else NA,'\n',
        "--------------------------------------",'\n',
        'Bootsrapping settings','\n',
        "--------------------------------------",'\n',
        'Bootstrapping            :',   object@bootstrap,'\n',
        'Number of bootsraps      :',   if(isTRUE(object@bootstrap)) object@nboots else NA,'\n',
        'Maximum sample records   :',   if(isTRUE(object@bootstrap)) object@maxrecords else NA,'\n',
        "======================================")
  }
}
)
