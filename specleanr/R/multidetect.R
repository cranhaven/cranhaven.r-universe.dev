#' @title List of outlier detection methods implemented in this package.
#'
#' @return List of methods
#' @export
#'
#' @examples
#' extractMethods()
#'
#'
extractMethods <- function(){

  methodcategories <- c('reference','univariate', 'opt', 'modelbased', 'cluster', 'densitybased', 'covariance')

  for (imethods in methodcategories) {

    if(imethods=='univariate'){

      univariate <- c('adjbox', 'iqr', 'hampel', 'jknife', 'seqfences','mixediqr',
                      'distboxplot','semiqr',  'zscore', 'logboxplot', "medianrule")

    }else if(imethods=='modelbased'){

      modelb <- c('onesvm', 'iforest')

    }else if(imethods=='cluster'){

      clb <- c('kmeans')

    }else if(imethods=='densitybased'){

      dbd <- c('lof', 'knn', 'glosh')

    }else if(imethods=='opt'){

      opt <- c('optimal')

    }else if(imethods=='reference'){

      reference <- c('reference')

    }else{
      covmd <- c('mahal')
    }
  }

  return(list(reference =reference, univariate = univariate, clustermethods = clb,
              densistybased = dbd, optimal = opt,
              modelbased = modelb, covariance=covmd))
}



#' @title Outlier detection method broad classification.
#'
#' @param category The different outlier categories including \code{mult}, \code{uni} and \code{ref}
#'
#' @return \code{vector} method broad categories
#'
#' @export
#' @examples
#'
#' x <- broad_classify(category = "mult")
#'

broad_classify <- function(category){

  match.arg(category, choices = c('uni', 'mult'))

  if(category=='uni'){

    methodsout <- c('adjbox', 'iqr', 'hampel', 'jknife', 'seqfences','mixediqr',
                    'distboxplot','semiqr',  'zscore', 'logboxplot', "medianrule", 'optimal')
  }else{

    methodsout <- c('onesvm', 'iforest','mahal', 'lof', 'knn', 'glosh','kmeans')
  }
  return(methodsout)
}

#Detect multiple outliers or clean data


#' @title Catch errors during methods implementation.
#'
#' @param func Outlier detection function
#' @param fname function name for messaging or warning identification.
#' @param spname species name being handled
#' @param verbose whether to return messages or not. Default \code{FALSE}.
#' @param warn whether to return warning or not. Default TRUE.
#' @param silence_true_errors show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#'
#' @return Handle errors
#'
handle_true_errors <- function(func, fname=NULL, spname=NULL, verbose=FALSE, warn=FALSE, silence_true_errors = TRUE){

  if(isTRUE(silence_true_errors)){

    tout <- tryCatch(expr = func, error = function(e) e)

    if(inherits(tout, "error")){

      if(isTRUE(warn)) warning(fname, ' returned an error, Please check data or parameters for group ', spname, ' and has not executed.', call. = FALSE)

      return(NA)

    } else {

      if(isTRUE(verbose))message(fname, ' was implemented successfully for species ', spname, '.')

      return(tout)
    }
  }else{
    func
  }
}

#' @noRd
#'

detect <- function(x,
                   var,
                   output,
                   exclude,
                   optpar,
                   kmpar,
                   ifpar,
                   lofpar,
                   jkpar,
                   gloshpar,
                   mahalpar,
                   knnpar,
                   zpar,
                   methods,
                   verbose,
                   spname,
                   warn,
                   missingness,
                   silence_true_errors,
                   sdm,
                   pc,
                   bootSettings,
                   na.inform){

  if(length((colnames(x)[colnames(x)==var]))<1) stop('variable ', var, ' is  not found in the species data provided for species ', spname, ' .')

  #check if the variable provided to check in outliers is numeric

  varcheck <- unlist(x[,var])

  if(!is(varcheck, 'numeric')) stop('Only numeric column is allowed for parameter var, variable.')

  if(isTRUE(sdm)){

    #check if the variable parameter provided in var does not have NAs

    if(any(is.na(unlist(varcheck)))==TRUE){

      if(isTRUE(verbose)) message("NAs found in the ", var, " parameter and removed successfully during computation.")

      vecNAs <- which(is.na(unlist(varcheck)==TRUE))

      #calculate the missingness in the var variable

      lenvar_NA <- length(vecNAs)/length(varcheck)

      if(lenvar_NA>missingness) stop(var, " will be removed from data due to NAs. Either increase missingness parameter > ", round(lenvar_NA, 3), " or use different var.")

      x <- x[-vecNAs,]

    }else{
      x
    }

    #check if a particular column has unnecessarily high numbers of NAs

    mValues <- apply(x, 2, function(col)sum(is.na(col))/length(col))


    #remove a column with high NAs instead of the rows if % missing values are greater than the user set %missingness. Default is 10%
    if(all(mValues<missingness)) xdata <- x else xdata <- x[, -which(mValues>=missingness)]

    #exclude columns that are not needed in the computation like the coordinates mostly for multivariate methods

    if(!is.null(exclude)) {
      #check if the columns to be excluded are in the data.
      check.exclude(x=x, exclude = exclude)

      if(!is.null(optpar$mode)) x2data <- na.omit(xdata) else x2data <- na.omit(xdata[,!colnames(xdata) %in% exclude])

    } else { #end of sdm data cleaning
      x2data <- na.omit(xdata)
    }

    #identify and remove non-numeric columns if sdm is TRUE

    df <- x2data[, which(sapply(x2data, is.numeric))]

    xd <- setdiff(colnames(x2data), y=colnames(df))

    if(length(xd)>=1) if(isTRUE(verbose)) message('Non numeric columns ', paste(xd, collapse =','), ' were removed from data.')

    if(is.null(ncol(df)) | !is(df, 'data.frame')){

      stop("Only one column left after discarding non-numeric columns and cannot compute SDMs. Check the str of your data.")

    } else if(ncol(df)==2) {

      warning("Only ", ncol(df), " are remaining the dataset and may fail in runing for biogeographical models.")
    }

    #compute principal component analysis to reduce dimensions

    #pc and bootstrap parameters
    defaults_pc <- list(exec=FALSE, q=TRUE, npc = 3, pcvar = 'PC1')

    pc     <- modifyList(defaults_pc, pc)
    pcs    <- pc$exec
    npc    <- pc$npc
    quiet  <- pc$q
    pcvar  <- pc$pcvar
    #boots
    defaults_boot <- list(run=FALSE, nb= 100, maxrecords = 30,  seed=1135, th = 0.6)

    bootSettings <- modifyList(defaults_boot, bootSettings)

    boot        <- bootSettings$run
    maxrecords  <- bootSettings$maxrecords
    nboots      <- bootSettings$nb
    nbootseed   <- bootSettings$seed
    th          <-  bootSettings$th

    if(isTRUE(pcs)){
      #stop if ecological ranges is used when PCA is set to TRUE

      if("optimal" %in%methods == TRUE) stop("If PCA is set to TRUE, remove optimal method from the list to detect outliers.")

      #try catch pca errors
      df <- tryCatch(pca(df, npc = npc, q= quiet),
                     error=function(e){
                       if(grepl("cannot rescale a constant/zero", e$message)==TRUE | grepl("subscript out of bounds", e$message)==TRUE) {
                         return(NULL)
                       }else{
                         if(grepl("he number of columns", e$message)==TRUE)stop('Numeric data variabales are less than or equal to ', npc,'. Either reduce the npc to 2 or the data is not highly dimensional.',call. = FALSE)
                       }
                     }
      )
      if(!is.null(df)) df else stop("PCA not computed due to zero or constant variance among variables. Remove all columns with zero variance.")

      if(isTRUE(boot)){

        if(!is.data.frame(df)) {

          NROWDF <- nrow(df[[1]])
        } else{

          NROWDF <- nrow(df)

          pcs <- FALSE
        }
        if(NROWDF <= maxrecords){

          df <- boots(df, boots = nboots, seed = nbootseed, pca = pcs)
        }else{
          df
          boot <- FALSE
          warning('Increase the maxrecords ', maxrecords,' to be greater than the rows in data to run bootstrap.', call. = FALSE)
        }
      }

    }else{

      if(isTRUE(boot)){

        if(nrow(df)  <= maxrecords){

          df <- boots(df, boots = nboots, seed = nbootseed, pca = pcs)

        }else{
          df
          boot <- FALSE
          warning('To run bootstrapping increase maxrecords to < nrows in reference DF, or bootsrapping will not run.', call. = FALSE)
        }
      }else{
        boot <- FALSE
        df
      }
    }

  }else{
    #remove NAs in the var

    boot <- FALSE
    pcs <- FALSE
    quiet <- FALSE
    maxrecords <- 10
    nboots <- 1
    nbootseed <- NULL
    pcvar <- NULL
    npc <- 3

    vecNAs <- which(is.na(unlist(varcheck)==TRUE))

    totNA <- length(vecNAs)

    propNA <- round((totNA/nrow(x))*100, 2)

    if(isTRUE(na.inform)) message(totNA, ' (', propNA, '%) NAs removed for parameter ', var, '.')

    if(length(vecNAs)>=1) xdata <- x[-vecNAs,] else xdata <- x


    multivarmethods <- broad_classify(category = "mult")

    removemet <- methods[which(methods%in%multivarmethods==TRUE)]

    if(length(removemet)>=1) stop("Please remove ", paste(removemet, collapse = ','), " from the methods to continue. Use broad_classify() and pick only univariate method category", call. = FALSE)

    df <- xdata
  }

  xmethods <- sapply(methods, function(imx){

    if(imx=='reference'){

      methodout = df

    }else if (imx=='optimal'){

      methodout <-  handle_true_errors(func = ecological_ranges(df, var = var, output= output, species = spname,
                                                                optimumSettings = list(optdf = optpar$optdf, optspcol = optpar$optspcol,
                                                                                       mincol = optpar$mincol, maxcol = optpar$maxcol,
                                                                                       ecoparam = optpar$ecoparam, direction= optpar$direction),
                                                                minval=optpar$minval, maxval=optpar$maxval, lat = optpar$lat, lon = optpar$lon,
                                                                ecoparam=optpar$ecoparam, direction = optpar$direction,
                                                                pct= optpar$par,
                                                                checkfishbase = optpar$checkfishbase, mode=optpar$mode, warn=optpar$warn),
                                       fname = imx, verbose = verbose, spname = spname,
                                       warn=warn, silence_true_errors = silence_true_errors)

    }else if (imx=='adjbox'){

      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout  <-  suppressMessages(handle_true_errors(func =  adjustboxplots(data = df[[bb]], var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                                        fname = imx, verbose = verbose, spname = spname,
                                                        warn=warn, silence_true_errors = silence_true_errors))
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })

        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{
        methodout  <-  suppressMessages(handle_true_errors(func =  adjustboxplots(data = df, var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                                           fname = imx, verbose = verbose, spname = spname,
                                                           warn=warn, silence_true_errors = silence_true_errors))
      }

    }else if(imx=='zscore'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = zscore(data = df[[bb]], var = var, output = output, mode = zpar$mode, type = zpar$type, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })

        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = zscore(data = df, var = var, output = output, mode = zpar$mode, type = zpar$type, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='iqr'){

      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func =  interquartile(data = df[[bb]], var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })

        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func =  interquartile(data = df, var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='semiqr'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func =  semiIQR(data = df[[bb]], var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func =  semiIQR(data = df, var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='hampel'){

      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = hampel(data = df[[bb]], var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })

        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = hampel(data = df, var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='jknife'){

      if(isTRUE(boot)){

        listout  <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = jknife(data = df[[bb]], var = var, output = output, mode = jkpar$mode, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })

        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{
        methodout <-  handle_true_errors(func = jknife(data = df, var = var, output = output, mode = jkpar$mode, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='mahal'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){
          mout = handle_true_errors(func = mahal(data = df[[bb]], exclude = exclude, output = output, mode=mahalpar$mode, pc=pcs, pcvar = pcvar, boot = boot),
                                    fname = imx, verbose = verbose, spname = spname,
                                    warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout = handle_true_errors(func = mahal(data = df, exclude = exclude, output = output, mode=mahalpar$mode, pc=pcs, pcvar = pcvar, boot = boot),
                                       fname = imx, verbose = verbose, spname = spname,
                                       warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='kmeans'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = xkmeans(data = df[[bb]], k= kmpar$k, exclude = exclude, output = output, mode = kmpar$mode,
                                                     method = kmpar$method, verbose=verbose, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = xkmeans(data = df, k= kmpar$k, exclude = exclude, output = output, mode = kmpar$mode,
                                                        method = kmpar$method, verbose=verbose, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }
    }else if(imx=='iforest'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){
          mout <-  handle_true_errors(func = isoforest(data = df[[bb]], size = ifpar$size, output=output, pc=pcs, pcvar = pcvar, boot = boot,
                                                       cutoff = ifpar$cutoff, exclude = exclude),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = isoforest(data = df, size = ifpar$size, output=output, pc=pcs, pcvar = pcvar, boot = boot,
                                                          cutoff = ifpar$cutoff, exclude = exclude),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='onesvm'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){
          mout <-  handle_true_errors(func = onesvm(data = df[[bb]],  exclude = exclude, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = onesvm(data = df,  exclude = exclude, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='lof'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){
          mout <-  handle_true_errors(func = xlof(data = df[[bb]], output =output, minPts = lofpar$minPts,
                                                  exclude = exclude, metric = lofpar$metric, mode=lofpar$mode, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = xlof(data = df, output =output, minPts = lofpar$minPts,
                                                     exclude = exclude, metric = lofpar$metric, mode=lofpar$mode, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='logboxplot'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = logboxplot(data = df[[bb]],  var = var, output = output, x= 1.5, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = logboxplot(data = df,  var = var, output = output, x= 1.5, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='medianrule'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){
          mout <-  handle_true_errors(func = medianrule(data = df[[bb]],  var = var, output = output, x= 2.3, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = medianrule(data = df,  var = var, output = output, x= 2.3, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='distboxplot'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = distboxplot(data = df[[bb]],  var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = distboxplot(data = df,  var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='seqfences'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = seqfences(data = df[[bb]],  var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y = listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = seqfences(data = df,  var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='mixediqr'){

      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = mixediqr(data = df[[bb]],  var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = mixediqr(data = df,  var = var, output = output, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else if(imx=='glosh'){
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = xglosh(data = df[[bb]], k = gloshpar$k,  output = output, metric = gloshpar$metric,
                                                    mode=gloshpar$mode, exclude = exclude, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = xglosh(data = df, k = gloshpar$k,  output = output, metric = gloshpar$metric,
                                                       mode=gloshpar$mode, exclude = exclude, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }else{
      if(isTRUE(boot)){

        listout <- lapply(seq_along(df), function(bb){

          mout <-  handle_true_errors(func = xknn(data = df[[bb]], output = output, metric = knnpar$metric,
                                                  mode=knnpar$mode, exclude = exclude, pc=pcs, pcvar = pcvar, boot = boot),
                                      fname = imx, verbose = verbose, spname = spname,
                                      warn=warn, silence_true_errors = silence_true_errors)
          if(!is.null(mout) && isTRUE(nrow(mout)>=1)) mout$id = bb
          mout
        })
        methodout <- checks(y=listout, var = var, nboots = nboots, th = th)

      }else{

        methodout <-  handle_true_errors(func = xknn(data = df, output = output, metric = knnpar$metric,
                                                     mode=knnpar$mode, exclude = exclude, pc=pcs, pcvar = pcvar, boot = boot),
                                         fname = imx, verbose = verbose, spname = spname,
                                         warn=warn, silence_true_errors = silence_true_errors)
      }

    }
  }, simplify = FALSE)

  listpars <- list(exec = pcs, npc=npc, q = quiet, pcvar =pcvar, run = boot, maxrecords = maxrecords, nboots = nboots, nbootseed = nbootseed )

  attributes(xmethods)$parlist <- listpars

  return(xmethods)
}

#' @title Ensemble multiple outlier detection methods.
#'
#' @description
#' The function allows to ensemble multiple outlier detection methods to ably compare the outliers flagged
#' by each method.
#'
#' @param data \code{dataframe or list}. Data sets for multiple or single species after of extraction of environment predictors.
#' @param var \code{character}. A variable to check for outliers especially the one with directly affects species distribution such as
#' maximum temperature of the coldest month for bioclimatic variables \code{(IUCN Standards and Petitions Committee, 2022))} or
#' stream power index for hydromorphological parameters \code{(Logez et al., 2012)}. This parameter is
#' necessary for the univariate outlier detection methods such as Z-score.
#' @param select \code{vector} The columns that will be used in outlier detection. Make sure only numeric columns are accepted.
#' @param output \code{character}. Either \code{clean}: for a data set with no outliers, or \code{outlier}: to output a dataframe with outliers. Default \code{outlier}.
#' @param exclude \code{vector}. Exclude variables that should not be considered in the fitting the one class model, for example \code{x} and \code{y} columns or
#' latitude/longitude or any column that the user doesn't want to consider.
#' @param ifpar \code{list}. Isolation forest parameter settings. Parameters of the isolation model that are required include
#'     the \strong{cutoff} to be used for denoting outliers. It ranges from \code{0 to 1} but Default \code{0.5}. Also,
#'     the \strong{size} of data partitioning for training should be determined. For more details check \code{(Liu et al. 2008)}
#' @param methods \code{vector}. Outlier detection methods considered. Use \code{\link{extractMethods}} to get outlier detection methods implemented in this package.
#' @param multiple \code{logical}. If the multiple species are considered, then multiple must be set to \code{TRUE} and \code{FALSE} for single species.
#' @param var_col \code{string}. A column with species names if \code{dataset} for species is a dataframe not a list. See \code{\link{pred_extract}} for extracting environmental data.
#' @param optpar \code{list}. Parameters for species optimal ranges like temperatures ranges. For details check \code{\link{ecological_ranges}}.
#' @param kmpar \code{list}. Parameters for k-means clustering like method and number of clusters for tuning. For details, check \code{\link{xkmeans}}.
#' @param lofpar \code{list}. Parameters for local outlier factor such as the distance matrix and mode of method implementation
#'  such as robust and soft mode. For details \code{\link{xlof}}.
#' @param jkpar \code{list}. Parameters for reverse jackknifing mainly the mode used. For details \code{\link{jknife}}.
#' @param gloshpar \code{list}. Parameters for global local outlier score from hierarchies such as distance metric used. For details \code{\link{xglosh}}.
#' @param mahalpar \code{list}. Parameters for Malahanobis distance which includes varying the mode of output  \code{\link{mahal}}.
#' @param knnpar \code{list}. Parameters for varying the distance matrix such as \code{Euclidean} or \code{Manhattan distance}. For details \code{\link{xknn}}
#' @param zpar \code{list}. Parameters for z-score such as \code{mode} and \code{x} parameter. For details \code{\link{zscore}}
#' @param spname \code{string}. species name being handled.
#' @param missingness \code{numeric}. Allowed missing values in a column to allow a user decide whether to remove the individual
#' columns or rows from the data sets. Default 0.1. Therefore,
#'      if a column has more than 10\% missing values, then it will be removed from the dataset rather than the rows.
#' @param verbose \code{logical}. whether to return messages or not. Default \code{FALSE}.
#' @param warn \code{logical}. Whether to return warning or not. Default \code{TRUE}.
#' @param silence_true_errors \code{logical}. Show execution errors and therefore for multiple species the code will break if one of the
#'      methods fails to execute.
#' @param sdm {logical} If the user sets \code{TRUE}, strict data checks will be done including removing all non-numeric
#'      columns from the datasets before identification of outliers. If set to \code{FALSE} non numeric columns will be left
#'      in the data but the variable of concern will checked if its numeric. Also, only univariate methods are allowed. Check
#'      \code{\link{broad_classify}} for the broad categories of the methods allowed.
#' @param na.inform \code{logical} Inform on the NAs removed in executing general datasets. Default \code{FALSE}.
#' @param bootSettings \code{list}. A list of parameters to implement bootstrapping mostly for records below 30.
#'      For details checks \code{\link{boots}}.
#' @param pc \code{list}. A list of parameters to implement principal component analysis for dimesnion reduction.
#'      For details checks \code{\link{pca}}.
#'
#'
#' @details
#' This function computes different outlier detection methods including univariate, multivariate and species
#'      ecological ranges to enables seamless comparison and similarities in the outliers detected by each
#'      method. This can be done for multiple species or a single species in a dataframe or lists or dataframes
#'      and thereafter the outliers can be extracted using the \code{\link{extract_clean_data}} function.
#'
#' @return A \code{list} of outliers or clean dataset of \code{datacleaner} class. The different attributes are
#' associated with the \code{datacleaner} class from \code{multidetect} function.
#'
#' \itemize{
#'         \item{\code{result}: \code{dataframe}. list of dataframes with the outliers flagged by each method.
#'         }
#'        \item{\code{mode}: \code{logical}. Indicating whether it was multiple TRUE or FALSE.}
#'         \item{\code{varused}: \code{character}. Indicating the variable used for the univariate outlier detection methods. }
#'         \item{\code{out}: \code{character}. Whether outliers where indicated by the user or no outlier data. }
#'         \item{\code{methodsused}: \code{vector}. The different methods used the outlier detection process.}
#'         \item{\code{dfname}: \code{character}. The dataset name for the species records.}
#'         \item{\code{exclude}: \code{vector}. The columns which were excluded during outlier detection, if any.}
#'         }
#'
#' @export
#'
#' @importFrom stats na.omit prcomp
#' @importFrom methods new
#' @importFrom utils modifyList
#'
#' @examples
#'
#' \donttest{
#' #' #====
#' #1. Mult detect for general data analysis using iris data
#' #===
#' # the outliers are introduced for testing purposes
#' irisdata1 <- iris
#'
#' #introduce outlier data and NAs
#' rowsOutNA1 <- data.frame(x= c(344, NA,NA, NA),
#'                          x2 = c(34, 45, 544, NA),
#'                          x3= c(584, 5, 554, NA),
#'                          x4 = c(575, 4554,474, NA),
#'                          x5 =c('setosa', 'setosa', 'setosa', "setosa"))
#'
#' colnames(rowsOutNA1) <- colnames(irisdata1)
#'
#' dfinal <- rbind(irisdata1, rowsOutNA1)
#'
#' #===========
#'
#' setosadf <- dfinal[dfinal$Species%in%"setosa",c("Sepal.Width", 'Species')]
#'
#' setosa_outlier_detection <- multidetect(data = setosadf,
#'                                         var = 'Sepal.Width',
#'                                         multiple = FALSE, #'one species
#'                                         methods = c("adjbox", "iqr", "hampel","jknife",
#'                                                     "seqfences", "mixediqr",
#'                                                     "distboxplot", "semiqr",
#'                                                     "zscore", "logboxplot", "medianrule"),
#'                                         silence_true_errors = FALSE,
#'                                         missingness = 0.1,
#'                                         sdm = FALSE,
#'                                         na.inform = TRUE)
#' #======
#' #2.all species
#' #=====
#' multspp_outlier_detection <- multidetect(data = dfinal,
#'                                          var = 'Sepal.Width',
#'                                          multiple = TRUE, #'for multiple species or groups
#'                                          var_col = "Species",
#'                                          methods = c("adjbox", "iqr", "hampel","jknife",
#'                                                      "seqfences", "mixediqr",
#'                                                      "distboxplot", "semiqr",
#'                                                      "zscore", "logboxplot", "medianrule"),
#'                                          silence_true_errors = FALSE,
#'                                          missingness = 0.1,
#'                                          sdm = FALSE,
#'                                          na.inform = TRUE)
#'
#' ggoutliers(multspp_outlier_detection)
#'
#'
#' #======
#' #3. Multidetect for environmental data
#' #======
#' #'Species data
#' data("abdata")
#'
#' #area of interest
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' abpred <- pred_extract(data = abdata,
#'                      raster= worldclim ,
#'                      lat = 'decimalLatitude',
#'                      lon= 'decimalLongitude',
#'                      colsp = 'species',
#'                      bbox = db,
#'                      minpts = 10,
#'                      list=TRUE,
#'                      merge=FALSE)
#'
#'
#' about_df <- multidetect(data = abpred, multiple = FALSE,
#'                      var = 'bio6',
#'                      output = 'outlier',
#'                      exclude = c('x','y'),
#'                      methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                 'logboxplot', 'lof','iforest', 'mahal', 'seqfences'))
#'
#' ggoutliers(about_df)
#'
#'
#' #==========
#' #4. For mulitple species in species distribution models
#' #======
#' data("efidata")
#' data("jdsdata")
#'
#' matchdata <- match_datasets(datasets = list(jds = jdsdata, efi=efidata),
#'                             lats = 'lat',
#'                             lons = 'lon',
#'                             species = c('speciesname','scientificName'),
#'                             date = c('Date', 'sampling_date'),
#'                             country = c('JDS4_site_ID'))
#' #extract data
#' rdata <- pred_extract(data = matchdata,
#'                      raster= worldclim ,
#'                      lat = 'decimalLatitude',
#'                      lon= 'decimalLongitude',
#'                      colsp = 'species',
#'                      bbox = db,
#'                      minpts = 10,
#'                      list=TRUE,
#'                      merge=FALSE)
#'
#' #optimal ranges in the multidetect: made up
#' multspout_df <- multidetect(data = rdata, multiple = TRUE,
#'                       var = 'bio6',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                   'logboxplot', 'lof','iforest', 'mahal', 'seqfences'))
#'
#' ggoutliers(multspout_df, "Anguilla anguilla")
#'
#' #====================================
#' #5. use optimal ranges as a method
#' #create species ranges
#' #===================================
#' #max temperature of "Thymallus thymallus" is made up to make it appear in outliers
#'
#' optdata <- data.frame(species= c("Phoxinus phoxinus", "Thymallus thymallus"),
#'                       mintemp = c(6, 1.6),maxtemp = c(20, 8.6),
#'                       meantemp = c(8.69, 8.4), #'ecoparam
#'                       direction = c('greater', 'greater'))
#'
#' ttdata <- rdata["Thymallus thymallus"]
#'
#' #even if one species, please indicate multiple to TRUE, since its picked from pred_extract function
#'
#' thymallus_out_ranges <- multidetect(data = ttdata, multiple = TRUE,
#'                       var = 'bio1',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                   'logboxplot', 'lof','iforest', 'mahal', 'seqfences', 'optimal'),
#'                       optpar = list(optdf=optdata, optspcol = 'species',
#'                                     mincol = "mintemp", maxcol = "maxtemp"))
#'
#' ggoutliers(thymallus_out_ranges)
#'}
#'
#' @references
#' \enumerate{
#'   \item IUCN Standards and Petitions Committee. (2022). THE IUCN RED LIST OF THREATENED SPECIESTM Guidelines for Using the IUCN Red List
#' Categories and Criteria Prepared by the Standards and Petitions Committee of the IUCN Species Survival Commission.
#' https://www.iucnredlist.org/documents/RedListGuidelines.pdf.
#' \item Liu, F. T., Ting, K. M., & Zhou, Z. H. (2008, December). Isolation forest.
#' In 2008 eighth ieee international conference on data mining (pp. 413-422). IEEE.
#' }
#'

multidetect <- function(data,
                        var,
                        select = NULL,
                        output = "outlier",
                        exclude = NULL,
                        multiple,
                        var_col = NULL,
                        optpar = list(optdf = NULL, ecoparam = NULL, optspcol = NULL, direction =NULL,
                                      maxcol = NULL, mincol = NULL, maxval = NULL, minval = NULL,
                                      checkfishbase =FALSE, mode = NULL, lat = NULL, lon = NULL, pct = 80,
                                      warn = FALSE),
                        kmpar =list(k=6, method='silhouette', mode='soft'),
                        ifpar = list(cutoff = 0.5, size=0.7),
                        mahalpar = list(mode='soft'),
                        jkpar = list(mode='soft'),
                        zpar = list(type='mild', mode='soft'),
                        gloshpar = list(k= 3, metric='manhattan', mode='soft'),
                        knnpar = list(metric='manhattan', mode='soft'),
                        lofpar = list(metric='manhattan', mode='soft', minPts= 10),
                        methods,
                        bootSettings = list(run=FALSE, nb=5, maxrecords = 30, seed=1135, th = 0.6),
                        pc = list(exec = FALSE, npc=2, q = TRUE, pcvar = 'PC1'),
                        verbose=FALSE, spname=NULL,warn=FALSE,
                        missingness = 0.1,
                        silence_true_errors = TRUE,
                        sdm = TRUE,
                        na.inform = FALSE){

  if(missing(data)) stop('Species data missing')

  #check if var is the excluded strings

  if(any(var%in%exclude==TRUE)==TRUE) stop("Remove ", var, " among the strings to be excluded.")

  match.argc(output, c("clean", "outlier"))

  if(length(var)>1 && isFALSE(multiple)) stop("For mulitple variables of concern, set multiple to TRUE.")

  #check if all methods indicated exist in the package

  methodsin <- Reduce(c, extractMethods()) #allowed in

  tfcheck <- unique(methods) %in% methodsin

  if(any(tfcheck==FALSE)){

    notsupported <- unique(methods)[which(tfcheck==FALSE)]

    stop('The methods -', paste(notsupported, collapse = ', '), '- are/is not accepted. Check extractMethods() for allowed methods.')
  }

  if(multiple ==FALSE & !is.null(var_col)) stop("For single species do not provide the var_col parameter.")

  #run for single dataframe

  if(isFALSE(multiple) && is.null(var_col) ){

    if(!is(data, 'data.frame')) stop('For a single species only a dataframe is accepted.')

    if(isTRUE(warn))if(nrow(data)<ncol(data)) warning('Number of rows are less than variables and some methods may not function properly.', call. = FALSE)

    if(!is.null(select)) dsel <- subset(x = data, select = select) else dsel <- data

    methodata <-  detect(x = dsel, var = var, output = output,
                         exclude = exclude,optpar = optpar,
                         kmpar = kmpar, ifpar = ifpar, jkpar = jkpar,
                         mahalpar = mahalpar, lofpar = lofpar,
                         zpar = zpar, gloshpar = gloshpar,
                         knnpar = knnpar,
                         methods = unique(methods),
                         verbose = verbose,
                         spname = spname,warn=warn,
                         missingness = missingness,
                         silence_true_errors = silence_true_errors,
                         sdm = sdm,
                         pc = pc,
                         bootSettings = bootSettings,
                         na.inform = na.inform)

    plist <- attributes(methodata)$parlist

  }else {

    if(is(data, 'list')){

      df <- data

    }else if(is(data, 'data.frame') ){

      if(length(var)>1){

        #check if all variables are in the dataset provided

        vartf <- var%in%colnames(data)

        if(all(vartf)==FALSE) stop("The variables ", paste(var[which(vartf==FALSE)], collapse = ', '), " are not in the provided dataset. check and try again.", call. = FALSE)

        df <- sapply(var, function(x) x <-  data, simplify = FALSE)

      }else{

        if(is.null(var_col)) stop('For multiple species dataframe, provide the species column name in the var_col parameter.')

        if(!any(colnames(data)%in%var_col)==TRUE) stop('The column name provided in var_col parameter is not in the species data.')

        if(!is.null(exclude)) if((var_col%in%exclude)==TRUE) warning("Remove the column for species names in the exclude parameter.")

        df <- split(data, f= data[,var_col])
      }

    }else{
      stop('Data format not recognised, Only lists of datasets or dataframe are accepted.')
    }
    methodata <- sapply(names(df), function(xp){

      dfinal<- df[[xp]]

      if(isTRUE(warn)) if(nrow(dfinal)<=ncol(dfinal)) warning('The ', nrow(dfinal),' rows for ',xp,' are less than variables and some methods may not function properly.')

      #if the variables are greater 1, then the data was partitioned by variables not species

      if(length(var)>1) var1 = xp else var1 = var

      if(!is.null(select)) dsel <- subset(x = dfinal, select = select) else dsel <- dfinal

      d <-  detect(x = dsel, var = var1, output = output,
                   exclude = exclude,optpar = optpar,
                   mahalpar = mahalpar, lofpar = lofpar,ifpar = ifpar, kmpar = kmpar,
                   zpar = zpar, gloshpar = gloshpar, knnpar = knnpar, jkpar = jkpar,
                   methods = unique(methods), verbose = verbose, spname = xp,warn=warn,
                   pc = pc,bootSettings = bootSettings,
                   missingness = missingness, silence_true_errors = silence_true_errors, sdm = sdm, na.inform = na.inform)
    }, simplify = FALSE)
    plist <- attributes(methodata[[1]])$parlist
  }
  if(isFALSE(multiple)){

    return(new('datacleaner', result = methodata, mode = multiple, varused = var,
               out = output, methodsused = unique(methods), dfname = deparse(substitute(data)),
               excluded = exclude, pc= plist$exec, pcretained = plist$npc, pcvariable = plist$pcvar,
               bootstrap = plist$run, nboots = plist$nboots, maxrecords= plist$maxrecords))
  }else{
    return(new('datacleaner', result = methodata, mode = multiple, varused = var,
               out = output, methodsused = unique(methods), dfname = deparse(substitute(data)),
               excluded = exclude, pc= plist$exec, pcretained = plist$npc, pcvariable = plist$pcvar,
               bootstrap = plist$run, nboots = plist$nboots, maxrecords= plist$maxrecords))

  }
}



