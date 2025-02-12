
read_SGP <- function(path = ".", type = NULL,
                     file.ext = ".RData", verbose = TRUE)
{
  types <- c("SGP","SGP.CV")

  info <- lapply(types,function(type){
    pattern <- paste0(type,file.ext)
    fullpath <- normalizePath(paste0(path,".*",pattern), mustWork=FALSE)
    infolder <- dirname(fullpath)
    filenames <- basename(grep(pattern=paste0(fullpath,"$"), value=TRUE,
                               x=list.files(infolder, full.names=TRUE)))
    list(pattern=pattern, infolder=infolder, filenames=filenames)
  })
  names(info) <- types

  if(!is.null(type)){
    type <- match.arg(type, choices=types)
    info <- info[type]
  }
  tt <- unlist(lapply(info,function(x)length(x$filenames)>0))

  fm <- out <- NULL
  error <- 0
  if(sum(tt) == 2){
    stop("Both 'SGP' and 'SGP.CV' file types were found at the path.\n",
         "  Set input 'type' to either type = 'SGP' or type = 'SGP.CV'")
  }
  if(sum(tt) == 0){
    if(verbose){
      tmp <- paste(paste0("'*",types,file.ext,"'"),collapse=" or ")
      message("No output file(s) of type/extension ",tmp," was found at the path")
    }
    error <- error + 1
  }
  if((sum(tt) == 1) & (error == 0)){
    type <- names(tt[tt])
    pattern <- info[[type]]$pattern
    infolder <- info[[type]]$infolder
    filenames <- info[[type]]$filenames

    #pattern2 <- "subset_[0-9]+_of_[0-9]+_SGP.RData"
    pattern2 <- paste0("subset_[0-9]+_of_[0-9]+_",type,file.ext)
    filenames2 <- grep(pattern2, filenames, value=TRUE)
    if(length(filenames2) > 0)
    {
      if(setequal(filenames,filenames2))
      {
        tmp <- strsplit(stringr::str_extract(filenames, pattern2), "_")
        files <- as.numeric(unlist(lapply(tmp,function(x) x[2])))
        nfiles <- unique(as.numeric(unlist(lapply(tmp,function(x) x[4]))))
        index <- order(files)

        # Checkpoint
        pp <- unlist(strsplit(pattern2, "\\[0-9]\\+"))
        if(length(nfiles) == 1L){
          fail <- ifelse(length(files)==nfiles,!all(files[index]==seq(nfiles)),TRUE)
          if((length(filenames)!=nfiles) | fail){
            stop(nfiles," output files with format '",paste0(pp[1],"*",pp[2],nfiles,pp[3]),"'",
                 " are expected: ",paste(1:nfiles,collapse=","))
          }
        }else{
          stop("Output files with different formats were found: \n",
               paste(paste0("    ",pp[1],"*",pp[2],nfiles,pp[3]),collapse="\n"))

        }
        filenames <- filenames[index]
      }else{
        stop("Output files with different formats were found at the path")
      }

    }else{
      if(length(filenames)>1){
        stop("More than one output file with type/extension '*",pattern,"' were found at the path")
      }
    }

    filenames <- normalizePath(paste0(infolder,"/",filenames))
    nfiles <- length(filenames)
    modelparms <- isCV <- issummary <- rep(NA,nfiles)
    names_parms <- c("n","ntraits","nTRN","nTST","nlambda","nfolds","nCV")

    for(k in seq(nfiles))
    {
      tmp <- load(filenames[k], verbose=FALSE)
      if(tmp[1] != "out"){ # change the name of the object if different from 'out'
        eval(parse(text = paste0("out <- ",tmp[1])))
      }
      if(!inherits(out, "SGP")){
        stop("Object in file '",basename(filenames[k]),"' is not of the class 'SGP'")
      }

      tmp <- names_parms[names_parms %in% names(out)]
      modelparms[k] <- paste0(paste0(tmp,"=",unlist(out[tmp])),collapse=", ")
      isCV[k] <- "SGP.CV" %in% attr(out,"type")
      issummary[k] <- "summary" %in% attr(out,"type")

      if(k == 1){
        fm <- out
      }else{
        if(isCV[k]){
          fm$CV <- do.call(c, list(fm$CV,out$CV))
        }else{
          fm$q <- fm$q + out$q
          fm$u <- rbind(fm$u, out$u)
          fm$yHat <- rbind(fm$yHat, out$yHat)
          fm$nsup <- rbind(fm$nsup, out$nsup)
          fm$lambda <- rbind(fm$lambda, out$lambda)
          fm$file_beta <- c(fm$file_beta, out$file_beta)
          fm$fileID <- c(fm$fileID, out$fileID)
        }
      }
      if(verbose){
        message("  Loaded file: '",basename(filenames[k]),"'. Type: ",
                paste(attr(out,"type"),collapse=", "))
      }
    }
    if(length(fm$file_beta)>0){
      stopifnot(length(unique(fm$file_beta))==1L)
      fm$file_beta <- fm$file_beta[1]
    }
    if(length(unique(modelparms))>1L){
      stop("Results are not from the same model settting: \n",
           paste(paste0("    ",unique(modelparms)),collapse="\n"))
    }
    if(any(isCV)){
      if(!any(issummary)){
        if((length(fm$CV) != fm$nfolds_CV)){
          stop("Read results from ",length(fm$CV)," folds of ",fm$nfolds_CV," that are expected")
        }
      }
    }else{
      if(!any(issummary)){
        if(nrow(fm$u) != fm$nTST){
          stop("Read results from nTST = ",nrow(fm$u)," of ",fm$nTST," that are expected")
        }
      }
    }
  }

  return(fm)
}
