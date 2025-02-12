
#====================================================================
read_summary <- function(path = ".", type = NULL,
                         file.ext = ".RData", verbose = TRUE)
{

  types <- c("SGP","SGP.CV")
  if(!is.null(type)){
    type <- match.arg(type, choices=types)
  }

  names_parms <- c("n","ntraits","nTRN","nTST","nlambda","nfolds","nCV")

  # Element names to average
  names0 <- c("nTRN","nTST","b","varU","varE","h2","accuracy","MSE","nsup","lambda")

  index1 <- index2<- names1 <- names2 <- NULL
  modelparms <- c()
  fm <- out <- NULL
  nfiles <- 0
  for(k in seq(path))
  {
    out <- errormsg <- msg <- NULL
    error <- FALSE
    tryCatch({ out <- read_SGP(path=path[k], type=type,
                               file.ext=paste0(".summary",file.ext),
                               verbose=verbose) },
             error=function(e){
                      error <<- TRUE
                      errormsg <<- conditionMessage(e)
                   })
    if(error){
      stop("At path '",path[k],"'\n",errormsg)
    }else{
      if(!is.null(out))
      {
        nfiles <- nfiles + 1
        tmp <- names_parms[names_parms %in% names(out)]
        modelparms <- c(modelparms, paste0(paste0(tmp,"=",unlist(out[tmp])),collapse=", "))

        if(!"summary" %in% attr(out,"type")){
          stop("Output file is not of the type '",paste0(type,", summary"),"'")
        }

        #names1 <- names0[names0 %in% names(out)]
        if(nfiles == 1){
          fm <- out
          if(fm$ntraits > 1){
            index1 <- grep("^nsup_",colnames(fm$nsup_trait),value=TRUE)
          }
          names1 <- names0[names0 %in% names(out)]

        }else{
          if(length(fm)==length(out)){
            if(length(setdiff(names(fm),names(out)))>0L){
              stop("Output files contain different elements' names")
            }
          }else{
            stop("Output files contain different number of elements")
          }
          names2 <- names0[names0 %in% names(out)]
          stopifnot(all(names1 == names2))
          # Combine outputs
          for(j in 1:length(names2)){  # sum all factors
            fm[[names2[j]]] <- fm[[names2[j]]] + out[[names2[j]]]
          }
          if(fm$ntraits > 1){
            index2 <- grep("^nsup_",colnames(out$nsup_trait),value=TRUE)
            stopifnot(all(index1 == index2))
            fm$nsup_trait[,index1] <- fm$nsup_trait[,index1] + out$nsup_trait[,index1]
          }
        }
      }
    }
  }

  if(length(unique(modelparms))>1L){
    stop("Results are not from the same model settting: \n",
         paste(paste0("    ",unique(modelparms)),collapse="\n"))
  }

  # Take the average
  if(nfiles > 1){
    for(j in 1:length(names1)){  # sum all factors
      fm[[names1[j]]] <- fm[[names1[j]]]/nfiles
    }
    if(fm$ntraits > 1){
      fm$nsup_trait[,index1] <- fm$nsup_trait[,index1]/nfiles
    }

    # Add optimal SGP
    index <- which(colnames(fm$accuracy)=="overall")
    TMP <- data.frame(accuracy=fm$accuracy[,index], MSE=fm$MSE[,index],
                      nsup=fm$nsup[,index], lambda=fm$lambda[,index])

    # Detect maximum accuracy
    index <- which.max(TMP$accuracy)
    if(length(index) == 0L){
      optCOR <- TMP[1, ,drop=FALSE]
      if(nrow(TMP)>1) optCOR[1,] <- NA
    }else{
      optCOR <- TMP[index, ,drop=FALSE]
    }
    fm$optCOR <- as.matrix(optCOR)[1,]

    # Detect minimum MSE
    index <- which.min(TMP$MSE)
    if(length(index) == 0L){
      optMSE <- TMP[1, ,drop=FALSE]
      if(nrow(TMP)>1) optMSE[1,] <- NA
    }else{
      optMSE <- TMP[index, ,drop=FALSE]
    }
    fm$optMSE <- as.matrix(optMSE)[1,]
  }

  return(fm)
}
