## =======================================================================
#' Restore R object from binary file.
#' @param quiet issue warning if file does not exists.
#' @param password encryption password, default NULL
#' @inheritParams D4TAlink-common-args
#' @inheritParams getTaskEnckey
#' @return Object stored in binary file, or \code{NULL} if file does not exist.
#' @importFrom openssl aes_cbc_decrypt sha256
#' @export
readBinary <- function(task,type,subdir=NULL,dirCreate=FALSE,ask=FALSE,quiet=FALSE,password=NULL) {
  fn <- binaryFn(task,type,ext="%e%",subdir=subdir,dirCreate=dirCreate)
  res <- NULL
  if(file.exists(gsub("%e%","sq5",fn))) {
    if(!is.null(password)) key <- password
    else  key <- getTaskEnckey(ask=ask)
    res <- unserialize(openssl::aes_cbc_decrypt(readRDS(gsub("%e%","sq5",fn)),key=openssl::sha256(charToRaw(key))))
  } else if(file.exists(gsub("%e%","sq4",fn))) {
    res <- readRDS(gsub("%e%","sq4",fn))
  } else if(file.exists(gsub("%e%","rds",fn))) {
    res <- readRDS(gsub("%e%","rds",fn))
  }
  if(is.null(res)&&!quiet) {
    warning(paste0("object '",type,"' not found"))
  }
  res
}

#' Save R object in binary file.
#' @param object R object to serialize.
#' @param encrypt encrypt the output, default: FALSE. If character string, then use the string as password.
#' @inheritParams D4TAlink-common-args
#' @inheritParams getTaskEnckey
#' @return the file name invisibly.
#' @importFrom openssl aes_cbc_encrypt sha256
#' @export
saveBinary <- function(object,task,type,subdir=NULL,dirCreate=TRUE,encrypt=FALSE,ask=FALSE) {
  fn <- binaryFn(task,type,ext=ifelse(encrypt,"sq5","rds"),subdir=subdir,dirCreate=dirCreate)
  if(is.character(encrypt)) {
    key <- encrypt
    if (nchar(key) < 8)
      stop("encryption key must have at least eight characters")
  } else if(encrypt) {
    key <- getTaskEnckey(ask=ask)
    saveRDS(openssl::aes_cbc_encrypt(serialize(object,NULL),key=openssl::sha256(charToRaw(key))),fn)
  } else saveRDS(object,fn)
  invisible(fn)
}
#' Save R object in encrypted binary file.
#' @inheritParams D4TAlink-common-args
#' @inheritParams saveBinary
#' @return the file name invisibly.
#' @export
saveBinaryE <- function(object,task,type,subdir=NULL,dirCreate=TRUE,ask=FALSE)
  saveBinary(object,task,type,subdir=subdir,dirCreate=dirCreate,ask=ask,encrypt=TRUE)
## =======================================================================

## =======================================================================
#' Save R object in binary file.
#' @param x object to save. It can be either a data frame, an object of type \code{AnnotatedDataFrame}, or a list thereof.
#' @param metadata prefix for names of worksheets holding metadata.
#' @param metadata.append array of metadata field names to be appended in header of tables.
#' @inheritParams D4TAlink-common-args
#' @inheritParams WriteXLS::WriteXLS
#' @inheritDotParams WriteXLS::WriteXLS
#' @return the file name invisibly.
#' @importFrom Biobase varMetadata pData
#' @importFrom openxlsx write.xlsx
#' @export
saveReportXls <- function(x,task,type,ext="xlsx",subdir=NULL,dirCreate=TRUE,
                          AdjWidth=TRUE,FreezeRow=1,FreezeCol=3,
                          metadata="metadata",
                          metadata.append=NULL,
                          ...)  {
  if(is.data.frame(x)||("AnnotatedDataFrame"%in%class(x))) x <- list(worksheet=x);
  if(!is.list(x)) stop("'x' must be a list");
  if(is.null(names(x))) stop("'x' must be a named list");
  for(i in names(x)) if("AnnotatedDataFrame"%in%class(x[[i]])) {
    if(!is.null(x[[paste(i,metadata)]])) stop("meta data exists");
    m <- Biobase::varMetadata(x[[i]]);
    d <- x[[i]] <- Biobase::pData(x[[i]]);
    x[[paste(i,metadata)]] <- cbind(data.frame(`field`=rownames(m)),m);
    if(!is.null(metadata.append)) {
      d <- as.data.frame(rbind(colnames(d),
       apply(d,2,function(x)gsub("(^[[:space:]]*)|([[:space:]]*$)","",
                                 as.character(x)))))
      for(f in metadata.append)
        d <- as.data.frame(rbind(m[colnames(d),f],d))
      colnames(d) <- NULL;
      x[[i]] <- d;
      if(FreezeRow==1) FreezeRow <- 2;
    };
  };
  if(!is.null(metadata.append)) {
    for(i in names(x)) if(!is.null(colnames(x[[i]]))) {
      d <- x[[i]]
      d <- as.data.frame(rbind(colnames(d),apply(d,2,
        function(x)gsub("(^[[:space:]]*)|([[:space:]]*$)","",as.character(x)))))
      colnames(d) <- NULL;
      x[[i]] <- d;
    }
  }
  fn<-reportFn(task,type,ext=ext,subdir=subdir,dirCreate=dirCreate);
  if(requireNamespace("openxlsx",quietly=1)) { # openxlsx
    for(i in 1:length(x)) {
      if(is.null(colnames(x[[i]]))) {
        v <- unlist(x[[i]][1,]) ; v[is.na(v)] <- "-"
        colnames(x[[i]]) <- v
        x[[i]] <- x[[i]][-1,]
      }
    }
    openxlsx::write.xlsx(x,fn);
  } else if(requireNamespace("WriteXLS",quietly=1)) { # WriteXLS
    with(x,WriteXLS::WriteXLS(names(x),ExcelFileName=fn,
                              col.names=is.null(metadata.append),
                              AdjWidth=AdjWidth,FreezeRow=FreezeRow,FreezeCol=FreezeCol,...));
  } else if(requireNamespace("XLConnect",quietly=1)) { # XLConnect
    wb <- XLConnect::loadWorkbook(fn,create=TRUE)
    for(n in names(x)) {
      XLConnect::createSheet(wb,name=n)
      XLConnect::writeWorksheet(wb,x[[n]],sheet=n,header=is.null(metadata.append),rownames=NULL)
    }
    XLConnect::saveWorkbook(wb)
  } else if(requireNamespace("xlsx",quietly=1)) { # xlsx
    a <- FALSE
    for(n in names(x)) {
      xlsx::write.xlsx(x[[n]],file=fn,sheetName=n,
                       row.names=FALSE,col.names=is.null(metadata.append),showNA=FALSE,append=a)
    }
  } else stop("no package installed to save Excel file")
  invisible(fn)
};
## =======================================================================

## =======================================================================
#' Output R object using function cat.
#' @param x R object to output.
#' @param sep separator
#' @param eof EOF
#' @inheritParams D4TAlink-common-args
#' @inheritParams base::cat
#' @return the file name invisibly.
#' @export
catReport <- function(x,task,type,ext="txt",subdir=NULL,dirCreate=TRUE,
                      sep="\n",eof="\n",...)  {
  cat(paste(paste(ifelse(is.numeric(x),"","\""),x,
                                            ifelse(is.numeric(x),"","\""),
                                            sep="",collapse=sep),eof,sep=""),
 file=fn<-reportFn(task,type,ext=ext,subdir=subdir,dirCreate=dirCreate),...);
  invisible(fn)
}
#' Read data into vector or list using function \code{\link{scan}}.
#' @inheritParams base::scan
#' @inheritDotParams base::scan
#' @inheritParams D4TAlink-common-args
#' @return the data read, or \code{NULL} if the file does not exist.
#' @export
scanReport <- function(task,type,ext="txt",subdir=NULL,dirCreate=TRUE,what="",...)
  scan(what=what,
 file=fn<-reportFn(task,type,ext=ext,subdir=subdir,dirCreate=dirCreate),
 ...)
## =======================================================================

## =======================================================================
#' Output R object using function \code{\link{write.csv}}.
#' @param x R object to output.
#' @param gzip unused.
#' @inheritParams D4TAlink-common-args
#' @inheritDotParams utils::write.table
#' @return the file name invisibly.
#' @importFrom utils write.csv
#' @export
saveReportTable <- function(x,task,type,ext="csv",subdir=NULL,dirCreate=TRUE,gzip=FALSE,...)  {
  if("AnnotatedDataFrame"%in%class(x)) x <- Biobase::pData(x)
  write.csv(x,fn<-reportFn(task,type,ext=sprintf(ifelse(gzip,"%s.gz","%s"),ext),
                           subdir=subdir,dirCreate=dirCreate),...)
  invisible(fn)
}
#' Read data into vector or list using function \code{\link{scan}}.
#' @inheritParams D4TAlink-common-args
#' @inheritDotParams utils::read.table
#' @return the data read, or \code{NULL} if the file does not exist.
#' @importFrom utils read.csv
#' @export
readReportTable <- function(task,type,ext="csv",subdir=NULL,dirCreate=FALSE,...) {
  fn <- reportFn(task,type,ext=ext,subdir=subdir,dirCreate=dirCreate);
  if(file.exists(fn)) return(read.csv(fn,...)); NULL;
};
## =======================================================================

## =======================================================================
#' Output R object in JSON format.
#' @param x R object to output.
#' @inheritParams D4TAlink-common-args
#' @return the file name invisibly.
#' @importFrom jsonlite toJSON
#' @export
saveReportJSON <- function(x,task,type,ext="json",subdir=NULL,dirCreate=TRUE)  {
  cat(jsonlite::toJSON(x),file=fn<-reportFn(task,type,ext=ext,subdir=subdir,dirCreate=dirCreate))
  invisible(fn)
}
#' Read JSON data into R object.
#' @inheritParams D4TAlink-common-args
#' @return the data read, or \code{NULL} if the file does not exist.
#' @importFrom jsonlite fromJSON
#' @export
readReportJSON <- function(task,type,ext="json",subdir=NULL,dirCreate=FALSE) {
  fn <- reportFn(task,type,ext=ext,subdir=subdir,dirCreate=dirCreate);
  if(file.exists(fn)) return(jsonlite::fromJSON(readChar(fn,file.size(fn)))); NULL;
}
## =======================================================================

## =======================================================================
#' Graphics devices for \code{JPEG} format bitmap files.
#' @param dim device height and width in \code{px}.
#' @param height device height in \code{px}.
#' @param width device height in \code{px}.
#' @inheritParams grDevices::jpeg
#' @inheritDotParams grDevices::jpeg
#' @inheritParams D4TAlink-common-args
#' @return the file name invisibly.
#' @inheritParams grDevices::jpeg
#' @export
jpegReport <- function(task,type,ext="jpg",subdir=NULL,dirCreate=TRUE,
                       dim=c(500,500),width=NULL,height=NULL,...) {
  if(!is.null(height)) dim[1] <- height;
  if(!is.null(width )) dim[2] <- width;
  fn <- reportFn(task,type,ext,subdir=subdir,dirCreate=dirCreate);
  if(file.exists(fn)) unlink(fn)
  grDevices::jpeg(fn,height=dim[1],width=dim[2],...)
  #graphics::par(lmitre=5,ljoin=1)
  invisible(fn)
};
#' Graphics devices for \code{pdf} format bitmap files.
#' @param dim device height and width in \code{mm}.
#' @param height device height in \code{mm}.
#' @param width device height in \code{mm}.
#' @param landscape if defined, orientation of the document.
#' @inheritParams grDevices::pdf
#' @inheritDotParams grDevices::pdf
#' @inheritParams D4TAlink-common-args
#' @return the file name invisibly.
#' @export
pdfReport <- function(task,type,ext="pdf",subdir=NULL,dirCreate=TRUE,
                      title=NA,file=NA,
                      dim=c(297,210),height=NULL,width=NULL,
                      landscape=NULL,...) {
  mm2in <- 25.4
  if(is.na(title)) title <- paste0(
    taskID(task)," - ",paste(type,collapse="-"),
    " - ",task$author)
  if(!is.null(height)) dim[1] <- height
  if(!is.null(width )) dim[2] <- width
  if(!is.null(landscape)) {
    if(landscape) dim <- sort(dim)
    else dim <- rev(sort(dim))
  }
  if(is.na(file)) fn <- reportFn(task,type,ext,subdir=subdir,dirCreate=dirCreate)
  else fn <- file;
  if(file.exists(fn)) unlink(fn);
  grDevices::pdf(fn,height=dim[1]/mm2in,width=dim[2]/mm2in,title=title,...)
  #graphics::par(lmitre=5,ljoin=1)
  invisible(fn)
}
#' Graphics devices for \code{PNG} format bitmap files.
#' @param dim device height and width in \code{px}.
#' @param height device height in \code{px}.
#' @param width device height in \code{px}.
#' @inheritParams grDevices::png
#' @inheritDotParams grDevices::png
#' @inheritParams D4TAlink-common-args
#' @return the file name invisibly.
#' @export
pngReport <- function(task,type,ext="png",subdir=NULL,dirCreate=TRUE,
                      dim=c(500,500),width=NULL,height=NULL,...) {
  if(!is.null(height)) dim[1] <- height
  if(!is.null(width )) dim[2] <- width
  fn <- reportFn(task,type,ext,subdir=subdir,dirCreate=dirCreate)
  if(file.exists(fn)) unlink(fn)
  grDevices::png(fn,height=dim[1],width=dim[2],...)
  #graphics::par(lmitre=5,ljoin=1)
  invisible(fn)
}
## =======================================================================

