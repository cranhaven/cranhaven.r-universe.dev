#' @importFrom utils installed.packages unzip
#' @importFrom data.table fread
#' @export
fread_zip = function(filezip = NULL,
                        extension = "BOTH",
                        sep="auto",
                        nrows=-1L,
                        header="auto",
                        na.strings="NA",
                        stringsAsFactors=FALSE,
                        verbose=getOption("datatable.verbose"),
                        autostart=1L,
                        skip=0L,
                        drop=NULL,
                        colClasses=NULL,
                        integer64=getOption("datatable.integer64"),
                        dec=if (sep!=".") "." else ",",
                        check.names=FALSE,
                        encoding="unknown",
                        quote="\"",
                        strip.white=TRUE,
                        fill=FALSE,
                        blank.lines.skip=FALSE,
                        key=NULL,
                        Names=NULL,
                        prefix=NULL,
                        showProgress = interactive(),   # default: TRUE
                        data.table=TRUE   # default: TRUE
){
  if ("data.table" %in% rownames(installed.packages()) == FALSE) {
    stop("data.table needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(is.null(filezip)){
    filezip = file.choose()
    fileEnding = substr(filezip,nchar(filezip)-3,nchar(filezip))
    if(fileEnding != ".zip"){
      stop("Please supply a valid .zip file")
    }
    filezip <- unzip(zipfile = filezip)
  }else{
    fileEnding = substr(filezip,nchar(filezip)-3,nchar(filezip))
    if(fileEnding != ".zip"){
      stop("Please supply a valid .zip file")


    }else{
      filezip <- unzip(zipfile = filezip)
    }
  }


  endings = list()

  if(tolower(extension) == "txt"){
    endings[1] =  "*\\.txt$"
  }
  if(tolower(extension) == "csv"){
    endings[1] =  "*\\.csv$"

  }
  if(tolower(extension) == "both"){
    endings[1] =  "*\\.txt$"
    endings[2] =  "*\\.csv$"
  }
  if((tolower(extension) %in% c("txt","csv","both")) == FALSE){
    stop("Pleas supply a valid value for 'extension',\n
         allowed values are: 'TXT','CSV','BOTH'.")
  }
  tempfiles = list()
  num = 1
  for(i in endings){
    tempfiles = filezip[grep(i,filezip)]
    num = num +1
    if(length(tempfiles) < 1){
      num = num+1
    }
    else{
      count = 0
      for(tbl in tempfiles){
        count = count+1
        DTname1 = paste0(gsub("/", "", tbl))
        DTname2 = substr(DTname1,2,nchar(DTname1))
        if(!is.null(Names)){
          if((length(Names) != length(tempfiles))| (class(Names) != "character")){
            stop("Names must a character vector of same length as the files to be read.")
          }else{
            DTname3 = Names[count]
          }

        }else{
          DTname3 = paste0(gsub(i, "", DTname2))
        }
        if(!is.null(prefix) && is.character(prefix)){
          DTname4 = paste(prefix,DTname3, sep = "")
        }else{
          DTname4 = DTname3
        }

        DTable <- data.table::fread(input = tbl,
                                    sep=sep,
                                    nrows=nrows,
                                    header=header,
                                    na.strings=na.strings,
                                    stringsAsFactors=stringsAsFactors,
                                    verbose = verbose,
                                    autostart=autostart,
                                    skip=skip,
                                    drop=drop,
                                    colClasses=colClasses,
                                    dec=if (sep!=".") "." else ",",
                                    check.names=check.names,
                                    encoding=encoding,
                                    quote=quote,
                                    strip.white=strip.white,
                                    fill=fill,
                                    blank.lines.skip=blank.lines.skip,
                                    key=key,
                                    showProgress=showProgress,
                                    data.table=data.table
        )

        assign_to_global <- function(pos=1){
          assign(x = DTname4,value = DTable, envir=as.environment(pos) )
        }
        assign_to_global()

        rm(DTable)
      }
    }
  }
}

