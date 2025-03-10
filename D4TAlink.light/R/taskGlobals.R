##### envvar #####
#' Get env var.
#' @param name name of variable
#' @param desc description of variable
#' @param quiet suppress error messages, default=FALSE.
#' @return Environment variable.
D4getenv <- function(name,desc,quiet=FALSE) {
  if(exists(name,envir=.pkgEnv)) {
    return(get(x=name,pos=.pkgEnv))
  } else if(quiet) {
    return(NA)
  } else stop(paste(desc,"not defined"))
}

##### root #####
#' Set the root of the task repository.
#' @inheritParams D4TAlink-common-args
#' @return Path to the current task root.
#' @export
setTaskRoot <- function(rootpath,dirCreate=FALSE){
  #rootpath <- normalizePath(rootpath,winslash="/",mustWork=FALSE)
  if(!dirCreate&&!file.exists(rootpath)) stop("root directory does not exist")
  if( dirCreate&&!file.exists(rootpath)) dir.create(rootpath,recursive=TRUE,showWarnings=FALSE)
  rootpath <- normalizePath(rootpath,winslash="/")
  assign(x="rootpath",value=rootpath,pos=.pkgEnv)
  invisible(getTaskRoot())
}

#' Get the root of the task repository.
#' @inheritParams D4getenv
#' @return Path to the current task root.
#' @examples getTaskRoot(quiet=TRUE)
#' @export
getTaskRoot <- function(quiet=FALSE)
  D4getenv("rootpath","Root path",quiet=quiet)

##### author #####

#' Set the name of the tasks author.
#' @inheritParams D4TAlink-common-args
#' @examples
#' setTaskAuthor("Doe Johns")
#' @return The current name of the tasks author.
#' @export
setTaskAuthor <- function(author){
  assign(x="authorname",value=author,pos=.pkgEnv)
  invisible(getTaskAuthor())
}

#' Get the name of the task author.
#' @inheritParams D4getenv
#' @return The current name of the tasks author.
#' @examples getTaskAuthor(quiet=TRUE)
#' @export
getTaskAuthor <- function(quiet=FALSE)
  D4getenv("authorname","Author",quiet=quiet)

##### sponsor #####

#' Set the name of the tasks sponsor.
#' @inheritParams D4TAlink-common-args
#' @examples
#' setTaskSponsor("SQU4RE")
#' @return The current name of the tasks sponsor.
#' @export
setTaskSponsor <- function(sponsor){
  assign(x="sponsorname",value=sponsor,pos=.pkgEnv)
  invisible(getTaskSponsor())
}

#' Get the name of the task sponsor.
#' @inheritParams D4getenv
#' @return The current name of the tasks sponsor.
#' @examples getTaskSponsor(quiet=TRUE)
#' @export
getTaskSponsor <- function(quiet=FALSE)
  D4getenv("sponsorname","Sponsor",quiet=quiet)

##### structure #####

#' Set task repository directory structure.
#' @inheritParams D4TAlink-common-args
#' @examples
#' fun <- function(project,package,taskname,sponsor) {
#'   basePath <- file.path("%ROOT%",sponsor,project,package)
#'   list(
#'     root = "%ROOT%",
#'     datasrc = file.path(basePath, "raw", "data_source"),
#'     data = file.path(basePath, "output","adhoc",taskname),
#'     bin  = file.path(basePath, "output","adhoc",taskname,"bin"),
#'     code = file.path(basePath, "progs"),
#'     doc  = file.path(basePath, "docs"),
#'     log  = file.path(basePath, "output","log")
#'   )
#' }
#' setTaskStructure(fun)
#' @return The task directory structure function invisibly.
#' @export
setTaskStructure <- function(pathgen){
  assign(x="pathgen",value=pathgen,pos=.pkgEnv)
  invisible(getTaskStructure())
}

#' Get repository directory structure.
#' @inheritParams D4getenv
#' @return The directory structure function.
#' @export
getTaskStructure <- function(quiet=FALSE)
  D4getenv("pathgen","Task file structure",quiet=quiet)

##### RmdTemplate #####

#' Set the path to the Rmd task template.
#' @param file path to the Rmd task template.
#' @inheritParams base::readLines
#' @return The path to the Rmd task template invisibly.
#' @export
setTaskRmdTemplate <- function(file,encoding="unknown"){
  if(file=="SQU4RE") {
    f <- system.file("template/SQU4REtemplate.Rmd",package="D4TAlink.light")
    file.copy(f,rfn<-tempfile(fileext=".Rmd"))
    dfn <- system.file("template/SQU4REtemplate.docx",package="D4TAlink.light")
    tin <- readLines(rfn,encoding=encoding,warn=FALSE)
    tin <- gsub("%TEMPLATEFILE%",dfn,tin,fixed=TRUE)
    writeLines(enc2utf8(tin),rfn,useBytes=TRUE)
    file <- rfn
  }
  assign(x="rmdTempl",value=file,pos=.pkgEnv)
  invisible(getTaskRmdTemplate())
}

#' Get the path to the Rmd task template.
#' @inheritParams D4getenv
#' @return The path to the Rmd task template.
#' @export
getTaskRmdTemplate <- function(quiet=FALSE)
  D4getenv("rmdTempl","R markdown template",quiet=quiet)

##### RscriptTemplate #####

#' Set the path to the R script task template.
#' @param file path to the Rmd task template.
#' @return The path to the Rmd task template invisibly.
#' @export
setTaskRscriptTemplate <- function(file){
  assign(x="rscriptTempl",value=file,pos=.pkgEnv)
  invisible(getTaskRscriptTemplate())
}

#' Get the path to the R script task template.
#' @inheritParams D4getenv
#' @return The path to the R script task template.
#' @export
getTaskRscriptTemplate <- function(quiet=FALSE)
  D4getenv("rscriptTempl","R script template",quiet=quiet)

##### EncryptionKey #####

#' Set the encryption key for binary data files.
#' @param key encryption key, if NULL then query key from user.
#' @return \code{NULL} invisibly.
#' @export
setTaskEnckey <- function(key){
  assign(x="sq5key",value=key,pos=.pkgEnv)
  invisible(NULL)
}

#' Get the encryption key for binary data files.
#' @param ask query encryption key from user, default FALSE.
#' @return The encryption key invisibly.
#' @export
getTaskEnckey <- function(ask=FALSE) {
  key <- NULL
  if(!exists("sq5key",envir=.pkgEnv)||ask) {
    if(requireNamespace("getPass",quietly=1)) {
      key <- getPass::getPass("encryption key")
    } else if(system("where stty")<1) {
      cat("encryption key:\n")
      system("stty -echo")
      key <- readline()
      system("stty echo")
      cat("\r \r")
    } else stop("package 'getPass' required to encrypt files, please install it")
  } else key <- get(x="sq5key",pos=.pkgEnv)
  if(nchar(key)<8) stop("encryption key must have at least eight characters")
  invisible(key)
}

