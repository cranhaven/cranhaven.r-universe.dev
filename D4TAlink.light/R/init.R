.pkgEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname){

  if(file.exists(v<-Sys.getenv("D4TAlink_root"))) setTaskRoot(v)

  if(nchar(v<-Sys.getenv("D4TAlink_author"))) setTaskAuthor(v)
  else if(nchar(v<-Sys.getenv("USERNAME"))) setTaskAuthor(v)
  else if(nchar(v<-Sys.getenv("USER"))) setTaskAuthor(v)
  else setTaskAuthor("N/A")

  if(nchar(v<-Sys.getenv("D4TAlink_sponsor"))) setTaskSponsor(v)
  else if(nchar(v<-Sys.getenv("D4TAlink_company"))) setTaskSponsor(v)
  else setTaskSponsor("D4TAlink")

  if(nchar(v<-Sys.getenv("D4TAlink_pathgen"))) setTaskStructure(eval(parse(text=v)))
  else setTaskStructure(pathsDefault)

  if(nchar(v<-Sys.getenv("D4TAlink_rmdtempl"))) setTaskRmdTemplate(v)
  else setTaskRmdTemplate(system.file("template/template.Rmd",package="D4TAlink.light"))

  if(nchar(v<-Sys.getenv("D4TAlink_rscripttempl"))) setTaskRscriptTemplate(v)
  else setTaskRscriptTemplate(system.file("template/template.R",package="D4TAlink.light"))

  if(nchar(v<-Sys.getenv("D4TAlink_sq5key"))) setTaskEnckey(v)

  #packageStartupMessage(sprintf("--- D4TAlink\n    author : %s\n    sponsor: %s\n    root   : %s\n ---",
  #                getTaskAuthor(quiet=TRUE),getTaskSponsor(quiet=TRUE),getTaskRoot(quiet=TRUE)))
}
