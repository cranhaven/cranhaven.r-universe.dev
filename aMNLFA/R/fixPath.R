#' helper function - removes the final slash at the end of a given string
#'
#' This function generates the initial itemwise aMNLFA models.
#' @name fixPath
#' @param somecharacter The aMNLFA object (created using the aMNLFA.object function) which provides instructions for the function.
#' @keywords MNLFA
#' @return outcharacter - string with slash at the end deleted
#' @export
#' @examples
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/examplefiles")
#'  fixPath(first)

#Remove final slash to allow for better use of file.path down the line.
fixPath<-function(somecharacter) {
  lastcharacter<-base::substring(somecharacter,nchar(somecharacter))
  newcharacter<-base::substr(somecharacter,1,nchar(somecharacter)-1)
  outcharacter<-somecharacter
  if (lastcharacter=="/") {outcharacter<-newcharacter}
  outcharacter
}
