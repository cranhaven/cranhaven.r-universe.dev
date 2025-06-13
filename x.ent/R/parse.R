#this is a function main which it will call the main script
options("encoding" = "UTF-8")
xparse <- function(json_path = "",verbose=FALSE)
{
    path    = paste(.libPaths()[1], "x.ent/Perl", sep='/')
    if(json_path == "")
    {
        command = paste("perl -I \"",path, "\" \"",path, "/", "Main.pl\"", sep='')
    }
    else
    {
        command = paste("perl -I \"",path, "\" \"",path, "/", "Main.pl\""," \"",json_path,"\"", sep='')
    }
    print(command)
    print("Please, wait ....")
    test <- try(system(command, intern=TRUE,wait=TRUE))
    #rm(list=ls())
    gc(verbose=T) #garbage collection to free up memory
    print(test)
    print("Let use functions for viewing the results: xshow(...), xhist(...), xplot(...)....")
}
xentity <- function()
{
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  lst_tag <- c();
  #dico
  if(length(conf$dico$tag) > 0)
  {
    for(i in 1:length(conf$dico$tag))
    {
      lst_tag <-add_unique(lst_tag,conf$dico$tag[i])
    }
  }
  #unitex
  if(length(conf$unitex$result$tag) > 0)
  {
    for(i in 1:length(conf$unitex$result$tag))
    {
      lst_tag <-add_unique(lst_tag,conf$unitex$result$tag[i])
    }
  }
  return(lst_tag)
}
xrelation<- function()
{
  conf = fromJSON(paste(.libPaths()[1], "x.ent/www/config/ini.json", sep='/'))
  lst_tag <- c();
  #relation
  if(length(conf$relation$link) > 0)
  {
    for(i in 1:length(conf$relation$link))
    {
      lst_tag <-add_unique(lst_tag,conf$relation$link[i])
    }
  }
  return(lst_tag)
}


