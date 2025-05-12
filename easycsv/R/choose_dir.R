#' @export
choose_dir = function(){
  os = Identify.OS()
  if(tolower(os) == "windows"){
    directory <- utils::choose.dir()
  }
    if(tolower(os) == "linux"){
      directory <- system("zenity --file-selection --directory", intern = TRUE)
    }
    if(tolower(os) == "macosx"){
      system("osascript -e 'tell app \"RStudio\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
             intern = FALSE, ignore.stderr = TRUE)
      directory <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
    }
  return(directory)
}
