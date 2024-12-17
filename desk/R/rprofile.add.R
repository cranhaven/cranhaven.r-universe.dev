rprofile.add = function(line){
  write(line, file = paste(R.home(component = "etc"),"/Rprofile.site", sep =""), append = T)
}
