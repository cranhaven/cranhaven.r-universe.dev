CADStat.update <- function()
{
  version.available <- available.packages(contriburl="http://gisdt-software.neptuneinc.org/R/bin/windows/contrib/2.2")["CADStat","Version"]
  version.installed <- installed.packages()["CADStat","Version"]
  if(compareVersion(version.available,version.installed)==1){
    print("A new version of CADStat is available and is being installed, close and restart CADStat")
    detach(package:CADStat)
    install.packages("CADStat",contriburl="http://gisdt-software.neptuneinc.org/R/bin/windows/contrib/2.2")    
  }else{
    print("You have the most recent version of CADStat")  
  }
}


