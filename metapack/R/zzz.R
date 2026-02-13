.onAttach<-function(libname, pkgname){   
   this.year <- format(Sys.time(), "%Y")
   # echo output to screen
   packageStartupMessage("## Meta-Analysis (metapack)")
   packageStartupMessage("## Copyright (C) ", this.year,
      " Lim et al.", sep="")
   packageStartupMessage("## Visit events.stat.uconn.edu/metapack")
}