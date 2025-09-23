.onAttach = function(libname, pkgname){
	packageStartupMessage(
			paste("Welcome to SeqExpMatch v", 
					utils::packageVersion("SeqExpMatch"), 
					sep = "")
	)	
}