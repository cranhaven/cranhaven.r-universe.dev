
# I'm not really sure how this needs to be, but in principle,
# we want to specify a minimum number of data columns and get back the
# other HMD columns, inferred if necessary.

#---------------------------------------------------------

#findPopColumn <- function(X){
#	Names <- colnames(X)
#	PopCol <- grepl("pop",tolower(Names))
#	if (sum(PopCol) == 1){
#		return(Names[PopCol])
#	} else {
#		warning("Population column missing or ambiguous\nYou'll have to specify it explicitly by name\n")
#		return(NA)
#	}
#}


# This is going to be more of a pain than I was expecting!!!

#as.Pop <- function(Data, PopCol = "Population", YearCol, MonthCol, DayCol, AgeCol ){
#	
#}

