#'@name extract_filename
#'@aliases extract_filename
#'@title Extract Information From File Name
#'@description This function split a string (file name) as per the requirement of the user. It is useful to extract  informations like compound name, plate number etc from the file name.
#'
#'@usage extract_filename (filename,split = " ",end = ".csv", remove = " ", sep = "-")
#'
#'@param filename name of the file (string).
#'
#'@param split regular expressions at which filename has to be split to create different sections.
#'
#'@param end extension (end portion) of filename that need to be removed.
#'
#'@param remove section that need to be omitted after splitting the filename.
#'
#'@param sep symbol to be added to separate sections (obtained after splitting) before combining (default is "-").
#'
#'@return A character vector. First element is the unsplit 'filename'. Second element is the processed 'filename'.Other elements are different sections after splitting the 'filename'.
#'
#'@author A.A Palakkan
#'
#'@examples
#'extract_filename("L-HEPG2_P3_72HRS.csv")
#'extract_filename("L_HEPG2_P3_72HRS.csv", split="_",end=".csv",remove="L",sep="")
#'
#'@keywords character
#'
#'@export



extract_filename<-function(filename,split = " ",end = ".csv", remove = " ", sep="-"){

a<-basename(as.character(filename))
name <- unlist(strsplit(a,end,fixed = TRUE))[1] # remove .csv from the end
nm2<-unlist(strsplit(name, split)) # split the file name in to different parts.
nm3<-nm2

for (j in seq_along(remove)) {
  for (i in seq_along(nm2)) {if (nm2[i]==remove[j]) nm3[i]<-" "}
}

cell<- paste(nm3[which(nm3!=" ")], collapse = sep)
nm4<- nm3[which(nm3!=" ")]
ab_name<-c(a,cell,nm4)
return(ab_name)
}

