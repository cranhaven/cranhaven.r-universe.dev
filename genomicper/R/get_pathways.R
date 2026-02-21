get_pathways <-
function(source="reactome",all_paths=TRUE,envir = ""){
if(missing(source)==FALSE){
if(source != "reactome"){
stop("Argument source must be set to:\"reactome\" (\"KEGG\" is no longer supported, custom pathways can be provided by user")
}
}
print("Arguments set:")
    print(paste("Source:" ,substitute(source),sep=""))
    print(paste("Annotate all pathways: ",substitute(all_paths),sep=""))
if(source=="reactome"){
pathways_description <- dbGetQuery(reactome.db::reactome_dbconn(), "SELECT * FROM pathway2name")
x<-unique(sub(":.*","",pathways_description[,2]))
print(x) #"select species"
sp <- readline(prompt="Select dataset(e.g. \"Homo sapiens\"):\n")
sp <- gsub("\"","",sp)
x<- grep(sp, pathways_description[,2],ignore.case=TRUE)
pathways_description <- pathways_description[x,] 
pathways_description[,2]<- sub(".*: ","",pathways_description[,2])
paths <- AnnotationDbi::as.list(reactome.db::reactomePATHID2EXTID)
pathways_description <- pathways_description[which(pathways_description[,1]%in% names(paths) == TRUE),]
pre <- readline(prompt="Assign an additional prefix for the pathways. Avoid the following characters \"-\",\"_\" (e.g.\"mypath\",\"rhsa\")\n Leave blank in case no prefix needed.\n")
if(all_paths==TRUE){
tt <- dim(pathways_description)[1]
print(paste(tt," reactome pathways to annotate",sep=""))
for(i in 1:tt){
print(pathways_description[i,2])
x <- which(names(paths)== pathways_description[i,1])
if(length(x)!=0){
path_tag<- gsub("[-,_]", "",pathways_description[i,1])
assign(paste(pre,path_tag,sep=""),paths[[x]], envir = envir)
}
}
}
if(all_paths==FALSE){
print(pathways_description)
subs <- readline(prompt="Select reactome IDs - comma separated (e.g. \"R-HSA-164843,R-HSA-446343,R-HSA-8876384,R-HSA-8964572,R-HSA-109582,R-HSA-1474244\"):\n")
subs <- strsplit(subs,",")[[1]]
subs <- gsub("\"","",subs)
tt <- length(subs)
y <- NULL
z <- NULL
for(i in 1:tt){
exs <- which(pathways_description[,1]== subs[i])
if(length(exs)!=0){
print(pathways_description[exs,2])
x <- which(names(paths)== pathways_description[exs,1])
if(length(x)!=0){
path_tag<- gsub("[-,_]", "",pathways_description[exs,1])
assign(paste(pre,path_tag,sep=""),paths[[x]], envir = envir)
z <- c(z,exs)
}

}
else{
y <- c(subs[i],y)
}
}
print(paste("Pathways without gene ids/not found:",y,sep=""))
pathways_description <- pathways_description[z,]
}
print(paste("Pathwas saved in workspace as: ",pre,"XXXXXX",sep=""))
}
return(pathways_description)
}
