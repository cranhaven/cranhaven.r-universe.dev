#' Genotype List String Conversion
#'
#' Main Workhorse wrapper for cross converting columnar table to GL string representaion.
#' @param Data String File name or R Data Frame.
#' @param Convert String Direction for conversion.
#' @param File.Output String Type of File.Output.
#' @param System String Genetic system (HLA or KIR) of the data being converted
#' @param HZY.Red Logical Reduction of homozygote genotypes to single allele.
#' @param DRB345.Check Logical Check DR haplotypes for consistency and flag unusual haplotypes.
#' @param Strip.Prefix Logical Should System/Locus prefixes be stripped from table data.
#' @param Abs.Fill Logical Should absent loci special designations be used.
#' @param Cores.Lim Integer How many cores can be used.
GLSconvert <- function(Data,Convert,File.Output="txt",System="HLA",HZY.Red=FALSE,DRB345.Check=FALSE,Strip.Prefix=TRUE,Abs.Fill=FALSE,Cores.Lim=1L) {

  # Check Parameters
  if( missing(Data) ) { Err.Log(FALSE,"P.Missing","Data") ; stop("Conversion Stopped.",call.=FALSE) }
  if( missing(Convert) ) { Err.Log(FALSE,"P.Missing","Convert") ; stop("Conversion Stopped.",call.=FALSE) }
  Check.Params.GLS(Convert,File.Output,System,HZY.Red,DRB345.Check,Cores.Lim)

  # MultiCore Limitations
  Cores <- Check.Cores(Cores.Lim,FALSE)

  # Set nomenclature system and Prefix stripping for pypop output
  if( System == "HLA" ) { System <- "HLA-" }
  if( File.Output == "pypop" ) { Strip.Prefix <- TRUE }

  # Read in Data and Set File.Output File Name
  if( is.character(Data) ) {
    if( file.exists(Data) ) {
      df <- read.table(file=Data,header=T,sep="\t", stringsAsFactors=FALSE, fill=T, comment.char = "#", strip.white=T, blank.lines.skip=T, colClasses="character")
      colnames(df) <- gsub("HLA.","HLA-",colnames(df))
      fileName <- getFileName(Data)
    } else { Err.Log(FALSE,"File.Error",Data) ; stop("Conversion Stopped.",call.=FALSE) }
  } else { df <- Data ; fileName <- "Converted" }
  df[] <- lapply(df, as.character)
  df[is.na(df)] <- ""

  # Check Data Structure/Formatting
  Check.Data(df,System,Convert)

  # Run Data Conversion
  switch(Convert,
         GL2Tab = { data.out <- GL2Tab.wrapper(df,System,Strip.Prefix,Abs.Fill,Cores) } ,
         Tab2GL = { data.out <- Tab2GL.wrapper(df,System,HZY.Red,Abs.Fill,Cores) } )

  # Output DR.HapFlag for HLA data
  if( System=="HLA-" && !DRB345.Check ) { data.out <- data.out[,-grep('DR.HapFlag',colnames(data.out))]  }

  # File Name Ouput Options
  switch(File.Output,
         txt = { fileName <- paste(fileName,".txt",sep="") },
         csv = { fileName <- paste(fileName,".csv",sep="") },
         pypop = { fileName <- paste(fileName,".pop",sep="") } )

  # File.Output Final Converted File
  switch(File.Output,
         R = return(data.out),
         txt = write.table(data.out,file=fileName,sep="\t",quote=F,col.names=T,row.names=F),
         csv = write.csv(data.out,file=fileName,quote=F,col.names=T,row.names=F),
         pypop = write.table(data.out,file=fileName,sep="\t",quote=F,col.names=T,row.names=F) )

}
