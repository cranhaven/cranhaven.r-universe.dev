#' Error Code Display and Logging
#'
#' Displays error codes attributable to data formatting and Locus/Allele naming. Writes to log file.
#' @param Output Logical indicating if Error logging should be written to a file.
#' @param x Log Code.
#' @param y Misc information relevant to error.
#' @param z Misc information relevant to error.
#' @note This function is for internal BIGDAWG use only.
Err.Log <- function (Output, x, y=NULL, z=NULL) {

  cat("*****ERROR!******\n")
  switch(x,
         #Parameters
         P.Missing = { Error <- paste("\nNo ",y," specified. This parameter is not optional. Please see vignette.",sep="") },
         P.Error = { Error <- paste("\nInvalid ",y," parameter. Please see vignette.",sep="") },
         Windows.Cores = { Error <- "\nYou have exceed the maximum allowable cores for Windows. Please see vignette." },

         #Formatting
         Bad.Data = { Error <- "\nYou seem to have subject data that are 0's or 1's, replace these with another value. Please see vignette." },
         Bad.DRB345.hap =  { Error <- "\nWe have encountered unanticipated DR haplotypes. Please see the 'Flagged_DRB345_Haplotypes.txt' output file." },
         Uneven.Prefix =  { Error <- "\nIt seems some (not all) of your loci are formatted as Locus*Allele. Please ensure all loci share a similar format." },
         Bad.Format.HLA = { Error <- "\nYour HLA data includes Locus*Allele genotype formatting. Please ensure all genotypes follow this format." },
         Bad.Format.Trim = { Error <- "\nYour HLA data does not appear to be formatted properly for trimming. Please see vignette." },
         Bad.Format.EVS = { Error <- "\nYour HLA data does not appear to be formatted properly for EVS stripping. Please see vignette." },
         Case.Con = { Error <- "\nYour data does not appear to contain both cases and controls. Please see vignette." },
         Loci.No = { Error <- "\nYou have opted to run the haplotype analysis with too few loci. Please check Set definitions." },
         Loci.No.AP = { Error <- "\nYou have set All.Pairwise to 'True' but one or more your defined locus sets contain too few loci. Please check Set definitions." },
         Low.Res = { Error <- "\nThe resolution of your HLA data is less than 2 or does not appear to be formatted properly. Please see vignette." },
         High.Res = { Error <- "\nYour HLA does not appear to be formatted properly, >4 fields detected. Please see vignette" },

         #Names
         Bad.Filename = {  Error <- paste("\nBIGDAWG could not locate a file labeled: ",y," in the specificied working directory.",sep="") },
         Bad.Locus.NA = { Error <- "\nYou seem to have specified a locus in the Loci.Set that is not present in your data file." },
         Bad.Locus.HLA = { Error <- paste("\nThere may be a discrepancy with HLA loci names. Unrecognized locus name(s) encountered: ",y,".",sep="") },
         Bad.Allele.HLA = { Error <- paste("\nThere may be a discrepancy with allele names. Unrecognized allele name(s) encountered: ",y,".",sep="") },

         #Other
         PhantomSets = { Error <- "\nYou have defined a locus set (Loci.Set) that does not exist in the sample data. Please check the defined locus set." },
         MultipleSets = { Error <- "\nWARNING!!! You have opted to run multiple sets with overlapping loci. To avoid duplication of effort and results from the all pairwise haplotype tests, the locus test, and/or the amino acid test(!!!), it is suggested you run these tests separately on either the largest loci set possible or all loci in a given data set." },
         No.Internet = { Error <- "\nYou do not seem to be connected to the internet. CheckRelease() or UpdateRelease() cannot proceed." },
         TooMany.Missing = { Error <- "\nYour data is missing too many values at each locus. Try using Missing='ignore' when running BIGDAWG and avoid haplotype test." },         #Notifications
         Ignore.Missing = { Error <- "\nConsider setting a missing threshold or running without the haplotype ('H') analysis. A large number of missing data in the haplotype analysis will affect performance, require large amounts of RAM, cause long wait times, and in the worst case crash your computer." },
         Big.Missing = { Error <- "\nThe number of allowable missing will affect performance.\nConsider running with a smaller 'Missing' value or without the haplotype ('H') analysis.\ncontinuing......" },
         AllPairwise.Merge = { Error <- "\nYou have opted to run all pairwise combinations and merge the final data tables. For a large number of loci, this could take a long time. You have been warned!" },
         NotHLA.Trim = { Error <- "\nTrimming only relevant to HLA data, no trimming performed." },
         NotHLA.EVS.rm = { Error <- "\nExpression variant suffix stripping only relevant to HLA data, no stripping performed." },
         Exon = { Error <- paste("\n You have defined an exon that does not exist in locus ",y,". Please adjust exons or loci.set.",sep="") },

         #GLS Notifications
         Tab.Format = { Error <- "\nThe conversion tool encountered GL string delimiters. This isn't valid data for Tab2GL converion. Please see vignette." },
         GL.Format = { Error <- "\nYour GL strings may not be properly formatted. Please see vignette." },
         File.Error = { Error <- paste("\nThe conversion tool could not locate a file labeled ",y," in the specified working directory.",sep="") },
         GTYPE.Amb = { Error <- paste("\nThis appears to contain genotype list piping ('|') for genotype ambiguity strings (data rows: ",y,"). This is not supported in GLSconversion.",sep="") },
         Table.Col = { Error <- "\nThe table for Tab2GL conversion is not properly formatted, too few columns. Please see vignette." },
         Table.Pairs = { Error <- "\nThe table for Tab2GL conversion is not properly formatted, no locus column pairs encountered. Please see vignette." },
         Table.Amb = { Error <- "\nYour data has duplicate identifying information rows, perhaps due to data genotype ambiguity." },
         Locus.MultiField = { Error <- paste("\nYour GL string may be invalid. A locus cannot appear in multiple gene fields! ",z,ifelse(grepl(",",z)," appear"," appears")," in multiple fields of the GL string: ", y, ". Please see vignette.",sep="") },
         Allele.Amb.Format = { Error <- paste("\nYour GL string may be invalid. The ambiguous allele ",y," is not properly formatted. Please see vignette.", sep="") },
         notHLA.GLS = { Error <- paste("\nYou may want GLS conversion for non-HLA data. Currently, BIGDAWG only supports HLA for automatic GLS conversion. Please see vignette.", sep="") }

  )

  cat(Error,"\n",file=stderr())
  if(Output) { write.table(Error,file="Error_Log.txt",sep="\t",quote=F,col.names=F,row.names=F,append=T) }

}
