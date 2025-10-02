#' Subtractor
#'
#' @param data_one first bedfile
#' @param data_two second bedfile
#' @param treament_name what column is the independent variable
#' @param field what column is the variable being compared
#' @param hoarder whether or not to retain the other data
#'
#' @return GRanges of the difference
#' @importFrom utils head
#'
#' @export
#'
#' @examples \donttest{ lab_sechellia.filename <- system.file("extdata",
#' "wild_sechellia.example_data.bed", package = "PopPsiSeqR")
#' lab.bg <- import.smvshift(lab_sechellia.filename)
#' lab.bg$sechellia <- "lab"
#' wild_sechellia.filename <- system.file("extdata",
#' "lab_sechellia.example_data.bed", package = "PopPsiSeqR")
#' wild.bg <- import.smvshift(wild_sechellia.filename)
#' wild.bg$sechellia <- "wild"
#' sub.traction <- subTractor(lab.bg, wild.bg ,treament_name = "sechellia")
#'}
subTractor<- function( data_one, data_two, treament_name = "pseudoparent", field="avg_simward_AFshift", hoarder = FALSE) {

  treat1 <- (data_one %>% head(n=1) %>% as.data.frame())[[treament_name]]#(data_one %>% head(n=1) %>% as.data.frame() %>% select(treatment_name))[[treatment_name]]
  treat2 <- (data_two %>% head(n=1) %>% as.data.frame())[[treament_name]]#(data_two %>% head(n=1) %>% as.data.frame() %>% select(treatment_name))[[treatment_name]]

  if (!hoarder) {
    data_one  %<>% as.data.frame() %>% select( c( "seqnames","start","end","width",field)) #%>% GRanges()
    data_two  %<>% as.data.frame() %>% select( c( "seqnames","start","end","width",field)) #%>% GRanges()inn

  } else {
    data_one <- data_one %>% as.data.frame()
    data_two <- data_two %>% as.data.frame()

  }
  merged_data.df <- inner_join(data_one,data_two,by=c("seqnames"="seqnames", "start"="start", "end"="end", "width"="width"), suffix = c(paste(".",treat1,sep=""), paste(".",treat2,sep="")))

  merged_data.df[[paste(treat1, "_minus_", treat2,sep="")]] <- merged_data.df[[paste(field, ".", treat1,sep="")]] -  merged_data.df[[paste(field, ".", treat2,sep="")]]

  return(merged_data.df %>% GenomicRanges::GRanges())

}
