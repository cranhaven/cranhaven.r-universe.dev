find_jiebar_stop <-
function() {
  ST <- readLines(jiebaR::STOPPATH, encoding = "UTF-8")
 # ST <- readLines(jiebaR::STOPPATH, encoding = Ruchardet::detectFileEncoding(jiebaR::STOPPATH)) 
  ST <- ST[-c(1:127, 137, 148:155, 878:882, 1180:1206, 1359, 1526:1534)]
  ST <- ST[!grepl("[a-zA-Z]", ST)]
  ST <- whetherencode(ST)
  ST <- ST[!is.na(ST) & ST != ""]
  ST <- unlist(strsplit(ST, "\\\\n|\\\\t|\\\\r"))
  ST <- unlist(strsplit(ST, "[[:blank:]]+|[[:space:]]+|[[:punct:]]+|\\d+"))
  ST <- ST[ST != "" & ST != "NA"]
  ST <- unique(ST)
  return(ST)
}
