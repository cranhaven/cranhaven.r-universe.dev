read_pvals <-
function (data_name = "", snps_ann = "", from = "workspace") 
{
    print("Arguments set:")
    print(substitute(data_name))
    print(substitute(snps_ann))
    print(from)
    if (from != "workspace"){
if(from != "directory") {
stop("Argument \"from\" must be set to \"workspace\" or \"directory\"")
}
}
    if (missing(data_name) == TRUE){
        stop("Argument data_name (GWAS p-values) missing")
    }
    if (missing(snps_ann) == TRUE ){
        stop("Argument snps_ann (SNPs location) missing")
}
    if (from == "workspace") {
        data <- data_name
        all_snps <- snps_ann
    }
    if (from == "directory") {
        data <- read.table(data_name, sep = "\t", header = T, 
            stringsAsFactors = FALSE)
        all_snps <- read.table(snps_ann, sep = "\t", header = T, 
            stringsAsFactors = FALSE)
    }
    colnames(data)[1] <- "name"
    colnames(all_snps)[1] <- "name"
    all_data <- merge(all_snps, data, by = "name", all.x = FALSE, 
        all.y = TRUE, incomparables = "NA")
    return(all_data)
}
