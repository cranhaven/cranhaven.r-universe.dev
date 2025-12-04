#' JSON2CSV
#'
#' This function converts DiMA (v5.0.9) JSON output file to a dataframe with 17
#' predefined columns which further acts as the input for other functions provided in this vDiveR package.
#'
#' @param json_data DiMA JSON output dataframe
#' @param host_name name of the host species
#' @param protein_name name of the protein
#' @return A dataframe which acts as input for the other functions in vDiveR package
#' @examples inputdf<-json2csv(JSON_sample)
#' @importFrom stats aggregate
#' @importFrom dplyr mutate_if right_join distinct
#' @importFrom tidyr replace_na
#' @export
json2csv <-function(json_data, host_name="unknown host", protein_name="unknown protein"){
    Group.2 <- x <- results.position <- results.diversity_motifs <- motif_short <- NULL

    data_flatten <- as.data.frame(json_data) %>%
        tidyr::unnest(cols = c(results.diversity_motifs))

    # data transformation
    motifs_incidence <- aggregate(data_flatten$incidence, list(data_flatten$results.position,data_flatten$motif_short), FUN=sum) %>%
        tidyr::spread(Group.2,x) %>%                           # transpose the rows (motif-long & incidence) to columns (index, major, minor, unique)
        dplyr::mutate_if(is.numeric, ~(replace_na(., 0)))      # replace NAN with 0
    #rename column 'Group.1' to 'results.position'


    colnames(motifs_incidence)[colnames(motifs_incidence) == 'Group.1'] <- "results.position"

    #sum the number of Index motif found in each position (if > 1 => multiIndex == TRUE)
    multiIndex<-data_flatten%>%
        group_by(results.position) %>%
        dplyr::summarize(multiIndex = sum(motif_short=="I")) %>%
        as.data.frame()

    #merge multiIndex to motifs_incidence df
    motifs_incidence <-right_join(motifs_incidence,multiIndex, by='results.position')%>%
        distinct()

    #replace multiIndex with boolean: x > 1 = TRUE and vice versa
    motifs_incidence$multiIndex <- ifelse(motifs_incidence$multiIndex>1, TRUE,FALSE)

    #extract the first encountered index kmer info if > 1 index is encountered for each position (rarely happens)
    index_data<-subset(data_flatten, motif_short == "I") %>% #extract rows that are of index
        group_by(results.position) %>% #group them based on position
        slice(1) %>% #take the first index encountered per position
        as.data.frame() #return the data in dataframe

    #replace low_support of NAN to FALSE
    index_data$results.low_support[is.na(index_data$results.low_support)] <- FALSE

    #combine both the index and variant motif information to motifs
    motifs<-right_join(motifs_incidence,index_data[c('query_name',"results.support","results.low_support","results.entropy","results.distinct_variants_incidence","results.position","results.total_variants_incidence","sequence",'highest_entropy.position','highest_entropy.entropy','average_entropy')],by='results.position')%>%
        distinct()

    #assign host
    motifs['host'] <- host_name

    #check if all expected columns are present
    #assert column with value of 0 if absent
    expected_columns<-c('results.position','I','Ma','Mi','U','multiIndex','query_name','results.support','results.low_support','results.entropy','results.distinct_variants_incidence','results.total_variants_incidence','sequence','highest_entropy.position','highest_entropy.entropy','average_entropy','host')
    missing_columns <- dplyr::setdiff(expected_columns, colnames(motifs))
    template <- data.frame(matrix(0, nrow = nrow(motifs), ncol = length(missing_columns))) # Create a template dataframe with missing columns filled with 0
    colnames(template) <- missing_columns

    motifs <- cbind(motifs, template) # Combine the template dataframe and the original motifs dataframe
    motifs <- motifs[, expected_columns] # Reorder columns to match the expected order

    #rename columns
    colnames(motifs)<-c('position','index.incidence','major.incidence','minor.incidence','unique.incidence','multiIndex','proteinName','count','lowSupport','entropy','distinctVariant.incidence','totalVariants.incidence','indexSequence','highestEntropy.position','highestEntropy','averageEntropy','host')
    #reorder the columns
    motifs<-motifs[,c(7,1,8,9,10,13,2,3,4,5,12,11,6,17,14,15,16)]
    #assign protein name
    motifs['proteinName'] <- protein_name
    motifs

}

