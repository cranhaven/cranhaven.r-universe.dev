
#Sort string in ProteinGroup column
sort_string_pg <- function(input_df,
                           sort_column,
                           split_pattern) {

  input_string <- input_df %>%
    dplyr::select(!!as.symbol(sort_column)) %>%
    unlist(.data, use.names = FALSE)

#remove spaces
adjusted_string <- stringr::str_remove_all(string = input_string, pattern = " ")

#split string based on split_pattern
splitted_string <- stringr::str_split(string = adjusted_string, pattern = split_pattern)

#sort splitted string
sorted_string <- comprehenr::to_list(for (i in splitted_string) stringr::str_sort(i))

#combine string again with ";"
output_string <- comprehenr::to_vec(for (i in sorted_string) paste(i, collapse = ";"))

  return(output_string)

}

#string analysis for trace_unique_common_pg
#keep only entries where proteins are not part of the proteingroup of the counterpart within binary comparison
analyze_string_pg <- function(input_df) {

index_df <- vector(mode = "logical")

#count nr of proteins per proteingroup #after sort_string_pg all with ";" separated
count_vec <- stringr::str_count(input_df[, 1, drop = TRUE], pattern = ";") + 1 #entries #first column - pg entries of input_df1

#check if proteins per proteingroup -input_df column 1 - are present in proteingroup of input_df column 3 (counterpart of binary comparison)
for (i in seq_len(length(count_vec))) {

 string <- unlist(stringr::str_split(string = input_df[i, 1, drop = TRUE], pattern = ";"))

 #number of proteins per proteingroup
 count <- length(string)

   for (x in 1:count_vec[i]) {
      if (stringr::str_detect(string = input_df[i, 3, drop = TRUE], pattern = string[x]) == TRUE) { #4th column - pg entries of input_df2 - appended to input_df1
        #if protein is in other proteingroup - reduce count by 1
        count <- count - 1
      }
    }

    #if protein was not detected in other protein group - count == length(string)
    if (count == length(string)) {
      index_df[i] <- TRUE
    } else if (count != length(string)) {
      index_df[i] <- FALSE
    }
  }

  #keep only the proteins of proteingroup which are not in proteingroup denotation of counterpart
  output_df <- input_df[index_df, ]

  return(output_df)
}

#Define used global varaibles
utils::globalVariables(c(":="))
