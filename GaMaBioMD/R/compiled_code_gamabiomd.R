# Expand Accession Number Ranges
#'
#' This function expands accession number ranges, creating a data frame with sample and accession numbers.
#'
#' @param accession_ranges A named list where each element represents an accession range.
#'   The names of the list elements should correspond to sample names.
#'
#' @return A data frame with columns 'sample' and 'accession'.
#' @export
#'
#' @examples
#' # Example of defining accession number ranges.
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015250",
#'   WRU8 = c("AF245628", "AF353208 to AF353210"),
#'   WPU13 = "L11934 to L11940",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333086")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#' @rdname A.expand_accession_ranges
#' @order 1
expand_accession_ranges <- function(accession_ranges) {
  # Function to expand accession number ranges
  expand_accession_range <- function(range) {
    ranges <- unlist(strsplit(range, ","))

    expanded_accessions <- character(0)

    for (r in ranges) {
      parts <- strsplit(trimws(r), " to ")[[1]]

      prefix <- gsub("[0-9]", "", parts[1])
      start_num <- as.numeric(gsub("[^0-9]", "", parts[1]))

      if (length(parts) == 1) {
        # For single values
        format_string <- ifelse(prefix %in% c("L", "U"), "%s%05d", "%s%06d")
        expanded_accessions <- c(expanded_accessions, sprintf(format_string, prefix, start_num))
      } else {
        # For ranges
        end_num <- as.numeric(gsub("[^0-9]", "", parts[2]))
        seq_range <- seq(start_num, end_num)

        format_string <- ifelse(prefix %in% c("L", "U"), "%s%05d", "%s%06d")
        expanded_accessions <- c(expanded_accessions, sprintf(format_string, prefix, seq_range))
      }
    }

    return(expanded_accessions)
  }

  # Create a data frame to store sample and accession numbers
  sam_acc <- data.frame(
    sample = character(),
    accession = character(),
    stringsAsFactors = FALSE
  )

  # Loop through the accession ranges and expand them
  for (key in names(accession_ranges)) {
    accession_range <- accession_ranges[[key]]
    expanded_accessions <- expand_accession_range(accession_range)

    # Create a data frame with sample and accession numbers
    temp_df <- data.frame(
      sample = rep(key, length(expanded_accessions)),
      accession = expanded_accessions
    )

    # Combine with the main data frame
    sam_acc <- rbind(sam_acc, temp_df)
  }

  # Return the resulting data frame
  return(sam_acc)
}
# Function to get sequence information for a given accession
#' Retrieves sequence information for a given list of accessions.
#'
#' @param accession accession number.
#' @param remove_dot_1 Logical. If TRUE, removes '.1' from accession numbers.
#'
#' @return A data frame containing sequence information for the given accessions.
#'
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' }
#' @importFrom traits ncbi_byid
#' @rdname B.get_sequence_information
#' @order 2
get_sequence_information <- function(accession, remove_dot_1 = FALSE) {
  # Function to get sequence information for a given accession
  get_seq_info <- function(accession) {
    result <- tryCatch(
      {
        ncbi_byid(ids = accession, verbose = TRUE)
      },
      error = function(e) {
        # Handle errors, for example, print an error message
        warning(paste("Error for accession", accession, ":", conditionMessage(e), "\n"))
        return(NULL)
      }
    )
    return(result)
  }

  # Apply the function to each accession in the input vector
  seq_info_list <- lapply(accession, get_seq_info)

  # Combine the results into a data frame
  seq_info <- do.call(rbind, seq_info_list)

  # Remove '.1' portion from acc_no if specified
  if (remove_dot_1) {
    seq_info$acc_no <- sub("\\.1$", "", seq_info$acc_no)
  }

  return(seq_info)
}
# Function to preprocess data for alignment
#' Preprocesses data for sequence alignment.
#'
#' This function merges sample and accession information with sequence information,
#' filters out rows with missing sequences, and extracts relevant columns for the final data.
#'
#' @param sam_acc A data frame containing sample and accession information.
#' @param seq_info A data frame containing sequence information.
#'
#' @return A list containing the resulting data frames: 'merged_data', 'main_data', 'final_data'.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#' }
#' @importFrom dplyr select filter group_by mutate ungroup row_number
#' @importFrom magrittr %>%
#' @rdname C.preprocess_for_alignment
#' @order 3
preprocess_for_alignment <- function(sam_acc, seq_info) {

  # Check if sam_acc has the expected columns
  sam_acc_columns <- c("accession", "sample")

  if (!all(sam_acc_columns %in% colnames(sam_acc))) {
    stop("sam_acc data frame does not have the expected columns.")
  }

  # Check if seq_info has the expected columns
  seq_info_columns <- c("acc_no", "taxon", "taxonomy", "gene_desc", "length", "sequence")
  if (!all(seq_info_columns %in% colnames(seq_info))) {
    stop("seq_info data frame does not have the expected columns.")
  }

  accession <- sam_acc$accession
  acc_no <- seq_info$acc_no

  # Step 1: Merge data frames using the 'accession' and 'acc_no' columns
  merged_data <- merge(sam_acc, seq_info, by.x = "accession", by.y = "acc_no", all.x = TRUE)

  sample <- sam_acc$sample

  taxon <- seq_info$taxon
  taxonomy <- seq_info$taxonomy
  gene_desc <- seq_info$gene_desc
  length <- seq_info$length
  sequence <- seq_info$sequence

  # Step 2: Select relevant columns for main data frame
  main_data <- merged_data %>%
    select(
      Accession = accession,
      SampleID = sample,
      Taxon = taxon,
      Taxonomy = taxonomy,
      Gene_desc = gene_desc,
      Length = length,
      Sequence = sequence
    )

  # Step 3: Filter out rows with NA in the 'Sequence' column
  main_data <- main_data %>%
    filter(!is.na(Sequence))

  SampleID <- main_data$SampleID
  SequenceID <- main_data$SequenceID
  Sequence <- main_data$Sequence

  # Step 4: Extract relevant columns for the 'final' data
  final_data <- main_data %>%
    group_by(SampleID) %>%
    mutate(SequenceID = paste0(SampleID, ".", row_number())) %>%
    ungroup() %>%
    select(SampleID, SequenceID, Sequence)

  # Return the resulting data frames
  return(list(merged_data = merged_data, main_data = main_data, final_data = final_data))
}
# Write Fasta
#' Writes a data frame to a FASTA file.
#'
#' This function takes a data frame with 'SequenceID' and 'Sequence' columns
#' and writes the contents to a FASTA file. Each row in the data frame corresponds
#' to a sequence in the FASTA file, with the 'SequenceID' used as the header line
#' and the 'Sequence' as the sequence line.
#'
#' @param data A data frame with 'SequenceID' and 'Sequence' columns.
#'
#' @return A character vector representing the contents of the FASTA file.
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' data <- final_data
#'
#' # Call the function
#' fasta_content <- write_fasta(data)
#'
#' # Print or use the `fasta_content` as needed
#' print(fasta_content)
#'
#' output_directory <- tempdir()
#' # Specify the output file path
#' output_file_path <- file.path(output_directory, "output.fasta")
#'
#' # Open a connection to the output file
#' output_file <- file(output_file_path, "w")
#'
#' # Write the content to the file
#' writeLines(fasta_content, output_file)
#'
#' # Close the output file
#' close(output_file)
#'
#' # Print a message indicating successful file creation
#' warning("FASTA file has been created and saved at:", output_file_path, "\n")
#' }
#' @export
#' @rdname D.write_fasta
#' @order 4
write_fasta <- function(data) {
  # Initialize an empty character vector
  fasta_content <- character()

  # Iterate through each row in the data frame
  for (i in 1:nrow(data)) {
    # Add the header line (">SequenceID") to the character vector
    fasta_content <- c(fasta_content, paste(">", data$SequenceID[i]))

    # Add the sequence line to the character vector
    fasta_content <- c(fasta_content, data$Sequence[i])
  }

  # Return the character vector representing the contents of the FASTA file
  return(fasta_content)
}
# Function to plot and return visualizations
#' Plots the number of sequences and the probability of sequences per SampleID.
#'
#' This function takes a data frame with 'SampleID' and 'SequenceID' columns and
#' creates two bar plots. The first plot shows the number of sequences per SampleID,
#' and the second plot shows the probability of sequences per SampleID.
#'
#' @param final_data A data frame with 'SampleID' and 'SequenceID' columns.
#'
#' @return A list containing two ggplot2 plots: 'plot_num_sequences' and 'plot_prob'.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # Example usage
#' plots <- SampleID_vs_NumSequences(final_data)
#'
#' output_directory <- tempdir()
#' # Set the file name for the TIFF images
#' tiff_file_num_sequences <- file.path(output_directory, "0. SampleID_vs_NumSequences.tiff")
#' tiff_file_prob <- file.path(output_directory, "0. SampleID_vs_Probability.tiff")
#'
#' # Set the width, height, and DPI parameters
#' width_inch <- 8
#' height_inch <- 8
#' dpi <- 300
#'
#' # Open the TIFF devices and save the plots
#' tiff(tiff_file_num_sequences, width = width_inch, height = height_inch, units = "in", res = dpi)
#' print(plots$plot_num_sequences)
#' dev.off()
#'
#' tiff(tiff_file_prob, width = width_inch, height = height_inch, units = "in", res = dpi)
#' print(plots$plot_prob)
#' dev.off()
#' }
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom dplyr group_by summarize mutate n
#' @importFrom ggplot2 ggplot aes_string geom_bar labs theme_minimal theme element_text
#' @importFrom magrittr %>%
#' @rdname E.SampleID_vs_NumSequences
#' @order 5
SampleID_vs_NumSequences <- function(final_data) {

  SampleID <- final_data$SampleID

  sequence_counts <- data.frame(SampleID = character(),
                                SequenceID = character(),
                                NumSequences = numeric(),
                                prob = numeric(),
                                stringsAsFactors = FALSE)

  # Create an empty vector NumSequences
  NumSequences <- sequence_counts$NumSequences
  prob <- sequence_counts$prob

  # Check the number of sequences in each SampleID
  sequence_counts <- final_data %>%
    group_by(SampleID) %>%
    summarize(NumSequences = n()) %>%
    mutate(prob = NumSequences / sum(NumSequences))

  # Create a bar plot with vertical x-axis labels for NumSequences
  plot_num_sequences <- ggplot(sequence_counts, aes_string(x = "SampleID", y = "NumSequences")) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Number of Sequences per SampleID",
         x = "SampleID",
         y = "Number of Sequences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  # Create a bar plot with vertical x-axis labels for prob
  plot_prob <- ggplot(sequence_counts, aes_string(x = "SampleID", y = "prob")) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Probability of Sequences per SampleID",
         x = "SampleID",
         y = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  # Return the plots
  return(list(plot_num_sequences = plot_num_sequences, plot_prob = plot_prob))
}
# Function to sample data if specified, otherwise use final data
#' Samples data from each SampleID group if specified, otherwise uses the final data.
#'
#' This function takes a data frame with 'SampleID' and 'SequenceID' columns and
#' either returns the original data frame (if sample_proportion is NULL) or
#' samples a specified proportion from each SampleID group.
#'
#' @param final_data A data frame with 'SampleID' and 'SequenceID' columns.
#' @param sample_proportion Proportion of data to sample from each SampleID group.
#'   If NULL, the original data frame is returned.
#'
#' @return A data frame either with the original data or sampled data based on the specified proportion.
#' @export
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data # use final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#' }
#' @importFrom dplyr group_by slice_sample ungroup
#' @rdname F.data_sampling
#' @order 6
data_sampling <- function(final_data, sample_proportion = NULL) {

  SampleID <- final_data$SampleID

  if (is.null(sample_proportion)) {
    # If sample_proportion is not specified, return the final data
    return(final_data)
  } else {
    # Sample a proportion from each SampleID group
    sampled_data <- final_data %>%
      group_by(SampleID) %>%
      slice_sample(prop = sample_proportion) %>%
      ungroup()
    return(sampled_data)
  }
}
#' Sequence Alignment and Analysis
#' @param data A data frame with 'SequenceID' and 'Sequence' columns.
#' @param type A character string specifying the type of sequence alignment ('global', 'local', etc.).
#' @param verbose An integer indicating the level of verbosity (0, 1, or 2).
#' @return A list containing matrices and results from sequence alignment.
#' @export
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' }
#' @importFrom  Biostrings DNAStringSet DNAString width pairwiseAlignment
#' @importFrom dplyr bind_rows group_by mutate slice_sample summarize ungroup
#' @importFrom utils write.table
#' @rdname G.alignment_info
#' @order 7
#' @references Faith, D. P. (1992). Conservation evaluation and phylogenetic diversity. Biological conservation, 61(1), 1-10.
alignment_info <- function(data, type = "global", verbose = 1) {
  # Create a DNAStringSet object from the sequences
  sequences_dna <- DNAStringSet(data$Sequence)

  data$SequenceID_length <- paste0(
    data$SequenceID,
    "_",
    width(sequences_dna)
  )

  # Initialize an empty matrix to store alignment scores
  num_sequences <- length(sequences_dna)
  score_matrix <- matrix(as.numeric(), nrow = num_sequences, ncol = num_sequences)

  # Initialize a list to store alignment results
  alignment_results_list <- list()

  # Initialize progress counter
  progress <- 0

  # Initialize total time taken
  total_time_taken <- 0

  # Calculate total_combinations
  total_combinations <- num_sequences * (num_sequences - 1) / 2

  # Create a matrix to store information
  alignment_info_matrix <- matrix(nrow = total_combinations, ncol = 5)
  colnames(alignment_info_matrix) <- c("Combination_No", "Combination", "Start_Time", "Time_Taken", "End_Time")

  # Record start time
  start_time <- Sys.time()
  if (verbose > 0) {
    warning("Alignment started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }

  # Perform pairwise sequence alignment and fill the score matrix
  for (i in 1:(num_sequences - 1)) {
    for (j in (i + 1):num_sequences) {
      # Increment progress counter
      progress <- progress + 1

      # Record start time for the combination
      combination_start_time <- Sys.time()

      alignment_results <- pairwiseAlignment(
        pattern = sequences_dna[i],
        subject = sequences_dna[j],
        type = type
      )

      # Record end time for the combination
      combination_end_time <- Sys.time()

      # Update information in the matrix
      alignment_info_matrix[progress, ] <- as.character(c(progress, paste(data$SequenceID[i], data$SequenceID[j], sep = "_"),
                                                          format(combination_start_time, "%Y-%m-%d %H:%M:%OS"),
                                                          as.numeric(difftime(combination_end_time, combination_start_time, units = "secs")),
                                                          format(combination_end_time, "%Y-%m-%d %H:%M:%OS")))

      # Calculate time taken for the current combination
      time_taken <- as.numeric(difftime(combination_end_time, combination_start_time, units = "secs"))

      # Update total time taken
      total_time_taken <- total_time_taken + time_taken

      # Calculate average time taken
      avg_time_taken <- total_time_taken / progress

      # Calculate estimated time remaining in seconds
      estimated_time_remaining_seconds <- avg_time_taken * (total_combinations - progress)

      # Calculate estimated end time
      complete_at <- combination_start_time + as.difftime(estimated_time_remaining_seconds, units = "secs")
      complete_at <- format(complete_at, "%Y-%m-%d %H:%M:%OS")

      # Store the alignment_results in the list with a name corresponding to the pair
      alignment_results_list[[paste(data$SequenceID[i], data$SequenceID[j], sep = "_")]] <- alignment_results # SequnceID

      # Store score score
      score <- alignment_results@score
      score_matrix[i, j] <- score

      # Print progress information
      if (verbose > 0) {
        warning("Progress:", progress, "out of", total_combinations, "& Whole task ends at:", complete_at, "\n")
      }

      # Print alignment_info_matrix after each combination
      if (verbose > 1) {
        warning("Alignment Information Matrix after combination", progress, ":\n")
        print(as.data.frame(alignment_info_matrix))
      }
    }
  }

  # Record end time
  end_time <- Sys.time()
  if (verbose > 0) {
    warning("Alignment started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    warning("Alignment ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

    # Convert total time taken into days, hours, minutes, and seconds
    total_days <- floor(total_time_taken / (24 * 3600))
    total_hours <- floor((total_time_taken %% (24 * 3600)) / 3600)
    total_minutes <- floor((total_time_taken %% 3600) / 60)
    total_seconds <- total_time_taken %% 60

    warning("Total time taken:", total_days, "days", total_hours, "hours", total_minutes, "minutes", total_seconds, "seconds\n")
    warning("Average time taken for a combination:", avg_time_taken, "Sec\n")
  }

  # Convert the upper triangle to a symmetric matrix
  score_matrix <- t(score_matrix)
  score_matrix[upper.tri(score_matrix)] <- NA

  # Set row and column names
  rownames(score_matrix) <- data$SequenceID
  colnames(score_matrix) <- data$SequenceID

  # Replace NA values in the score matrix with 0
  score_matrix[is.na(score_matrix)] <- 0

  # Normalize the score matrix
  max_value <- max(score_matrix)
  normalized_score_matrix <- (score_matrix / max_value)

  # percent similarity matrix ####

  # Initialize an empty matrix to store total aligned positions
  total_aligned_positions_matrix <- matrix(as.numeric(), nrow = num_sequences, ncol = num_sequences)

  # Initialize an empty matrix to store matching positions
  number_of_matching_positions_matrix <- matrix(as.numeric(), nrow = num_sequences, ncol = num_sequences)

  # Initialize an empty matrix to percent similarity
  percent_similarity_matrix <- matrix(as.numeric(), nrow = num_sequences, ncol = num_sequences)

  # Calculate total aligned positions and matching positions
  for (i in 1:(num_sequences - 1)) {
    for (j in (i + 1):num_sequences) {
      alignment_results <- alignment_results_list[[paste(data$SequenceID[i], data$SequenceID[j], sep = "_")]]

      # Extract alignment information
      alignment_pattern <- as.character(alignment_results@pattern)
      alignment_subject <- as.character(alignment_results@subject)

      # Convert alignment_pattern and alignment_subject to character vectors
      alignment_pattern_char <- unlist(strsplit(as.character(alignment_pattern), ''))
      alignment_subject_char <- unlist(strsplit(as.character(alignment_subject), ''))

      # Create a numeric vector based on the conditions
      alignment_numeric_vector <- ifelse(alignment_pattern_char == "-" | alignment_subject_char == "-", 0,
                                         ifelse(alignment_pattern_char == alignment_subject_char, 1, -1))

      # Calculate Total Aligned Positions and Number of Matching Positions
      total_aligned_positions <- as.numeric(sum(alignment_numeric_vector != 0))
      number_of_matching_positions <- as.numeric(sum(alignment_numeric_vector == 1))
      percent_similarity <- (number_of_matching_positions/ total_aligned_positions)

      total_aligned_positions_matrix[i, j] <- total_aligned_positions
      number_of_matching_positions_matrix[i, j] <- number_of_matching_positions
      percent_similarity_matrix[i, j] <- percent_similarity
    }
  }

  # Convert the upper triangle to a symmetric matrix
  total_aligned_positions_matrix <- t(total_aligned_positions_matrix)
  total_aligned_positions_matrix[upper.tri(total_aligned_positions_matrix)] <- NA

  number_of_matching_positions_matrix <- t(number_of_matching_positions_matrix)
  number_of_matching_positions_matrix[upper.tri(number_of_matching_positions_matrix)] <- NA

  percent_similarity_matrix <- t(percent_similarity_matrix)
  percent_similarity_matrix[upper.tri(percent_similarity_matrix)] <- NA

  # Set row and column names
  rownames(total_aligned_positions_matrix) <- data$SequenceID
  colnames(total_aligned_positions_matrix) <- data$SequenceID

  rownames(number_of_matching_positions_matrix) <- data$SequenceID
  colnames(number_of_matching_positions_matrix) <- data$SequenceID

  rownames(percent_similarity_matrix) <- data$SequenceID
  colnames(percent_similarity_matrix) <- data$SequenceID

  # Return the results
  return(list(score_matrix = score_matrix,
              normalized_score_matrix = normalized_score_matrix,
              total_aligned_positions_matrix = total_aligned_positions_matrix,
              number_of_matching_positions_matrix = number_of_matching_positions_matrix,
              percent_similarity_matrix = percent_similarity_matrix,
              alignment_results_list = alignment_results_list,
              alignment_info_matrix = alignment_info_matrix))
}
#' Compute Average Similarity Matrix
#'
#' This function computes the average similarity matrix based on the percent similarity matrix.
#'
#' @param percent_similarity_matrix A matrix containing percent similarity values between sequences.
#'   Rows and columns should be labeled with SequenceIDs.
#'
#' @return A matrix containing average similarity values between SampleIDs.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#' }
#' @rdname H.compute_average_similarity_matrix
#' @order 8
compute_average_similarity_matrix <- function(percent_similarity_matrix) {
  # Create a copy of the original matrix
  complete_percent_similarity <- as.matrix(percent_similarity_matrix)

  # Assuming percent_similarity_matrix is your original matrix
  complete_percent_similarity[upper.tri(complete_percent_similarity)] <-
    t(complete_percent_similarity)[upper.tri(complete_percent_similarity)]

  # Set all diagonal elements to 1
  diag(complete_percent_similarity) <- 1

  # Extract SampleIDs from the row names
  sample_ids <- sub("\\..*", "", rownames(complete_percent_similarity))

  # Create an empty matrix to store average similarities
  average_percent_similarity <- matrix(0, nrow = length(unique(sample_ids)),
                                       ncol = length(unique(sample_ids)))

  rownames(average_percent_similarity) <- colnames(average_percent_similarity) <- unique(sample_ids)

  # Iterate through unique SampleIDs and compute average similarity
  for (sample_id1 in unique(sample_ids)) {
    for (sample_id2 in unique(sample_ids)) {
      # Extract similarities for pairs of SequenceIDs within the SampleIDs
      similarities <- complete_percent_similarity[
        grep(sample_id1, rownames(complete_percent_similarity)),
        grep(sample_id2, colnames(complete_percent_similarity))
      ]

      # Compute the average similarity
      average_percent_similarity[sample_id1, sample_id2] <- mean(similarities)
    }
  }

  # Return the average similarity matrix
  return(average_percent_similarity)
}
#' Generate Heatmaps
#'
#' This function generates interactive and static heatmaps based on the average similarity matrix.
#'
#' @param average_percent_similarity A matrix containing average similarity values between SampleIDs.
#'   Rows and columns should be labeled with SampleIDs.
#' @param html_title Title for the interactive heatmap (HTML version).
#' @param tiff_title Title for the static heatmap (TIFF version).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param width_inch Width of the heatmap in inches.
#' @param height_inch Height of the heatmap in inches.
#' @param dpi Dots per inch for the static heatmap.
#'
#' @return A list containing the file names of the generated heatmaps.
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile theme_bw scale_fill_gradient element_blank
#' @importFrom heatmaply heatmaply
#' @importFrom reshape2 melt
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' heatmap_files <- generate_heatmaps(average_percent_similarity)
#'
#' # Save the interactive heatmap as an HTML file ####
#' html <- file.path(output_directory, "5. heatmap.html")
#' # htmlwidgets should be installed and loaded
#' # htmlwidgets::saveWidget(heatmap_files$html, file = html)
#'
#' # save the TIFFE images ####
#'
#' # heatmap_tiff_file1
#' tiff1 <- file.path(output_directory, "5. heatmap1.tiff")
#' tiff(tiff1, width = width_inch, height = height_inch, units = "in", res = dpi)
#' heatmap(as.matrix(average_percent_similarity), main = "Heatmap of Average Similarity")
#'
#' # Close the TIFF device
#' dev.off()
#'
#' # heatmap_tiff_file2
#' tiff2 <- file.path(output_directory, "5. heatmap2.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff2, width = width_inch, height = height_inch, units = "in", res = dpi)
#' print(heatmap_files$tiff2)
#'
#' # Close the TIFF device
#' dev.off()
#'
#' # heatmap_tiff_file3
#' # tiff3 <- file.path(output_directory, "5. heatmap3.tiff")
#'
#' # Open the TIFF device and create the heatmap.2 with hierarchical clustering dendrogram
#' # gplots should be installed and loaded
#' # gplots::heatmap.2(as.matrix(average_percent_similarity),
#' #          dendrogram = "row",
#' #          Colv = "Rowv",
#' #          scale = "row",
#' #          main = "Heatmap of Average Similarity")
#'
#' # Close the TIFF device
#' # dev.off()
#' }
#' @rdname I.generate_heatmaps
#' @order 9
generate_heatmaps <- function(average_percent_similarity,
                              html_title = "Heatmap of Average Similarity",
                              tiff_title = "Heatmap of Average Similarity",
                              xlab = "SampleID", ylab = "SampleID",
                              width_inch = 8, height_inch = 6, dpi = 300) {

  # Create the interactive heatmap with dendrogram branches and save as HTML
  heatmap_html_file <- heatmaply(as.matrix(average_percent_similarity),
                                 Rowv = TRUE, Colv = TRUE,
                                 main = html_title,
                                 xlab = xlab, ylab = ylab)

  melted_data <- melt(average_percent_similarity)

  Var1 <- melted_data$Var1
  Var2 <- melted_data$Var2
  value <- melted_data$value

  heatmap_tiff_file2 <- ggplot(data = melted_data,
                               aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    theme_bw() +
    scale_fill_gradient(low = "blue", high = "green", limits = c(0, 1)) +
    labs(title = tiff_title, x = xlab, y = ylab) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(vjust = 0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  # Return the file names of the generated heatmaps
  return(list(html = heatmap_html_file,
              tiff2 = heatmap_tiff_file2))
}
#' Perform Hierarchical Clustering for average similarity matrix
#'
#' This function performs hierarchical clustering based on a similarity matrix and provides additional information
#' such as a colored dendrogram, clustered data, sample percentage within clusters, and total percentage for each cluster.
#'
#' @param similarity_matrix A matrix containing similarity values between SampleIDs.
#' @param num_clusters_option The desired number of clusters. Default is 10.
#' @param cut_height_option The height at which the dendrogram should be cut to obtain clusters. Default is 0.2.
#'
#' @return A list containing the dendrogram, clustered data, sample percentage within clusters, and total percentage for each cluster.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' clustering_result <- clustering_average_similarity(average_percent_similarity)
#'
#' # Extract the dendrogram and clustered data
#' dend_colored <- clustering_result$dendrogram
#' clustered_data <- clustering_result$clustered_data
#' Cluster_SampleID_Percentage <- clustering_result$Cluster_SampleID_Percentage
#' Cluster_TotalPercentage <- clustering_result$Cluster_TotalPercentage
#'
#' tiff_file <- file.path(output_directory, "6. hierarchical_clustering_dendrogram_colored.tiff")
#'
#' # Save the dendrogram as a TIFF image
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#' plot(dend_colored, main = "Colored Hierarchical Clustering Dendrogram")
#' dev.off()
#'
#' # Save the clustered data frame to a CSV file
#' write.csv(clustered_data, file.path(output_directory, "7. clustered_data.csv"), row.names = FALSE)
#' }
#' @importFrom stats as.dist cutree hclust as.dendrogram
#' @importFrom dendextend color_branches
#' @importFrom dplyr select mutate group_by summarize n ungroup pull arrange distinct
#' @importFrom ggplot2 element_text
#' @rdname J.clustering_average_similarity
#' @order 10
clustering_average_similarity <- function(similarity_matrix, num_clusters_option = 10, cut_height_option = 0.2) {

  similarity_matrix <- as.matrix(similarity_matrix)

  # Check if num_clusters_option is greater than the number of rows or columns
  if (num_clusters_option > nrow(similarity_matrix) || num_clusters_option > ncol(similarity_matrix)) {
    num_clusters_option <- min(nrow(similarity_matrix), ncol(similarity_matrix))
  }

  # Perform hierarchical clustering using the similarity matrix
  hc <- hclust(as.dist(1 - similarity_matrix), method = "complete")

  # Create a dendrogram object
  dend <- as.dendrogram(hc)

  # Option 1: Cut the dendrogram to obtain clusters based on the number of clusters
  clusters_option <- cutree(hc, k = num_clusters_option)

  # Option 2: Cut the dendrogram to obtain clusters based on a specified height
  clusters_height <- cutree(hc, h = cut_height_option)

  # Choose between options based on your preference
  if (length(unique(clusters_option)) < length(unique(clusters_height))) {
    clusters <- clusters_option
    num_clusters <- length(unique(clusters_option))
  } else {
    clusters <- clusters_height
    num_clusters <- length(unique(clusters_height))
  }

  # Add color to the dendrogram based on clusters
  dend_colored <- color_branches(dend, k = num_clusters)

  # Create a data frame and order it based on the Clusters column
  clustered_data <- data.frame(SampleID = names(clusters), Clusters = as.integer(clusters)) %>%
    arrange(Clusters)

  SampleID <- clustered_data$SampleID
  Clusters <- clustered_data$Clusters
  Percentage <- clustered_data$Percentage

  # Calculate the percentage and add a new column
  Cluster_SampleID_Percentage <- clustered_data %>%
    group_by(Clusters, SampleID) %>%
    mutate(Percentage = n() / nrow(clustered_data) * 100)

  # Create a new data frame with Cluster, SampleID, and Percentage columns
  Cluster_SampleID_Percentage <- Cluster_SampleID_Percentage %>%
    select(Clusters, SampleID, Percentage) %>%
    distinct()

  # Create a new data frame with Cluster, SampleID, and Percentage columns
  Cluster_TotalPercentage <- Cluster_SampleID_Percentage %>%
    select(Clusters, SampleID, Percentage) %>%
    distinct() %>%  # Remove duplicate rows, assuming the same combination appears multiple times
    arrange(Clusters) %>%  # Order the data frame based on the Clusters column
    group_by(Clusters) %>%  # Group by Clusters
    summarize(Total_Percentage = sum(Percentage))  # Calculate the total percentage for each cluster

  # Return the dendrogram, clustered data, and additional data frames
  return(list(dendrogram = dend_colored, clustered_data = clustered_data,
              Cluster_SampleID_Percentage = Cluster_SampleID_Percentage,
              Cluster_TotalPercentage = Cluster_TotalPercentage))
}
#' Perform Hierarchical Clustering for Percent Similarity
#'
#' This function performs hierarchical clustering based on a percent similarity matrix and provides additional information
#' such as a colored dendrogram, clustered data, sample percentage within clusters, and total percentage for each cluster.
#'
#' @param similarity_matrix A matrix containing percent similarity values between SampleIDs.
#' @param num_clusters_option The desired number of clusters. Default is 10.
#' @param cut_height_option The height at which the dendrogram should be cut to obtain clusters. Default is 0.2.
#' @param add_to_final_data A logical value indicating whether to add the clustering results to the final data. Default is TRUE.
#'
#' @return A list containing the dendrogram, clustered data, sample percentage within clusters, and total percentage for each cluster.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' clustering_result <- clustering_percent_similarity(percent_similarity_matrix)
#'
#' # Extract the dendrogram and clustered data
#' dend_colored <- clustering_result$dendrogram
#' clustered_data <- clustering_result$clustered_data
#' Cluster_SampleID_Percentage <- clustering_result$Cluster_SampleID_Percentage
#' Cluster_TotalPercentage <- clustering_result$Cluster_TotalPercentage
#'
#' tiff_file <- file.path(output_directory, "6. hierarchical_clustering_dendrogram_colored.tiff")
#'
#' # Save the dendrogram as a TIFF image
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#' plot(dend_colored, main = "Colored Hierarchical Clustering Dendrogram")
#' dev.off()
#'
#' # Save the clustered data frame to a CSV file
#' write.csv(clustered_data, file.path(output_directory, "7. clustered_data.csv"), row.names = FALSE)
#' }
#' @rdname K.clustering_percent_similarity
#' @order 11
clustering_percent_similarity <- function(similarity_matrix, num_clusters_option = 10, cut_height_option = 0.2, add_to_final_data = TRUE) {

  similarity_matrix <- as.matrix(similarity_matrix)

  # Check if num_clusters_option is greater than the number of rows or columns
  if (num_clusters_option > nrow(similarity_matrix) || num_clusters_option > ncol(similarity_matrix)) {
    num_clusters_option <- min(nrow(similarity_matrix), ncol(similarity_matrix))
  }

  # Perform hierarchical clustering using the similarity matrix
  hc <- hclust(as.dist(1 - similarity_matrix), method = "complete")

  # Create a dendrogram object
  dend <- as.dendrogram(hc)

  # Option 1: Cut the dendrogram to obtain clusters based on the number of clusters
  clusters_option <- cutree(hc, k = num_clusters_option)

  # Option 2: Cut the dendrogram to obtain clusters based on a specified height
  clusters_height <- cutree(hc, h = cut_height_option)

  # Choose between options based on your preference
  if (length(unique(clusters_option)) < length(unique(clusters_height))) {
    clusters <- clusters_option
    num_clusters <- length(unique(clusters_option))
  } else {
    clusters <- clusters_height
    num_clusters <- length(unique(clusters_height))
  }

  # Add color to the dendrogram based on clusters
  dend_colored <- color_branches(dend, k = num_clusters)

  # Determine which data frame to update based on the parameter
  if (add_to_final_data) {
    # Ensure that Clusters and Percentage columns exist and are initialized
    if (!("Clusters" %in% names(final_data))) {
      final_data$Clusters <- NA  # Initialize with some default value or appropriate logic
    }

    if (!("Percentage" %in% names(final_data))) {
      final_data$Percentage <- NA  # Initialize with some default value or appropriate logic
    }
    clustered_data <- final_data
  } else {
    # Ensure that Clusters and Percentage columns exist and are initialized
    if (!("Clusters" %in% names(sampled_data))) {
      sampled_data$Clusters <- NA  # Initialize with some default value or appropriate logic
    }

    if (!("Percentage" %in% names(sampled_data))) {
      sampled_data$Percentage <- NA  # Initialize with some default value or appropriate logic
    }
    clustered_data <- sampled_data
  }

  Clusters <- clustered_data$Clusters
  SampleID <- clustered_data$SampleID
  Percentage <- clustered_data$Percentage

  # Arrange clustered_data based on the "Clusters" column
  clustered_data <- clustered_data %>%
    arrange(Clusters)

  # Calculate the percentage and add a new column
  Cluster_SampleID_Percentage <- clustered_data %>%
    group_by(Clusters, SampleID) %>%
    mutate(Percentage = n() / nrow(clustered_data) * 100) %>%
    arrange(Clusters)

  # Create a new data frame with Cluster, SampleID, and Percentage columns
  Cluster_SampleID_Percentage <- Cluster_SampleID_Percentage %>%
    select(Clusters, SampleID, Percentage) %>%
    distinct() %>%
    arrange(Clusters)

  # Create a new data frame with Cluster, SampleID, and Percentage columns
  Cluster_TotalPercentage <- Cluster_SampleID_Percentage %>%
    select(Clusters, SampleID, Percentage) %>%
    distinct() %>%  # Remove duplicate rows, assuming the same combination appears multiple times
    arrange(Clusters) %>%  # Order the data frame based on the Clusters column
    group_by(Clusters) %>%  # Group by Clusters
    summarize(Total_Percentage = sum(Percentage))  # Calculate the total percentage for each cluster

  # Return the dendrogram, clustered data, and additional data frames
  return(list(dendrogram = dend_colored,
              clustered_data = clustered_data,  # Return the arranged clustered_data
              Cluster_SampleID_Percentage = Cluster_SampleID_Percentage,
              Cluster_TotalPercentage = Cluster_TotalPercentage))
}
#' Bubble Plot Count Function
#'
#' This function generates a bubble plot using ggplot2.
#'
#' @param clustered_data A data frame containing clustered data.
#' @param title The title of the plot.
#' @param x_label The label for the x-axis.
#' @param y_label The label for the y-axis.
#' @param size_label The label for the size variable.
#' @param color_label The label for the color variable.
#'
#' @return A ggplot object representing the bubble plot.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' clustering_result <- clustering_average_similarity(average_percent_similarity)
#'
#' # Extract the dendrogram and clustered data
#' dend_colored <- clustering_result$dendrogram
#' clustered_data <- clustering_result$clustered_data
#' Cluster_SampleID_Percentage <- clustering_result$Cluster_SampleID_Percentage
#' Cluster_TotalPercentage <- clustering_result$Cluster_TotalPercentage
#'
#' tiff_file <- file.path(output_directory, "6. hierarchical_clustering_dendrogram_colored.tiff")
#'
#' # Save the dendrogram as a TIFF image
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#' plot(dend_colored, main = "Colored Hierarchical Clustering Dendrogram")
#' dev.off()
#'
#' # Save the clustered data frame to a CSV file
#' write.csv(clustered_data, file.path(output_directory, "7. clustered_data.csv"), row.names = FALSE)
#'
#' # Example usage with clustered_data
#' clustered_data <- clustered_data # Load or generate your clustered data
#' bubble_plot_count <- bubble_plot_count(clustered_data = clustered_data,
#'                                       title = "Bubble Plot of Clusters",
#'                                       x_label = "Clusters",
#'                                       y_label = "Sample ID",
#'                                       size_label = "Count",
#'                                       color_label = "Sample ID")
#'
#' # Save the bubble plot as a TIFF image
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' # Set the file name for the TIFF image
#' tiff_file <- file.path(output_directory, "bubble_plot_count.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Print and save the bubble plot
#' print(bubble_plot_count)
#'
#' # Close the TIFF device
#' dev.off()
#' }
#' @importFrom ggplot2 aes
#' @rdname L.bubble_plot_count
#' @order 12
bubble_plot_count <- function(clustered_data, title, x_label, y_label, size_label, color_label) {

  Clusters <- clustered_data$Clusters
  SampleID <- clustered_data$SampleID

  # Compute count using dplyr::count
  count_data <- clustered_data %>%
    count(Clusters, SampleID, name = "Count")

  Count <- count_data$Count

  # Merge the count_data back to clustered_data
  clustered_data <- merge(clustered_data, count_data, by = c("Clusters", "SampleID"), all.x = TRUE)

  # Create the plot
  plot <- ggplot(clustered_data, aes(x = as.factor(Clusters), y = SampleID, size = Count, color = as.factor(SampleID))) +
    geom_count() +
    labs(title = title,
         x = x_label,
         y = y_label,
         size = size_label,
         color = color_label) +
    theme_minimal()

  return(plot)
}
#' Bubble Plot Percentage Function
#'
#' This function generates a bubble plot using ggplot2 based on percentage data.
#'
#' @param Cluster_SampleID_Percentage A data frame containing cluster, sample ID, and percentage data.
#' @param title The title of the plot.
#' @param x_label The label for the x-axis.
#' @param y_label The label for the y-axis.
#' @param size_label The label for the size variable.
#' @param color_label The label for the color variable.
#'
#' @return A ggplot object representing the bubble plot.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' clustering_result <- clustering_percent_similarity(percent_similarity_matrix)
#'
#' # Extract the dendrogram and clustered data
#' dend_colored <- clustering_result$dendrogram
#' clustered_data <- clustering_result$clustered_data
#' Cluster_SampleID_Percentage <- clustering_result$Cluster_SampleID_Percentage
#' Cluster_TotalPercentage <- clustering_result$Cluster_TotalPercentage
#'
#' tiff_file <- file.path(output_directory, "6. hierarchical_clustering_dendrogram_colored.tiff")
#'
#' # Save the dendrogram as a TIFF image
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#' plot(dend_colored, main = "Colored Hierarchical Clustering Dendrogram")
#' dev.off()
#'
#' # Save the clustered data frame to a CSV file
#' write.csv(clustered_data, file.path(output_directory, "7. clustered_data.csv"), row.names = FALSE)
#'
#' # Example usage with Cluster_SampleID_Percentage
#' Cluster_SampleID_Percentage <- Cluster_SampleID_Percentage
#' bubble_plot_percentage <- bubble_plot_percentage(Cluster_SampleID_Percentage,
#'                                                 title = "Bubble Plot",
#'                                                 x_label = "Clusters",
#'                                                 y_label = "Sample ID",
#'                                                 size_label = "Percentage",
#'                                                 color_label = "Sample ID")
#'
#' # Save the bubble plot as a TIFF image
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' # Set the file name for the TIFF image
#' tiff_file <- file.path(output_directory, "bubble_plot_percentage.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Print and save the bubble plot
#' print(bubble_plot_percentage)
#'
#' # Close the TIFF device
#' dev.off()
#' }
#' @rdname M.bubble_plot_percentage
#' @order 13
bubble_plot_percentage <- function(Cluster_SampleID_Percentage, title, x_label, y_label, size_label, color_label) {

  Clusters <- Cluster_SampleID_Percentage$Clusters
  SampleID <- Cluster_SampleID_Percentage$SampleID
  Percentage <- Cluster_SampleID_Percentage$Percentage

  plot <- ggplot(Cluster_SampleID_Percentage, aes(x = as.factor(Clusters), y = SampleID, size = Percentage, color = as.factor(SampleID))) +
    geom_count() +
    labs(title = title,
         x = x_label,
         y = y_label,
         size = size_label,
         color = color_label) +
    theme_minimal()

  return(plot)
}
#' Generate Phylogenetic Tree and Color Palette for Average Similarity
#'
#' This function generates a Neighbor-Joining phylogenetic tree and a color palette based on the average similarity matrix.
#'
#' @param similarity_matrix A matrix containing pairwise similarities between samples.
#'
#' @return A list containing the phylogenetic tree and a color palette.
#' @export
#' @importFrom ape nj
#' @importFrom stats dist
#' @importFrom grDevices rainbow
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' clustering_result <- clustering_average_similarity(average_percent_similarity)
#'
#' # Extract the dendrogram and clustered data
#' dend_colored <- clustering_result$dendrogram
#' clustered_data <- clustering_result$clustered_data
#' Cluster_SampleID_Percentage <- clustering_result$Cluster_SampleID_Percentage
#' Cluster_TotalPercentage <- clustering_result$Cluster_TotalPercentage
#'
#' tiff_file <- file.path(output_directory, "6. hierarchical_clustering_dendrogram_colored.tiff")
#'
#' # Save the dendrogram as a TIFF image
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#' plot(dend_colored, main = "Colored Hierarchical Clustering Dendrogram")
#' dev.off()
#'
#' # Save the clustered data frame to a CSV file
#' write.csv(clustered_data, file.path(output_directory, "7. clustered_data.csv"), row.names = FALSE)
#'
#' # Example usage with similarity_matrix
#' result <- tree_average_similarity(average_percent_similarity)
#' tree <- result$tree
#' color_palette <- result$color_palette
#'
#' output_directory <- tempdir()
#'
#' tree_newick <- file.path(output_directory, "phylogenetic_tree_nj.nwk")
#'
#' # Save the phylogenetic tree as a Newick file
#' # ape::write.tree(tree, file = tree_newick)
#'
#' # Save the phylogenetic tree as a TIFF image
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' # Set the file name for the TIFF image
#' tiff_file <- file.path(output_directory, "phylogenetic_tree.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Plot the phylogenetic tree vertically
#' plot(tree, main = "Neighbor-Joining Tree", cex = 1, direction = "downward")
#'
#' # Close the TIFF device
#' dev.off()
#'
#' # Set the file name for the TIFF image with rainbow-colored branches
#' tiff_file_rainbow <- file.path(output_directory, "phylogenetic_tree_rainbow.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file_rainbow, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Plot the phylogenetic tree vertically with rainbow-colored branches
#' plot(tree, main = "NJ Tree", cex = 1, direction = "downward", tip.color = color_palette)
#'
#' # Close the TIFF device
#' dev.off()
#' }
#' @rdname N.tree_average_similarity
#' @order 14
tree_average_similarity <- function(similarity_matrix) {
  # Create neighbor-joining tree
  tree <- nj(dist(as.matrix(1 - similarity_matrix)))

  # Generate a color palette based on the number of tips in the tree
  num_tips <- length(tree$tip.label)
  color_palette <- rainbow(num_tips)

  # Return the tree and color palette
  return(list(tree = tree, color_palette = color_palette))
}
#' Generate Phylogenetic Tree and Color Palette for Percent Similarity Matrix
#'
#' This function generates a Neighbor-Joining phylogenetic tree and a color palette based on the percent similarity matrix.
#'
#' @param percent_similarity_matrix A matrix containing pairwise percent similarities between samples.
#'
#' @return A list containing the phylogenetic tree and a color palette.
#' @export
#'
#' @examples
#' \donttest{
#' accession_ranges <- list(
#'   SRU1 = "AJ240966 to AJ240970",
#'   STU2 = "AB015240 to AB015245",
#'   WPU13 = "L11934 to L11939",
#'   INU20 = c("AF277467 to AF277470", "AF333080 to AF333085")
#' )
#'
#' # Use the function to expand accession ranges
#' sam_acc <- expand_accession_ranges(accession_ranges)
#' print(sam_acc)
#'
#' # 2 get_sequence_information
#' accessions_to_query <- sam_acc$accession
#' seq_info <- get_sequence_information(accessions_to_query, remove_dot_1 = TRUE)
#' print(seq_info)
#' result <- preprocess_for_alignment(sam_acc, seq_info)
#'
#' # Access the resulting data frames
#' merged_data <- result$merged_data
#' main_data <- result$main_data
#' final_data <- result$final_data
#'
#' # If you want to sample 10% from each SampleID group:
#' sampled_data <- data_sampling(final_data, sample_proportion = 0.1)
#'
#' alignment_results <- alignment_info(final_data, type = "global", verbose = 1)
#'
#' # Access the resulting data frames
#' score_matrix <- alignment_results$score_matrix
#' normalized_score_matrix <- alignment_results$normalized_score_matrix
#'
#' total_aligned_positions_matrix <- alignment_results$total_aligned_positions_matrix
#' number_of_matching_positions_matrix <- alignment_results$number_of_matching_positions_matrix
#'
#' percent_similarity_matrix <- alignment_results$percent_similarity_matrix
#'
#' alignment_results_list <- alignment_results$alignment_results_list
#'
#' alignment_info_matrix <- alignment_results$alignment_info_matrix
#'
#' output_directory <- tempdir()
#'
#' # Save the list of alignment results to an RDS file
#' saveRDS(alignment_results_list, file.path(output_directory, "alignment_results_list.rds"))
#'
#' # Save matrices to files
#' write.table(score_matrix, file.path(output_directory, "score_matrix.txt"), sep = "\t")
#' average_percent_similarity <- compute_average_similarity_matrix(percent_similarity_matrix)
#' print(average_percent_similarity)
#'
#' output_directory <- tempdir()
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' clustering_result <- clustering_percent_similarity(percent_similarity_matrix)
#'
#' # Extract the dendrogram and clustered data
#' dend_colored <- clustering_result$dendrogram
#' clustered_data <- clustering_result$clustered_data
#' Cluster_SampleID_Percentage <- clustering_result$Cluster_SampleID_Percentage
#' Cluster_TotalPercentage <- clustering_result$Cluster_TotalPercentage
#'
#' tiff_file <- file.path(output_directory, "6. hierarchical_clustering_dendrogram_colored.tiff")
#'
#' # Save the dendrogram as a TIFF image
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#' plot(dend_colored, main = "Colored Hierarchical Clustering Dendrogram")
#' dev.off()
#'
#' # Save the clustered data frame to a CSV file
#' write.csv(clustered_data, file.path(output_directory, "7. clustered_data.csv"), row.names = FALSE)
#'
#' # Example usage with percent_similarity_matrix
#' result <- tree_percent_similarity(percent_similarity_matrix)
#' tree <- result$tree
#' color_palette <- result$color_palette
#'
#' tree_newick <- file.path(output_directory, "phylogenetic_tree_nj.nwk")
#'
#' # Save the phylogenetic tree as a Newick file
#' # ape::write.tree(tree, file = tree_newick)
#'
#' # Save the phylogenetic tree as a TIFF image
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' # Set the file name for the TIFF image
#' tiff_file <- file.path(output_directory, "phylogenetic_tree.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Plot the phylogenetic tree vertically
#' plot(tree, main = "Neighbor-Joining Tree", cex = 1, direction = "downward")
#'
#' # Close the TIFF device
#' dev.off()
#'
#' # Set the file name for the TIFF image with rainbow-colored branches
#' tiff_file_rainbow <- file.path(output_directory, "phylogenetic_tree_rainbow.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file_rainbow, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Plot the phylogenetic tree vertically with rainbow-colored branches
#' plot(tree, main = "NJ Tree", cex = 1, direction = "downward", tip.color = color_palette)
#'
#' # Close the TIFF device
#' dev.off()
#' }
#' @rdname O.tree_percent_similarity
#' @order 15
tree_percent_similarity <- function(percent_similarity_matrix) {
  # Create a copy of the original matrix
  complete_percent_similarity <- as.matrix(percent_similarity_matrix)

  # Assuming percent_similarity_matrix is your original matrix
  complete_percent_similarity[upper.tri(complete_percent_similarity)] <-
    t(complete_percent_similarity)[upper.tri(complete_percent_similarity)]

  # Set all diagonal elements to 1
  diag(complete_percent_similarity) <- 1

  # Create neighbor-joining tree
  tree <- nj(dist(as.matrix(1 - complete_percent_similarity)))

  # Generate a color palette based on the number of tips in the tree
  num_tips <- length(tree$tip.label)
  color_palette <- rainbow(num_tips)

  # Return the tree and color palette
  return(list(tree = tree, color_palette = color_palette))
}
