# This is the one of the main blocking methods in Steorts, Ventura, Sadinle, Fienberg (2014), Privacy in Statistical Databases.
#If you use this code, please cite Steorts, R., Ventura, S., Sadinle, M., and Fienberg, S. (2014). "Blocking Comparisons for Record Linkage." Privacy in Statistical Databases (Lecture Notes in Computer Science 8744), ed. J Domingo-Ferrer, Springer, 252-268, doi:10.1007/978-3-319-11257-220.

#klsh Copyright 2018 Rebecca C. Steorts (beka@stat.duke.edu)

#klsh is free software: you can redistribute it and/or modify it
#under the terms of the Creative Commons license, either version 3 of the license, or (at your option) any later version.

#klsh is distributed in the hope it will be useful, but without ANY WARRANTY; without
# even the implied warranty of merchantability or fitness for a particular purpose.
#Specifically, you may share the software in any medium or format and you may adapt the software.
#Credit must be given when either of these are given to indicate if and what changes were made.
#The software may not be used for noncommerical purposes.
#If you are interested in using the software for commercial purposes, please contact the author above.
###########################################################################################################

#' Function to token a string into its k components
#'
#' @param string A string or record
#' @param k A parameter k, which is the number of shingle, tokens, or grams to break the string into
#' @return Computes the tokenized or grammed version of a string
#' @export
#' @examples
#' tokenify("Alexander",2)
#' tokenify("Alexander Smith", 2)

tokenify <- function(string, k) {
	k_substring <- function(start) {
		substring(string,start,start+k-1)
	}
	tokens <- sapply(X=seq(1:(nchar(string)-k+1)), k_substring)
	return(tokens)
}

#' Function to convert a record into a bag of tokens with a fieldwise flag
#'
#' @import blink
#' @param record String or record
#' @param k Parameter k, which is the number of shingle, tokens, or grams to break the string into
#' @param fieldwise Flag where the defalt setting to include the record as the entire string
#' @return Computes the bag of tokens for a string
#' @export
#' @examples
#' data(RLdata500)
#' data.500 <- RLdata500[-c(2,4)]
#' bag_of_word_ify(data.500[1,c(-2)],k=2)
#' bag_of_word_ify(data.500[300,c(-2)],k=2)
#' names(bag_of_word_ify(data.500[300,c(-2)],k=2))

bag_of_word_ify <- function(record, k, fieldwise=FALSE){
	# factors only convert to characters properly elementwise,
	# not by applying as.character() to a whole row of a data
	# frame
	char_record <- lapply(record,as.character)
	if(!fieldwise) {
		# Collapse the whole record into one space-separated string
		string <- paste(char_record, collapse=" ")
		tokens <- tokenify(string,k=k)
	} else {
		tokens <- do.call(c,lapply(char_record, tokenify, k=k))
	}
	bag_of_tokens <- table(tokens)
	return(bag_of_tokens)
}	

#' Function to convert all records into a bag of tokens
#'
#' @import stats
#' @import utils
#' @import blink
#' @param r.set Record set
#' @param k Parameter k, which is the number of shingle, tokens, or grams to break the string into
#' @param fieldwise Flag where the defalt setting to include the record as the entire string
#' @return Computes the bag of tokens for a record set
#' @export
#' @examples
#' data(RLdata500)
#' data.500 <- RLdata500[-c(2,4)]
#' sacks_of_bags_of_words(data.500[1:3,c(-2)],k=2)

sacks_of_bags_of_words <- function(r.set, k, fieldwise=FALSE) {
	sacks <- alply(.data=r.set, .margins=1, .fun= bag_of_word_ify, .expand=FALSE,k=k,fieldwise=fieldwise)
	return(sacks)
}

#' Function to calculate the inverse document frequency given a shingled bag of words
#'
#' @import blink
#' @param sack_of_bags Sack of bag of words
#' @return Computes the inverse document frequency for a bag of words
#' @export
#' @examples
#' data(RLdata500)
#' data.500 <- RLdata500[-c(2,4)]
#' sack <- sacks_of_bags_of_words(data.500[1:3,c(-2)],k=2)
#' (idf <- calc_idf(sack))
#' match(names(sack[[1]]), names(idf))

calc_idf <- function(sack_of_bags) {
	n.bags <- length(sack_of_bags)
	# Take the union of all bags' words, WITHOUT removing duplicates
	# then run table() on it
	# Count for each word = # of bags containing that word
	# The names of that table are the universal set of all words
	   # names() run on a single bag gives the words it contains
	   # lapply(sack_of_bags, names) gives a list with all words from all bags
	   # do.call(c,lapply(sack_of_bags, names)) takes their union
	   # table() to get counts
	universal_table <- table(do.call(c,lapply(sack_of_bags, names)))
	# universal_table contains prevalence of each word
	# Take log and subtract from log number of documents
	return(log(n.bags) - log(universal_table))
}

#' Function that generates unit random vectors and 
#' takes (weighted) projections onto the random unit vectors given a bag of words
#'

#' @import blink
#' @import stats
#' @import utils
#' @param sack_of_bags Sack of bag of words
#' @param weighting_table Weighting table (inverse document frequency)
#' @return Computes the inverse document frequency for a bag of words
#' @export
#' @examples
#' data(RLdata500)
#' data.500 <- RLdata500[-c(2,4)]
#' sack <- sacks_of_bags_of_words(data.500[1:3,c(-2)],k=2)
#' idf <- calc_idf(sack)
#' match(names(sack[[1]]), names(idf))
#' rproject_bags(sack, idf)

rproject_bags <- function(sack_of_bags, weighting_table) {
	n.bags <- length(sack_of_bags)
	n.words <- length(weighting_table)
	word_names <- names(weighting_table)
	
	# Generate a standard random Gaussian in the vector space of words
	rgaussian <- rnorm(n=n.words)
	# Normalize it to get a unit-length vector
	rdirection <- rgaussian/sqrt(sum(rgaussian^2))
	
	# Take weighted inner product for one bag of words
	weighted_projection <- function(a_bag) {
		relevant_comp <- match(names(a_bag), word_names)
		sum(rdirection[relevant_comp]*(a_bag*weighting_table[relevant_comp]))
	}
	all_projections <- laply(.data=sack_of_bags, .fun=weighted_projection)
	return(all_projections)
}

#' Function that reduces a bag of words into a signature matrix using multiple
#' random projections
#'
#' @import blink
#' @import plyr
#' @param sack_of_bags Sack of bag of words
#' @param p Number of random projections p
#' @param weighting_table Weighting table (inverse document frequency)
#' @return Computes a signature matrix using multiple random projections and
#' the inverse document frequency weights
#' @export
#' @examples
#' data(RLdata500)
#' data.500 <- RLdata500[-c(2,4)]
#' sack <- sacks_of_bags_of_words(data.500[1:3,c(-2)],k=2)
#' idf <- calc_idf(sack)
#' bag_signatures(sack, p=5, idf)

bag_signatures <- function(sack_of_bags, p, weighting_table) {
	signatures <- raply(.n=p, .expr=rproject_bags(sack_of_bags,weighting_table))
	# raply returns 1 row per replication so we want to transpose
	return(t(signatures))
}

#' Function that reduces a bag of words into a signature matrix using multiple
#' random projections
#'
#' @import blink
#' @param r.set Set of records
#' @param p Number of random projections p
#' @param num.blocks The total number of desired blocks
#' @param k The total number of tokens
#' @param fieldwise Flag with default FALSE
#' @param quiet Flag to turn on printed progress, default to TRUE
#' @return The blocks from performing KLSH 
#' @export
#' @examples
#' data(RLdata500)
#' data.500 <- RLdata500[-c(2,4)]
#' klsh.blocks <- klsh(data.500, p=20, num.blocks=5, k=2)

klsh <- function(r.set, p, num.blocks, k, fieldwise = FALSE, quiet = TRUE) {
	if(!quiet) print("Filling the sack of bags of tokens")
	sack_filling <- system.time(sack <- sacks_of_bags_of_words(r.set, k=k, fieldwise=fieldwise), gcFirst=FALSE)
	if(!quiet) print(sack_filling)
	if(!quiet) print("Getting IDF weights")
	idf_weighting <- system.time(idf <- calc_idf(sack), gcFirst=FALSE)
	if(!quiet) print(idf_weighting)
	if(!quiet) print("Finding signatures by random projection")
	projecting <- system.time(signatures <- bag_signatures(sack, p=p, idf), gcFirst=FALSE)
	if(!quiet) print(projecting)
	if(!quiet) print("Clustering by k-means")
	clustering <- system.time(clusters <- kmeans(signatures, centers=num.blocks, iter.max=100)$cluster,gcFirst=FALSE)
	if(!quiet) print(clustering)
	if(!quiet) print("Entering records into blocks")
	records_per_block <- function(b) { which(clusters == b)}
	blocking <- system.time(blocks <- lapply(1:num.blocks,records_per_block),gcFirst=FALSE)
	if(!quiet) print(blocking)
	return(blocks)
}