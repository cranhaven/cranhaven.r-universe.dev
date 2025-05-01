#' @import slam
rE_dtm <-
function(x, todtm = FALSE, re_control = list(wordLengths = c(1, 25))) {
    x <- NLP::content(x)
	controlname <- names(re_control)	
    docnum <- length(x)
    lap <- lapply(x, EAch_tAblE)
    rm(x)
	lapply_name <- lapply(lap, names)
    lapply_unique <- sort(unique(unlist(lapply_name))) # no NA after sort
	no_size0 <- 1
	if ("" %in% lapply_unique){
		lapply_unique <- lapply_unique[-1]
		no_size0 <- 0
	}
	if ("dictionary" %in% controlname)
        lapply_unique <- intersect(lapply_unique, re_control$dictionary)
	least_one <- ifelse(length(lapply_unique) > 0, 1, 0)
	if (least_one == 1 & !"dictionary" %in% controlname & "wordLengths" %in% controlname){	
		nchar_unique <- nchar(lapply_unique)
		lapply_unique <- lapply_unique[nchar_unique >= re_control$wordLengths[1] & nchar_unique <= re_control$wordLengths[2]]
		rm(nchar_unique)
		least_one <- ifelse(length(lapply_unique) > 0, 1, 0)
	}
	# for have
	if (least_one == 1 & "have" %in% controlname){
		table_have <- tAblE(unlist(lapply_name))
		table_have <- table_have[table_have >= re_control$have[1] & table_have <= re_control$have[2]]
		lapply_unique <- intersect(lapply_unique, names(table_have))
		rm(table_have)
		least_one <- ifelse(length(lapply_unique) > 0, 1, 0)
	}
	# ensure all in the unique
	if (least_one == 1 & (no_size0 == 0 | any(c("dictionary", "have", "wordLengths") %in% controlname))){
		lapply_name <- lapply(lapply_name, intersect, lapply_unique)
	}
	# start
	if (least_one == 1){
		lap_match <- lapply(lapply_name, match, table = lapply_unique)
		triple_i <- unlist(lap_match)
		triple_j <- rep.int(1: docnum, lengths(lap_match))
		rm(lap_match)
		triple_v <- unlist(mapply("[", lap, lapply_name, SIMPLIFY = FALSE))
		## maybe matrix
		## names to be NULL for package topicmodels
		names(triple_v) <- NULL
		m <- slam::simple_triplet_matrix(triple_i, triple_j, triple_v, nrow = length(lapply_unique), ncol = docnum, dimnames = list(Terms = lapply_unique, Docs = NULL))
		rm(triple_i, triple_j, triple_v, lapply_name)
	}
	if (least_one == 1 & "bounds" %in% controlname){
		sum_bounds <- slam::row_sums(m)
		index_bounds <- which(sum_bounds >= re_control$bounds[1] & sum_bounds <= re_control$bounds[2])
		least_one <- ifelse(length(index_bounds)>0, 1, 0)
		if (least_one == 1) m <- m[index_bounds, ]
		rm(sum_bounds, index_bounds)
	}
	if ("weighting" %in% controlname) {
        FUN_weighting <- match.fun(re_control$weighting)
	} else {
		FUN_weighting <- tm::weightTf
	}
	if (least_one == 1){
		tddtm <- tm::as.TermDocumentMatrix(m, weighting = FUN_weighting)
		if (todtm) tddtm <- t(tddtm)
	} else {
		m <- simple_triplet_zero_matrix(nrow = 1, ncol = docnum)
		rownames(m) <- "NA"
		message("There is in fact no word. Return a one column/row dtm/tdm with the name NA !")
		tddtm <- tm::as.TermDocumentMatrix(m, weighting = tm::weightTf)
		if (todtm) tddtm <- t(tddtm)
	}
	return(tddtm)
}

EAch_tAblE <-
function(x) {
	if (x == "") x <- " "
    x <- unlist(strsplit(x, "\\s+"))
    tx <- tAblE(x)
    asnu <- as.numeric(tx)
    names(asnu) <- names(tx)
	asnu
}

tAblE <-
function(x) {
	# This is from package tm
    u <- sort(unique(x))
    v <- tabulate(match(x, u))
    names(v) <- u
    v
}

rE_dtm_new <-
function(x, todtm = FALSE, re_control = list(wordLengths = c(1, 25)), MYSTOP=NULL) {
    x <- NLP::content(x)
	controlname <- names(re_control)	
    docnum <- length(x)
    lap <- lapply(x, EAch_tAblE)
    rm(x)
	lapply_name <- lapply(lap, names)
    lapply_unique <- sort(unique(unlist(lapply_name))) # no NA after sort
	no_size0 <- 1
	if ("" %in% lapply_unique){
		lapply_unique <- lapply_unique[-1]
		no_size0 <- 0
	}
	if (! is.null(MYSTOP)){
		if (MYSTOP[1] %in% c("jiebar", "auto", "jiebaR")) MYSTOP <- c(find_jiebar_stop())
		is_in_stop <- which(lapply_unique %in% MYSTOP)
		if (length(is_in_stop) > 0) lapply_unique=lapply_unique[-is_in_stop]
	}
	if ("dictionary" %in% controlname)
        lapply_unique <- intersect(lapply_unique, re_control$dictionary)
	least_one <- ifelse(length(lapply_unique) > 0, 1, 0)
	if (least_one == 1 & !"dictionary" %in% controlname & "wordLengths" %in% controlname){	
		nchar_unique <- nchar(lapply_unique)
		lapply_unique <- lapply_unique[nchar_unique >= re_control$wordLengths[1] & nchar_unique <= re_control$wordLengths[2]]
		rm(nchar_unique)
		least_one <- ifelse(length(lapply_unique) > 0, 1, 0)
	}
	# for have
	if (least_one == 1 & "have" %in% controlname){
		table_have <- tAblE(unlist(lapply_name))
		table_have <- table_have[table_have >= re_control$have[1] & table_have <= re_control$have[2]]
		lapply_unique <- intersect(lapply_unique, names(table_have))
		rm(table_have)
		least_one <- ifelse(length(lapply_unique) > 0, 1, 0)
	}
	# ensure all in the unique
	if (least_one == 1 & (no_size0 == 0 | any(c("dictionary", "have", "wordLengths") %in% controlname))){
		lapply_name <- lapply(lapply_name, intersect, lapply_unique)
	}
	# start
	if (least_one == 1){
		lap_match <- lapply(lapply_name, match, table = lapply_unique)
		triple_i <- unlist(lap_match)
		triple_j <- rep.int(1: docnum, lengths(lap_match))
		rm(lap_match)
		triple_v <- unlist(mapply("[", lap, lapply_name, SIMPLIFY = FALSE))
		## maybe matrix
		## names to be NULL for package topicmodels
		names(triple_v) <- NULL
		m <- slam::simple_triplet_matrix(triple_i, triple_j, triple_v, nrow = length(lapply_unique), ncol = docnum, dimnames = list(Terms = lapply_unique, Docs = NULL))
		rm(triple_i, triple_j, triple_v, lapply_name)
	}
	if (least_one == 1 & "bounds" %in% controlname){
		sum_bounds <- slam::row_sums(m)
		index_bounds <- which(sum_bounds >= re_control$bounds[1] & sum_bounds <= re_control$bounds[2])
		least_one <- ifelse(length(index_bounds)>0, 1, 0)
		if (least_one == 1) m <- m[index_bounds, ]
		rm(sum_bounds, index_bounds)
	}
	if ("weighting" %in% controlname) {
        FUN_weighting <- match.fun(re_control$weighting)
	} else {
		FUN_weighting <- tm::weightTf
	}
	if (least_one == 1){
		tddtm <- tm::as.TermDocumentMatrix(m, weighting = FUN_weighting)
		if (todtm) tddtm <- t(tddtm)
	} else {
		m <- simple_triplet_zero_matrix(nrow = 1, ncol = docnum)
		rownames(m) <- "NA"
		message("There is in fact no word. Return a one column/row dtm/tdm with the name NA !")
		tddtm <- tm::as.TermDocumentMatrix(m, weighting = tm::weightTf)
		if (todtm) tddtm <- t(tddtm)
	}
	return(tddtm)
}