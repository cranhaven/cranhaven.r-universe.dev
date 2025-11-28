Biokey <- function(data, from="", to="", recalculate=TRUE, internal=FALSE, force=FALSE) {
 mat <- matrix(c(
  "bracket",  "backreferenced",
  "bracket",  "indented",
  "bracket",  "serial",
  "bracket",  "newick",
  ##
  "branched", "bracket",
  "branched", "indented",
  "branched", "serial",
  "branched", "newick",
  ##
  "indented", "bracket",
  "indented", "serial",
  "indented", "newick",
  ##
  "serial",   "bracket",
  "serial",   "indented",
  "serial",   "newick",
  ##
  "classif",  "newick",
  "classif",  "table",
  ##
  "newick",   "classif",
  ##
  "table",    "classif"
  ), ncol=2, byrow=TRUE, dimnames=list(NULL, c("from", "to")))
  available.pairs <- apply(mat, 1, paste, collapse=" => ")
if (!force & !paste(from, to, sep=" => ") %in% available.pairs) {
 stop("only these conversions are possible:\n ",
  paste0(available.pairs, "\n "),
  "use force=TRUE to overrun")
}
##
.depth <- function(vec) { # calculate depths, e.g., indents in indented keys
 depths <- numeric(length(vec))
 for(n in 2:length(depths)) {
  if(vec[n] != vec[n-1]) {
   m <- match(vec[n], vec[1:(n-1)])
   if(!is.na(m)) {
    depths[n] <- depths[m]
    } else {
    depths[n] <- depths[n-1] + 1
    }
   } else {
   depths[n] <- depths[n-1]
   }
  }
 depths
}
##
data <- as.data.frame(data, stringsAsFactors=FALSE) # in case 'key' is a matrix
##
if (from == "newick") {
 tmp <- data[1, 1] # "newick" string should be in 'data' first cell
 tmp <- gsub(":[0-9.]+", "", tmp) # remove branch lengths, if any
 oprs <- which(unlist(strsplit(tmp, NULL)) == "(") # positions of opening parentheses
 lbls <- oprs # labels
 for (i in seq_along(oprs)) {
  .x <- gsub("\\(([^()]|(?R))*\\)", "", substr(tmp, oprs[i], nchar(tmp)), perl=TRUE) # removes all fully match parentheses with content, leaves labels, commas and "tails"
  lbls[i] <- gsub("[(,);].*$", "", .x) # removes everything after the supposed label, works because labels separated at least by comma
 }
 lbls[lbls == ""] <- "NA" # if there are no node labels, use NA-like _strings_
 tmp <- gsub("\\)[^(,);]+", "\\)", tmp) # now remove node labels, if any
 tmp <- gsub("^\\((.*) *,([^(,);]+)\\);", "(\\2,\\1));", tmp) # if the root node name is the last, it goes to the first position
 tmp <- gsub("[ ;]", "", tmp) # remove spaces because we will use them later, also remove semicolon in the end
 tmp <- trimws(gsub("([(,)])", " \\1 ", tmp)) # add spaces around symbols to split and remove trailing/leading spaces
 tmp <- unlist(strsplit(tmp, " ")) # split by spaces and remove list "shell"
 tmp <- tmp[!tmp %in% c(",", ";")] # remove chunks with commas and semicolons
 maxrank <- max(table(tmp)) + 2 # biggest possible rank, because first opening bracket introduces two ranks (group and lower)
 rrs <- Recode(tmp, c("(", ")"), c(-1, 1)) # opening parenthesis lowers rank, closing makes it higher
 rrs <- suppressWarnings(as.numeric(rrs)) # because all text strings will issue warnings
 rrs[is.na(rrs)] <- 0 # ... so these past text strings will be zeroes now
 rrs <- maxrank + cumsum(rrs) # kind of relative ranks with maxrank base (without maxrank base, they might be negative)
 tmp <- tmp[tmp != ")"] # now remove closing parentheses
 ids <- ifelse(!(tmp == "("), 1, rrs) # if terminals, rank=1, if higher, rank is relative rank
 ids <- as.numeric(as.factor(as.numeric(ids))) # yes, funny but allows to recalculate ranks from lowers to highest
 tmp[tmp == "("] <- lbls # opening parentheses turn into labels
 tmp <- gsub("_", " ", tmp) # replace Newick's underscores with spaces
 key <- cbind(ids, NA, tmp, NA)
 key <- key[key[, 3] != "", ] # remove empty terminals
}
if (from == "bracket") {
 key <- cbind(data, suppressWarnings(as.numeric(data[, 3]))) # goto's into 4th column
 key[, 3] <- ifelse(is.na(key[, 4]), key[, 3], "") # 3rd column with terminals only
 for(n in 2:nrow(key)) {
  jump <- key[n-1, 4] # 4th column must contain goto's
  if(!is.na(jump)) {
   first <- key[1:(n-1), ]
   move2 <- (seq_len(nrow(key)))[key[, 1] == jump]
   second <- key[move2, ]
   third <- key[-c(1:(n-1), move2), ]
   key <- rbind(first, second, third)
   row.names(key) <- NULL
   }
  }
}
if (from == "branched") { # the most simple format, similar to internal 'key'
 key <- cbind(data, NA) # add fake 4th column
}
if (from == "indented") { # similar to 'branched' but have idents as first column
 key <- cbind(data[, -1], NA) # skip indents (they will be recalculated if needed)
}
if (from == "serial") { # similar to branched but ids are two ref colums (id + pair)
 idsf <- paste(data[, 1], data[, 2])
 idsr <- paste(data[, 2], data[, 1])
 newid <- idsf
 for (n in seq_along(idsf)) newid[n] <- newid[which(idsr %in% idsf[n])]
 newid <- as.numeric(factor(newid, levels=unique(newid)))
 key <- cbind(newid, data[, 3:4], NA) # discard old ref columns, add fake 4th column
}
if (from == "classif") {
 data <- data[, 1:2] # if there are more columns, ignore them
 data <- data[!duplicated(data[, 1:2]), ] # remove row duplicates (possible result of line above)
 if (to != "table") { # we need _relative_ ranks and higher groups separate from terminals
 data[, 1] <- as.numeric(as.factor(data[, 1])) # convert absolute numeric ranks to relative
 data[, 3] <- character(nrow(data))
 higher <- data[, 1] > min(data[, 1])
 data[higher, 3] <- data[higher, 2] # keep higher names in 3rd column (temporary)
 data[higher, 2] <- "" # remove names of all higher groups
 dd <- diff(data[, 1]) # how many rows to add
 dd <- abs(sum(dd[dd < -1] + 1)) # each minus below -1 is one more row
 for(n in 1:(dd + nrow(data)-1)) { # insert fake rows to propagate all ranks
  dfs <- data[n, 1] - data[n+1, 1]
  if(dfs > 1) {
   before <- data[1:n, ]
   after <- data[-(1:n), ]
   insert <- data[n, ]
   insert[, 1] <- data[n, 1] - 1 # intermediate rank here
   insert[, 2] <- "" # no terminal here
   insert[, 3] <- "NA" # fake higher group
   data <- rbind(before, insert, after)
   row.names(data) <- NULL
   }
  }
 key <- cbind(data[, 1], data[, 3], data[, 2], NA) # add fake 4th column
} else { # for "table" (maybe also for keys?), we need to keep original ranks and names
 key <- cbind(data[, 1], NA, data[, 2], NA) # add fake 2nd (classifs do not have descriptions) and 4th columns
}
}
if (from == "table") {
data <- sapply(data, Ditto)
data <- data[, rev(colnames(data))]
ranks <- data
ranks[] <- ""
ranks[1, ] <- colnames(ranks)
ranks <- apply(ranks, 2, Fill)
ranks.data <- paste(t(ranks), t(data))
ranks.data <- gsub("[0-9.]+ *$", "", ranks.data)
ranks.data <- ranks.data[ranks.data != ""]
ranks.data <- gsub("([0-9.]+) ([^0-9]+)$", "\\1\t\\2", ranks.data)
ranks.data <- do.call(rbind, strsplit(ranks.data, "\t"))
key <- data.frame(ranks.data[, 1], NA, ranks.data[, 2], NA, stringsAsFactors=FALSE)
}
## at this point, all "from" conversions should finish their output and return internal 'key'
## 'key' is the universal internal type: linear, branched, with four columns:
## (1) ids or ranks (numbers),
## (2) descriptions or higher categories (text),
## (3) terminals (text),
## (4) goto's (if any, otherwise NAs) -- needed only to _output_ bracket (e.g., with recalculated numbers or with backreferences)
colnames(key) <- c("id", "description", "terminal", "goto")
key[is.na(key[, 3]), 3] <- "" # sometimes, terminals contain NAs instead of empty strings (depends on input format)
##
## recalculation:
if (from == "classif" | from == "newick" | from == "table") recalculate <- FALSE # these types do not need recalculation
if (recalculate) {
 newids <- as.numeric(factor(key[, 1], levels=unique(key[, 1]))) # recalculate (1) ids in order of appearance
 newgotos <- Recode(key[, 4], key[, 1], newids) # and (2) goto's
 key[, 1] <- newids
 key[, 4] <- newgotos
}
## "to" part:
##
if (to == "classif") {
 res <- key[, c(1, 3)] # classifs should have ranks a.k.a. ids and terminals
 colnames(res) <- c("RANK", "NAME") # not absolutely necessary but adds understanding
}
if (to == "indented") {
 indents <- .depth(key[, 1])
 res <- cbind(indents, key[, 1:3]) # indents as numbers
}
if (to == "serial") {
 refs <- numeric(nrow(key))
 for(n in seq_along(refs)) {
  w <-  which(key[, 1] %in% key[n, 1])
  refs[n] <- w[w != n]
 }
 refs <- cbind(seq_along(refs), refs) # refs as two columns (id and pair); to make typographic, add parentheses around 'refs'
 res <- cbind(refs, key[, 2:3]) # add two new id columns and skip 1st column with old ids
}
if (to == "bracket") {
 for (n in seq_len(nrow(key))) {
 if(key[n, 3] == "") key[n, 3] <- key[n+1, 1] # goto's taken from next step and placed into 3rd column
 }
 res <- key[order(key[, 1]), 1:3] # now theses and anti-theses are together
}
if (to == "backreferenced") {
 back <- numeric(nrow(key))
 for (n in seq_len(nrow(key))) {
  if(key[n, 3] == "") {
   key[n, 3] <- key[n+1, 1] # goto taken from next step id and placed into 4th column
   back[n+1] <- key[n, 1] # backreference for next step taken from previous step id
  }
 }
 back[back == 0] <- "" # replace skipped back's with empty strings
 key <- cbind(key[, 1], back, key[, 2:3]) # backrefrences as separate character column, to make typographic, add parentheses around 'back'
 res <- key[order(key[, 1]), ] # now theses and anti-theses are together
}
if (to == "newick") {
 indents <- .depth(key[, 1])
 dfs <- indents - c(indents[2:(length(indents))], 0)
 brt <- Recode(sign(dfs), c(-1, 0, 1), c("(", ",", ")"))
 mul <- abs(dfs) + (dfs == 0)
 for (i in seq_along(brt)) brt[i] <- paste(rep(brt[i], mul[i]), collapse="")
 tmp <- cbind(key[, 3], brt)
 tmp <- paste0(paste0(t(tmp), colalpse=""), collapse="")
 tmp <- gsub(")(", "),(", tmp, fixed=TRUE)
 tmp <- gsub("([A-z0-9])\\(", "\\1,\\(", tmp) # 'name(' -> 'name,('
 tmp <- gsub("\\)([A-z0-9])", "\\),\\1", tmp) # ')name' -> '),name'
 tmp <- gsub("(,", "(", tmp, fixed=TRUE) # remove left empty terminals
 tmp <- gsub(",)", ")", tmp, fixed=TRUE) # remove right empty terminals
 tmp <- gsub(",*\\(\\)", "", tmp) # some unknown empties
 tmp <- gsub(",,+", ",", tmp) # remove empty terminals, again
 tmp <- gsub(" ", "_", tmp, fixed=TRUE) # Newick dislikes spaces, at least in terminals
 nn <- paste0("(", tmp, ");") # add root node and Newick's "end of tree"
 if (from == "classif") {
  lbls <- key[grep("\\(", brt), 2] # labels for opening parentheses (including double and more)
  lbls <- c("", lbls) # add empty root node label
  lbls[lbls == "NA"] <- "" # make "NA" string labels (fake subgroups) empty
  .PPadd <- function(txt, labels){ # adds labels to closing matches of the each opening parenthesis
  txts <- unlist(strsplit(txt, NULL))
  cpp <- opp <- which(txts == "(")
  txtn <- Recode4(txts, c("(", ")"), c(-1, 1), 0)
  txtl <- length(txtn)
  for (i in seq_along(opp)) {
   pos <- (opp[i] + which(cumsum(txtn[opp[i]:txtl]) == 0)[1]) - 1 # we need the first match
   txts[pos] <- paste0(txts[pos], labels[i])
   }
  paste0(txts, collapse="")
  }
 res <- .PPadd(nn, lbls)
 } else {
 res <- nn
 }
}
if (to == "table") {
 allranks <- sort(unique(key[, 1]))
 for (i in seq_along(allranks)) {
  tmp <- key[, 3]
  tmp[(as.numeric(key[, 1]) < as.numeric(allranks[i]))] <- "" # empty values of lower ranks
  tmp <- Fill(tmp) # fill them from values above
  assign(allranks[i], tmp) # make as many variables as there are ranks
 }
 res <- do.call(cbind, mget(allranks))
 res <- res[as.numeric(key[, 1]) == min(as.numeric(allranks)), ]
}
if (internal) res <- key # allows to output internal 4-column object
res
}

Numranks <- function(nums=NULL, ranks=NULL, add=NULL, empty="Species") {
 if(!is.null(nums) & !is.null(ranks)) stop("either 'nums' or 'ranks' (but not both) must be specified")
 mat <- matrix(c(
  0.5, "Varietes",
  0.8, "Subspecies",
  1.0, "Species",
  1.2, "Subsectio",
  1.5, "Sectio",
  1.8, "Subgenus",
  2.0, "Genus",
  2.2, "Subtribus",
  2.5, "Tribus",
  2.8, "Subfamilia",
  3.0, "Familia",
  3.2, "Superfamilia",
  3.5, "Infraordo",
  3.8, "Subordo",
  4.0, "Ordo",
  4.2, "Superordo",
  4.5, "Infraclassis",
  4.8, "Subclassis",
  5.0, "Classis",
  5.2, "Superclassis",
  5.8, "Subphylum",
  6.0, "Phylum",
  6.2, "Superphylum",
  6.5, "Infraregnum",
  6.8, "Subregnum",
  7.0, "Regnum",
  7.2, "Superregnum"
  ), ncol=2, byrow=TRUE, dimnames=list(NULL, c("num", "rank")))
 if(!is.null(add)) {
  mat <- rbind(mat, matrix(c(add, names(add)), ncol=2))
 }
 if (is.null(nums) & is.null(ranks)) res <- data.frame(mat)
 if(!is.null(nums) & is.null(ranks)) {
  res <- Recode(nums, mat[, 1], mat[, 2])
 }
 if(is.null(nums) & !is.null(ranks)) {
  ranks <- gsub("[^a-z]", "", tolower(ranks))
  ranks <- Recode(ranks,
   c("tribe", "subtribe",  "section", "subsection", "family",  "subfamily",  "superfamiliy", "order", "suborder", "superorder", "kingdom", "subkingdom"),
   c("tribus","subtribus", "sectio",  "subsectio",  "familia", "subfamilia", "superfamilia", "ordo",  "subordo",  "superordo",  "regnum",  "subregnum"))
  ranks[ranks == ""] <- empty
  .cap <- function(.x) { # from help(toupper)
   sapply(strsplit(.x, split=" "), function(.xx)
    paste0(toupper(substring(.xx, 1, 1)), substring(.xx, 2), collapse=" "))
  }
  res <- as.numeric(RecodeR(.cap(ranks), mat[, 2], mat[, 1]))
 }
res
}
