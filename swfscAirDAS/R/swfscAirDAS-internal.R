# Internal, helper functions for swfscAirDAS


###############################################################################
# Copied from swfscDAS
.print_file_line <- function(file.das, line.num, print.which) {
  ###Inputs
  # file.das: filename; either length one or the same length as line.num
  # line.num: line numbers
  # print.which: numbers which to print file + line number message
  line.num.len <- length(line.num)
  if (length(file.das) == 1) file.das <- rep(file.das, line.num.len)
  stopifnot(
    length(file.das) == line.num.len,
    length(print.which) >= 1,
    all(between(print.which, 1, line.num.len))
  )
  
  df.out <- data.frame(file.das, line.num)[print.which, ]
  
  message.out <- sapply(unique(df.out$file.das), function(i) {
    line.num.out <- df.out[df.out$file.das == i, "line.num"]
    paste(
      "File:", i, "|",
      ifelse(length(line.num.out) > 1, "Line numbers:", "Line number:"),
      paste(line.num.out, collapse = ", ")
    )
  })
  
  paste(message.out, collapse = "\n")
}


###############################################################################
# Check that conditions are valid for AirDAS data
.airdas_conditions_check <- function(x) {
  # x: character; condition name(s)
  # Output: x, or an error message
  
  conditions.acc <- c(
    "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
    "Haze", "Kelp", "RedTide", "AltFt", "SpKnot", 
    "ObsL", "ObsB", "ObsR", "Rec", "VLI", "VLO", "VB", "VRI", "VRO"
  )
  
  if (is.null(x)) {
    x <- c(
      "Bft", "CCover", "Jelly", "HorizSun", "VertSun", 
      "Haze", "Kelp", "RedTide", "AltFt", "SpKnot"
    )
    
  } else {
    if (!all(x %in% conditions.acc))
      stop("Please ensure that all 'conditions' are ",
           "one of the following accepted values:\n",
           paste(conditions.acc, collapse  = ", "))
    
    if (!("Bft" %in% x))  stop("The conditions argument must include 'Bft'")
  }
  
  x
}


###############################################################################
# Functions for doing accurate numeric comparisons with floating points
# Same as the swfscDAS internals
.less <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i < y) & !isTRUE(all.equal(i, y))}, as.logical(1))
}

.greater <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i > y) & !isTRUE(all.equal(i, y))}, as.logical(1))
}

.less_equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i < y) | isTRUE(all.equal(i, y))}, as.logical(1))
}

.greater_equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) {(i > y) | isTRUE(all.equal(i, y))}, as.logical(1))
}

.equal <- function(x, y) {
  stopifnot(length(y) == 1)
  vapply(x, function(i) isTRUE(all.equal(i, y)), as.logical(1))
}


###############################################################################
# Helper functions for airdas_check

# Remove multiple characters from the same string
.gsub_multi <- function(pattern, replacement, x) {
  for (i in pattern) x <- gsub(i, replacement, x)
  x
}


# Check that specified values can be convereted to a numeric
.check_numeric <- function(z, event.code, z.col) {
  # z: airdas_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that cannot be converted to a numeric
  
  stopifnot(
    inherits(z, "airdas_df"),
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      z1.na <- is.na(z.vec)
      z2.na <- is.na(suppressWarnings(as.numeric(z.vec)))
      stopifnot(all(which(z1.na) %in% which(z2.na)))
      
      z.out <- c(z.out, z.curr$idx[z2.na != z1.na])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are part of a set of accepted values
.check_character <- function(z, event.code, z.col, vals.accepted, na.eff) {
  # z: airdas_df object
  # event.code: character; event code(s) by which to filter z
  # z.col: Column(s) which to check
  # vals.accepted: character; accepted (expected) values. Should not include NA
  # na.eff: if 1, NAs have no special consideration; 
  #   if 2, NAs are ok when off effort; 
  #   if 3, NAs are always ok (i.e. NA is added to vals.accepted)
  ### Output: indices of z where z.col is not one of vals.accepted
  
  stopifnot(
    inherits(z, "airdas_df"),
    z.col %in% c(paste0("Data", 1:7)),
    "idx" %in% names(z), 
    na.eff %in% c(1, 2, 3)
  )
  
  z.out <- c()
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      z.oneff <- z.curr[["OnEffort"]]
      
      idx.logical <- if (na.eff == 1) {
        !(z.vec %in% vals.accepted)
      } else if (na.eff == 2) {
        !((z.vec %in% vals.accepted) | (is.na(z.vec) & !z.oneff))
      } else if (na.eff == 3) {
        !(z.vec %in% c(vals.accepted, NA))
      } else {
        stop("Invalid value for na.eff, please report this as an issue")
      }
      
      z.out <- c(z.out, z.curr$idx[idx.logical])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are a certain length
.check_character_length <- function(z, event.code, z.col, len.accepted, na.eff) {
  # z: airdas_df object
  # event.code: character; event code(s) by which to filter z
  # z.col: Column(s) which to check
  # len.accepted: numeric; number of characters allowed
  # na.eff: if 1, NAs are not ok (NA %in% 2 -> FALSE; 
  #   if 2, NAs are ok when off effort; 
  #   if 3, NAs are always ok (i.e. NA is added to len.accepted)
  ### Output: indices of z where z.col is not of length len.accepted
  
  stopifnot(
    inherits(z, "airdas_df"),
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z), 
    na.eff %in% c(1, 2, 3)
  )
  
  z.out <- c()
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      z.oneff <- z.curr[["OnEffort"]]
      
      idx.logical <- if (na.eff == 1) {
        !(nchar(z.vec) %in% len.accepted) #NA %in% 2 -> FALSE
      } else if (na.eff == 2) {
        !((nchar(z.vec) %in% len.accepted) | (is.na(z.vec) & !z.oneff))
      } else if (na.eff == 3) {
        !(nchar(z.vec) %in% c(len.accepted, NA))
      } else {
        stop("Invalid value for na.eff, please report this as an issue")
      }
      
      z.out <- c(z.out, z.curr$idx[idx.logical])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are NA
.check_isna <- function(z, event.code, z.col) {
  # z: airdas_dfr or airdas_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that is NA
  
  stopifnot(
    inherits(z, "airdas_df") | inherits(z, "airdas_dfr"), 
    z.col %in% paste0("Data", 1:7) | (identical(event.code, "1") & z.col %in% c("DateTime", "Lat", "Lon")),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      z.out <- c(z.out, z.curr$idx[!is.na(z.vec)])
    }
  }
  
  sort(unique(z.out))
}


# Check that specified values are not NA
.check_nona <- function(z, event.code, z.col) {
  # z: airdas_dfr or airdas_df object
  # event.code: character; event code by which to filter z
  # z.col: Column which to check; must be one of the Data# columns
  ### Output: indices of z that is NA
  
  stopifnot(
    inherits(z, "airdas_df") | inherits(z, "airdas_dfr"), 
    z.col %in% paste0("Data", 1:7),
    "idx" %in% names(z)
  )
  
  z.out <- c()
  for (i in event.code) {
    for (j in z.col) {
      z.curr <- z[z$Event == i, ]
      z.vec <- z.curr[[j]]
      
      z.out <- c(z.out, z.curr$idx[is.na(z.vec)])
    }
  }
  
  sort(unique(z.out))
}


# Provide output in format expected by airdas_check()
.check_list <- function(z1, z2, z3, z4) {
  # z1: x
  # z2: x.lines
  # z3: idx.
  # z4: txt.
  ### Output: list formatted to be added to error.out
  
  stopifnot(inherits(z1, "airdas_dfr"))
  list(z1$file_das[z3], z1$line_num[z3], z3, z2[z3], rep(z4, length(z3)))
}

###############################################################################
