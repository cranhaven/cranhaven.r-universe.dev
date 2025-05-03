#'@import abind
#'@importFrom methods is
#'@importFrom ClimProjDiags Subset
.chunk <- function(chunk, n_chunks, selectors) {
  if (any(chunk > n_chunks)) {
    stop("Requested chunk index out of bounds.")
  }
  if (length(chunk) == 1 && length(n_chunks) == 1) {
    if (!is.null(attr(selectors, 'chunk'))) {
      attr(selectors, 'chunk') <- c((attr(selectors, 'chunk')['chunk'] - 1) * n_chunks + 
                                      chunk,
                                    attr(selectors, 'chunk')['n_chunks'] * n_chunks)
    } else {
      attr(selectors, 'chunk') <- c(chunk = unname(chunk), n_chunks = unname(n_chunks))
    }
  } else {
    # Chunking arrays of multidimensional selectors. 
    # This should be done in Start.R but implies modifications.
    if (length(chunk) != length(n_chunks)) {
      stop("Wrong chunk specification.")
    }
    #NOTE: 1. It should be for above? not nultidimensional selector
    #      2. it was !is.null before, but it should be is.null (?)
    #    if (!is.null(attr(selectors, 'values'))) {
    #      stop("Multidimensional chunking only available when selector ",
    #           "values provided.")
    #    }
    if (is.null(dim(selectors))) {
      stop("Multidimensional chunking only available when multidimensional ",
           "selector values provided.")
    }
    if (length(dim(selectors)) != length(chunk)) {
      stop("As many chunk indices and chunk lengths as dimensions in the ",
           "multidimensional selector array must be specified.")
    }
    old_indices <- attr(selectors, 'indices')
    old_values <- attr(selectors, 'values')
    selectors <- ClimProjDiags::Subset(selectors, names(chunk),
                                       lapply(names(chunk),
                               function(x) {
                                 n_indices <- dim(selectors)[x]
                                 chunk_sizes <- rep(floor(n_indices / n_chunks[x]), n_chunks[x])
                                 chunks_to_extend <- n_indices - chunk_sizes[1] * n_chunks[x]
                                 if (chunks_to_extend > 0) {
                                   chunk_sizes[1:chunks_to_extend] <- chunk_sizes[1:chunks_to_extend] + 1
                                 }
                                 chunk_size <- chunk_sizes[chunk[x]]
                                 offset <- 0
                                 if (chunk[x] > 1) {
                                   offset <- sum(chunk_sizes[1:(chunk[x] - 1)])
                                 }
                                 1:chunk_sizes[chunk[x]] + offset
                               }))
    attr(selectors, 'indices') <- old_indices
    attr(selectors, 'values') <- old_values
  }
  selectors
}

.ReplaceVariablesInString <- function(string, replace_values, allow_undefined_key_vars = FALSE) {
  # This function replaces all the occurrences of a variable in a string by 
  # their corresponding string stored in the replace_values.
  if (length(strsplit(string, "\\$")[[1]]) > 1) {
    parts <- strsplit(string, "\\$")[[1]]
    output <- ""
    i <- 0
    for (part in parts) {
      if (i %% 2 == 0) {
        output <- paste(output, part, sep = "")
      } else {
        if (part %in% names(replace_values)) {
          output <- paste(output, .ReplaceVariablesInString(replace_values[[part]], replace_values, allow_undefined_key_vars), sep = "")
        } else if (allow_undefined_key_vars) {
          output <- paste0(output, "$", part, "$")
        } else {
          stop(paste('Error: The variable $', part, '$ was not defined in the configuration file.', sep = ''))
        }
      }
      i <- i + 1
    }
    output
  } else {
    string
  }
}

.ReplaceGlobExpressions <- function(path_with_globs, actual_path, 
                                    replace_values, tags_to_keep, 
                                    dataset_name, permissive) {
  # The goal of this function is to replace the shell globbing expressions in
  # a path pattern (that may contain shell globbing expressions and Load() 
  # tags) by the corresponding part of the real existing path.
  # What is done actually is to replace all the values of the tags in the 
  # actual path by the corresponding $TAG$
  #
  # It takes mainly two inputs. The path with expressions and tags, e.g.:
  #   /data/experiments/*/$EXP_NAME$/$VAR_NAME$/$VAR_NAME$_*$START_DATE$*.nc
  # and a complete known path to one of the matching files, e.g.:
  #   /data/experiments/ecearth/i00k/tos/tos_fc0-1_19901101_199011-199110.nc
  # and it returns the path pattern but without shell globbing expressions:
  #   /data/experiments/ecearth/$EXP_NAME$/$VAR_NAME$/$VAR_NAME$_fc0-1_$START_DATE$_199011-199110.nc
  #
  # To do that, it needs also as inputs the list of replace values (the 
  # association of each tag to their value).
  #
  # All the tags not present in the parameter tags_to_keep will be repalced.
  #
  # Not all cases can be resolved with the implemented algorithm. In an
  # unsolvable case a warning is given and one possible guess is returned.
  #
  # In some cases it is interesting to replace only the expressions in the
  # path to the file, but not the ones in the file name itself. To keep the
  # expressions in the file name, the parameter permissive can be set to 
  # TRUE. To replace all the expressions it can be set to FALSE.
  
  # Tests
  #a <- "/esarchive/exp/ecearth/a13c/3hourly/$var$_*/$var$_*-LR_historical_r1i1p1f1_gr_$chunk$.nc"
  #b <- "/esarchive/exp/ecearth/a13c/3hourly/psl_f6h/psl_E3hrPt_EC-Earth3-LR_historical_r1i1p1f1_gr_195001010000-195001312100.nc"
  #c <- list(dat = 'dat1', var = 'psl', chunk = '195001010000-195001312100')
  #d <- c('dat', 'var', 'chunk')
  #e <- 'dat1'
  #f <- FALSE #TRUE/0/1/2/3
  #r <- .ReplaceGlobExpressions(a, b, c, d, e, f)
  
  clean <- function(x) {
    if (nchar(x) > 0) {
      x <- gsub('\\\\', '', x)
      x <- gsub('\\^', '', x)
      x <- gsub('\\$', '', x)
      x <- unname(sapply(strsplit(x, '[',fixed = TRUE)[[1]], function(y) gsub('.*]', '.', y)))
      do.call(paste0, as.list(x))
    } else {
      x
    }
  }
  
  strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
  
  if (permissive == 0) {
    permissive <- FALSE
  } else {
    if (permissive == TRUE) {
      permissive_levels <- 1
    } else {
      permissive_levels <- round(permissive[1])
      permissive <- TRUE
    }
  }
  
  if (permissive) {
    actual_path_chunks <- strsplit(actual_path, '/')[[1]]
    if (permissive_levels >= length(actual_path_chunks)) {
      stop("Error: Provided levels out of scope in parameter 'permissive'.")
    }
    permissive_levels <- 1:permissive_levels
    permissive_levels <- length(actual_path_chunks) - (rev(permissive_levels) - 1)
    actual_path <- paste(actual_path_chunks[-permissive_levels], collapse = '/')
    file_name <- paste(actual_path_chunks[permissive_levels], collapse = '/')
    if (length(actual_path_chunks) > 1) {
      file_name <- paste0('/', file_name)
    }
    path_with_globs_chunks <- strsplit(path_with_globs, '/')[[1]]
    path_with_globs <- paste(path_with_globs_chunks[-permissive_levels], 
                             collapse = '/')
    path_with_globs_no_tags <- .ReplaceVariablesInString(path_with_globs, replace_values)
    file_name_with_globs <- paste(path_with_globs_chunks[permissive_levels], collapse = '/')
    if (length(path_with_globs_chunks) > 1) {
      file_name_with_globs <- paste0('/', file_name_with_globs)
    }
    right_known <- head(strsplit(file_name_with_globs, '*', fixed = TRUE)[[1]], 1)
    right_known_no_tags <- .ReplaceVariablesInString(right_known, replace_values)
    path_with_globs_no_tags_rx <- utils::glob2rx(paste0(path_with_globs_no_tags, right_known_no_tags))
    match <- regexpr(gsub('$', '', path_with_globs_no_tags_rx, fixed = TRUE), paste0(actual_path, file_name))
    if (match != 1) {
      stop("Incorrect parameters to replace glob expressions. The path with expressions does not match the actual path.")
    }
    #if (attr(match, 'match.length') - nchar(right_known_no_tags) < nchar(actual_path)) {
    #  path_with_globs_no_tags <- paste0(path_with_globs_no_tags, right_known_no_tags, '*')
    #  file_name_with_globs <- sub(right_known, '/*', file_name_with_globs)
    #}
  }
  path_with_globs_rx <- utils::glob2rx(path_with_globs)
  values_to_replace <- c()
  tags_to_replace_starts <- c()
  tags_to_replace_ends <- c()
  give_warning <- FALSE
  for (tag in tags_to_keep) {
    matches <- gregexpr(paste0('$', tag, '$'), path_with_globs_rx, fixed = TRUE)[[1]]
    lengths <- attr(matches, 'match.length')
    if (!(length(matches) == 1 && matches[1] == -1)) {
      for (i in 1:length(matches)) {
        left <- NULL
        if (matches[i] > 1) {
          left <- .ReplaceVariablesInString(substr(path_with_globs_rx, 1, matches[i] - 1), replace_values)
          left_known <- strReverse(head(strsplit(strReverse(left), strReverse('.*'), fixed = TRUE)[[1]], 1))
        }
        right <- NULL
        if ((matches[i] + lengths[i] - 1) < nchar(path_with_globs_rx)) {
          right <- .ReplaceVariablesInString(substr(path_with_globs_rx, matches[i] + lengths[i], nchar(path_with_globs_rx)), replace_values)
          right_known <- head(strsplit(right, '.*', fixed = TRUE)[[1]], 1)
        }
        final_match <- NULL
        match_limits <- NULL
        if (!is.null(left)) {
          left_match <- regexpr(paste0(left, replace_values[[tag]], right_known), actual_path)
          match_len <- attr(left_match, 'match.length')

          right_known_nchar <- nchar(clean(right_known))
          if (identical(right_known_nchar, integer(0))) right_known_nchar <- 0
          left_match_limits <- c(left_match + match_len - 1 - right_known_nchar - nchar(replace_values[[tag]]) + 1,
                                 left_match + match_len - 1 - right_known_nchar)

          if (!(left_match < 1)) {
            match_limits <- left_match_limits
          }
        }
        right_match <- NULL
        if (!is.null(right)) {
          right_match <- regexpr(paste0(left_known, replace_values[[tag]], right), actual_path)
          match_len <- attr(right_match, 'match.length')

          left_known_nchar <- nchar(clean(left_known))
          if (identical(left_known_nchar, integer(0))) left_known_nchar <- 0
          right_match_limits <- c(right_match + left_known_nchar,
                                  right_match + left_known_nchar + nchar(replace_values[[tag]]) - 1)
          if (is.null(match_limits) && !(right_match < 1)) {
            match_limits <- right_match_limits
          }
        }
        if (!is.null(right_match) && !is.null(left_match)) {
          if (!identical(right_match_limits, left_match_limits)) {
            give_warning <- TRUE
          }
        }
        if (is.null(match_limits)) {
          stop("Too complex path pattern specified for ", dataset_name,
               ". Specify a simpler path pattern for this dataset.")
        }
        values_to_replace <- c(values_to_replace, tag)
        tags_to_replace_starts <- c(tags_to_replace_starts, match_limits[1])
        tags_to_replace_ends <- c(tags_to_replace_ends, match_limits[2])
      }
    }
  }
  
  actual_path_with_tags <- actual_path
  if (length(tags_to_replace_starts) > 0) {
    reorder <- sort(tags_to_replace_starts, index.return = TRUE)
    tags_to_replace_starts <- reorder$x
    values_to_replace <- values_to_replace[reorder$ix]
    tags_to_replace_ends <- tags_to_replace_ends[reorder$ix]
    while (length(values_to_replace) > 0) {
      actual_path_with_tags <- paste0(substr(actual_path_with_tags, 1, head(tags_to_replace_starts, 1) - 1),
                                      '$', head(values_to_replace, 1), '$',
                                      substr(actual_path_with_tags, head(tags_to_replace_ends, 1) + 1, nchar(actual_path_with_tags)))
      extra_chars <- nchar(head(values_to_replace, 1)) + 2 - (head(tags_to_replace_ends, 1) - head(tags_to_replace_starts, 1) + 1)
      values_to_replace <- values_to_replace[-1]
      tags_to_replace_starts <- tags_to_replace_starts[-1]
      tags_to_replace_ends <- tags_to_replace_ends[-1]
      tags_to_replace_starts <- tags_to_replace_starts + extra_chars
      tags_to_replace_ends <- tags_to_replace_ends + extra_chars
    }
  }
  
  if (give_warning) {
    .warning(paste0("Too complex path pattern specified for ", dataset_name, 
                    ". Double check carefully the '$Files' fetched for this dataset or specify a simpler path pattern."))
  }
  
  if (permissive) {
    paste0(actual_path_with_tags, file_name_with_globs)
  } else {
    actual_path_with_tags
  }
}

.FindTagValue <- function(path_with_globs_and_tag, actual_path, tag) {
  
  addition_warning = FALSE
  
  if (!all(sapply(c(path_with_globs_and_tag, actual_path, tag), is.character))) {
    stop("All 'path_with_globs_and_tag', 'actual_path' and 'tag' must be character strings.")
  }
  
  if (grepl('$', tag, fixed = TRUE)) {
    stop("The provided 'tag' must not contain '$' symbols.")
  }
  full_tag <- paste0('$', tag, '$')
  
  if (!grepl(full_tag, path_with_globs_and_tag, fixed = TRUE)) {
    stop("The provided 'path_with_globs_and_tag' must contain the tag in 'tag' surrounded by '$' symbols.")
  }
  
  parts <- strsplit(path_with_globs_and_tag, full_tag, fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    parts <- c(parts, '')
  }
  parts[1] <- paste0('^', parts[1])
  parts[length(parts)] <- paste0(parts[length(parts)], '$')
  
  # Group the parts in 2 groups, in a way that both groups have a number
  # of characters as similar as possible.
  part_lengths <- sapply(parts, nchar)
  group_len_diffs <- sapply(1:(length(parts) - 1), 
                            function(x) {
                              sum(part_lengths[(x + 1):length(parts)]) - sum(part_lengths[1:x])
                            }
  )
  clp <- chosen_left_part <- which.min(group_len_diffs)[1]
  
  left_expr <- paste(parts[1:clp], collapse = full_tag)
  
  #because ? means sth, use . (any char) to substitute ?
  left_expr <- gsub('?', '.', left_expr, fixed = TRUE)
  test_left_expr <- left_expr
  
  # because * means zero or more char, use . to substitute *. 
  # And the * behind . means zero or more char. '?' for lazy evaluation.
  left_expr <- gsub('*', '.*?', left_expr, fixed = TRUE)
  left_expr <- gsub(full_tag, '.*?', left_expr, fixed = TRUE)
  
  # To test if the pattern matches only one... dont use lazy evaluation
  test_left_expr <- gsub('*', '.*', test_left_expr, fixed = TRUE)
  test_left_expr <- gsub(full_tag, '.*', test_left_expr, fixed = TRUE)
  
  # Find the match chars from left
  left_match <- regexec(left_expr, actual_path)[[1]]
  test_left_match <- regexec(test_left_expr, actual_path)[[1]]
  
  if (left_match < 0) {
    stop("Unexpected error in .FindTagValue.")
  }
  
  if (attr(test_left_match, "match.length") != attr(left_match, "match.length")) {
    addition_warning = TRUE
    warning("Detect more than one possibility derived from the global expression of path.")
  }
  
  #Cut down the left match part
  actual_path_sub <- substr(actual_path, 
                            attr(left_match, 'match.length') + 1, 
                            nchar(actual_path))
  
  #----------Search match chars from right
  right_expr <- paste(parts[(clp + 1):(length(parts))], collapse = full_tag)
  right_expr <- gsub('?', '.', right_expr, fixed = TRUE)
  
  test_right_expr <- right_expr
  # For lazy evaulation to work, pattern and string have to be reversed.
  right_expr <- gsub('*', '.*?', right_expr, fixed = TRUE)
  right_expr <- gsub(full_tag, '.*?', right_expr, fixed = TRUE)
  right_expr <- gsub('$', '^', right_expr, fixed = TRUE)
  
  # To test if the pattern matches only one... dont use lazy evaluation
  test_right_expr <- gsub('*', '.*', test_right_expr, fixed = TRUE)
  test_right_expr <- gsub(full_tag, '.*', test_right_expr, fixed = TRUE)
  test_right_expr <- gsub('$', '^', test_right_expr, fixed = TRUE)
  
  rev_str <- function(s) {
    paste(rev(strsplit(s, NULL)[[1]]), collapse = '')
  }
  
  right_expr <- rev_str(right_expr)
  test_right_expr <- rev_str(test_right_expr)
  
  right_expr <- gsub('?*.', '.*?', right_expr, fixed = TRUE)
  right_match <- regexec(right_expr, rev_str(actual_path))[[1]]
  
  test_right_expr <- gsub('*.', '.*', test_right_expr, fixed = TRUE)
  test_right_match <- regexec(test_right_expr, rev_str(actual_path_sub))[[1]]
  
  if (right_match < 0) {
    stop("Unexpected error in .FindTagValue.")
  }
  
  if (attr(test_right_match, "match.length") != attr(right_match, "match.length")) {
    addition_warning = TRUE
    warning(paste0("Detect more than one possibility derived from the global ",
                   "expression of path."))
  }
  
  #-------------get tag value
  right_match[] <- nchar(actual_path) - 
    (right_match[] + attr(right_match, 'match.length') - 1) + 1
  
  if (addition_warning) {
    warning(paste0("The extracted parameter ", full_tag, " is ",
                   substr(actual_path, left_match + attr(left_match, 'match.length'), 
                          right_match - 1),
                   ". Check if all the desired files were read in. ",
                   "If not, specify parameter '", tag, 
                   "' by values instead of indices, or set parameter ",
                   "'path_glob_permissive' as TRUE"))
  }
  
  if ((left_match + attr(left_match, 'match.length')) > 
      (right_match - 1)) {
    NULL
  } else {
    substr(actual_path, left_match + attr(left_match, 'match.length'),
           right_match - 1)
  }
}

.message <- function(...) {
  # Function to use the 'message' R function with our custom settings
  # Default: new line at end of message, indent to 0, exdent to 3, 
  #  collapse to \n*
  args <- list(...)
  
  ## In case we need to specify message arguments
  if (!is.null(args[["appendLF"]])) {
    appendLF <- args[["appendLF"]]
  } else {
    ## Default value in message function
    appendLF <- TRUE
  } 
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value in message function
    domain <- NULL
  }
  args[["appendLF"]] <- NULL
  args[["domain"]] <- NULL
  
  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL
  
  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n*"
  }
  args[["collapse"]] <- NULL
  
  ## Message tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "* "
  }
  args[["tag"]] <- NULL
  
  message(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
  ), collapse = collapse)), appendLF = appendLF, domain = domain)
}

.warning <- function(...) {
  # Function to use the 'warning' R function with our custom settings
  # Default: no call information, indent to 0, exdent to 3, 
  #  collapse to \n
  args <- list(...)
  
  ## In case we need to specify warning arguments
  if (!is.null(args[["call."]])) {
    call <- args[["call."]]
  } else {
    ## Default: don't show info about the call where the warning came up
    call <- FALSE
  }
  if (!is.null(args[["immediate."]])) {
    immediate <- args[["immediate."]]
  } else {
    ## Default value in warning function
    immediate <- FALSE
  }
  if (!is.null(args[["noBreaks."]])) {
    noBreaks <- args[["noBreaks."]]
  } else {
    ## Default value warning function
    noBreaks <- FALSE
  }
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value warning function
    domain <- NULL
  }
  args[["call."]] <- NULL
  args[["immediate."]] <- NULL
  args[["noBreaks."]] <- NULL
  args[["domain"]] <- NULL
  
  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL
  
  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n!"
  }
  args[["collapse"]] <- NULL
  
  ## Warning tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "! Warning: "
  }
  args[["tag"]] <- NULL
  
  warning(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
  ), collapse = collapse)),  call. = call, immediate. = immediate, 
  noBreaks. = noBreaks, domain = domain)
}

# Function to permute arrays of non-atomic elements (e.g. POSIXct)
# Function to permute arrays of non-atomic elements (e.g. POSIXct)
.aperm2 <- function(x, new_order) {
  old_dims <- dim(x)
  attr_bk <- attributes(x)
  if ('dim' %in% names(attr_bk)) {
    attr_bk[['dim']] <- NULL
  }
  if (is.numeric(x)) {
    x <- aperm(x, new_order)
  } else {
    y <- array(1:length(x), dim = dim(x))
    y <- aperm(y, new_order)
    x <- x[as.vector(y)]
  }
  dim(x) <- old_dims[new_order]
  attributes(x) <- c(attributes(x), attr_bk)
  x
}

# Function to bind arrays of non-atomic elements (e.g. POSIXct)
# 'x' and 'y' must have dimension names
# parameter 'along' must be a dimension name
.abind2 <- function(x, y, along) {
  x_along <- which(names(dim(x)) == along)
  if (x_along != length(dim(x))) {
    tmp_order_x <- c((1:length(dim(x)))[-x_along], x_along)
    x <- .aperm2(x, tmp_order_x)
  }
  y_along <- which(names(dim(y)) == along)
  if (y_along != length(dim(y))) {
    tmp_order_y <- c((1:length(dim(y)))[-y_along], y_along)
    y <- .aperm2(y, tmp_order_y)
  }
  r <- c(x, y)
  new_dims <- dim(x)
  new_dims[length(new_dims)] <- dim(x)[length(dim(x))] + dim(y)[length(dim(y))]
  dim(r) <- new_dims
  if (x_along != length(dim(x))) {
    final_order <- NULL
    if (x_along > 1) {
      final_order <- c(final_order, (1:length(dim(r)))[1:(x_along - 1)])
    }
    final_order <- c(final_order, length(dim(r)))
    final_order <- c(final_order, (1:length(dim(r)))[x_along:(length(dim(r)) - 1)])
    r <- .aperm2(r, final_order)
  }
  r
}

# This function is a helper for the function .MergeArrays.
# It expects as inputs two named numeric vectors, and it extends them
# with dimensions of length 1 until an ordered common dimension
# format is reached.
# The first output is dims1 extended with 1s.
# The second output is dims2 extended with 1s.
# The third output is a merged dimension vector. If dimensions with
# the same name are found in the two inputs, and they have a different
# length, the maximum is taken.
.MergeArrayDims <- function(dims1, dims2) {
  new_dims1 <- c()
  new_dims2 <- c()
  while (length(dims1) > 0) {
    if (names(dims1)[1] %in% names(dims2)) {
      pos <- which(names(dims2) == names(dims1)[1])
      dims_to_add <- rep(1, pos - 1)
      if (length(dims_to_add) > 0) {
        names(dims_to_add) <- names(dims2[1:(pos - 1)])
      }
      new_dims1 <- c(new_dims1, dims_to_add, dims1[1])
      new_dims2 <- c(new_dims2, dims2[1:pos])
      dims1 <- dims1[-1]
      dims2 <- dims2[-c(1:pos)]
    } else {
      new_dims1 <- c(new_dims1, dims1[1])
      new_dims2 <- c(new_dims2, 1)
      names(new_dims2)[length(new_dims2)] <- names(dims1)[1]
      dims1 <- dims1[-1]
    }
  }
  if (length(dims2) > 0) {
    dims_to_add <- rep(1, length(dims2))
    names(dims_to_add) <- names(dims2)
    new_dims1 <- c(new_dims1, dims_to_add)
    new_dims2 <- c(new_dims2, dims2)
  }
  list(new_dims1, new_dims2, pmax(new_dims1, new_dims2))
}

# This function takes two named arrays and merges them, filling with
# NA where needed.
# dim(array1)
#          'b'   'c'         'e'   'f'
#           1     3           7     9
# dim(array2)
#    'a'   'b'         'd'         'f'   'g'
#     2     3           5           9     11
# dim(.MergeArrays(array1, array2, 'b'))
#    'a'   'b'   'c'   'e'   'd'   'f'   'g'
#     2     4     3     7     5     9     11
.MergeArrays <- function(array1, array2, along) {
  if (!(is.null(array1) || is.null(array2))) {
    if (!(identical(names(dim(array1)), names(dim(array2))) &&
          identical(dim(array1)[-which(names(dim(array1)) == along)],
                    dim(array2)[-which(names(dim(array2)) == along)]))) {
      new_dims <- .MergeArrayDims(dim(array1), dim(array2))
      dim(array1) <- new_dims[[1]]
      dim(array2) <- new_dims[[2]]
      for (j in 1:length(dim(array1))) {
        if (names(dim(array1))[j] != along) {
          if (dim(array1)[j] != dim(array2)[j]) {
            if (which.max(c(dim(array1)[j], dim(array2)[j])) == 1) {
              na_array_dims <- dim(array2)
              na_array_dims[j] <- dim(array1)[j] - dim(array2)[j]
              na_array <- array(dim = na_array_dims)
              array2 <- abind(array2, na_array, along = j)
              names(dim(array2)) <- names(na_array_dims)
            } else {
              na_array_dims <- dim(array1)
              na_array_dims[j] <- dim(array2)[j] - dim(array1)[j]
              na_array <- array(dim = na_array_dims)
              array1 <- abind(array1, na_array, along = j)
              names(dim(array1)) <- names(na_array_dims)
            }
          }
        }
      }
    }
    if (!(along %in% names(dim(array2)))) {
      stop("The dimension specified in 'along' is not present in the ",
           "provided arrays.")
    }
    array1 <- abind(array1, array2, along = which(names(dim(array1)) == along))
    names(dim(array1)) <- names(dim(array2))
  } else if (is.null(array1)) {
    array1 <- array2
  }
  array1
}

# Takes as input a list of arrays. The list must have named dimensions.
.MergeArrayOfArrays <- function(array_of_arrays) {
  MergeArrays <- .MergeArrays
  array_dims <- (dim(array_of_arrays))
  dim_names <- names(array_dims)
  
  # Merge the chunks.
  for (dim_index in 1:length(dim_names)) {
    dim_sub_array_of_chunks <- dim_sub_array_of_chunk_indices <- NULL
    if (dim_index < length(dim_names)) {
      dim_sub_array_of_chunks <- array_dims[(dim_index + 1):length(dim_names)]
      names(dim_sub_array_of_chunks) <- dim_names[(dim_index + 1):length(dim_names)]
      dim_sub_array_of_chunk_indices <- dim_sub_array_of_chunks
      sub_array_of_chunk_indices <- array(1:prod(dim_sub_array_of_chunk_indices),
                                          dim_sub_array_of_chunk_indices)
    } else {
      sub_array_of_chunk_indices <- NULL
    }
    sub_array_of_chunks <- vector('list', prod(dim_sub_array_of_chunks))
    dim(sub_array_of_chunks) <- dim_sub_array_of_chunks
    for (i in 1:prod(dim_sub_array_of_chunks)) {
      if (!is.null(sub_array_of_chunk_indices)) {
        chunk_sub_indices <- which(sub_array_of_chunk_indices == i, arr.ind = TRUE)[1, ]
      } else {
        chunk_sub_indices <- NULL
      }
      for (j in 1:(array_dims[dim_index])) {
        new_chunk <- do.call('[[', c(list(x = array_of_arrays),
                                     as.list(c(j, chunk_sub_indices))))
        if (is.null(new_chunk)) {
          stop("Chunks missing.")
        }
        if (is.null(sub_array_of_chunks[[i]])) {
          sub_array_of_chunks[[i]] <- new_chunk
        } else {
          sub_array_of_chunks[[i]] <- MergeArrays(sub_array_of_chunks[[i]],
                                                  new_chunk,
                                                  dim_names[dim_index])
        }
      }
    }
    array_of_arrays <- sub_array_of_chunks
    rm(sub_array_of_chunks)
    gc()
  }
  
  array_of_arrays[[1]]
}

.MergeChunks <- function(shared_dir, suite_id, remove) {
  MergeArrays <- .MergeArrays
  
  args <- NULL
  shared_dir <- paste0(shared_dir, '/STARTR_CHUNKING_', suite_id)
  
  all_chunk_files_original <- list.files(paste0(shared_dir, '/'), '.*\\.Rds$')
  all_chunk_files <- gsub('\\.Rds$', '', all_chunk_files_original)
  chunk_filename_parts_all_components <- strsplit(all_chunk_files, '__')
  all_components <- sapply(chunk_filename_parts_all_components, '[[', 1)
  components <- unique(all_components)
  result <- vector('list', length(components))
  names(result) <- components
  for (component in components) {
    chunk_files_original <- all_chunk_files_original[which(all_components == component)]
    chunk_filename_parts <- chunk_filename_parts_all_components[which(all_components == component)]
    chunk_filename_parts <- lapply(chunk_filename_parts, '[', -1)
    if (length(unique(sapply(chunk_filename_parts, length))) != 1) {
      stop("Detected chunks with more dimensions than others.")
    }
    dim_names <- sapply(chunk_filename_parts[[1]],
                        # TODO: strsplit by the last '_' match, not the first.
                        function(x) strsplit(x, '_')[[1]][1])
    # TODO check all files have exactly the same dimnames
    found_chunk_indices <- sapply(chunk_filename_parts,
                                  function(x) as.numeric(sapply(strsplit(x, '_'), '[[', 2)))
    found_chunk_indices <- array(found_chunk_indices,
                                 dim = c(length(dim_names),
                                         length(found_chunk_indices) / length(dim_names))
    )
    found_chunks_str <- apply(found_chunk_indices, 2, paste, collapse = '_')
    
    if (length(args) > 0) {
      if ((length(args) %% 2) != 0) {
        stop("Wrong number of parameters.")
      }
      expected_dim_names <- args[(1:(length(args) / 2) - 1) * 2 + 1]
      if (any(!is.character(expected_dim_names))) {
        stop("Expected dimension names in parameters at odd positions.")
      }
      dim_indices <- args[(1:(length(args) / 2) - 1) * 2 + 2]
      if (!any(dim_indices == 'all')) {
        stop("Expected one dimension index to be 'all'.")
      }
      dim_to_merge <- which(dim_indices == 'all')
      if (length(dim_indices) > 1) {
        if (!all(is.numeric(dim_indices[-dim_to_merge]))) {
          stop("Expected all dimension index but one to be numeric.")
        }
      }
      # Check expected dim names match dim names
      ## TODO
      # Merge indices that vary along dim_to_merge whereas other fixed by dim_indices
      # REMOVE FILES
      ## TODO
      stop("Feature not implemented.")
    } else {
      chunks_indices <- 1:length(dim_names)
      chunks_indices <- lapply(chunks_indices, function(x) sort(unique(found_chunk_indices[x, ])))
      names(chunks_indices) <- dim_names
      
      # Load all found chunks into the array 'array_of_chuks'.
      array_dims <- sapply(chunks_indices, length)
      names(array_dims) <- dim_names
      array_of_chunks <- vector('list', prod(array_dims))
      dim(array_of_chunks) <- array_dims
      array_of_chunks_indices <- array(1:prod(array_dims), array_dims)
      for (i in 1:prod(array_dims)) {
        chunk_indices <- which(array_of_chunks_indices == i, arr.ind = TRUE)[1, ]
        j <- 1
        chunk_indices_on_file <- sapply(chunk_indices,
                                        function(x) {
                                          r <- chunks_indices[[j]][x]
                                          j <<- j + 1
                                          r
                                        })
        found_chunk <- which(found_chunks_str == paste(chunk_indices_on_file,
                                                       collapse = '_'))[1]
        if (length(found_chunk) > 0) {
          num_tries <- 5
          found <- FALSE
          try_num <- 1
          while ((try_num <= num_tries) && !found) {
            array_of_chunks[[i]] <- try({
              readRDS(paste0(shared_dir, '/',
                             chunk_files_original[found_chunk]))
            })
            if (is(array_of_chunks[[i]], 'try-error')) {
              message("Waiting for an incomplete file transfer...")
              Sys.sleep(5)
            } else {
              found <- TRUE
            } 
            try_num <- try_num + 1
          }
          if (!found) {
            stop("Could not open one of the chunks. Might be a large chunk ",
                 "in transfer. Merge aborted, files have been preserved.")
          }
        }
      }
      
      result[[component]] <- .MergeArrayOfArrays(array_of_chunks)
      rm(array_of_chunks)
      gc()
    }
  }
  
  if (remove) {
    sapply(all_chunk_files_original, 
           function(x) {
             file.remove(paste0(shared_dir, '/', x))
           })
  }
  
  result
}

.KnownLonNames <- function() {
  known_lon_names <- c('lon', 'longitude', 'x', 'i', 'nav_lon')
}

.KnownLatNames <- function() {
  known_lat_names <- c('lat', 'latitude', 'y', 'j', 'nav_lat')
}

.ReplaceElementInVector <- function(x, target, new_val) {
  # x is a vector with name
  # target is a string
  # new_val is a vector with name
  # E.g., Change [a = 2, b = 3] to [c = 1, d = 2, b = 3], then:
  #       x = c(a = 2, b = 3), target = 'a', new_val = c(c = 1, d = 2)
  new_names <- unlist(lapply(as.list(names(x)), function(x) if (x == target) names(new_val) else x))
  new_list <- vector('list', length = length(new_names))
  for (i in 1:length(new_list)) {
    new_list[[i]] <- c(new_val, x)[which(c(names(new_val), names(x)) == new_names[i])]
  }
  return(unlist(new_list))
}

.withWarnings <- function(expr) {
    myWarnings <- NULL
    wHandler <- function(w) {
      myWarnings <<- c(myWarnings, list(w))
      invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(expr, warning = wHandler)
    list(value = val, warnings = myWarnings)
}

# This function writes startR_autosubmit.sh to local startR_autosubmit folder, under expID/
write_autosubmit_bash <- function(chunks, cluster, autosubmit_suite_dir) {
  # "chunks" should be the argument "chunks" in Compute() plus the redundant margin dims,
  # e.g., list(dat = 1, var = 1, sdate = 1, time = 1, lat = 2, lon = 3)

  # Loop through chunks to create load script for each
  for (n_chunk in 0:(prod(unlist(chunks)) - 1)) {

    # Create chunk args
    chunk_names <- names(chunks)
    chunk_args <- matrix(NA, 2, length(chunks))
    chunk_args[1, ] <- paste0('%JOBS.CHUNK_', n_chunk, '.', chunk_names, '%')
    chunk_args[2, ] <- paste0('%JOBS.CHUNK_', n_chunk, '.', chunk_names, '_N%')
    chunk_args <- paste0('(', paste(c(chunk_args), collapse = ' '), ')')

    bash_script_template <- file(system.file('chunking/Autosubmit/startR_autosubmit.sh',
                                 package = 'startR'))
    bash_script_lines <- readLines(bash_script_template)
    close(bash_script_template)

    # Rewrite chunk_args=
    bash_script_lines <- gsub('^chunk_args=*', paste0('chunk_args=', chunk_args),
                              bash_script_lines)
    # Include init commands 
    bash_script_lines <- gsub('^include_init_commands',
                              paste0(paste0(cluster[['init_commands']], collapse = '\n'), '\n'),

                              bash_script_lines)
    # Rewrite include_module_load
    bash_script_lines <- gsub('^include_module_load',
                              paste0('module load ', cluster[['r_module']]),
                              bash_script_lines)
    # Rewrite cd run_dir
    # If run_dir is not specified, the script will run under ${proj_dir}
    if (!is.null(cluster[['run_dir']])) {
      bash_script_lines <- gsub('^cd_run_dir',
                                paste0('cd ', cluster[['run_dir']]),
                                bash_script_lines)
    } else {
      bash_script_lines <- gsub('^cd_run_dir', 'cd ${proj_dir}',
                                bash_script_lines)
    }

    # Save modified .sh file under local$PROJECT_PATH in expdef.yml
    #NOTE: dest_dir is ecflow_suite_dir_suite in ByChunks_autosubmit()
    #NOTE: the file will be copied to proj/ by "autosubmit create"
    dest_dir <- file.path(autosubmit_suite_dir, paste0("/STARTR_CHUNKING_", cluster$expid))

    if (!file.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }
    writeLines(bash_script_lines, paste0(dest_dir, '/startR_autosubmit_', n_chunk, '.sh'))
  }
}

# This function generates the .yml files under autosubmit conf/
write_autosubmit_confs <- function(chunks, cluster, autosubmit_suite_dir) {
  # "chunks" is from Compute() input, e.g., chunks <- list(lat = 2, lon = 3)
  # "cluster" is the argument "cluster" in Compute(), to set machine configuration
  # "autosubmit_suite_dir" should be the local folder that has R script, like ecflow_suite_dir in Compute() 

  # Get config template files from package
  template_dir <- system.file('chunking/Autosubmit/', package = 'startR')
  config_files <- list.files(template_dir, pattern = "*\\.yml$")

  for (i_file in config_files) {

    conf <- yaml::read_yaml(file.path(template_dir, i_file))
    conf_type <- strsplit(i_file, split = "[.]")[[1]][1]

############################################################
    if (conf_type == "autosubmit") {

      #Q: Should it be the total amount of chunk?
      conf$config$MAXWAITINGJOBS <- as.integer(prod(unlist(chunks)))  # total amount of chunk
      #NOTE: Nord3 max. amount of queued jobs is 366
      if (conf$config$MAXWAITINGJOBS > 366) conf$config$MAXWAITINGJOBS <- 366
      conf$config$TOTALJOBS <- as.integer(cluster$max_jobs)

############################################################
    } else if (conf_type == "expdef") {
      conf$default$EXPID <- cluster$expid
      conf$default$HPCARCH <- cluster$queue_host
      # PROJECT_PATH should be where submit.sh and load....R stored --> local startR_autosubmit folder, under expID/ 
      conf$local$PROJECT_PATH <- file.path(autosubmit_suite_dir, paste0("STARTR_CHUNKING_", cluster$expid))

############################################################
    } else if (conf_type == "jobs") {

      chunks_vec <- lapply(lapply(chunks, seq, 1), rev) # list(lat = 1:2, lon = 1:3)
      chunk_df <- expand.grid(chunks_vec)
      nchunks <- nrow(chunk_df)
      chunk_name <- paste0("CHUNK_", 0:(nchunks - 1))

      # Fill in common configurations
      jobs <- conf$JOBS
      # wallclock from '01:00:00' to '01:00'
      jobs[[1]]$WALLCLOCK <- substr(cluster$job_wallclock, 1, 5)
      jobs[[1]]$PLATFORM <- cluster$queue_host
      jobs[[1]]$THREADS <- as.integer(cluster$cores_per_job)
      jobs[[1]][paste0(names(chunks), "_N")] <- as.integer(unlist(chunks))
      jobs[[1]][names(chunks)] <- ""

      # Create chunks and fill in info for each chunk
      if (nchunks > 1) {
        jobs <- c(jobs, rep(jobs, nchunks - 1))
        names(jobs) <- chunk_name
      }
      for (i_chunk in 1:nchunks) {
        jobs[[i_chunk]][names(chunks)] <- chunk_df[i_chunk, ]
        jobs[[i_chunk]]$FILE <- paste0('startR_autosubmit_', i_chunk - 1, '.sh')
      }

      conf$JOBS <- jobs

############################################################
    } else if (conf_type == "platforms") {
      if (tolower(cluster$queue_host) != "local") {
        conf$Platforms[[cluster$queue_host]]$USER <- cluster$hpc_user
        conf$Platforms[[cluster$queue_host]]$PROCESSORS_PER_NODE <- as.integer(cluster$cores_per_job)
        if (!is.null(cluster$extra_queue_params)) {
          tmp <- unlist(cluster$extra_queue_params)
          for (ii in 1:length(tmp)) {
            tmp[ii] <- paste0('\"', tmp[ii], '\"')
          }
          conf$Platforms[[cluster$queue_host]]$CUSTOM_DIRECTIVES <- paste0('[ ', paste(tmp, collapse = ','), ' ]')
       }
      }

############################################################
    } else {
      stop("File ", i_file, " is not considered in this function.")
    }

############################################################
    # Output directory
    dest_dir <- paste0("/esarchive/autosubmit/", cluster$expid, "/conf/")
    dest_file <- paste0(conf_type, "_", cluster$expid, ".yml")

    # Write config file inside autosubmit dir
    yaml::write_yaml(conf, paste0(dest_dir, dest_file))
    Sys.chmod(paste0(dest_dir, dest_file), mode = "755", use_umask = F)

  } # for loop each file
}
