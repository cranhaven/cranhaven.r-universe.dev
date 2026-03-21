#require(openssl)
#require(stringr)


#' Corpora to Word List
#'
#' Converts a collection of documents to a word list
#' @export
#'
#' @param paths Paths of plaintext documents
#' @param ascii_only Will omit non-ascii characters if TRUE
#' @param custom_regex If not NA, will override ascii_only and
#'                     this will determine what a valid word
#'                     consists of
#' @param max_word_length Maximum length of extracted words
#' @param min_word_count Minimum number of occurrences for a
#'                       word to be added to word list
#' @param stopword_fn Filename containing stopwords to use or a list of
#'                    stopwords (if length > 1)
#' @param max_size Maximum size of list
#' @param min_word_length Minimum length of words
#' @param output_file File to write list to
#' @param json_path If input text is JSON, then it will be parsed as such
#'                  if this is a character of JSON keys to follow
#' @return A `character` vector of words
corpora_to_word_list <- function(
  paths,
  ascii_only=TRUE,
  custom_regex=NA,
  max_word_length=20,
  stopword_fn=DEFAULT_STOPWORDS,
  min_word_count=5,
  max_size = 16^3,
  min_word_length=3,
  output_file=NA,
  json_path=NA
){
  corpora = unlist(lapply(
    paths,
    function(x) readChar(x, file.info(x)$size)
  ))

  if (!is.na(json_path)){
    json_data = lapply(corpora, jsonlite::fromJSON)
    corpora = lapply(
      json_data,
      function(x){
        for (key in json_path){
          x = x[[key]]
        }
        x
      }
    )

  }
  corpora = tolower(gsub('[-_*.?,":\\\\/\']',' ', corpora))
  if (custom_regex){
    tokenized_corpora = stringr::str_extract_all(
      corpora,
      custom_regex
    )
  } else if (ascii_only){
   tokenized_corpora = stringr::str_extract_all(
     corpora,
     '[a-z]+'
   )
  } else {
    tokenized_corpora = stringr::str_extract_all(
      corpora,
      '\\d+'
    )
  }

  if (is.na(stopword_fn[1])){
    stopwords = character(0)
  } else if (length(stopword_fn) > 1){
    stopwords = stopword_fn
  } else {
    stopwords = utils::read.csv(stopword_fn, stringsAsFactors=FALSE)[,1,T]
  }
  filtered_tokenized_corpora = lapply(
    tokenized_corpora,
    function(x){
      x = x[nchar(x) >= min_word_length & nchar(x) <= max_word_length]
      x = x[!x %in% c(stopwords, paste0(stopwords, 's'), paste0(stopwords, 'es'))[seq_len(3*length(stopwords))]]
      return(x)
    }
  )

  word_table = table(unlist(filtered_tokenized_corpora))
  word_table = word_table[word_table >= min_word_count]
  word_table = sort(word_table, decreasing=TRUE)[1:min(max_size, length(word_table))]

  words = names(word_table)

  if (!is.na(output_file)){
    utils::write.table(
      data.frame(word=words),
      file=output_file,
      quote=FALSE,row.names=FALSE, col.names=FALSE
    )
  }

  return(words)
}

#' Key to English
#'
#' Hashes field to sequence of words from a list.
#' @export
#' @param x - field to hash
#' @param hash_function `character` name of hash function or hash `function` itself,
#'                       returning a hexadecimal character
#' @param phrase_length `numeric` of words to use in each hashed key
#' @param corpus_path `character` path to word list, as a single-column text file with one
#'                    word per row
#' @param word_list `character` list of words to use in phrases
#' @param hash_subsection_size `numeric` length of each subsection of hash to use for word index. 16^N
#'                              unique words can be used for a size of N. This value times
#'                              phrase_length must be less than or equal to the length of the
#'                              hash output. Must be less than 14.
#' @param sep `character` separator to use between each word.
#' @param word_trans A `function`, `list` of functions, or 'camel' (for CamelCase). If
#'                   a list is used, then the index of the word of each phrase is
#'                   mapped to the corresponding function with that index,
#'                   recycling as necessary
#'
#' @param suppress_warnings `logical` value indicating if warning of non-character
#'                                   input should be suppressed
#'
#' @param hash_output_length optional `numeric` if the provided hash function is not a `character`. This is used
#'                           to send warnings if the hash output is too small to provide full range of all
#'                           possible combinations of outputs.
#'
#' @param forced_limit for multiple word lists, this is the maximum number of values used for calculating the index
#'                     (prior to taking the modulus) for each word in a phrase. Using this may speed up processing
#'                     longer word lists with a large least-common-multiple among individual word list lengths. This
#'                     will introduce a small amount of bias into the randomness. This value should be much larger than
#'                     any individual word list whose length is not a factor of this value.
#'
#' @param numeric_append_range optional `numeric` value of two integers indicating range of integers to append onto data
#'
#' @return `character` vector of hashed field resembling phrases
#'
#' @examples
#' # hash the numbers 1 through 5
#' keyToEnglish(1:5)
#'
#' # alternate upper and lowercase, 3 words only
#' keyToEnglish(1:5, word_trans=list(tolower, toupper), phrase_length=3)
keyToEnglish <- function(
  x,
  hash_function='md5',
  phrase_length=5,
  corpus_path=NA,
  word_list=wl_common,
  hash_subsection_size=3,
  sep='',
  word_trans='camel',
  suppress_warnings=FALSE,
  hash_output_length=NA,
  forced_limit=NA,
  numeric_append_range=NA
){
  if (hash_subsection_size > 13){
    stop('hash_subsection_size must not be greater than 13.')
  }
  KTI_METHOD_LIST=list(
    md5=openssl::md5,
    md4=openssl::md4,
    sha256=openssl::sha256,
    sha512=openssl::sha512,
    sha384=openssl::sha384,
    sha1=openssl::sha1,
    sha2=openssl::sha2,
    sha224=openssl::sha224
  )

  KTI_SIZES = list(
    md5=32,
    md4=32,
    sha256=64,
    sha512=128,
    sha384=96,
    sha1=40,
    sha2=64,
    sha224=56
  )

  if (is.character(hash_function)){
    hash_output_length = KTI_SIZES[[hash_function]]
  }

  if (is.na(word_list[1])){
    word_list = utils::read.csv(corpus_path, header=FALSE)[,1,T]
  }

  if (!is.na(numeric_append_range[1])){
    if (is.list(word_list)){
      # nothing special
    } else {
      word_list = replicate(phrase_length, word_list, simplify=FALSE)
    }
    append = as.character(seq.int(from=numeric_append_range[1], to=numeric_append_range[2]))
    word_list[[length(word_list)+1]] = append
  }

  if (is.list(word_list)){
    multiple_word_lists=TRUE
    word_list_lengths = sapply(word_list, length)

    if (all(word_list_lengths == word_list_lengths[1])){
      word_list_length = word_list_lengths[1]
    } else {
      word_list_length = LCM(word_list_lengths)
      if (!is.na(forced_limit) & forced_limit < word_list_length){
        word_list_length=forced_limit
        if (
          any(
            word_list_lengths < forced_limit
          ) &
          !suppress_warnings
        ){
          warning('The forced_limit value you chose is too small to cover all words in all lists.')
        }
      }
      if (word_list_length > 16^(hash_subsection_size)){

        if (!suppress_warnings) warning(
          'LCM of provided word lists is greater than range of values. Value is being limited.'
          )
        word_list_length = 16^hash_subsection_size
      }

      if (hash_output_length < log(word_list_length) & !is.na(hash_output_length)){
        if (!suppress_warnings){
          warning('The hash you chose cannot cover all combinations of input.')
        }
      }
    }

    phrase_length = length(word_list)


  } else {
    multiple_word_lists=FALSE
    if (hash_output_length < phrase_length * hash_subsection_size){
      warning('The hash you chose cannot cover all combinations of input.')
    }
  }


  if (!is.character(x)){
    if (!suppress_warnings) warning('Converting input to character')
    x = as.character(x)
  }

  trans_funcs = list()
  for (i in seq_along(word_trans)){
    if (class(word_trans) == 'function'){
      word_trans_function = word_trans
    } else {
      if (class(word_trans[[i]]) == 'function')
        word_trans_function = word_trans[[i]]
      else if (is.na(word_trans[[i]]))
        word_trans_function = identity
      else if (word_trans == 'camel')
        word_trans_function = stringr::str_to_title
      else
        word_trans_function = get(word_trans)
    }
    trans_funcs[[i]] = word_trans_function
  }

  if (multiple_word_lists){
    #
    original_word_lists = word_list
    word_list = paste0('X',seq_len(word_list_length))
    n_trans_funcs = length(trans_funcs)

    original_trans_funcs = trans_funcs
    trans_funcs <- lapply(
      seq_along(word_list_lengths),
      function(i){
        function(x) original_trans_funcs[[1 + (i-1) %% n_trans_funcs ]](
          original_word_lists[[i]][
            1 + (as.numeric(gsub('X','', x)) %% word_list_lengths[i])
            ]
        )
      }

    )
  }

  if (class(hash_function) != 'function')
    hash_function = KTI_METHOD_LIST[[hash_function]]

  split_hashes = stringr::str_extract_all(
    hash_function(x),
    sprintf('.{%s}', hash_subsection_size)
  )

  n_words = length(word_list)

  seq_idx = seq_len(phrase_length)

  if (hash_subsection_size <= 7 ){
    convert <- strtoi
  } else {
    # this is slower for longer lists, but it handles values >= 2^32
    convert <- as.numeric
  }

  if (length(trans_funcs) > 1){
    word_trans_function <- function(x, i){
      sapply(i, function(j) trans_funcs[[1 + (j-1) %% length(trans_funcs)]](x[j]))
    }

    new_keys = unlist(lapply(
      split_hashes,
      function(x) paste(word_trans_function(word_list[
        1 + convert(paste0('0x', x[seq_idx])) %% n_words
      ], seq_idx), collapse=sep)
    ))
  } else {

    new_keys = unlist(lapply(
      split_hashes,
      function(x) paste(
        word_trans_function(
          word_list[convert(paste0('0x', x[seq_idx])) %% n_words]
          ),
        collapse=sep
      )
    ))
  }

  return(new_keys)
}

#' Hash to Sentence
#'
#' Hashes data to a sentence that contains 54 bits of entropy
#'
#' @export
#' @param x - Input data, which will be converted to `character` if not already `character`
#' @param ... - Other parameters to pass to `keyToEnglish()`,
#'              besides `word_list`, `hash_subsection_size`, and `hash_function`
#'
#' @return `character` vector of hashed field resembling phrases
hash_to_sentence <- function(
  x,
  ...
){
  keyToEnglish(
    x,
    word_list=keyToEnglish::wml_long_sentence,
    hash_subsection_size=3,
    hash_function='sha224',
    ...
  )
}

#' Generate Random Sentences
#'
#' Randomly generate sentences with a specific structure
#'
#' @export
#' @param n `numeric` number of sentences to generate
#' @param punctuate `logical` value of whether to add spaces, capitalize first letter, and append period
#' @param fast `logical`
#'
#' @return `character` vector of randomly generated sentences
generate_random_sentences <- function(
  n,
  punctuate=TRUE,
  fast=FALSE
){
  salt_bytes = openssl::rand_bytes(128)
  salt_bytes[salt_bytes==0] = as.raw(1)
  salt = rawToChar(salt_bytes)
  x = openssl::rand_num(n)
  x = paste(x, salt)

  if (!fast){
    if (!punctuate){
      return(hash_to_sentence(x))
    } else {
      capitalizer=list(
        stringr::str_to_title,
        identity,
        identity,
        identity,
        identity,
        identity
      )
      return(paste0(
        hash_to_sentence(
          x,
          sep=' ',
          word_trans=capitalizer
        ),
        '.'
      ))
    }
  } else {
      word_list = keyToEnglish::wml_long_sentence
      n_words = length(word_list)
      word_matrix = matrix('', nrow=n, ncol=n_words)
      for (i in 1:length(word_list)){
        word_matrix[,i] = sample(word_list[[i]], n, replace=TRUE)
      }
      if (punctuate){
        word_matrix[,1] = stringr::str_to_title(word_matrix[,1])
        return(
          paste0(apply(
            word_matrix,
            1,
            paste,
            collapse=' '
          ),'.')
        )
      } else {
        return(
          apply(
            word_matrix,
            1,
            function(x) paste(stringr::str_to_title(x), collapse='')
          )
        )
      }
    }
}


#' Least Common Multiple
#'
#' Calculates least common multiple of a list of numbers
#'
#' @export
#'
#' @param ... Any number of `numeric` vectors or nested `list`s containing such
#'
#' @return A `numeric` that is the least common multiple of the input values
LCM <- function(...){
  vals = unlist(list(...))
  n_vals = length(vals)
  if (n_vals==1){
    return(vals)
  }
  return(
    Reduce(lcm, vals[2:n_vals], vals[1])
  )
}

#' Greater Common Denominator
#'
#' Calculates greatest common denominator of a list of numbers
#'
#' @export
#'
#' @param ... Any number of `numeric` vectors or nested `list`s containing such
#'
#' @return A `numeric` that is the greatest common denominator of the input values
GCD <- function(...){
  vals = unlist(list(...))
  n_vals = length(vals)
  if (n_vals==1){
    return(vals)
  }
  return(
    Reduce(gcd, vals[2:n_vals], vals[1])
  )
}

lcm <- function(x, y){
  return(x*y/gcd(x,y))
}

gcd <- function(x, y){
  if (x == y){
    return(x)
  }
  v0 = min(x,y)
  v = v0
  last_factor = 1
  while (v > 1){
    if (x %% v == 0 & y %% v == 0){
      return(v)
    }
    if (v >= v0 %/% last_factor + 1){
      last_factor = last_factor + 1
      v = ceiling(v0/last_factor)
    } else {
      v = v - 1
    }
  }
  return(1)
}

#' Uniqueness Probability
#'
#' Calculates probability that all `r` elements of a set of size `N` are unique
#' @export
#' @param N `numeric` size of set. Becomes unstable for values greater than 10^16.
#' @param r `numeric` number of elements selected with replacement
#'
#' @return `numeric` probability that all `r` elements are unique
uniqueness_probability <- function(N, r){
  exp(lgamma(N+1)-lgamma(N - r + 1) - r * log(N))
}

#' Uniqueness Max Size
#'
#' Returns approximate number of elements that you can select out
#' of a set of size `N` if the probability of there being any duplicates
#' is less than or equal to `p`
#' @export
#'
#' @param N `numeric` size of set elements are selected from, or a `list` of
#'          `list`s of `character` vectors (e.g., `wml_animals`)
#' @param p `numeric` probability that there are any duplicate elements
#'
#' @returns `numeric` value indicating size. Value will most likely be non-integer
#'
#' @examples
#' # how many values from 1-1,000 can I randomly select before
#' # I have a 10% chance of having at least one duplicate?
#'
#' uniqueness_max_size(1000,0.1)
#' # 14.51
uniqueness_max_size <- function(N, p){
  if (is.list(N)){
    N = prod(unlist(lapply(N, length)))
  }
  sqrt(2*log(1/(1-p))*N)
}

