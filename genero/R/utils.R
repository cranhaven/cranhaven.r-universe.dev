

remove_accents <- function(string){
  # accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  accents <- "\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9\u00e1\u00e9\u00ed\u00f3\u00fa\u00fd\u00c1\u00c9\u00cd\u00d3\u00da\u00dd\u00e4\u00eb\u00ef\u00f6\u00fc\u00c4\u00cb\u00cf\u00d6\u00dc\u00e2\u00ea\u00ee\u00f4\u00fb\u00c2\u00ca\u00ce\u00d4\u00db\u00f1\u00d1\u00e7"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}

match_replace <- function (v, dic, na = NA, force = TRUE){
  matches <- dic[[2]][match(v, dic[[1]])]
  out <- matches
  if(!is.na(na)){
    na_to_chr(out, na)
  }
  if (!force)
    out[is.na(matches)] <- v[is.na(matches)]
  out
}

which_in <- function (x, y) x[x %in% y]

na_proportion <- function(x){
  if(length(x) < 4) return(0)
  sum(is.na(x))/length(x)
}

many_words_proportion <- function(x) sum(grepl("[^\\s]([ ]{1,})[^\\s]",x))/length(x)

na_to_chr <- function(x, na){
  x[is.na(x)] <- na
  x
}

insert_column <- function(d, vector, target, col_name){
  if(ncol(d) == 1){
    d[[col_name]] <- vector
    return(d)
  }
  new_col <- data.frame(vector, stringsAsFactors = FALSE)
  names(new_col) <- col_name
  cbind(d[,1:target,drop=FALSE], new_col, d[,(target+1):length(d),drop=FALSE])
}



