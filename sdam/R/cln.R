
## 
## FUNCTION cln() to clean and re-encode glyphs and Greek characters
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.4.2 (23-08-2022)
##
##
## PARAMETERS
##
## x        (scalar or vector, with character to clean)
## level    (clean level, 0 for no-clean, default 1 to 9)
##
## OPTIONAL PARAMETERS
##
## what     (additional characters to clean)
## na.rm    (logical, remove NAs?)
## case     (1 for 1st uppercase, 2 lower, 3 upper)
## repl     (data frame with text for replacement)
## unlist   (return vector when vector?)
##
## DEPENDS: cs() (if 'case')
##

cln <-
function (x, level = 1, what, na.rm, case, repl, unlist) 
{
    xo <- x
    ifelse(missing(what) == TRUE, what <- c("?", "+", "*"), what <- c("?", 
        "+", "*", what))
    ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
        TRUE, na.rm <- TRUE, na.rm <- FALSE)
    ifelse(missing(unlist) == FALSE && isTRUE(unlist == FALSE) == 
        TRUE, unlist <- FALSE, unlist <- TRUE)
    if (isTRUE(level > 0) == TRUE) {
        if (isTRUE(is.data.frame(x) == TRUE) == TRUE || isTRUE(is.data.frame(x[[1]]) == 
            TRUE) == TRUE) {
            if (isTRUE(is.data.frame(x[[1]]) == TRUE) == TRUE) {
                warning("\"x\" is list of data frames and only first data frame is considered.")
                x <- as.data.frame(x[[1]])
            }
            else {
                invisible(NA)
            }
            flgdf <- TRUE
            rnx <- rownames(x)
            for (w in seq_len(length(what))) {
                x <- as.data.frame(sapply(x, function(z) as.list(gsub(paste0("\\", 
                  what[w], sep = ""), "", z))))
            }
            rm(w)
            rownames(x) <- rnx
            x[is.null(x)] <- NA
            x[x == ""] <- NA
            xdf <- data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
            if (isTRUE(level > 1) == TRUE) {
                xdf <- as.data.frame(sapply(xdf, function(z) as.list(gsub(paste0("\\", 
                  ".$", sep = ""), "", z))), check.names = FALSE)
                xdf <- as.data.frame(apply(xdf, 2, function(z) gsub("\\s+", 
                  " ", z)), check.names = FALSE)
                xdf <- as.data.frame(apply(xdf, 2, function(z) gsub("-\\s", 
                  "-", z)), check.names = FALSE)
                xdf <- as.data.frame(apply(xdf, 2, function(z) gsub("\\s-", 
                  "-", z)), check.names = FALSE)
                xdf <- as.data.frame(apply(xdf, 2, function(z) gsub("/\\s", 
                  "/", z)), check.names = FALSE)
                xdf <- as.data.frame(apply(xdf, 2, function(z) gsub("\\s/", 
                  "/", z)), check.names = FALSE)
                xdf <- as.data.frame(apply(xdf, 2, function(z) gsub("\\s$", 
                  "", z)), check.names = FALSE)
                ifelse(isTRUE(length(rnx) == 1) == TRUE, xdf <- as.data.frame(t(xdf), 
                  row.names = rnx), rownames(xdf) <- rnx)
            }
            x <- as.list(sapply(xdf, as.character))
        }
        else {
            flgdf <- FALSE
            ifelse(is.factor(x) == TRUE, x <- as.vector(x), NA)
            ifelse(is.vector(x) == TRUE, flgvc <- TRUE, flgvc <- FALSE)
            ifelse(is.list(x) == TRUE, flgvl <- TRUE, flgvl <- FALSE)
            is.na(x) <- x == "NULL"
            if (isTRUE(level > 1) == TRUE) {
                x <- gsub("\\s+", " ", x)
                x <- gsub("-\\s", "-", x)
                x <- gsub("\\s-", "-", x)
                x <- gsub("/\\s", "/", x)
                x <- gsub("\\s/", "/", x)
                x <- gsub("\\s$", "", x)
            }
            else {
                NA
            }
        }
    }
    else {
        return(x)
    }
    if (isTRUE(flgdf == FALSE) == TRUE && isTRUE(length(x) == 
        1) == TRUE) {
        xx1 <- strsplit(x, "")[[1]]
    }
    else {
        xx1 <- strsplit(paste(as.vector(unlist(x)), collapse = ""), 
            "")[[1]]
    }
    dbe <- c("<U+0080>", "\201"     , "<U+0082>", "<U+0083>", 
        "<U+0084>", "<U+0085>", "<U+0086>", "<U+0087>", "<U+0088>", 
        "<U+0089>", "<U+008A>", "<U+008B>", "<U+008C>", "\215"     , 
        "<U+008E>", "\217"     , "\220"     , "<U+0091>", "<U+0092>", 
        "<U+0093>", "<U+0094>", "<U+0095>", "<U+0096>", "<U+0097>", 
        "<U+0099>", "<U+0099>", "<U+009A>", "<U+009B>", "<U+009C>", 
        "\235"     , "<U+009E>", "<U+009F>")
    names(dbe) <- c("80", "81", "82", "83", "84", "85", "86", 
        "87", "88", "89", "8A", "8B", "8C", "8D", "8E", "8F", 
        "90", "91", "92", "93", "94", "95", "96", "97", "99", 
        "99", "9A", "9B", "9C", "9D", "9E", "9F")
    ifelse(isTRUE(level > 0) == TRUE && (isTRUE("<" %in% xx1) == 
        TRUE && isTRUE(">" %in% xx1) == TRUE), flgx <- TRUE, 
        flgx <- FALSE)
    if (isTRUE(length(x) == 1) == TRUE) {
        if (isTRUE(flgx == TRUE) == TRUE) {
            ck <- which(xx1 %in% "<")
            x2 <- vector()
            ifelse(isTRUE(ck[1] == 1) == TRUE, NA, x2 <- append(x2, 
                xx1[1:ck[1] - 1L]))
            for (i in seq_len(length(ck))) {
                x2 <- append(x2, as.vector(dbe[which(names(dbe) %in% 
                  paste(xx1[(ck[i] + 5L):(ck[i] + 6L)], collapse = ""))]))
                if (isTRUE(ck[i] == max(ck)) == TRUE) {
                  ifelse(isTRUE(tail(xx1, 1) == ">") == TRUE, 
                    NA, x2 <- append(x2, xx1[(ck[i] + 8L):length(xx1)]))
                }
                else {
                  x2 <- append(x2, xx1[(ck[i] + 8L):(ck[i + 1L] - 
                    1L)])
                }
            }
            rm(i)
            x <- paste(x2, collapse = "")
        }
        x1 <- as.vector(x)
        if (is.na(x1) == TRUE) 
            return(x)
        ifelse(isTRUE(level == 0) == TRUE, NA, x1 <- paste(strsplit(x1, 
            "")[[1]][which(!(strsplit(x1, "")[[1]] == "?"))], 
            collapse = ""))
        if (isTRUE(level > 1) == TRUE) {
            x1 <- gsub("\\s*\\([^\\)]\\)", "", x1)
            for (w in seq_len(length(what))) {
                x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                  "")[[1]] == what[w]))], collapse = "")
            }
            rm(w)
        }
        else if (isTRUE(level == 1) == TRUE) {
            x1 <- paste(strsplit(x1, "")[[1]][which(!(strsplit(x1, 
                "")[[1]] == "?"))], collapse = "")
        }
        else {
            invisible(NA)
        }
        utix1 <- utf8ToInt(x1)
        if (isTRUE(is.na(utix1) == TRUE) == TRUE) {
            flgna <- TRUE
            utix1 <- utf8ToInt(iconv(x1, "", "UTF-8"))
        }
        else {
            flgna <- FALSE
        }
        if (isTRUE(any(utix1 > 255) == TRUE) == TRUE) {
            chk <- which(utix1 > 255)
            utix0 <- utix1
            utix0[chk - 1L] <- utix1[chk - 1L] + 1L
            utix1 <- utix0[-chk]
            flgc <- TRUE
        }
        else {
            flgc <- FALSE
        }
        gs1 <- which(as.raw(utix1) %in% c("e2", "e4", "f6", "fc"))
        gs2 <- which(as.raw(utix1) %in% c("cf", "ce"))
        gs2a <- which(as.raw(utix1) %in% c("c2", "c3", "c4", 
            "c5", "c8"))
        gs3 <- which(as.raw(utix1) %in% c("e1"))
        if (isTRUE(length(gs3) > 0) == TRUE) {
            invisible(NA)
        }
        else {
            gs3 <- NULL
        }
        if (isTRUE(length(c(gs1, gs2, gs2a, gs3)) == 0) == TRUE) {
            if (missing(case) == FALSE && is.numeric(case) == 
                TRUE) {
                return(cs(x1, level = level, case = case, flgdf = flgdf, 
                  na.rm = na.rm))
            }
            else {
                return(x1)
            }
        }
        xx <- strsplit(rawToChar(as.raw(utix1)), "")[[1]]
        ifelse(isTRUE(tail(xx, 1) == "+") == TRUE, flgp <- TRUE, 
            flgp <- FALSE)
        ifelse(isTRUE(tail(xx, 1) == "*") == TRUE, flga <- TRUE, 
            flga <- FALSE)
        if (isTRUE(length(c(gs2, gs2a, gs3)) == 0) == TRUE) {
            res <- paste(xx, collapse = "")
            names(res) <- xo
            if (missing(case) == FALSE && is.numeric(case) == 
                TRUE) {
                return(cs(res, level = level, case = case, flgdf = flgdf, 
                  na.rm = na.rm))
            }
            else {
                return(res)
            }
        }
        else {
            res <- vector()
            gsx <- sort(c(gs2, gs2a, gs3))
        }
        if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
            res <- append(res, xx[1:(min(gsx) - 1L)])
        }
        else {
            invisible(NA)
        }
        late <- c(c("e2", "e4", "f6", "fc"), c("cf", "ce"), c("c2", 
            "c3", "c4", "c5", "c8"), c("e1"))
        for (j in seq_along(gsx)) {
            i <- gsx[j]
            if (isTRUE(i %in% c(gs2, gs2a)) == TRUE) {
                tmp <- paste(xx[i:(i + 1L)], collapse = "")
                res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                  to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                  to = "UTF-8"))
                if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                  collapse = ""))) %in% c(c("cf", "ce"), c("c2", 
                  "c3"), c("e1")))) == FALSE && suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                  0) {
                  if (isTRUE(i < (length(xx) - 1L)) == TRUE) {
                    ifelse(isTRUE(j == length(gsx)) == TRUE, 
                      res <- append(res, xx[(i + 2L):length(xx)]), 
                      res <- append(res, xx[(i + 2L):(gsx[j + 
                        1L] - 1L)]))
                  }
                  else {
                    invisible(NA)
                  }
                }
            }
            else if (isTRUE(i %in% gs3) == TRUE) {
                tmp <- paste(xx[i:(i + 2L)], collapse = "")
                res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                  to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                  to = "UTF-8"))
                if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                  collapse = ""))) %in% c(c("cf", "ce"), c("c2", 
                  "c3"), c("e1")))) == FALSE && suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                  0) {
                  if (isTRUE(i < (length(xx) - 1L)) == TRUE) {
                    ifelse(isTRUE(j == length(gsx)) == TRUE, 
                      res <- append(res, xx[(i + 3L):length(xx)]), 
                      res <- append(res, xx[(i + 3L):(gsx[j + 
                        1L] - 1L)]))
                  }
                  else {
                    invisible(NA)
                  }
                }
            }
            if (isTRUE(flgc == TRUE) == TRUE || (isTRUE(j < max(seq_along(gsx))) == 
                TRUE && isTRUE(length(c(gs2, gs3)) == 0) == TRUE)) {
                if (isTRUE((i + 2L) < gsx[j + 1]) == TRUE) {
                  res <- append(res, xx[(i + 2L):(gsx[j + 1L] - 
                    1L)])
                }
                else {
                  invisible(NA)
                }
            }
            else if (isTRUE(length(gsx) == 1) == TRUE) {
                res <- append(res, xx[(i + 2L):length(xx)])
            }
        }
        rm(j)
        if (isTRUE(flgc == TRUE) == TRUE) {
            if (isTRUE((length(res) + (length(chk) * 2L)) < length(xx)) == 
                TRUE) {
                res <- append(res, xx[(length(res) + (length(chk) * 
                  2L)):length(xx)])
            }
            else {
                invisible(NA)
            }
        }
        else if ((isTRUE(max(gsx) == length(res)) == TRUE) && 
            (any(is.na(xx[(length(res) + (max(gsx))):length(xx)])) == 
                FALSE)) {
            res <- append(res, xx[(length(res) + (max(gsx))):length(xx)])
        }
        if (("+" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
            1)]) && isTRUE(flgp == TRUE) == TRUE) {
            res <- append(res, "+")
        }
        else if (("*" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
            1)]) && isTRUE(flgp == TRUE) == TRUE) {
            res <- append(res, "*")
        }
        if (isTRUE(length(gs2a) == 0) == TRUE || suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
            collapse = ""))) %in% c(c("cf", "ce"), c("c2", "c3"), 
            c("e1"))) == FALSE)) {
            if (all(c(" ", "/", " ") %in% strsplit(x, "")[[1]]) == 
                TRUE) {
                if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
                  tmpr <- NULL
                  resl <- as.list(paste(c(tmpr, res), collapse = ""))
                }
                else {
                  tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                    c(" ", "/", " "))) - 2):length(xx)], collapse = ""), 
                    from = "UTF-8", to = "UTF-16LE", toRaw = TRUE), 
                    from = "UTF-16LE", to = "UTF-8")
                  resl <- as.list(paste(c(res, tmpr), collapse = ""))
                }
            }
            else {
                resl <- as.list(paste(res, collapse = ""))
            }
            if (isTRUE(unlist == FALSE) == TRUE) {
                names(resl) <- xo
            }
            else {
                ifelse(isTRUE(flgvc == TRUE) == TRUE, resl <- unlist(resl, 
                  use.names = FALSE), NA)
            }
            if (isTRUE(length(strsplit(paste(resl[[1]], tmp, 
                sep = ""), "")[[1]]) == length(strsplit(x, "")[[1]])) == 
                TRUE) {
                if (missing(case) == FALSE && is.numeric(case) == 
                  TRUE) {
                  return(cs(resl, level = level, case = case, 
                    flgdf = flgdf, na.rm = na.rm))
                }
                else {
                  return(resl)
                }
            }
            else {
                if (isTRUE(flgc == TRUE) == TRUE) {
                  st <- length(strsplit(paste(resl[[1]], tmp, 
                    sep = ""), "")[[1]])
                  ed <- length(strsplit(x1, "")[[1]])
                  resl <- paste(c(strsplit(resl[[1]], "")[[1]], 
                    strsplit(x1, "")[[1]][(st + 1):ed]), collapse = "")
                }
                else {
                  invisible(NA)
                }
                if (missing(case) == FALSE && is.numeric(case) == 
                  TRUE) {
                  return(cs(resl, level = level, case = case, 
                    flgdf = flgdf, na.rm = na.rm))
                }
                else {
                  return(resl)
                }
            }
        }
        else {
            x1p <- paste(res, collapse = "")
            gs2p <- which(suppressWarnings(as.raw(utf8ToInt(x1p))) %in% 
                c("cf", "ce"))
            gs3p <- which(suppressWarnings(as.raw(utf8ToInt(x1p))) %in% 
                c("e1"))
            if (isTRUE(length(gs3p) > 0) == TRUE) {
                gs3p <- gs3p[which(gs3p%%2 != 0)]
            }
            else {
                gs3p <- NULL
            }
            if (isTRUE(length(c(gs2p, gs3p)) > 0) == TRUE) {
                xxp <- strsplit(x1p, "")[[1]]
                xxpp <- xxp[min(c(gs2p, gs3p)):length(xxp)]
                x1pp <- paste(xxpp, collapse = "")
                gs2pp <- which(suppressWarnings(as.raw(utf8ToInt(x1pp))) %in% 
                  c("cf", "ce"))
                gs3pp <- which(suppressWarnings(as.raw(utf8ToInt(x1pp))) %in% 
                  c("e1"))
                if (isTRUE(length(gs3pp) > 0) == TRUE) {
                  gs3pp <- gs3pp[which(gs3pp%%2 != 0)]
                }
                else {
                  gs3pp <- NULL
                }
                x3 <- strsplit(rawToChar(as.raw(utf8ToInt(x1pp))), 
                  "")[[1]]
                resp <- vector()
                for (i in sort(c(gs2pp, gs3pp))) {
                  if (isTRUE(i %in% c(gs2pp)) == TRUE) {
                    tmp <- paste(x3[i:(i + 1L)], collapse = "")
                    resp <- append(resp, iconv(iconv(tmp, from = "UTF-8", 
                      to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                      to = "UTF-8"))
                  }
                  else if (isTRUE(i %in% gs3pp) == TRUE) {
                    tmp <- paste(x3[i:(i + 2L)], collapse = "")
                    resp <- append(resp, iconv(iconv(tmp, from = "UTF-8", 
                      to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                      to = "UTF-8"))
                  }
                }
                rm(i)
            }
            else {
                resp <- res
            }
            if (all(c(" ", "/", " ") %in% strsplit(x, "")[[1]]) == 
                TRUE) {
                if (isTRUE(min(c(gs2, gs2a, gs3)) > 1) == TRUE) {
                  tmpr <- iconv(iconv(paste(xx[seq_len(min(c(gs2, 
                    gs2a, gs3)) - 1)], collapse = ""), from = "UTF-8", 
                    to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                    to = "UTF-8")
                  respl <- as.list(paste(c(tmpr, resp), collapse = ""))
                }
                else {
                  tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                    c(" ", "/", " "))) - 2):length(xx)], collapse = ""), 
                    from = "UTF-8", to = "UTF-16LE", toRaw = TRUE), 
                    from = "UTF-16LE", to = "UTF-8")
                  respl <- as.list(paste(c(resp, tmpr), collapse = ""))
                }
            }
            else {
                respl <- as.list(paste(resp, collapse = ""))
            }
            if (isTRUE(level < 1) == TRUE) {
                ifelse(isTRUE(flgp == TRUE) == TRUE, respl <- as.list(paste(c(respl, 
                  "+"), collapse = "")), NA)
                ifelse(isTRUE(flga == TRUE) == TRUE, respl <- as.list(paste(c(respl, 
                  "*"), collapse = "")), NA)
            }
            if (isTRUE(unlist == FALSE) == TRUE) {
                names(respl) <- xo
            }
            else {
                ifelse(isTRUE(flgvc == TRUE) == TRUE, respl <- unlist(respl, 
                  use.names = FALSE), NA)
            }
            if (missing(case) == FALSE && is.numeric(case) == 
                TRUE) {
                return(cs(respl, level = level, case = case, 
                  flgdf = flgdf, na.rm = na.rm))
            }
            else {
                return(respl)
            }
        }
    }
    else if (isTRUE(length(x) > 1) == TRUE) {
        if (isTRUE(typeof(x) == "list") == TRUE) {
            if (isTRUE("people" %in% names(x)) == TRUE) 
                warning("\"people\" and list of lists are not yet fully supported")
            x1 <- unlist(x, use.names = FALSE)
        }
        else {
            x1 <- as.vector(x)
        }
        resl <- vector("list", length = length(x1))
        for (k in seq_len(length(x1))) {
            xi <- as.character(x1[k])
            if (isTRUE(flgx == TRUE) == TRUE) {
                xx1 <- strsplit(xi, "")[[1]]
                if (isTRUE("<" %in% strsplit(xi, "")[[1]]) == 
                  TRUE && isTRUE(">" %in% strsplit(xi, "")[[1]]) == 
                  TRUE) {
                  ck <- which(strsplit(xi, "")[[1]] %in% "<")
                  x2 <- vector()
                  ifelse(isTRUE(ck[1] == 1) == TRUE, NA, x2 <- append(x2, 
                    xx1[1:ck[1] - 1L]))
                  for (i in seq_len(length(ck))) {
                    x2 <- append(x2, as.vector(dbe[which(names(dbe) %in% 
                      paste(xx1[(ck[i] + 4L):(ck[i] + 5L)], collapse = ""))]))
                    if (isTRUE(ck[i] == max(ck)) == TRUE) {
                      ifelse(isTRUE(tail(xx1, 1) == ">") == TRUE, 
                        NA, x2 <- append(x2, xx1[(ck[i] + 7L):length(xx1)]))
                    }
                    else {
                      x2 <- append(x2, xx1[(ck[i] + 7L):(ck[i + 
                        1L] - 1L)])
                    }
                  }
                  rm(i)
                  xi <- paste(x2, collapse = "")
                }
            }
            if (isTRUE(level > 1) == TRUE) {
                ifelse(isTRUE(level > 8) == TRUE, xi <- sub(" [(].*", 
                  "", xi), xi <- gsub("\\s*\\([^\\)]\\)", "", 
                  xi))
                for (w in seq_len(length(what))) {
                  xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                    "")[[1]] == what[w]))], collapse = "")
                }
                rm(w)
                xi <- paste(trimws(strsplit(xi, "/")[[1]]), collapse = "/")
            }
            else if (isTRUE(level == 1) == TRUE) {
                xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                  "")[[1]] == "?"))], collapse = "")
            }
            else {
                invisible(NA)
            }
            ifelse(isTRUE(level == 0) == TRUE, invisible(NA), 
                xi <- paste(strsplit(xi, "")[[1]][which(!(strsplit(xi, 
                  "")[[1]] == "?"))], collapse = ""))
            if (is.na(xi) == TRUE || all(strsplit(xi, "")[[1]] %in% 
                c(letters, LETTERS, " ", strsplit(intToUtf8(c(33:47, 
                  58:64, 91:96, c(228, 235, 239, 246, 252))), 
                  "")[[1]])) == TRUE) {
                resl[[k]] <- xi
            }
            else {
                utixi <- utf8ToInt(xi)
                if (isTRUE(is.na(utixi) == TRUE) == TRUE) {
                  flgna <- TRUE
                  utixi <- utf8ToInt(iconv(xi, "", "UTF-8"))
                }
                else {
                  flgna <- FALSE
                }
                if (isTRUE(any(utixi > 255) == TRUE) == TRUE) {
                  chk <- which(utixi > 255)
                  utix0 <- utixi
                  utix0[chk - 1L] <- utixi[chk - 1L] + 1L
                  utixi <- utix0[-chk]
                  flgc <- TRUE
                }
                else {
                  flgc <- FALSE
                }
                gs1 <- which(as.raw(utixi) %in% c("e2", "e4", 
                  "f6", "fc"))
                gs2 <- which(as.raw(utixi) %in% c("cf", "ce"))
                gs2a <- which(as.raw(utixi) %in% c("c2", "c3", 
                  "c4", "c5", "c8"))
                gs3 <- which(as.raw(utixi) %in% c("e1"))
                if (isTRUE(length(gs3) > 0) == TRUE) {
                  invisible(NA)
                }
                else {
                  gs3 <- NULL
                }
                if (isTRUE(length(c(gs1, gs2, gs2a, gs3)) == 
                  0) == TRUE) {
                  resl[[k]] <- xi
                }
                else {
                  xx <- strsplit(rawToChar(as.raw(utixi)), "")[[1]]
                  ifelse(isTRUE(tail(xx, 1) == "+") == TRUE, 
                    flgp <- TRUE, flgp <- FALSE)
                  ifelse(isTRUE(tail(xx, 1) == "*") == TRUE, 
                    flga <- TRUE, flga <- FALSE)
                  gsx <- sort(c(gs1, gs2, gs2a, gs3))
                  res <- vector()
                  if (isTRUE(min(c(gs1, gs2, gs2a, gs3)) > 1) == 
                    TRUE) {
                    res <- append(res, xx[1:(min(gsx) - 1)])
                  }
                  else {
                    invisible(NA)
                  }
                  for (j in seq_along(gsx)) {
                    i <- gsx[j]
                    if (isTRUE(i %in% c(gs2, gs2a)) == TRUE) {
                      tmp <- paste(xx[i:(i + 1L)], collapse = "")
                      res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                        to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                        to = "UTF-8"))
                      if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                        collapse = ""))) %in% c(c("cf", "ce"), 
                        c("c2", "c3"), c("e1")))) == FALSE && 
                        suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                          0) {
                        if (isTRUE(i < (length(xx) - 1L)) == 
                          TRUE) {
                          ifelse(isTRUE(j == length(gsx)) == 
                            TRUE, res <- append(res, xx[(i + 
                            2L):length(xx)]), res <- append(res, 
                            xx[(i + 2L):(gsx[j + 1L] - 1L)]))
                        }
                        else {
                          invisible(NA)
                        }
                      }
                    }
                    else if (isTRUE(i %in% gs3) == TRUE) {
                      tmp <- paste(xx[i:(i + 2L)], collapse = "")
                      res <- append(res, iconv(iconv(tmp, from = "UTF-8", 
                        to = "UTF-16LE", toRaw = TRUE), from = "UTF-16LE", 
                        to = "UTF-8"))
                      if (suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                        collapse = ""))) %in% c(c("cf", "ce"), 
                        c("c2", "c3"), c("e1")))) == FALSE && 
                        suppressWarnings(as.raw(utf8ToInt(tmp))) != 
                          0) {
                        if (isTRUE(i < (length(xx) - 1L)) == 
                          TRUE) {
                          ifelse(isTRUE(j == length(gsx)) == 
                            TRUE, res <- append(res, xx[(i + 
                            3L):length(xx)]), res <- append(res, 
                            xx[(i + 3L):(gsx[j + 1L] - 1L)]))
                        }
                        else {
                          invisible(NA)
                        }
                      }
                    }
                    if (isTRUE(flgc == TRUE) == TRUE || (isTRUE(j < 
                      max(seq_along(gsx))) == TRUE && isTRUE(length(c(gs2, 
                      gs3)) == 0) == TRUE)) {
                      if (isTRUE((i + 2L) < gsx[j + 1]) == TRUE) {
                        res <- append(res, xx[(i + 2L):(gsx[j + 
                          1L] - 1L)])
                      }
                      else {
                        invisible(NA)
                      }
                    }
                    else if (isTRUE(length(gsx) == 1) == TRUE) {
                      res <- append(res, xx[(i + 2L):length(xx)])
                    }
                  }
                  rm(j)
                  if (isTRUE(flgc == TRUE) == TRUE) {
                    if (isTRUE((length(res) + (length(chk) * 
                      2L)) < length(xx)) == TRUE) {
                      res <- append(res, xx[(length(res) + (length(chk) * 
                        2L)):length(xx)])
                    }
                    else {
                      invisible(NA)
                    }
                  }
                  else if ((isTRUE(max(gsx) == length(res)) == 
                    TRUE) && (any(is.na(xx[(length(res) + (max(gsx))):length(xx)])) == 
                    FALSE)) {
                    res <- append(res, xx[(length(res) + (max(gsx))):length(xx)])
                  }
                  if (isTRUE(length(c(gs2, gs2a, gs3)) > 0) == 
                    TRUE) {
                    if ("+" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
                      1)]) {
                      res <- append(res, "+")
                    }
                    else if ("*" %in% xx[max(c(gs2, gs2a, gs3)):(length(xx) - 
                      1)]) {
                      res <- append(res, "*")
                    }
                  }
                  if (isTRUE(length(gs2a) > 0) == TRUE && suppressWarnings(any(as.raw(utf8ToInt(paste(res, 
                    collapse = ""))) %in% c(c("cf", "ce"), c("c2", 
                    "c3"), c("e1"))) == TRUE)) {
                    xj <- paste(res, collapse = "")
                    gs2p <- which(suppressWarnings(as.raw(utf8ToInt(xj))) %in% 
                      c("cf", "ce"))
                    gs3p <- which(suppressWarnings(as.raw(utf8ToInt(xj))) %in% 
                      c("e1"))
                    if (isTRUE(length(gs3p) > 0) == TRUE) {
                      gs3p <- gs3p[which(gs3p%%2 != 0)]
                    }
                    else {
                      gs3p <- NULL
                    }
                    if (isTRUE(length(c(gs2p, gs3p)) == 0) == 
                      TRUE) {
                      resl[[k]] <- xj
                    }
                    else {
                      xxp <- strsplit(xj, "")[[1]]
                      xxpp <- xxp[min(c(gs2p, gs3p)):length(xxp)]
                      x1pp <- paste(xxpp, collapse = "")
                      gs2pp <- which(suppressWarnings(as.raw(utf8ToInt(x1pp))) %in% 
                        c("cf", "ce"))
                      gs3pp <- which(suppressWarnings(as.raw(utf8ToInt(x1pp))) %in% 
                        c("e1"))
                      if (isTRUE(length(gs3pp) > 0) == TRUE) {
                        gs3pp <- gs3pp[which(gs3pp%%2 != 0)]
                      }
                      else {
                        gs3pp <- NULL
                      }
                      if (isTRUE(length(c(gs2pp, gs3pp)) > 0) == 
                        TRUE && isTRUE(any(utf8ToInt(x1pp) > 
                        255) == FALSE) == TRUE) {
                        x3 <- strsplit(rawToChar(as.raw(utf8ToInt(x1pp))), 
                          "")[[1]]
                        res <- vector()
                        for (i in sort(c(gs2pp, gs3pp))) {
                          if (isTRUE(i %in% c(gs2pp)) == TRUE) {
                            tmp <- paste(x3[i:(i + 1L)], collapse = "")
                            res <- append(res, iconv(iconv(tmp, 
                              from = "UTF-8", to = "UTF-16LE", 
                              toRaw = TRUE), from = "UTF-16LE", 
                              to = "UTF-8"))
                          }
                          else if (isTRUE(i %in% c(gs3p, gs3pp)) == 
                            TRUE) {
                            tmp <- paste(x3[i:(i + 2L)], collapse = "")
                            res <- append(res, iconv(iconv(tmp, 
                              from = "UTF-8", to = "UTF-16LE", 
                              toRaw = TRUE), from = "UTF-16LE", 
                              to = "UTF-8"))
                          }
                          else {
                            invisible(NA)
                          }
                        }
                        rm(i)
                        if (isTRUE(flgna == TRUE) == TRUE && 
                          isTRUE(flgc == TRUE) == TRUE) {
                          if (isTRUE(which(strsplit(xi, "")[[1]] %in% 
                            strsplit(tmp, "")[[1]]) < length(utix0)) == 
                            TRUE) {
                            st <- which(strsplit(xi, "")[[1]] %in% 
                              strsplit(tmp, "")[[1]])
                            ed <- length(utix0)
                            resl[[k]] <- paste(c(res, strsplit(xi, 
                              "")[[1]][(st + 1):ed]), collapse = "")
                          }
                          else {
                            invisible(NA)
                          }
                        }
                        if (all(c(" ", "/", " ") %in% strsplit(xi, 
                          "")[[1]]) == TRUE) {
                          if (isTRUE(length(c(gs2, gs2a, gs3)) > 
                            0) == TRUE && isTRUE(min(c(gs2, gs2a, 
                            gs3)) > 1) == TRUE) {
                            tmpr <- NULL
                            if (isTRUE(flgp == TRUE) == TRUE && 
                              isTRUE(level < 2) == TRUE) {
                              resl[[k]] <- paste(c(tmpr, res, 
                                "+"), collapse = "")
                            }
                            else if (isTRUE(flga == TRUE) == 
                              TRUE && isTRUE(level < 2) == TRUE) {
                              resl[[k]] <- paste(c(tmpr, res, 
                                "*"), collapse = "")
                            }
                            else {
                              resl[[k]] <- paste(c(tmpr, res), 
                                collapse = "")
                            }
                          }
                          else {
                            tmpr <- iconv(iconv(paste(xx[(max(which(xx %in% 
                              c(" ", "/", " "))) - 2):length(xx)], 
                              collapse = ""), from = "UTF-8", 
                              to = "UTF-16LE", toRaw = TRUE), 
                              from = "UTF-16LE", to = "UTF-8")
                            if (isTRUE(flgp == TRUE) == TRUE && 
                              isTRUE(level < 2) == TRUE) {
                              resl[[k]] <- paste(c(res, tmpr, 
                                "+"), collapse = "")
                            }
                            else if (isTRUE(flga == TRUE) == 
                              TRUE && isTRUE(level < 2) == TRUE) {
                              resl[[k]] <- paste(c(res, tmpr, 
                                "*"), collapse = "")
                            }
                            else {
                              resl[[k]] <- paste(c(res, tmpr), 
                                collapse = "")
                            }
                          }
                        }
                        else {
                          if (isTRUE(flgp == TRUE) == TRUE && 
                            isTRUE(level < 2) == TRUE) {
                            resl[[k]] <- paste(c(res, "+"), collapse = "")
                          }
                          else if (isTRUE(flga == TRUE && isTRUE(level < 
                            2) == TRUE) == TRUE) {
                            resl[[k]] <- paste(c(res, "*"), collapse = "")
                          }
                          else {
                            resl[[k]] <- paste(res, collapse = "")
                          }
                        }
                      }
                      else {
                        resl[[k]] <- paste(res, collapse = "")
                      }
                    }
                  }
                  else {
                    if (all(c(" ", "/", " ") %in% strsplit(xi, 
                      "")[[1]]) == TRUE) {
                      if (isTRUE(length(c(gs2, gs2a, gs3)) > 
                        0) == TRUE && isTRUE(min(c(gs2, gs2a, 
                        gs3)) > 1) == TRUE) {
                        tmpr <- NULL
                        if (isTRUE(flgp == TRUE) == TRUE && isTRUE(level < 
                          2) == TRUE) {
                          resl[[k]] <- paste(c(tmpr, res, "+"), 
                            collapse = "")
                        }
                        else if (isTRUE(flga == TRUE && isTRUE(level < 
                          2) == TRUE) == TRUE) {
                          resl[[k]] <- paste(c(tmpr, res, "*"), 
                            collapse = "")
                        }
                        else {
                          resl[[k]] <- paste(c(tmpr, res), collapse = "")
                        }
                      }
                      else {
                        tmpr <- iconv(iconv(paste(xx[(which(xx %in% 
                          c("/")) - 1):length(xx)], collapse = ""), 
                          from = "UTF-8", to = "UTF-16LE", toRaw = TRUE), 
                          from = "UTF-16LE", to = "UTF-8")
                        resl[[k]] <- paste(c(res, tmpr), collapse = "")
                      }
                    }
                    else {
                      if (isTRUE(flgp == TRUE) == TRUE && isTRUE(level < 
                        2) == TRUE) {
                        resl[[k]] <- paste(c(res, "+"), collapse = "")
                      }
                      else if (isTRUE(flga == TRUE) == TRUE && 
                        isTRUE(level < 2) == TRUE) {
                        resl[[k]] <- paste(c(res, "*"), collapse = "")
                      }
                      else {
                        resl[[k]] <- paste(res, collapse = "")
                      }
                    }
                  }
                }
            }
        }
        rm(k)
        if (isTRUE(length(what) > 3) == TRUE && isTRUE(level > 
            1) == TRUE) {
            for (i in seq(4, length(what))) {
                resl <- rapply(resl, function(x) ifelse(x == 
                  what[i], NA, x), how = "replace")
            }
            rm(i)
        }
        else {
            NA
        }
        if (is.null(names(x[[1]])) == TRUE) {
            resll <- resl
            names(resll) <- x1
        }
        else {
            n <- length(names(x[[1]]))
            sts <- seq(1, length(resl), by = n)
            resll <- list()
            for (k in seq_along(seq(1, length(resl), by = n))) {
                tmp <- resl[sts[k]:(sts[k] + n - 1)]
                names(tmp) <- names(x[[1]])
                resll[length(resll) + 1] <- list(tmp)
            }
            rm(k)
        }
        if (isTRUE(flgdf == TRUE) == TRUE) {
            if (isTRUE(flgx == TRUE) == TRUE || any(xx1 %in% 
                dbe) == TRUE) {
                Sys.setlocale(category = "LC_ALL", locale = ".1250")
                resdf <- noquote(matrix(unlist(resll), ncol = ncol(xdf), 
                  byrow = FALSE, dimnames = list(rownames(xdf), 
                    colnames(xdf))), right = TRUE)
            }
            else {
                resdf <- data.frame(matrix(unlist(resll), ncol = ncol(xdf), 
                  byrow = FALSE, dimnames = list(rownames(xdf), 
                    colnames(xdf))), check.names = FALSE, stringsAsFactors = FALSE)
            }
            if (missing(repl) == FALSE) {
                if ((is.data.frame(repl) == FALSE | isTRUE(ncol(repl) < 
                  2) == TRUE) && is.vector(repl) == FALSE) {
                  warning("'repl' must be a data frame with two columns or a 2-length vector.")
                  invisible(NA)
                }
                else {
                  if (is.vector(repl) == TRUE && is.data.frame(repl) == 
                    FALSE) {
                    resdf <- as.data.frame(mapply(gsub, repl[1], 
                      repl[2], resdf, USE.NAMES = FALSE), stringsAsFactors = FALSE)
                  }
                  else {
                    for (i in seq_len(nrow(repl))) {
                      resdf <- as.data.frame(mapply(gsub, repl[i, 
                        1], repl[i, 2], resdf, USE.NAMES = FALSE), 
                        stringsAsFactors = FALSE)
                    }
                    rm(i)
                  }
                  rownames(resdf) <- rownames(xdf)
                  colnames(resdf) <- colnames(xdf)
                }
            }
            is.na(resdf) <- resdf == "NULL"
            resdf[resdf == ""] <- NA
            if (missing(case) == FALSE && is.numeric(case) == 
                TRUE) {
                return(cs(resdf, level = level, case = case, 
                  flgdf = flgdf, na.rm = na.rm))
            }
            else {
                ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == 
                  TRUE) == TRUE, return(resdf[complete.cases(resdf), 
                  ]), return(resdf))
            }
        }
        else if (isTRUE(flgdf == FALSE) == TRUE) {
            if (missing(repl) == FALSE) {
                if (is.vector(repl) == TRUE && is.data.frame(repl) == 
                  FALSE) {
                  resll <- as.data.frame(mapply(gsub, repl[1], 
                    repl[2], resll, USE.NAMES = FALSE), stringsAsFactors = FALSE)
                }
                else {
                  resdfl <- data.frame(matrix(unlist(resll), 
                    ncol = 1, byrow = FALSE), check.names = FALSE, 
                    stringsAsFactors = FALSE)
                  for (i in seq_len(nrow(repl))) {
                    resdfl <- as.data.frame(mapply(gsub, repl[i, 
                      1], repl[i, 2], resdfl, USE.NAMES = FALSE), 
                      stringsAsFactors = FALSE)
                  }
                  rm(i)
                  resll <- as.vector(resdfl)
                }
            }
            if (isTRUE(unlist == FALSE) == TRUE) {
                names(resll) <- xo
            }
            else {
                resll <- unlist(resll, use.names = FALSE)
            }
            if (missing(case) == FALSE && is.numeric(case) == 
                TRUE) {
                return(cs(resll, level = level, case = case, 
                  flgdf = flgdf, na.rm = na.rm))
            }
            else {
                resll <- lapply(resll, function(z) {
                  ifelse(isTRUE(z == "") == TRUE, z <- NA, NA)
                  return(z)
                })
                if (isTRUE(unlist == FALSE) == TRUE) {
                  names(resll) <- xo
                }
                else {
                  ifelse(isTRUE(flgvc == TRUE) == TRUE, resll <- unlist(resll, 
                    use.names = FALSE), NA)
                }
                if (isTRUE(na.rm == TRUE) == TRUE) {
                  return(resll[which(!(is.na(resll)))])
                }
                else {
                  return(resll)
                }
            }
        }
    }
}
