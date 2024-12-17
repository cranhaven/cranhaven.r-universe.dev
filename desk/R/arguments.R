arguments = function (fun, width = options("width")$width) {
    out = paste(deparse(args(fun))[-length(deparse(args(fun)))],
        collapse = "", sep = "--")
    out = gsub("function (", "", out, fixed = TRUE)
    out = gsub(") ", "", out, fixed = TRUE)
    out = gsub("     ", " ", out, fixed = TRUE)
    if (out == "")
        stop("Arguments of function are not well specified")
    if (grepl("\\.\\.\\.", out) == 1 & regexpr("\\.\\.\\.", out) ==
        1) {
        out = gsub("\\.\\.\\., ", "", out)
        out = eval(parse(text = paste0("alist(", out, ")")))
        id = sapply(out, is.character)
        fx = as.character(out)
        fx[id] = paste0("\"", fx[id], "\"")
        o = paste0(names(out), ifelse(names(out) != "", " = ",
            ""), fx)
        o = c("...", o)
    }else if (grepl("\\.\\.\\.", out) == 1 & regexpr("\\.\\.\\.",
        out) != 1) {
        out = gsub(", \\.\\.\\.", " ", out)
        out = eval(parse(text = paste0("alist(", out, ")")))
        id = sapply(out, is.character)
        fx = as.character(out)
        fx[id] = paste0("\"", fx[id], "\"")
        o = paste0(names(out), ifelse(names(out) != "", " = ",
            ""), fx)
        o = c(o, "...")
    }else {
        out = eval(parse(text = paste0("alist(", out, ")")))
        id = sapply(out, is.character)
        fx = as.character(out)
        fx[id] = paste0("\"", fx[id], "\"")
        o = paste0(names(out), ifelse(names(out) != "", " = ",
            ""), fx)
    }
    ix = nchar(o)
    br.upper = width * 0.8
    br.lower = width * 0.5

    repeat{
        int = cut(cumsum(ix), seq(0, 1000, br.upper))
        lx = table(int) != 0
        l = unlist(lapply(1:length(levels(int)), function(x) paste(paste(c(o[int ==
            levels(int)[x]], ""), collapse = ", "), "\n")))
        l[length(l[lx])] = gsub(",  \n", "", l[length(l[lx])],
            fixed = TRUE)

        #         if(any(nchar(l[lx]) > width)) next
        if (length(l[lx]) == 1 | length(l[lx]) == 2) break
        m = sd(sapply((l[lx])[-length(l[lx])], nchar))
        if(m < 10) break
        br.upper = br.upper - 1
        if (br.lower >= br.upper)
            break
    }
    cat(l[lx])
}
