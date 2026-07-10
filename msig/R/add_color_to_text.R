
add_color_to_text <- function(text,key,color){
    # text is one vector
    # key is one vector
    # color is one vector
    # length key == length color
    names(color) <- tolower(key)
    from <- lapply(key, function(i) stringr::str_extract_all(text,
                                                             stringr::fixed(i,TRUE))) |>
        unlist() |>
        unique()
    if (length(from)==0) return(text)
    to <- sprintf('<span style="background-color:%s">%s</span>',
                  color[tolower(from)],from)
    names(to) <- from
    stringr::str_replace_all(text,to)
}
