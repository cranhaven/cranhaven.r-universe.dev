encode_xml2 <- function(txt) {
  txt %>%
    str_replace_all(intToUtf8(227), "a") %>%
    str_replace_all(intToUtf8(225), "a") %>%
    str_replace_all(intToUtf8(224), "a") %>%
    str_replace_all(intToUtf8(228), "a") %>%
    str_replace_all(intToUtf8(226), "a") %>%

    str_replace_all(intToUtf8(196), "A") %>%
    str_replace_all(intToUtf8(193), "A") %>%
    str_replace_all(intToUtf8(192), "A") %>%
    str_replace_all(intToUtf8(195), "A") %>%
    str_replace_all(intToUtf8(194), "A") %>%

    str_replace_all(intToUtf8(233), "e") %>%
    str_replace_all(intToUtf8(234), "e") %>%
    str_replace_all(intToUtf8(201), "E") %>%
    str_replace_all(intToUtf8(202), "E") %>%

    str_replace_all(intToUtf8(237), "i") %>%
    str_replace_all(intToUtf8(205), "I") %>%

    str_replace_all(intToUtf8(245), "o") %>%
    str_replace_all(intToUtf8(243), "o") %>%
    str_replace_all(intToUtf8(246), "o") %>%
    str_replace_all(intToUtf8(244), "o") %>%

    str_replace_all(intToUtf8(213), "O") %>%
    str_replace_all(intToUtf8(211), "O") %>%
    str_replace_all(intToUtf8(214), "O") %>%
    str_replace_all(intToUtf8(212), "O") %>%

    str_replace_all(intToUtf8(250), "u") %>%
    str_replace_all(intToUtf8(252), "u") %>%
    str_replace_all(intToUtf8(218), "U") %>%
    str_replace_all(intToUtf8(220), "U") %>%

    str_replace_all(intToUtf8(231), "c") %>%
    str_replace_all(intToUtf8(199), "C") %>%

    str_replace_all(intToUtf8(241), "n") %>%
    str_replace_all(intToUtf8(209), "N")
}
