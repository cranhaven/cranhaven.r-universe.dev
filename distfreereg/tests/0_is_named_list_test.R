distfreereg:::is_named_list(c(1, 2))# FALSE
distfreereg:::is_named_list(c(a = 1, b = 2))# FALSE
distfreereg:::is_named_list(list("hi"))# FALSE
distfreereg:::is_named_list(list("hi", b = 4))# FALSE
distfreereg:::is_named_list(list())# TRUE
distfreereg:::is_named_list(list(a = 1, b = 2))# TRUE