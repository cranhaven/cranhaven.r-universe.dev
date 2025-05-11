list_1 <- list(a = 1, b = 2:5)
list_2 <- list(b = 6, c = 7, d = 8, override = list(x = 101))
list_3 <- list(override = list(x = 102), e = 9)
list_4 <- list(override = list(y = 103), e = 10)

comb_list <- distfreereg:::combine_dfr_arg_lists(list_1, list_2, list_3, list_4)

cl <- list(a = 1, b = 2:5, c = 7, d = 8, e = 9, override = list(x = 101, y = 103))

message('identical(comb_list, cl) (should be TRUE): ', identical(comb_list, cl))
