list_1 <- list(0, a = 1, b = 2)
list_2 <- list(b = 3, c = 4, d = 5, 9)

# Note that the unnamed elements are treated as if they shared a name; the
# earliest-occurring one is put in the returned list (without a name).
comb_list <- distfreereg:::combine_lists(list_1, list_2)

cl <- list(0, a = 1, b = 2, c = 4, d = 5)

message('identical(comb_list, cl) (should be TRUE): ', identical(comb_list, cl))
