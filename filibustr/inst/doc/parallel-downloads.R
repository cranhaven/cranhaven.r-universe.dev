## ----call library-------------------------------------------------------------
library(filibustr)


## ----check for parallel packages----------------------------------------------
rlang::check_installed(c("carrier", "mirai"), version = c("0.3.0",  "2.5.1"))


## ----set parallel processes---------------------------------------------------
# detect the number of cores available on your machine
parallel::detectCores()

# launch a specific number of processes, or
mirai::daemons(4)
# launch a process on all but one available cores
mirai::daemons(parallel::detectCores() - 1)


## ----download Voteview data---------------------------------------------------
# download Voteview data from multiple Congresses
get_voteview_members(congress = 95:118)

get_voteview_rollcall_votes(congress = 95:118)


## ----close daemons------------------------------------------------------------
mirai::daemons(0)

