## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE---------------------------------------------------------------
# library(rladybugdb)
# 
# db <- lb_database("analytics.lbug", read_only = TRUE)
# conn <- lb_connection(db)
# on.exit({
#   lb_close(conn)
#   lb_close(db)
# }, add = TRUE)
# 
# lb_query(conn, "CALL SHOW_TABLES() RETURN *")
# lb_query(conn, "MATCH (n) RETURN label(n) AS label, count(*) AS rows")

## ----eval=FALSE---------------------------------------------------------------
# old_db <- lb_database("old.lbug")
# old_conn <- lb_connection(old_db)
# lb_close(lb_execute(old_conn, "EXPORT DATABASE 'migration-export'"))
# lb_close(old_conn)
# lb_close(old_db)

## ----eval=FALSE---------------------------------------------------------------
# new_db <- lb_database("new.lbug")
# new_conn <- lb_connection(new_db)
# on.exit({
#   lb_close(new_conn)
#   lb_close(new_db)
# }, add = TRUE)
# lb_close(lb_execute(new_conn, "IMPORT DATABASE 'migration-export'"))

