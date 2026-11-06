## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE---------------------------------------------------------------
# library(rladybugdb)
# 
# db <- lb_database("extensions.lbug")
# conn <- lb_connection(db)
# on.exit({
#   lb_close(conn)
#   lb_close(db)
# }, add = TRUE)
# 
# lb_list_extensions(conn)
# lb_close(lb_load_extension(conn, "algo"))

## ----eval=FALSE---------------------------------------------------------------
# lb_close(lb_execute(
#   conn,
#   "CALL project_graph('SocialGraph', ['Person'], ['KNOWS'])"
# ))
# 
# page_rank <- lb_query(
#   conn,
#   paste(
#     "CALL page_rank('SocialGraph')",
#     "RETURN node.name AS name, rank AS pagerank",
#     "ORDER BY rank DESC"
#   )
# )
# 
# communities <- lb_query(
#   conn,
#   paste(
#     "CALL louvain('SocialGraph')",
#     "RETURN louvain_id, collect(node.name) AS members",
#     "ORDER BY louvain_id"
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# lb_close(lb_load_extension(conn, "fts"))
# lb_close(lb_execute(
#   conn,
#   paste0(
#     "CALL CREATE_FTS_INDEX(",
#     "'Book', 'book_index', ['title', 'abstract'], stemmer := 'porter')"
#   )
# ))
# 
# hits <- lb_query(
#   conn,
#   paste(
#     "CALL QUERY_FTS_INDEX('Book', 'book_index', $query, top := 10)",
#     "RETURN node.title AS title, score ORDER BY score DESC"
#   ),
#   parameters = list(query = "quantum machine")
# )

## ----eval=FALSE---------------------------------------------------------------
# lb_close(lb_load_extension(conn, "vector"))
# lb_close(lb_execute(
#   conn,
#   paste0(
#     "CALL CREATE_VECTOR_INDEX(",
#     "'Book', 'book_title_index', 'title_embedding', metric := 'cosine')"
#   )
# ))
# 
# nearest <- lb_query(
#   conn,
#   paste(
#     "CALL QUERY_VECTOR_INDEX('Book', 'book_title_index', $embedding, 5)",
#     "RETURN node.title AS title, distance ORDER BY distance"
#   ),
#   parameters = list(embedding = as.numeric(query_embedding))
# )

## ----eval=FALSE---------------------------------------------------------------
# for (extension in c("duckdb", "postgres", "sqlite")) {
#   lb_close(lb_load_extension(conn, extension))
# }
# 
# lb_close(lb_execute(conn, "ATTACH 'warehouse.duckdb' AS wh (dbtype duckdb)"))
# duck_rows <- lb_query(conn, "LOAD FROM wh.events RETURN * LIMIT 100")
# 
# lb_close(lb_execute(
#   conn,
#   "ATTACH 'host=localhost dbname=app' AS pg (dbtype postgres)"
# ))
# postgres_rows <- lb_query(conn, "LOAD FROM pg.public.events RETURN * LIMIT 100")
# 
# lb_close(lb_execute(conn, "ATTACH 'app.sqlite' AS sq (dbtype sqlite)"))
# sqlite_rows <- lb_query(conn, "LOAD FROM sq.events RETURN * LIMIT 100")

## ----eval=FALSE---------------------------------------------------------------
# parquet_rows <- lb_query(conn, "LOAD FROM 'events/*.parquet' RETURN *")
# 
# lb_close(lb_load_extension(conn, "iceberg"))
# # Configure the catalog according to the LadybugDB Iceberg documentation,
# # then scan the attached catalog.table with LOAD FROM.
# 
# lb_close(lb_load_extension(conn, "adbc"))
# # Install the database vendor's ADBC driver, attach it using that driver's
# # connection options, then query its tables with LOAD FROM.

