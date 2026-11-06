## ----setup, include=FALSE-----------------------------------------------------
library(rladybugdb)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
db <- lb_database(":memory:")
conn <- lb_connection(db)

## -----------------------------------------------------------------------------
with_lb_connection(":memory:", {
  ladybugdb_version()
})

## -----------------------------------------------------------------------------
lb_close(lb_execute(
  conn,
  "CREATE NODE TABLE Person (name STRING, age INT64, PRIMARY KEY(name))"
))
lb_close(lb_execute(
  conn,
  "CREATE NODE TABLE City (name STRING, PRIMARY KEY(name))"
))
lb_close(lb_execute(
  conn,
  "CREATE REL TABLE LivesIn (FROM Person TO City, since INT64)"
))

lb_close(lb_execute(
  conn,
  "CREATE (:Person {name: $name, age: $age})",
  parameters = list(name = "Alice", age = 30L)
))
lb_close(lb_execute(conn, "CREATE (:City {name: 'London'})"))
lb_close(lb_execute(
  conn,
  paste(
    "MATCH (p:Person {name: 'Alice'}), (c:City {name: 'London'})",
    "CREATE (p)-[:LivesIn {since: 2018}]->(c)"
  )
))

lb_query(
  conn,
  paste(
    "MATCH (p:Person)-[r:LivesIn]->(c:City)",
    "RETURN p.name AS person, p.age AS age, c.name AS city, r.since AS since"
  )
)

## -----------------------------------------------------------------------------
result <- lb_execute(
  conn,
  "UNWIND range(1, 25) AS id RETURN id, 'row ' + CAST(id AS STRING) AS label"
)

print(result, n = 3)
first <- lb_fetch(result, n = 10)
second <- lb_fetch(result, n = 10)
lb_result_info(result)
lb_query_summary(result)

lb_reset(result)
identical(as.data.frame(result), as.data.frame(result))
lb_close(result)

## ----eval=requireNamespace("arrow", quietly=TRUE)-----------------------------
result <- lb_execute(conn, "MATCH (p:Person) RETURN p.name AS name, p.age AS age")
arrow_table <- as_arrow_table(result)
lb_close(result)
arrow_table

## ----eval=requireNamespace("arrow", quietly=TRUE)-----------------------------
lb_close(lb_execute(
  conn,
  "CREATE NODE TABLE Event (id INT64, note STRING, PRIMARY KEY(id))"
))
copy_result <- lb_copy_from_df(
  conn,
  data.frame(id = c(1, 2), note = c("", NA_character_)),
  "Event"
)
lb_close(copy_result)
lb_query(conn, "MATCH (e:Event) RETURN e.* ORDER BY e.id")

## -----------------------------------------------------------------------------
dbi_conn <- DBI::dbConnect(Ladybug(), dbname = ":memory:")
DBI::dbGetQuery(dbi_conn, "RETURN $value AS value", params = list(value = 42L))
DBI::dbDisconnect(dbi_conn)

## ----eval=requireNamespace("igraph", quietly=TRUE)----------------------------
graph_result <- lb_execute(
  conn,
  "MATCH (p:Person)-[r:LivesIn]->(c:City) RETURN p, r, c"
)
graph <- as_igraph(graph_result)
lb_close(graph_result)
graph

## -----------------------------------------------------------------------------
lb_close(conn)
lb_close(db)

