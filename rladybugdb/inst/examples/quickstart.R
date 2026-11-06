library(rladybugdb)

with_lb_connection(":memory:", {
  lb_close(lb_execute(
    conn,
    "CREATE NODE TABLE Person (id INT64, name STRING, PRIMARY KEY(id))"
  ))
  lb_close(lb_execute(
    conn,
    "CREATE (:Person {id: $id, name: $name})",
    parameters = list(id = 1L, name = "Ada")
  ))

  lb_query(conn, "MATCH (p:Person) RETURN p.id AS id, p.name AS name")
})
