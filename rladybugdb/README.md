# rladybugdb

> Native R access to LadybugDB, an embedded columnar graph database with
> Cypher queries.

[![R ≥ 4.1](https://img.shields.io/badge/R-%E2%89%A54.1-276DC3?logo=r)](https://cran.r-project.org)
[![LadybugDB 0.20.4](https://img.shields.io/badge/LadybugDB-0.20.4-e63946)](https://github.com/LadybugDB/ladybug)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

`rladybugdb` binds directly to the
[LadybugDB](https://github.com/LadybugDB/ladybug) C API through Rcpp. LadybugDB,
formerly known as Kuzu, runs in the R process: no Python or server is needed.
The package supports the engine's Cypher query language without claiming
conformance beyond the behavior tested here.

## Installation

```r
# install.packages("remotes")
remotes::install_github("hadimaster65555/rladybugdb")
```

The source package contains the pinned LadybugDB 0.20.4 source code. During
installation, the configure script verifies that source archive and compiles a
static library with R's own C++20 toolchain. No executable code or libraries
are downloaded during installation.

Installing therefore needs a C++20 compiler and CMake 3.15 or newer. The engine
is about 1,050 translation units, which the configure script compiles with at
most two parallel jobs, so plan for roughly 15 to 25 minutes for a first
install and a few GB of free space in the temporary build directory. The
installed package itself is small; the intermediate static library is large
because it carries the debug information R's default flags request. Upgrades
rebuild the engine, so they cost the same.

```r
library(rladybugdb)
ladybugdb_version()
#> [1] "0.20.4"
```

## Quick start

```r
library(rladybugdb)

local({
db <- lb_database(":memory:") # use a file path for a persisted database
conn <- lb_connection(db)
on.exit({
  lb_close(conn)
  lb_close(db)
}, add = TRUE)

lb_close(lb_execute(
  conn,
  "CREATE NODE TABLE Person (name STRING, age INT64, PRIMARY KEY(name))"
))
lb_close(lb_execute(
  conn,
  "CREATE NODE TABLE City (name STRING, country STRING, PRIMARY KEY(name))"
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
lb_close(lb_execute(conn, "CREATE (:City {name: 'London', country: 'UK'})"))
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
    "MATCH (p:Person)-[:LivesIn]->(c:City)",
    "RETURN p.name AS person, c.name AS city, p.age AS age"
  )
)
#>   person   city age
#> 1  Alice London  30
})
```

`with_lb_connection()` is a compact alternative when a connection should be
scoped to one expression.

## Query results and streaming

`lb_execute()` returns an explicit result object. Materializing or printing it
does not consume its streaming cursor, and result objects can be closed more
than once safely.

```r
result <- lb_execute(conn, "UNWIND range(1, 100000) AS id RETURN id")
on.exit(lb_close(result), add = TRUE)

print(result, n = 5)          # bounded, non-consuming preview
first <- lb_fetch(result, 1000)
second <- lb_fetch(result, 1000)
lb_reset(result)
info <- lb_result_info(result)
timing <- lb_query_summary(result)
```

`lb_next_result()` exposes subsequent results from a multi-statement query.
`lb_set_timeout()` and `lb_interrupt()` control long-running work, while
`lb_begin()`, `lb_commit()`, and `lb_rollback()` provide transaction control.

## Arrow and bulk loading

When `arrow` is installed, results move through the Arrow C Data Interface
without first becoming an R data frame. Fetching can be bounded by batch size.

```r
table <- as_arrow_table(result)
batch <- lb_fetch_arrow(result, n = 10000)
```

`lb_copy_from_arrow()` registers an Arrow object as an in-memory LadybugDB
table and copies it into an existing node table. `lb_copy_from_df()` uses this
path when Arrow is available and warns before using its lower-fidelity CSV
fallback. `lb_copy_from_csv()` remains the explicit file-oriented loader.
Identifiers, file paths, delimiters, and COPY options are validated and quoted.

## DBI

The package retains its graph-specific `lb_*` interface and also provides a
DBI backend.

```r
local({
dbi_conn <- DBI::dbConnect(Ladybug(), dbname = ":memory:", bigint = "character")
on.exit(DBI::dbDisconnect(dbi_conn), add = TRUE)

DBI::dbExecute(
  dbi_conn,
  "CREATE NODE TABLE Item (id INT64, name STRING, PRIMARY KEY(id))"
)
DBI::dbGetQuery(dbi_conn, "RETURN $id AS id", params = list(id = 1L))
})
```

DBI table methods operate on LadybugDB node and relationship tables. Graph
schema and traversal remain available directly through Cypher. This is a
Cypher backend, so SQL `SELECT` strings and relational `dbWriteTable()` /
`dbAppendTable()` assumptions do not apply; create graph tables with Cypher and
load them with the `lb_copy_*()` functions. LadybugDB does not currently expose
a stable affected-row count through its C result summary, so `dbExecute()`
returns `0` after successful DDL/DML. The compatible DBI driver contract is
covered by a focused DBItest subset.

## Type mapping

| LadybugDB type | Default R representation |
|---|---|
| BOOL | `logical` |
| INT8 / INT16 / INT32 | `integer` |
| INT64 / SERIAL | `double` |
| UINT32 / FLOAT / DOUBLE | `double` |
| INT128 / DECIMAL | exact `character` |
| STRING / UUID / JSON | `character` |
| DATE | `Date` |
| TIMESTAMP variants | `POSIXct` in UTC |
| BLOB | a `raw` vector in a list column |
| INTERVAL | structured `lb_interval` value |
| LIST / ARRAY | list column |
| MAP / STRUCT / UNION | structured list value |
| NODE / REL / RECURSIVE_REL | structured graph value |
| NULL | type-appropriate `NA` or `NULL` in a list column |

Set `options(rladybugdb.bigint = "character")` for exact character INT64 and
UINT64 results, or use `"integer64"` with the optional `bit64` package for
signed INT64. UINT64 remains character in `integer64` mode because `bit64` has
no unsigned representation. The default `"double"` mode is convenient but is
only exact through 2^53.

R parameters support logical, integer, double, character, factors, `Date`,
`POSIXct`, `bit64::integer64`, raw bytes, lists, and named lists. Every typed R
missing scalar binds as database `NULL`. Raw values use LadybugDB's documented
STRING-to-BLOB cast, so use them in a BLOB-typed context or `CAST($value AS
BLOB)`.

## Graph conversion and extensions

`as_igraph()` and `as_tbl_graph()` convert returned NODE, REL, and recursive
path values while preserving labels, properties, isolated nodes, and exact
internal identifiers.

The CRAN build can list installed extensions and load a compatible extension
that was installed outside R:

```r
lb_list_extensions(conn)

# After installing a compatible extension outside R:
load_result <- lb_load_extension(conn, "algo")
lb_close(load_result)
```

`lb_install_extension()` and `lb_update_extension()` report an unsupported
operation in the CRAN build because native remote-extension networking is not
linked.

See `vignette("extensions", package = "rladybugdb")` for PageRank, Louvain,
BM25 full-text search, HNSW vector search, and data interoperability examples.

## Stored databases and offline builds

LadybugDB 0.20.4 reads the storage format written by the previously bundled
0.15.x engine; this is covered by a committed compatibility fixture. Back up a
database before opening it with a new engine. See
`vignette("storage-migration", package = "rladybugdb")` for migration and
recovery steps.

Source-package builds are offline by default because the pinned LadybugDB
source is included under `tools/vendor`. Maintainers can refresh that archive
for a new pinned engine version with:

```r
Rscript tools/vendor.R
R CMD INSTALL .
```

The pinned version is stored in `tools/lbug_version`; source checksums are in
`tools/lbug_source_checksums`. The maintainer script verifies the upstream
source, applies the documented CRAN portability patch, and recreates the local
source archive.

The bundled engine is built with the compiler, optimisation, debugging,
architecture, hardening, and sanitizer settings R reports, but without
diagnostic-only flags such as `-Wall` and `-pedantic`: the vendored
third-party sources use documented compiler extensions whose warnings are not
actionable here. This package's own sources in `src/` are unaffected and are
still compiled with R's full flags. To reproduce the diagnostics CRAN's Debian
machines report, point `R_MAKEVARS_USER` at a file setting `CFLAGS` and
`CXX20FLAGS` to `-g -O2 -Wall -pedantic` and run `R CMD check --as-cran` on
the built tarball.

Runnable package examples are installed under `inst/examples`.

## License

MIT © rladybugdb authors. LadybugDB is distributed under its
[MIT License](https://github.com/LadybugDB/ladybug/blob/main/LICENSE).
