
<!-- README.md is generated from README.Rmd. -->

# sqlserverconnect

<!-- badges: start -->

<!-- badges: end -->

## Contents

- [Installation](#installation)
- [Quick start](#quick-start)
  - [Windows Authentication (trusted
    connection)](#windows-authentication-trusted-connection)
  - [Username + password
    authentication](#username--password-authentication)
- [Pooled connections](#pooled-connections)
- [DBI vs pool: when to use which?](#dbi-vs-pool-when-to-use-which)
- [Shiny Use](#shiny-use)
- [Why use sqlserverconnect?](#why-use-sqlserverconnect)
- [Built on](#built-on)

`sqlserverconnect` provides a minimal, user-friendly interface for
connecting to Microsoft SQL Server from R.

It wraps **DBI** (with the **odbc** driver) and optionally **pool** with
a small set of consistent helpers:

- `db_connect()` – create a DBI connection (default) or a connection
  pool
- `db_disconnect()` – safely close either a DBI connection or a pool

The goal is to offer a lightweight API without the repeated
setup/cleanup boilerplate that shows up in scripts and Shiny apps.

## Installation

You can install the development version of **sqlserverconnect** from
GitHub:

``` r
# install.packages("remotes")
remotes::install_github("drosenman/sqlserverconnect")
```

## Quick start

### Windows Authentication (trusted connection)

When using Windows Authentication, you typically don’t need `uid`/`pwd`.
Keep `trusted = TRUE` (the default).

``` r
library(sqlserverconnect)
library(DBI)

conn <- db_connect(
  server   = "localhost",
  database = "master"
)

DBI::dbGetQuery(conn, "SELECT TOP (5) name, create_date FROM sys.databases")

db_disconnect(conn)
```

### Username + password authentication

For SQL authentication, set `trusted = FALSE` and provide `uid` and
`pwd`.

**Tip:** avoid hardcoding passwords in scripts. Use environment
variables, a keyring, or another secret manager.

``` r
library(sqlserverconnect)
library(DBI)

conn <- db_connect(
  server   = "localhost",
  database = "master",
  uid      = Sys.getenv("SQLSERVER_UID"),
  pwd      = Sys.getenv("SQLSERVER_PWD"),
  trusted  = FALSE
)

DBI::dbGetQuery(conn, "SELECT TOP (5) name FROM sys.tables")

db_disconnect(conn)
```

## Pooled connections

`db_connect()` supports pooled connections via the **pool** package. Set
`pool = TRUE` to create a pool, or leave it as the default (`FALSE`) for
a regular DBI connection.

``` r
library(sqlserverconnect)
library(DBI)

pool <- db_connect(
  server   = "localhost",
  database = "master",
  pool     = TRUE
)

DBI::dbGetQuery(pool, "SELECT TOP (5) name FROM sys.databases")

db_disconnect(pool)
```

## DBI vs pool: when to use which?

- Use a plain DBI connection (`pool = FALSE`) for interactive scripts
  and short-lived jobs.
- Use a pool (`pool = TRUE`) for Shiny apps or long-running processes
  where you want connections managed and reused.

| Feature / Use case | `db_connect(pool = FALSE)` | `db_connect(pool = TRUE)` |
|----|----|----|
| Interactive scripts | Simple and direct | Usually unnecessary |
| Long-running jobs | May time out if idle | Better handling of idle / reused conns |
| Shiny apps | Risk of too many connections | Recommended best practice |
| Parallel workloads | Each worker opens its own conn | Pool can reuse connections (per process) |
| Cleanup | `db_disconnect()` | `db_disconnect()` |

## Shiny Use

In Shiny, create the pool once (at startup), reuse it everywhere, and
close it when the app stops.

``` r
# global.R (or at the top of app.R)
library(sqlserverconnect)

db_pool <- db_connect(
  server   = "localhost",
  database = "master",
  pool     = TRUE
)

onStop(function() {
  db_disconnect(db_pool)
})
```

## Why use sqlserverconnect?

- Minimal surface area (two main functions)
- Clear, explicit arguments
- Works with either a DBI connection or a pool
- Reduces boilerplate while staying close to DBI/odbc/pool

If you frequently connect to SQL Server from R, this package keeps your
workflow clean and consistent.

## Built on

`sqlserverconnect` is built on these packages:

- **DBI** – provides the database interface and `dbConnect()` generic
- **odbc** – provides the ODBC driver (`odbc::odbc()`) used by DBI to
  talk to SQL Server
- **pool** – optional connection pooling for DBI connections
  (recommended for Shiny apps)
